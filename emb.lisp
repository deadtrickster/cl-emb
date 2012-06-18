;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2004.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; Parts of the source are taken from LSP, written by
;;; John Wiseman and copyright 2001, 2002 I/NET Inc.
;;; (http://www.inetmi.com/)
;;; See lsp-LICENSE.txt


(in-package :cl-emb)

(defpackage :cl-emb-intern (:use :cl))

(defvar *function-package* (find-package :cl-emb-intern)
  "Package the emb function body gets interned to.")

(defvar *debug* nil
  "Debugging for CL-EMB.")

(defvar *locking-function* nil
  "Function to call to lock access to an internal hash table. Must accept
a function designator which must be called with the lock hold.")


(defmacro with-lock (&body body)
  "Locking all accesses to *functions*"
  `(cond (*locking-function*
           (funcall *locking-function* #'(lambda () ,@body)))
          (t ,@body)))
  
(defgeneric execute-emb (name &key env generator-maker)
  (:documentation "Execute named emb code. Returns a string. Keyword parameter ENV
to pass objects to the code. ENV must be a plist."))

(defmethod execute-emb ((name string) &key env generator-maker)
  (funcall (get-emb-function name) :env env :generator-maker generator-maker :name name))

(defmethod execute-emb ((name pathname) &key env generator-maker)
  (let ((fun (or (get-emb-function name)
                 (emb-function-function (register-emb name name)))))
    (funcall fun :env env :generator-maker generator-maker :name name)))

(defvar *functions* (make-hash-table :test #'equal)
  "Table mapping names to emb-function instances.")

(defclass emb-function ()
  ((path :initarg :path
         :accessor emb-function-path)
   (time :initarg :time
         :accessor emb-function-time)
   (function :initarg :function
             :accessor emb-function-function)
   (form :initarg :form
         :initform nil
         :accessor  emb-function-form)))

(defun make-emb-function (path time function &optional form)
  "Constructor for class EMB-FUNCTION."
  (make-instance 'emb-function
                 :path path
                 :time time
                 :function function
                 :form form))


(defun pprint-emb-function (name)
  "DEBUG function. Pretty prints function form, if *DEBUG* was t
when the function was registered."
  (with-lock 
    (pprint (emb-function-form (gethash name *functions*)))))


(defun clear-emb-all ()
  "Remove all registered emb code."
  (with-lock
    (clrhash *functions*)))

(defun clear-emb (name)
  "Remove named emb code."
  (with-lock
    (remhash name *functions*)))

(defun clear-emb-all-files ()
  "Remove all registered file emb code (registered/executed by a pathname)."
  (with-lock
    (maphash (lambda (key value) (declare (ignore value))
                     (when (typep key 'pathname) (remhash key *functions*)))
             *functions*)))

(defun get-emb-function (name)
  "Returns the named function implementing a registered emb code.
Rebuilds it when text template was a file which has been modified."
  (with-lock
    (let* ((emb-function (gethash name *functions*))
           (path (when emb-function (emb-function-path emb-function))))
      (cond ((and (not (typep name 'pathname)) (null emb-function))
             (error "Function ~S not found." name))
            ((null emb-function)
             (return-from get-emb-function))
            ((and path
                  (> (file-write-date path) (emb-function-time emb-function)))
             ;; Update when file is newer
             (multiple-value-bind (function form)
                 (construct-emb-function (contents-of-file path))
               (setf (emb-function-time emb-function) (file-write-date path)
                     (emb-function-function emb-function) function
                     (emb-function-form emb-function) form))))
      (emb-function-function emb-function))))


(defgeneric register-emb (name code)
  (:documentation "Register given CODE as NAME."))

(defmethod register-emb (name (code pathname))
  (multiple-value-bind (function form)
      (construct-emb-function (contents-of-file code))
    (with-lock
      (setf (gethash name *functions*)
            (make-emb-function code
                               (file-write-date code)
                               function
                               form)))))

(defmethod register-emb (name (code string))
  (multiple-value-bind (function form)
      (construct-emb-function code)
    (with-lock
      (setf (gethash name *functions*)
            (make-emb-function nil
                               (get-universal-time)
                               function
                               form)))))

(defvar *emb-start-marker* "<%"
  "Start of scriptlet or expression. Remember that a following #\=
indicates an expression.")

(defvar *emb-end-marker* "%>"
  "End of scriptlet or expression.")


(defparameter *set-special-list*
  '(("escape" . "cl-emb:*escape-type*")))

(defparameter *set-parameter-list*
  '(("xml" . ":xml")
    ("html" . ":html")
    ("url" . ":url")
    ("uri" . ":uri")
    ("url-encode" . ":url-encode")
    ("raw" . ":raw")))

;; TODO: Refactor! Looks a bit clumsy.
(defun set-specials (match registers)
  "Parse parameter(s) of @set and set special variables
like e. g. *ESCAPE-TYPE*."
  ;;  <% @set escape=xml schnuffel=poe %>
  (declare (ignore match))
  (let ((setf-pairs
         (let ((setf-list nil))
           (dolist (pair (cl-ppcre:split "\\s+" (first registers))
                    (when (first setf-list)
                      (format nil "~{ ~A~}" (reverse setf-list))))
             (destructuring-bind (left right)
                 (cl-ppcre:split "=" pair)
               (let ((place (rest (assoc left *set-special-list* :test #'equalp)))
                     (value (rest (assoc right *set-parameter-list* :test #'equalp))))
                 (when (and place value)
                   (push (concatenate 'string place " " value) setf-list))))))))
    (if setf-pairs
        (format nil "(setf ~A)" setf-pairs)
        "")))

(defparameter *template-tag-expand*
  `(("\\s+@if\\s+(\\S+)\\s*"      . " (cond ((cl-emb::getf-emb \"\\1\") ")
    ("\\s+@else\\s*"              . " ) (t ")
    ("\\s+@endif\\s*"             . " )) ")
    ("\\s+@unless\\s+(\\S+)\\s*"  . " (cond ((not (cl-emb::getf-emb \"\\1\")) ")
    ("\\s+@endunless\\s*"         . " )) ")
    ("=?\\s+@var\\s+(\\S+)\\s+-(\\S+)\\s+(\\S+)\\s*"
     . "= (cl-emb::echo (cl-emb::getf-emb \"\\1\") :\\2 :\\3) ")
    ("=?\\s+@var\\s+(\\S+)\\s*"   . "= (cl-emb::echo (cl-emb::getf-emb \"\\1\")) ")
    ("\\s+@repeat\\s+(\\d+)\\s*"  . " (dotimes (i \\1) ")
    ("\\s+@repeat\\s+(\\S+)\\s*"  . " (dotimes (i (or (cl-emb::getf-emb \"\\1\") 0)) ")
    ("\\s+@endrepeat\\s*"         . " ) ")
    ("\\s+@loop\\s+(\\S+)\\s*"    . " (dolist (env (cl-emb::getf-emb \"\\1\")) ")
    ("\\s+@endloop\\s*"           . " ) ")
    ("\\s+@genloop\\s+(\\S+)\\s*" . " (let ((env) 
                                            (%gen (funcall generator-maker :\\1 
                                                           (cl-emb::getf-emb \"\\1\"))))
                                           (loop
                                            (when (funcall %gen :test) (return))
                                            (setq env (funcall %gen :next))
                                            (progn ")
    ("\\s+@endgenloop\\s*"        . " ))) ")
    ("\\s+@with\\s+(\\S+)\\s*"    . " (let ((env (cl-emb::getf-emb \"\\1\"))) ")
    ("\\s+@endwith\\s*"           . " ) ")
    ("\\s+@include\\s+(\\S+)\\s*" . "= (let ((cl-emb:*escape-type* cl-emb:*escape-type*))
                                            (cl-emb:execute-emb (merge-pathnames \"\\1\" template-path-default) :env env :generator-maker generator-maker)) ")
    ("\\s+@call\\s+(\\S+)\\s*"    . "= (let ((cl-emb:*escape-type* cl-emb:*escape-type*))
                                            (cl-emb:execute-emb \"\\1\" :env env :generator-maker generator-maker)) ")
    ("\\s+@insert\\s+(\\S+)\\s*"  . "= (cl-emb::contents-of-file (merge-pathnames (cl-emb::getf-emb \"\\1\") template-path-default)) ")
    ("\\s+@set\\s+(.*?)\\s*"      . ,(function set-specials))
    )
  "List of conses. FIRST is regex, REST replacement (STRING or FUNCTION).
Functions get called with two parameters: match and list of registers.")

;; Code from Edi Weitz's TBNL <http://weitz.de/tbnl/>
(defun escape-for-xml (string)
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            do (case char
                 ((#\<) (write-string "&lt;" out))
                 ((#\>) (write-string "&gt;" out))
                 ((#\") (write-string "&quot;" out))
                 ((#\') (write-string "&#39;" out))
                 ((#\&) (write-string "&amp;" out))
                 (otherwise (write-char char out)))))))

;; Inspired by Edi Weitz' ESCAPE-FOR-HTML
(defun url-encode (string)
  "URL-encode a string."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop for char = (read-char in nil nil)
            while char
            if (find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-.")
               do (write-char char out)
            else if (char= char #\Space)
                    do (write-char #\+ out)
                 else
                    do (format out "%~2,'0x" (char-code char))))))


(defun string-to-keyword (string)
  "Interns a given STRING uppercased in the keyword package."
  (nth-value 0 (intern (string-upcase string) :keyword)))

(defmacro getf-emb (key)
  "Search either plist TOPENV or ENV according to the search path in KEY. KEY
is a string."
  (let ((plist (if (char= (char key 0) #\/)
                   (find-symbol "TOPENV" emb:*function-package*)
                   (find-symbol "ENV" emb:*function-package*)))
        (path-parts (cl-ppcre:split "/" key :sharedp t)))
    (labels ((dig-plist (plist keys)
               (if (null keys)
                   plist
                   (dig-plist
                    (if (zerop (length (first keys)))
                        plist
                        `(getf ,plist ,(string-to-keyword (first keys))))
                    (rest keys)))))
      (dig-plist plist path-parts))))


(defvar *escape-type* :raw
  "Default value for escaping @var output.")

(defun echo (string &key (escape *escape-type*))
  "Emit given STRING. Escape if wanted (global or via ESCAPE keyword).
STRING can be NIL."
  (let ((str (or string "")))
    (case escape 
      ((:html :xml)
       (escape-for-xml str))
      ((:url :uri :url-encode)
       (url-encode str))
      (otherwise                        ; incl. :raw
       str))))

(defun insert-file (filename)
  "Get given file FILENAME."
  (contents-of-file filename))

(defmethod make-replace ((replacement string))
  "Make replacement string for PPCRE:REGEX-REPLACE-ALL"
  (concatenate 'string
               *emb-start-marker*
               replacement
               *emb-end-marker*))

(let ((scanner-hash (make-hash-table :test #'equal)))
  (defun scanner-for-expand-template-tag (tag)
    "Returns a CL-PPCRE scanner which matches a template tag expanded by EXPAND-TEMPLATE-TAGS.
Scanners are memoized in SCANNER-HASH once they are created."
    (or (gethash tag scanner-hash)
        (setf (gethash tag scanner-hash)
              (ppcre:create-scanner tag))))
  (defun clear-expand-template-tag-hash ()
    "Removes all scanners for template tags from cache."
    (clrhash scanner-hash)))

(defmethod make-replace ((replacement function))
  "Make replacement function for PPCRE:REGEX-REPLACE-ALL"
  (lambda (match &rest registers)
    (concatenate 'string
                 *emb-start-marker*
                 (funcall replacement match registers)
                 *emb-end-marker*)))
  
(defun expand-template-tags (string)
  "Expand template-tags (@if, @else, ...) to Common Lisp.
Replacement and regex in *TEMPLATE-TAG-EXPAND*"
  (labels ((expand-tags (string &optional (expands *template-tag-expand*))
             (let ((regex (scanner-for-expand-template-tag (concatenate 'string
                                       "(?is)"
                                       (ppcre:quote-meta-chars *emb-start-marker*)
                                       (first (first expands))
                                                                        (ppcre:quote-meta-chars *emb-end-marker*))))
                   (replacement (make-replace (rest (first expands)))))
               (if (null (rest expands))
                   (ppcre:regex-replace-all regex string replacement :simple-calls t)
                   (expand-tags
                    (ppcre:regex-replace-all regex string replacement :simple-calls t)
                    (rest expands))))))
    (expand-tags
     ;; First remove code within comments
     (ppcre:regex-replace-all
      (scanner-for-expand-template-tag (concatenate 'string "(?is)" (ppcre:quote-meta-chars *emb-start-marker*)
                                                    "#.*?#" (ppcre:quote-meta-chars *emb-end-marker*)))
      string ""))))

(defun construct-emb-function (code)
  "Builds and compiles the emb-function."
  (let ((form 
         `,(let ((*package* *function-package*))
                (read-from-string
                 (format nil "(lambda (&key env generator-maker name)(declare (ignorable env generator-maker))
                               (let ((topenv env)
                                     (template-path-default (if (typep name 'pathname) name *default-pathname-defaults*)))
                                    (declare (ignorable topenv template-path-default))
                                (with-output-to-string (*standard-output*)
                                (progn ~A))))"
                         (construct-emb-body-string
                          (expand-template-tags code)))))))
    (values (compile nil form)
            (when *debug* form))))

(defun contents-of-file (pathname)
  "Returns a string with the entire contents of the specified file."
  (with-open-file (in pathname :direction :input)
    ;; See http://www.emmett.ca/~sabet/licensets/slurp.html
    (let* ((file-length (file-length in))
           (seq (make-string file-length))
           (pos (read-sequence seq in)))
      (if (< pos file-length)
          (subseq seq 0 pos)
          seq))))

;; (i) Converts text outside <% ... %> tags into calls
;; to WRITE-STRING, (ii) Text inside <% ... %>
;; ("scriptlets") is straight lisp code, (iii) Text inside <%= ... %>
;; ("expressions") becomes the argument to (FORMAT t "~A" ...)
;; The markers <% and %> can be overridden by setting
;; *emb-start-marker* and *emb-end-marker*
(defun construct-emb-body-string (code &optional (start 0))
  "Takes a string containing an emb code and returns a string
containing the lisp code that implements that emb code."
  (multiple-value-bind (start-tag start-code tag-type)
      (next-code code start)
    (if (not start-tag)
      (format nil "(write-string ~S)" (subseq code start))
      (let ((end-code (search *emb-end-marker* code :start2 start-code)))
	(if (not end-code)
	  (error "EOF reached in EMB inside open '~A' tag." *emb-start-marker*)
	  (format nil "(write-string ~S) ~A ~A"
		  (subseq code start start-tag)
		  (format nil (tag-template tag-type)
			  (subseq code start-code end-code))
		  (construct-emb-body-string
                   code
                   (+ end-code (length *emb-end-marker*)))))))))


;; Finds the next scriptlet or expression tag in EMB source.  Returns
;; nil if none are found, otherwise returns 3 values:
;;  1. The position of the first character of the start tag.
;;  2. The position of the contents of the tag.
;;  3. The type of tag (:scriptlet or :expression).
(defun next-code (string start)
  (let ((start-tag (search *emb-start-marker* string :start2 start)))
    (if (not start-tag)
      nil
      (if (and (> (length string) (+ start-tag (length *emb-start-marker*)))
	       (eql (char string (+ start-tag (length *emb-start-marker*)))
                    #\=))
	(values start-tag
                (+ start-tag 1 (length *emb-start-marker*))
                :expression)
	(values start-tag
                (+ start-tag (length *emb-start-marker*))
                :scriptlet)))))


;; Given a tag type (:scriptlet or :expression), returns a format
;; string to be used to generate source code from the contents of the
;; tag.
(defun tag-template (tag-type)
  (ecase tag-type
    ((:scriptlet) "~A")
    ((:expression) "(format t \"~~A\" ~A)")))

