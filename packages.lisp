;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2004.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package #:cl-user)

(defpackage #:cl-emb
  (:nicknames #:emb)
  (:use #:cl)
  (:export #:execute-emb
           #:register-emb
           #:pprint-emb-function
           #:clear-emb
           #:clear-emb-all
           #:clear-emb-all-files
           #:clear-expand-template-tag-hash
           #:*debug*
           #:*emb-start-marker*
           #:*emb-end-marker*
           #:*escape-type*
           #:*locking-function*
           #:*function-package*))
