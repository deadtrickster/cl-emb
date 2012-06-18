;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; This software is Copyright (c) Stefan Scholl, 2004.
;;; Stefan Scholl grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.


(in-package #:cl-user)

(defpackage #:cl-emb.system
  (:use #:cl
        #:asdf))

(in-package #:cl-emb.system)

(defsystem #:cl-emb
    :version "0.4.3"
    :author "Stefan Scholl <stesch@no-spoon.de>"
    :licence "Lesser Lisp General Public License"
    :depends-on (#:cl-ppcre)
    :components ((:file "packages")
                 (:file "emb" :depends-on ("packages"))))
