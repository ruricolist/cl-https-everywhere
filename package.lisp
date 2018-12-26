;;;; package.lisp

(defpackage #:cl-https-everywhere
  (:use #:cl #:alexandria #:serapeum #:overlord
    #:overlord/net)
  (:nicknames #:https-everywhere)
  (:shadow :if)
  (:export
   #:rewrite-uri))

(in-package :cl-https-everywhere)

(defmacro if (test then else)
  `(cl:if ,test ,then ,else))
