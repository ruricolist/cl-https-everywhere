;;;; package.lisp

(defpackage #:cl-https-everywhere
  (:use #:cl #:alexandria #:serapeum)
  (:nicknames #:https-everywhere)
  (:export
   #:rewrite-uri))

