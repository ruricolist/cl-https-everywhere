;;;; cl-https-everywhere.asd
(in-package #:asdf-user)

(asdf:defsystem #:cl-https-everywhere
  :description "Use HTTPS Everywhere rules from Lisp."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:cl-ppcre
               #:cxml
               #:uiop
               #:quri
               #:cl-tld)
  :components ((:file "package")
               (:static-file "rulesets.xml")
               (:file "rulesets" :depends-on ("package"))
               (:file "compiler" :depends-on ("rulesets"))
               (:file "lang" :depends-on ("compiler"))
               (:file "build" :depends-on ("lang"))
               (:file "rewrite" :depends-on ("build"))))

