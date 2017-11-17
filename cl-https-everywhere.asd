;;;; cl-https-everywhere.asd
(in-package #:asdf-user)

(asdf:defsystem #:cl-https-everywhere
  :description "Use HTTPS Everywhere rules from Lisp."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:serapeum
               #:cl-ppcre
               #:fxml
               #:uiop
               #:quri
               #:cl-tld
               #:overlord
               #:overlord/net)
  :components ((:file "package")
               (:file "rulesets" :depends-on ("package"))
               (:file "compiler" :depends-on ("rulesets"))
               (:file "lang" :depends-on ("compiler"))
               (:file "build" :depends-on ("lang"))
               (:file "rewrite" :depends-on ("build"))))

