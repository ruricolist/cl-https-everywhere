(in-package #:cl-https-everywhere)

(overlord:define-constant +rules-dir+
    (merge-pathnames "https-everywhere/rules/")
  :test #'equal)

(overlord:defconst/deps +ruleset-files+
    ;; uiop:directory-files is too slow
    (directory
     (make-pathname :defaults +rules-dir+
                    :type "xml"
                    :name :wild))
  (:depends-on '+rules-dir+)
  (unless (uiop:directory-exists-p +rules-dir+)
    (uiop:run-program "git submodule update --init --depth=1")
    (assert (uiop:directory-exists-p +rules-dir+)))
  (:depends-on +rules-dir+))

(overlord:file-target rulesets "rulesets.xml" (temp)
  (with-output-to-file (out temp :if-exists :rename-and-delete
                                 :external-format :utf-8)
    (format t "~&Concatenating rulesets.xml.~%")
    (format out "<rulesets>~%")
    (dolist (file +ruleset-files+)
      (with-input-from-file (in file :external-format :utf-8)
        (copy-stream in out)))
    (format out "~%</rulesets>"))
  (:depends-on '+ruleset-files+)
  (:depends-on +ruleset-files+))

(defparameter *rulesets*
  (overlord:require-as :cl-https-everywhere/rulesets-file "rulesets"))
