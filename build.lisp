(in-package #:cl-https-everywhere)

(loom:define-constant +rules-dir+
  (merge-pathnames "https-everywhere/rules/")
  :test #'equal)

(loom:defconst/deps +ruleset-files+
    ;; uiop:directory-files is too slow
    (directory
     (make-pathname :defaults +rules-dir+
                    :type "xml"
                    :name :wild))
  (:depends-on '+rules-dir+)
  (unless (uiop:directory-exists-p +rules-dir+)
    (:sh "git submodule update --init --depth=1"))
  (:depends-on +rules-dir+))

(loom:file-target "rulesets.xml"
    (with-output-to-file (out loom:*target*
                              :if-exists :rename-and-delete)
      (:echo "Concatenating rulesets.xml.")
      (format out "<rulesets>~%")
      (dolist (file +ruleset-files+)
        (with-input-from-file (in file)
          (copy-stream in out)))
      (format out "~%</rulesets>"))
  (:depends-on '+ruleset-files+)
  (dolist (file +ruleset-files+)
    (:depends-on file)))

(defparameter *rulesets*
  (loom:require-as 'rulesets-file "rulesets"))


