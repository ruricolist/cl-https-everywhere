(in-package #:cl-https-everywhere)

(defmacro with-feedback ((start end) &body body)
  `(progn
     (format t "~&~@?" ,start)
     (force-output)
     (multiple-value-prog1 (progn ,@body)
       (format t "~@?~%" ,end)
       (force-output))))

(overlord:define-constant +https-everywhere-repo+
  "https://github.com/EFForg/https-everywhere.git")

(overlord:directory-target https-everywhere "https-everywhere/"
  (progn
    (when (uiop:directory-exists-p https-everywhere)
      (uiop:delete-directory-tree https-everywhere :validate t))
    (with-feedback ("Fetching rules..." "Fetched rules")
      (:run `("git" "clone" "--depth=1" ,+https-everywhere-repo+))))
  (:depends-on '+https-everywhere-repo+))

(overlord:defvar/deps *ruleset-files*
    ;; uiop:directory-files is too slow
    (progn
      (overlord/http:online-only ()
        (with-feedback ("Updating rules..." "Rules updated.")
          (:run
           "cd https-everywhere; git fetch --depth 1; git reset --hard origin/master")))
      (directory
       (path-join https-everywhere
                  #p"rules/"
                  (make-pathname :type "xml" :name :wild))))
  (:depends-on '+https-everywhere-repo+)
  (:depends-on https-everywhere))

(overlord:file-target rulesets "rulesets.xml" (temp)
  (with-output-to-file (out temp :if-exists :rename-and-delete
                                 :external-format :utf-8)
    (with-feedback ("Concatenating rulesets.xml..." "Built rulesets.xml")
      (format out "<rulesets>~%")
      (dolist (file *ruleset-files*)
        (with-input-from-file (in file :external-format :utf-8)
          (copy-stream in out)))
      (format out "~%</rulesets>")))
  (:depends-on '*ruleset-files*)
  (:depends-on *ruleset-files*))

(overlord:deftask clean ()
  (uiop:delete-directory-tree
   #p"https-everywhere/"
   :validate (op (uiop:subpathp _ (asdf:system-relative-pathname :cl-https-everywhere "")))))

(overlord:deftask maintainer-clean ()
  (uiop:delete-file-if-exists rulesets)
  (:depends-on 'clean))

(defparameter *rulesets*
  (overlord:require-as :cl-https-everywhere/rulesets-file "rulesets"))
