(in-package #:cl-https-everywhere)

(overlord:defconfig +https-everywhere-repo+
  "https://github.com/EFForg/https-everywhere.git")

(overlord:file-target https-everywhere "https-everywhere/Makefile" ()
  (let* ((dir #p"https-everywhere/")
         (opts `(:directory ,dir)))
    (if (uiop:directory-exists-p dir)
        ;; Updating doesn't matter that much.
        (overlord/http:online-only ()
          (ignore-errors
           (:message "Updating rules...")
           (:cmd opts "git fetch --depth 1")
           (:cmd opts "git reset --hard origin/master")))
        (progn
          (:message "Fetching rules...")
          (:cmd opts "git clone --depth=1" +https-everywhere-repo+)))))

(overlord:defvar/deps *ruleset-files*
    ;; uiop:directory-files is too slow
    (directory
     (:path "https-everywhere/src/chrome/content/rules/*.xml"))
  (:depends-on '+https-everywhere-repo+)
  (:depends-on https-everywhere))

(overlord:file-target rulesets "rulesets.xml" (temp)
  (with-output-to-file (out temp :if-exists :rename-and-delete
                                 :external-format :utf-8)
    (progn
      (:message "Concatenating rulesets.xml...")
      (format out "<rulesets>~%")
      (dolist (file *ruleset-files*)
        (with-input-from-file (in file :external-format :utf-8)
          (copy-stream in out)))
      (format out "~%</rulesets>")))
  (:depends-on '*ruleset-files*)
  (:depends-on-all *ruleset-files*))

(overlord:deftask clean ()
  (uiop:delete-directory-tree
   #p"https-everywhere/"
   :validate (op (uiop:subpathp _ (asdf:system-relative-pathname :cl-https-everywhere "")))))

(overlord:deftask maintainer-clean ()
  (uiop:delete-file-if-exists rulesets)
  (:depends-on 'clean))

(defparameter *rulesets*
  (overlord:require-as :cl-https-everywhere/rulesets-file "rulesets"))
