(in-package #:cl-https-everywhere)

(overlord:defconfig +https-everywhere-repo+
  "https://github.com/EFForg/https-everywhere.git")

(overlord:file-target https-everywhere-version
    "https-everywhere/git-version.txt"
    (in)
  (:always t)
  (if (uiop:directory-exists-p #p".git/")
      (overlord/net:online-only ()
        ;; Updating doesn't matter that much.
        (ignore-errors
         (:message "Updating rules...")
         (:cmd "git fetch --depth 1")
         (:cmd "git reset --hard origin/master")))
      (progn
        (:message "Fetching rules...")
        (:cmd "git clone --depth=1 --"
              +https-everywhere-repo+
              ".")))
  (if (uiop:directory-exists-p ".git/")
      (let ((version
              (trim-whitespace
               (:cmd '(:output :string) "git rev-parse HEAD"))))
        (overlord:write-file-if-changed version in))
      (uiop:delete-file-if-exists in)))

(overlord:defvar/deps *ruleset-files*
    ;; uiop:directory-files is too slow
    (directory
     (:path "https-everywhere/src/chrome/content/rules/*.xml"))
  (:depends-on '+https-everywhere-repo+)
  (:depends-on https-everywhere-version))

(overlord:file-target rulesets "rulesets.xml" (temp)
  (:depends-on '*ruleset-files*)
  (:depends-on-all *ruleset-files*)
  (with-output-to-file (out temp :if-exists :rename-and-delete
                                 :external-format :utf-8)
    (progn
      (:message "Concatenating rulesets.xml...")
      (format out "<rulesets>~%")
      (dolist (file *ruleset-files*)
        (with-input-from-file (in file :external-format :utf-8)
          (copy-stream in out)))
      (format out "~%</rulesets>"))))

(defun clean ()
  (let ((base (asdf:system-relative-pathname :cl-https-everywhere "")))
    (uiop:delete-directory-tree
     (path-join base #p "https-everywhere/")
     :validate (op (uiop:subpathp _ base)))))

(defun maintainer-clean ()
  (clean)
  (uiop:delete-file-if-exists rulesets))

(defparameter *rulesets*
  (overlord:require-as :cl-https-everywhere/rulesets-file "rulesets"))
