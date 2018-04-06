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

(overlord:file-target rulesets "rulesets.xml" (temp)
  (:depends-on '+https-everywhere-repo+)
  (:depends-on https-everywhere-version)
  (let ((files
          ;; uiop:directory-files is too slow
          (directory
           (:path "https-everywhere/src/chrome/content/rules/*.xml"))))
    (with-output-to-file (out temp :if-exists :rename-and-delete
                                   :external-format :utf-8)
      (progn
        (:message "Concatenating rulesets.xml...")
        (format out "<rulesets>~%")
        (do-each (file files)
          (with-input-from-file (in file :external-format :utf-8)
            (copy-stream in out)))
        (format out "~%</rulesets>")))))

(defparameter *rulesets*
  (overlord:require-default :cl-https-everywhere/rulesets-file "rulesets"))
