(in-package #:cl-https-everywhere)

(defconfig +https-everywhere-repo+
    "https://github.com/EFForg/https-everywhere.git")

(file-target https-everywhere-version
    (:path "https-everywhere/git-version.txt"
     :dest dest)
  (redo-always)
  (if (uiop:directory-exists-p #p".git/")
      (overlord/net:online-only ()
        ;; Updating doesn't matter that much.
        (ignore-errors
         (message "Updating rules...")
         (cmd "git fetch --depth 1")
         (cmd "git reset --hard origin/master")))
      (progn
        (message "Fetching rules...")
        (cmd "git clone --depth=1 --"
             +https-everywhere-repo+
             ".")))
  (if (uiop:directory-exists-p ".git/")
      (let ((version
              (trim-whitespace
               ($cmd "git rev-parse HEAD"))))
        (write-file-if-changed version dest))
      (uiop:delete-file-if-exists dest)))

(file-target rulesets (:path "rulesets.xml" :out temp)
  (depends-on '+https-everywhere-repo+)
  (depends-on https-everywhere-version)
  (let* ((pat
           (make-pathname
            :directory '(:relative "https-everywhere" "src" "chrome" "content" "rules")
            :name :wild
            :type "xml"))
         ;; uiop:directory-files is too slow
         (files (directory pat)))
    (unless files
      (error "No rules in ~a" pat))
    (with-output-to-file (out temp :external-format :utf-8)
      (progn
        (message "Concatenating rulesets.xml...")
        (format out "<rulesets>~%")
        (do-each (file files)
          (with-input-from-file (in file :external-format :utf-8)
            (copy-stream in out)))
        (format out "~%</rulesets>")))))

(defparameter *rulesets*
  (vernacular:require-default :cl-https-everywhere/rulesets-file "rulesets.xml"))
