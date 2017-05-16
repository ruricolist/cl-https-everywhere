(in-package #:cl-https-everywhere)

(deftype parsed-rule ()
  '(tuple string string))

(deftype scanner ()
  '(or function null))

(defstruct-read-only (compiled-rule (:conc-name rule.))
  (from :type function)
  (to :type string))

(defstruct-read-only (ruleset
                      (:conc-name ruleset.)
                      (:constructor %make-ruleset))
  (name :type string)
  (targets :type list)
  (rules :type list)
  (exclusions :type scanner)
  (disabled nil :type boolean))

(defun make-ruleset (&key name targets rules exclusions disabled)
  (%make-ruleset :name name
                 :targets targets
                 :disabled disabled
                 :rules (mapply #'compile-rule rules)
                 :exclusions (compile-exclusions exclusions)))

#+sbcl (declaim (sb-ext:freeze-type compiled-rule))
#+sbcl (declaim (sb-ext:freeze-type ruleset))

(defmethod print-object ((self ruleset) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (ruleset.name self))
    (when (ruleset.disabled self)
      (format stream " DISABLED"))))

(defun excluded? (ruleset uri)
  (declare (type string uri))
  (etypecase-of scanner (ruleset.exclusions ruleset)
    (null nil)
    (function (ppcre:scan (ruleset.exclusions ruleset) uri))))

(defun literal-target? (target)
  (and (stringp target) (not (find #\* target))))

(defun parse-target (target)
  (check-type target string)
  (if (not (find #\* target))
      target
      `(:regex ,(~> target
                    (ppcre:regex-replace "^\\*" _ "^.+")
                    (ppcre:regex-replace "\\*$" _ "[^.]+$")
                    (ppcre:regex-replace "\\*" _ "[^.]+")))))

(defun uri-host (uri)
  (nth-value 2 (quri:parse-uri uri)))

(defun compile-rule (from to)
  (let ((scanner (ppcre:create-scanner from))
        (to (ppcre:regex-replace-all "\\$(\\d+)" to "\\\\\\1"))) ;ouch
    (make-compiled-rule :from scanner :to to)))

(defmethod apply-rule ((rule compiled-rule) string)
  (with-slots (from to) rule
    (ppcre:regex-replace from string to)))

(defun compile-exclusions (patterns)
  (if (null patterns) nil
      (ppcre:create-scanner
       (alternation (mapcar (op `(:regex ,_)) patterns)))))

(defun alternation (choices)
  (if (single choices)
      (first choices)
      `(:alternation ,@choices)))
