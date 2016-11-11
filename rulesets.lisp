(in-package #:cl-https-everywhere)

(deftype parsed-rule ()
  '(tuple string string))

(deftype scanner ()
  '(or function null))

(defclass compiled-rule ()
  ((from :initarg :from :type function :reader rule.from)
   (to :initarg :to :type string :reader rule.to)))

(defclass ruleset ()
  ((name :type string :initarg :name
         :reader ruleset.name)
   (targets :type list :initarg :targets :reader ruleset.targets)
   (raw-rules :type list :initarg :raw-rules :reader ruleset.raw-rules)
   (rules :type list :initarg :rules :reader ruleset.rules)
   (raw-exclusions :type list :initarg :raw-exclusions :reader ruleset.raw-exclusions)
   (exclusions :type scanner :initarg :exclusions)
   (disabled :type boolean :initform nil :initarg :disabled
             :reader ruleset.disabled?)))

(defmethods ruleset
    (self name targets raw-rules rules exclusions raw-exclusions disabled)
  (:method initialize-instance :after (self &key)
    (setf rules (mapply #'compile-rule raw-rules)
          exclusions (compile-exclusions raw-exclusions)))

  (:method make-load-form (self &optional env)
    (declare (ignore env))
    `(make 'ruleset
           :name ,name
           :targets ',targets
           :raw-rules ',raw-rules
           :raw-exclusions ',raw-exclusions
           :disabled ,disabled))

  (:method print-object (self stream)
    (print-unreadable-object (self stream :type t)
      (format stream "~a" name)
      (when disabled
        (format stream " DISABLED"))))

  (:method excluded? (self (uri string))
    (etypecase-of scanner exclusions
      (null nil)
      (function (ppcre:scan exclusions uri)))))

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
    (make 'compiled-rule :from scanner :to to)))

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
