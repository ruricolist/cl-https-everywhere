(in-package #:cl-https-everywhere)

(deftype parsed-rule ()
  '(cons string (cons string null)))

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

(defmethod initialize-instance :after ((self ruleset) &key)
  (with-slots (rules exclusions raw-rules raw-exclusions) self
    (setf rules (mapply #'compile-rule raw-rules)
          exclusions (compile-exclusions raw-exclusions))))

(defmethod make-load-form ((self ruleset) &optional env)
  (declare (ignore env))
  (with-slots (name targets raw-exclusions raw-rules disabled) self
    `(make 'ruleset
           :name ,name
           :targets ',targets
           :raw-rules ',raw-rules
           :raw-exclusions ',raw-exclusions
           :disabled ,disabled)))

(defmethod print-object ((self ruleset) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (ruleset.name self))
    (when (ruleset.disabled? self)
      (format stream " DISABLED"))))

(defmethod excluded? ((ruleset ruleset) (uri string))
  (with-slots (exclusions) ruleset
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
