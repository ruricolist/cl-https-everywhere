(in-package #:cl-https-everywhere)

(defclass rulesets-compiler (fxml.sax:default-handler)
  ((compiler :accessor compiler)
   (rulesets :initform nil :accessor rulesets)))

(defmethods rulesets-compiler (self rulesets compiler)
  (:method fxml.sax:end-document (self)
    (nreverse rulesets))
  (:method fxml.sax:start-element (self ns lname qname attrs)
    (string-case lname
      ("rulesets")
      ("ruleset"
       (let ((c (make 'ruleset-compiler)))
         (setf compiler c)
         (fxml.sax:start-element c ns lname qname attrs)))
      (t (fxml.sax:start-element compiler ns lname qname attrs))))
  (:method fxml.sax:end-element (self ns lname qname)
    (declare (ignore ns qname))
    (when (equal lname "ruleset")
      (let ((ruleset (fxml.sax:end-document compiler)))
        (push ruleset rulesets))
      (slot-makunbound self 'compiler))))

(defclass ruleset-compiler (fxml.sax:default-handler)
  ((name :type string)
   (targets :initform nil)
   (rules :initform nil)
   (exclusions :initform nil)
   (disabled :initform nil)))

(defmacro with-attributes (binds attrs &body body)
  "Given a list of (var name) bindings for attributes in ATTRS, do the
bindings, iterating over the list of ATTRS only once."
  (let ((binds (loop for bind in binds
                     if (symbolp bind)
                       collect (list bind (string-downcase bind))
                     else collect bind)))
    (once-only (attrs)
      `(let ,(mapcar #'car binds)
         ;; Do the bindings.
         ,(with-gensyms (a)
            `(dolist (,a ,attrs)
               (string-case (fxml.sax:attribute-local-name ,a)
                 ,@(loop for (sym name) in binds
                         collect `(,name (setf ,sym (fxml.sax:attribute-value ,a)))))))
         ,@body))))

(defmethods ruleset-compiler
    (self name disabled targets rules exclusions)
  (:method fxml.sax:end-document (self)
    (unless rules
      (error "No rules"))
    (make-ruleset :disabled disabled
                  :name name
                  :targets targets
                  :rules rules
                  :exclusions exclusions))
  (:method fxml.sax:start-element (self ns lname qname attrs)
    (declare (ignore ns qname))
    (string-case lname
      ("ruleset"
       (with-attributes ((name "name") (off "default_off")) attrs
         (when off
           (setf disabled t))
         (setf (slot-value self 'name) name)))
      ("target"
       (with-attributes (host) attrs
         (push host targets)))
      ("rule"
       (with-attributes (from to) attrs
         (push (list from to) rules)))
      ("exclusions"
       (with-attributes (pattern) attrs
         (push pattern exclusions))))))

(defun compile-ruleset-file (file)
  (fxml:parse (pathname file) (make-ruleset-compiler)))

(defun compile-rulesets-file (file)
  (fxml:parse (pathname file) (make-rulesets-compiler)))

(defun compile-rulesets-stream (stream)
  (check-type stream stream)
  (fxml:parse stream (make-rulesets-compiler)))

(defun make-ruleset-compiler ()
  (make 'ruleset-compiler))

(defun make-rulesets-compiler ()
  (make 'rulesets-compiler))
