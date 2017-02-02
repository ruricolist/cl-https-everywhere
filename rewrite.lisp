(in-package #:cl-https-everywhere)

(defun permute-host (host)
  "The (undocumented!) logic HTTPS Everywhere uses to match
wildcards."
  (assert (not (find #\/ host)))
  (let ((segments (split-sequence #\. host)))
    (append (loop for i from 0 below (length segments)
                  collect (with-output-to-string (s)
                            (loop for j from 0
                                  for (segment . more) on segments
                                  do (if (= i j)
                                         (write-char #\* s)
                                         (write-string segment s))
                                     (when more
                                       (write-char #\. s)))))
            (loop for tail on (cddr segments)
                  while (length> tail 2)
                  collect (concat "*." (string-join tail "."))))))

(defun get-rulesets (uri)
  (let ((host (string-downcase (uri-host uri))))
    (append (gethash host *rulesets*)
            (mappend (op (gethash _ *rulesets*))
                     (permute-host host)))))

(defun rewrite-uri (uri)
  "Rewrite URI to use HTTPS, if possible.

Returns three values:

- The possibly rewritten URI (a string);
- Whether the URI returned is HTTPS;
- And whether any rewriting was done.

Three values are necessary to distinguish the case where the URI
passed in was *already* an HTTPS URI."
  (setf uri (coerce (trim-whitespace uri) 'simple-string))
  (handler-case
      (let ((scheme (uri-scheme uri)))
        (if (equal scheme "https")
            (values uri t nil)
            (let ((uri2 (rewrite-uri-1 uri)))
              (if (equal uri uri2)
                  (values uri2 nil nil)
                  (values uri2 t t)))))
    (quri:uri-error ()
      (values uri nil nil))))

(defun rewrite-uri-1 (uri)
  (reduce (lambda (uri ruleset)
            (if (excluded? ruleset uri)
                uri
                (reduce (lambda (uri rule)
                          (apply-rule rule uri))
                        (ruleset.rules ruleset)
                        :initial-value uri)))
          (get-rulesets uri)
          :initial-value uri))

(defun uri-scheme (uri)
  (values (quri:parse-uri uri)))

(assert (equal '("http://example.com/" nil nil)
               (multiple-value-list
                (rewrite-uri "http://example.com/"))))

(assert (equal '("https://www.eff.org/" t nil)
               (multiple-value-list
                (rewrite-uri "https://www.eff.org/"))))
