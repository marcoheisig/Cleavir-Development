(cl:in-package #:cleavir-development)

;;; Graphviz allows label specifications in a small subset of HTML. The
;;; following functions simplify the creation of labels in the S-expression
;;; based HTML notation of CL-DOT.

(defun make-html-label (&key caption properties)
  `(:html
    ()
    (:table
     ((:border "0") (:cellborder "0") (:cellspacing "0"))
     (:tr () (:td ((:colspan "2") (:align "center")) ,caption))
     ,@(mapcar #'make-html-label-row properties))))

(defun make-html-label-row (property)
  (destructuring-bind (key . value) property
    (check-type key string)
    (check-type value string)
    `(:tr ()
          (:td ((:align "left"))
               (:b () ,key))
          (:td ((:align "left"))
               ,value))))

;;; The naming convention of some Cleavir classes is NAME-X, where X is the
;;; name of its direct superclass, e.g. INSTRUCTION or AST. We provide a
;;; function to remove such suffixes. The result is a lowercase string.

(defun strip-suffix (name suffix)
  (let ((name-string (string-downcase name))
        (suffix-string (string-downcase suffix)))
    (let ((start1 (- (length name-string)
                     (length suffix-string))))
      (if (string= name-string suffix-string :start1 start1)
          (subseq name-string 0 start1)
          name-string))))
