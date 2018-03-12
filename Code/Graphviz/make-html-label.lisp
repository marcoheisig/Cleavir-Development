(cl:in-package #:cleavir-development)

;;; Graphviz allows label specifications in a small subset of HTML. The
;;; following functions simplify the creation of labels in the S-expression
;;; based HTML notation of CL-DOT.

(defun make-html-label-row (property)
  (destructuring-bind (key . value) property
    (check-type key string)
    (check-type value string)
    `(:tr ()
          (:td ((:align "left"))
               (:b () ,key))
          (:td ((:align "left"))
               ,value))))

(defun make-html-label (&key caption properties)
  `(:html
    ()
    (:table
     ((:border "0") (:cellborder "0") (:cellspacing "0"))
     (:tr () (:td ((:colspan "2") (:align "center")) ,caption))
     ,@(mapcar #'make-html-label-row properties))))
