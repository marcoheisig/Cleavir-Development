(cl:in-package #:cleavir-development)

;;; Graphviz allows label specifications in a small subset of HTML. This
;;; file contains some utilities for the creation of labels in the
;;; S-expression based HTML notation of CL-DOT.

(defun html-table (&key caption rows)
  (let ((width (loop for row in rows
                     maximize (html-row-length row))))
    `(:html
      ()
      (:table
       ((:border "0")
        (:cellborder "0")
        (:cellspacing "0"))
       (:tr ()
            (:td ((:colspan ,(princ-to-string (max 1 width)))
                  (:align "center"))
                 ,caption))
       ,@rows))))

(defun html-row-length (row)
  (destructuring-bind (tr attributes &rest rows) row
    (check-type tr (eql :tr))
    (check-type attributes list)
    (length rows)))

(defun html-key-value-row (key value)
  `(:tr ()
        (:td ((:align "right"))
             (:b () ,key))
        (:td ((:align "left"))
             ,value)))
