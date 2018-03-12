(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass flowchart (graph)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-outgoing-edges append
    ((graph flowchart) (instruction cleavir-ir:instruction))
  (flet ((make-control-arc (successor)
           (make-edge successor))
         (make-data-arc (output)
           (make-edge output :style :dashed)))
    (append
     (mapcar #'make-control-arc (cleavir-ir:successors instruction))
     (mapcar #'make-data-arc (cleavir-ir:outputs instruction)))))

(defmethod graphviz-known-objects append
    ((graph flowchart) (instruction cleavir-ir:instruction))
  (declare (ignore graph))
  (append
   (cleavir-ir:predecessors instruction)
   (cleavir-ir:inputs instruction)))

(defmethod graphviz-known-objects append
    ((graph flowchart) (datum cleavir-ir:datum))
  (append
   (cleavir-ir:defining-instructions datum)
   (cleavir-ir:using-instructions datum)))
