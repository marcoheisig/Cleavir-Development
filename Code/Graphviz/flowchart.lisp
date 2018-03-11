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
           (make-instance 'edge
             :object successor))
         (make-data-arc (output)
           (make-instance 'edge
             :object output
             :style :dashed)))
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
