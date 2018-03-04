(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass flowchart (graph)
  ())

(defclass flowchart-edge (edge)
  ())

(defclass control-arc (flowchart-edge)
  ())

(defclass data-arc (flowchart-edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-outgoing-edges append
    ((graph flowchart) (instruction cleavir-ir:instruction))
  (flet ((make-control-arc (successor)
           (make-edge graph 'control-arc instruction successor))
         (make-data-arc (output)
           (make-edge graph 'data-arc instruction output)))
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
