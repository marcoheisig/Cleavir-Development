(cl:in-package #:cleavir-development)

(defclass ast-graph (graph)
  ())

(defmethod cl-dot:graph-object-points-to
    ((graph ast-graph) (ast-node cleavir-ast:ast))
  (declare (ignore graph))
  (loop for child in (cleavir-ast:children ast-node)
        collect (make-instance 'cl-dot:attributed
                  :object child)))
