(cl:in-package #:cleavir-development)

(defclass cst-graph (graph)
  ())

(defmethod cl-dot:graph-object-points-to
    ((graph cst-graph) (atom-cst concrete-syntax-tree:atom-cst))
  (declare (ignore graph atom-cst))
  nil)

(defmethod cl-dot:graph-object-points-to
    ((graph cst-graph) (cons-cst concrete-syntax-tree:cons-cst))
  (declare (ignore graph))
  (list (cst:first cons-cst)
        (cst:rest cons-cst)))
