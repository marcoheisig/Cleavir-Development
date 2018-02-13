(in-package #:cleavir-development)

(defclass cst () ())

(defmethod cl-dot:graph-object-node
    ((graph cst) (node concrete-syntax-tree:cst))
  (let ((label (string-downcase (symbol-name (class-name (class-of node))))))
    (make-instance 'cl-dot:node
      :attributes `(:label ,label :shape :box))))

(defmethod cl-dot:graph-object-points-to
    ((graph cst) (node concrete-syntax-tree:cons-cst))
  nil)

(defmethod coerce-to-graph-node ((list list) (graph cst))
  (concrete-syntax-tree:cstify list))
