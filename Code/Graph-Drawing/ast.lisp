(in-package #:cleavir-development)

(defclass ast () ())

(defmethod cl-dot:graph-object-node
    ((graph ast) (node cleavir-ast:ast))
  (let ((label (string-downcase (symbol-name (class-name (class-of node))))))
    (make-instance 'cl-dot:node
      :attributes `(:label ,label :shape :box))))

(defmethod cl-dot:graph-object-points-to
    ((graph ast) (ast-node cleavir-ast:ast))
  (loop for child in (cleavir-ast:children ast-node)
        collect (make-instance 'cl-dot:attributed
                  :object child)))
