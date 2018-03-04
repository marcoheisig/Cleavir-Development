(cl:in-package #:cleavir-development)

(defclass graph ()
  ())

(defmethod graphviz-node-label ((graph graph) (node t))
  (declare (ignore graph))
  (string-downcase
   (class-name
    (class-of node))))

(defmethod graphviz-node-fillcolor ((graph graph) (node t))
  (declare (ignore graph node))
  :white)

(defmethod graphviz-node-shape ((graph graph) (node t))
  (declare (ignore graph node))
  :box)

(defmethod graphviz-node-style ((graph graph) (node t))
  (declare (ignore graph node))
  :filled)

(defmethod cl-dot:graph-object-node
    ((graph graph) (node t))
  (make-instance 'cl-dot:node
    :attributes
    `(:label ,(graphviz-node-label graph node)
      :fillcolor ,(graphviz-node-fillcolor graph node)
      :shape ,(graphviz-node-shape graph node)
      :style ,(graphviz-node-style graph node))))
