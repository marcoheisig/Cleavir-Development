(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

;;; These methods provide values for the fillcolor, shape and style
;;; attribute of a graph node. Each value must be a keyword.

(defgeneric graphviz-node-fillcolor (graph node))

(defgeneric graphviz-node-shape (graph node))

(defgeneric graphviz-node-style (graph node))

;;; Each graph node has a caption and a list of properties. Each caption
;;; must be a string. Each property must be a cons of two strings, the key
;;; and the corresponding value.

(defgeneric graphviz-node-caption (graph node))

(defgeneric graphviz-node-properties (graph node)
  (:method-combination append))

;;; The next methods inform CL-DOT about the outgoing and incoming edges of
;;; a node. They return lists of instances of the class CL-DOT:ATTRIBUTED.

(defgeneric graphviz-outgoing-edges (graph node)
  (:method-combination append))

(defgeneric graphviz-incoming-edges (graph node)
  (:method-combination append))

;;; This method informs CL-DOT of other nodes in the graph. It returns a
;;; list of graph nodes.

(defgeneric graphviz-known-nodes (graph node)
  (:method-combination append))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass graph ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Methods on Nodes

(defmethod graphviz-node-caption ((graph graph) (node t))
  (string-downcase
   (class-name
    (class-of node))))

(defmethod graphviz-node-properties append ((graph graph) (node t))
  '())

(defmethod graphviz-node-fillcolor ((graph graph) (node t))
  :white)

(defmethod graphviz-node-shape ((graph graph) (node t))
  :box)

(defmethod graphviz-node-style ((graph graph) (node t))
  :filled)

(defmethod cl-dot:graph-object-node
    ((graph graph) (node t))
  (make-instance 'cl-dot:node
    :attributes
    `(:label ,(make-html-label
               :caption (graphviz-node-caption graph node)
               :properties (graphviz-node-properties graph node))
      :fillcolor ,(graphviz-node-fillcolor graph node)
      :shape ,(graphviz-node-shape graph node)
      :style ,(graphviz-node-style graph node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Methods on Edges

(defmethod cl-dot:graph-object-points-to
    ((graph graph) (node t))
  (graphviz-outgoing-edges graph node))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph graph) (node t))
  (graphviz-incoming-edges graph node))

(defmethod cl-dot:graph-object-knows-of
    ((graph graph) (object t))
  (graphviz-known-nodes graph object))

(defmethod graphviz-outgoing-edges append
    ((graph graph) (node t))
  '())

(defmethod graphviz-incoming-edges append
    ((graph graph) (node t))
  '())

(defmethod graphviz-known-nodes append
    ((graph graph) (node t))
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A simple constructor for edges

(defun make-edge (target &rest attributes)
  (make-instance 'cl-dot:attributed
    :attributes attributes
    :object target))
