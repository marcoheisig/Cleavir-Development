(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric graphviz-graph-attributes (graph)
  (:method-combination graphviz-attributes))

(defgeneric graphviz-node-attributes (graph node)
  (:method-combination graphviz-attributes))

(defgeneric graphviz-edge-attributes (graph edge from to edge-number)
  (:method-combination graphviz-attributes))

;;; The caption and the properties of a node are used to compute its label.
;;; Each pair of strings in the list of properties is rendered as a
;;; key-value entry below the caption.

(defgeneric graphviz-node-caption (graph node))

(defgeneric graphviz-node-properties (graph node)
  (:method-combination append))

;;; The edge drawing protocol is somewhat involved, but for a good reason.
;;; We want to be able to subclass graphs to add or remove edges or to
;;; change the appearance of some edges.  To do so, the protocol works in
;;; three steps.  In the first step, a generic function is used to
;;; determine all types of edges that potentially reach a given node.  In
;;; the second step, the node is queried for incoming and outgoing edges of
;;; each of these types.  In the third step, the current graph type, edge
;;; type, start node, target node and the position of the edge in the
;;; sequence returned from the previous step are used to derive the
;;; attributes of that edge.

(defgeneric graphviz-potential-edges (graph node)
  (:method-combination append))

(defgeneric graphviz-outgoing-edge-targets (graph edge node))

(defgeneric graphviz-incoming-edge-origins (graph edge node))

(defgeneric graphviz-known-nodes (graph node)
  (:method-combination append))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass graph ()
  ())

(defclass edge ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Methods

(defmethod graphviz-graph-attributes
    ((graph graph))
  '())

(defmethod graphviz-node-attributes
    ((graph graph) node)
  '())

(defmethod graphviz-edge-attributes
    ((graph graph) edge from to edge-number)
  '())

(defmethod graphviz-node-caption
    ((graph graph) node)
  (string-downcase
   (class-name
    (class-of node))))

(defmethod graphviz-node-properties append
    ((graph graph) node)
  '())

(defmethod graphviz-potential-edges append
    ((graph graph) node)
  '())

(defmethod graphviz-outgoing-edge-targets
    ((graph graph) (edge edge) node)
  '())

(defmethod graphviz-incoming-edge-origins
    ((graph graph) (edge edge) node)
  '())

(defmethod graphviz-known-nodes append
    ((graph graph) node)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CL-DOT Integration

(defmethod cl-dot:graph-object-node
    ((graph graph) node)
  (make-instance 'cl-dot:node
    :attributes
    `(:label
      ,(make-html-label
        :caption (graphviz-node-caption graph node)
        :properties (graphviz-node-properties graph node))
      ,@(graphviz-node-attributes graph node))))

(defmethod cl-dot:graph-object-points-to
    ((graph graph) node)
  (loop for edge in (graphviz-potential-edges graph node)
        append
        ;; There must be a more graceful way to loop over sequences...
        (loop for target in (coerce (graphviz-outgoing-edge-targets graph edge node) 'list)
              for edge-number from 0
              collect
              (make-instance 'cl-dot:attributed
                :attributes (graphviz-edge-attributes graph edge node target edge-number)
                :object target))))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph graph) node)
  (loop for edge in (graphviz-potential-edges graph node)
        append
        (loop for origin in (coerce (graphviz-incoming-edge-origins graph edge node) 'list)
              for edge-number from 0
              collect
              (make-instance 'cl-dot:attributed
                :attributes (graphviz-edge-attributes graph edge origin node edge-number)
                :object origin))))

(defmethod cl-dot:graph-object-knows-of
    ((graph graph) object)
  (graphviz-known-nodes graph object))
