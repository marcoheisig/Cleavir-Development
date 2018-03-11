(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric graphviz-node-caption (graph node))

(defgeneric graphviz-node-properties (graph node)
  (:method-combination append))

(defgeneric graphviz-node-fillcolor (graph node))

(defgeneric graphviz-node-shape (graph node))

(defgeneric graphviz-node-style (graph node))

(defgeneric graphviz-edge-label (graph edge from to))

(defgeneric graphviz-edge-weight (graph edge from to))

(defgeneric graphviz-edge-style (graph edge from to))

(defgeneric make-edge (graph edge from to))

(defgeneric graphviz-outgoing-edges (graph object)
  (:method-combination append))

(defgeneric graphviz-incoming-edges (graph object)
  (:method-combination append))

(defgeneric graphviz-known-objects (graph object)
  (:method-combination append))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Base Classes

(defclass graph ()
  ())

(defclass edge ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Methods on Nodes

(defmethod graphviz-node-caption ((graph graph) (node t))
  (declare (ignore graph))
  (string-downcase
   (class-name
    (class-of node))))

(defmethod graphviz-node-properties append ((graph graph) (node t))
  (declare (ignore graph node))
  '())

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
    `(:label ,(make-html-label
               :caption (graphviz-node-caption graph node)
               :properties (graphviz-node-properties graph node))
      :fillcolor ,(graphviz-node-fillcolor graph node)
      :shape ,(graphviz-node-shape graph node)
      :style ,(graphviz-node-style graph node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Default Methods on Edges

(defmethod graphviz-edge-label ((graph graph) (edge edge) from to)
  (declare (ignore graph edge from to))
  "")

(defmethod graphviz-edge-weight ((graph graph) (edge edge) from to)
  (declare (ignore graph edge from to))
  1)

(defmethod graphviz-edge-style ((graph graph) (edge edge) from to)
  (declare (ignore graph edge from to))
  :solid)

(defmethod make-edge ((graph graph) (edge symbol) from to)
  (make-edge graph (make-instance edge) from to))

(defmethod cl-dot:graph-object-points-to
    ((graph graph) (object t))
  (graphviz-outgoing-edges graph object))

(defmethod cl-dot:graph-object-pointed-to-by
    ((graph graph) (object t))
  (graphviz-incoming-edges graph object))

(defmethod cl-dot:graph-object-knows-of
    ((graph graph) (object t))
  (graphviz-known-objects graph object))

(defmethod graphviz-outgoing-edges append
    ((graph graph) (object t))
  (declare (ignore graph object))
  '())

(defmethod graphviz-incoming-edges append
    ((graph graph) (object t))
  (declare (ignore graph object))
  '())

(defmethod graphviz-known-objects append
    ((graph graph) (object t))
  (declare (ignore graph object))
  '())

(defmethod make-edge ((graph graph) (edge edge) from to)
  (make-instance 'cl-dot:attributed
    :object to
    :attributes
    `(:label ,(graphviz-edge-label graph edge from to)
      :style ,(graphviz-edge-style graph edge from to)
      :weight ,(graphviz-edge-weight graph edge from to))))
