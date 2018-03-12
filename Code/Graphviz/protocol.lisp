(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric graphviz-node-fillcolor (graph node))

(defgeneric graphviz-node-shape (graph node))

(defgeneric graphviz-node-style (graph node))

(defgeneric graphviz-node-caption (graph node))

(defgeneric graphviz-node-properties (graph node)
  (:method-combination append))

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

(defclass edge (cl-dot:attributed)
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
  '())

(defmethod graphviz-incoming-edges append
    ((graph graph) (object t))
  '())

(defmethod graphviz-known-objects append
    ((graph graph) (object t))
  '())

(defmethod initialize-instance
    ((instance edge) &rest initargs
     &key (label "") (style :solid) (weight 1))
  (apply #'call-next-method instance
         :attributes `(:label ,label
                       :style ,style
                       :weight ,weight)
         initargs))
