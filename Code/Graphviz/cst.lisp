(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass cst (graph)
  ())

(defclass cst-edge (edge)
  ())

(defclass cst-first-edge (cst-edge)
  ())

(defclass cst-rest-edge (cst-edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-outgoing-edges append
    ((graph cst) (atom-cst concrete-syntax-tree:atom-cst))
  (declare (ignore graph atom-cst))
  nil)

(defmethod graphviz-outgoing-edges append
    ((graph cst) (cons-cst concrete-syntax-tree:cons-cst))
  (list
   (make-edge graph 'cst-first-edge cons-cst (cst:first cons-cst))
   (make-edge graph 'cst-rest-edge cons-cst (cst:rest cons-cst))))

(defmethod graphviz-node-label
    ((graph cst) (cons-cst concrete-syntax-tree:cons-cst))
  (declare (ignore graph cons-cst))
  "cons")

(defmethod graphviz-node-fillcolor
    ((graph cst) (cons-cst concrete-syntax-tree:cons-cst))
  (declare (ignore graph cons-cst))
  :gray)

(defmethod graphviz-node-label
    ((graph cst) (atom-cst concrete-syntax-tree:atom-cst))
  (declare (ignore graph))
  (cst:raw atom-cst))

(defmethod graphviz-edge-label
    ((graph cst) (edge cst-first-edge) from to)
  (declare (ignore graph edge from to))
  "first")

(defmethod graphviz-edge-label
    ((graph cst) (edge cst-rest-edge) from to)
  (declare (ignore graph edge from to))
  "rest")
