(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass cst (graph)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-outgoing-edges append
    ((graph cst) (atom-cst concrete-syntax-tree:atom-cst))
  (declare (ignore graph atom-cst))
  nil)

(defmethod graphviz-outgoing-edges append
    ((graph cst) (cons-cst cst:cons-cst))
  (list
   (make-instance 'edge :object (cst:first cons-cst) :label "first")
   (make-instance 'edge :object (cst:rest cons-cst) :label "rest")))

(defmethod graphviz-node-caption
    ((graph cst) (node cst:cons-cst))
  (declare (ignore graph node))
  "cons")

(defmethod graphviz-node-caption
    ((graph cst) (node cst:atom-cst))
  (declare (ignore graph node))
  "atom")

(defmethod graphviz-node-properties append
    ((graph cst) (cst cst:cst))
  `(("source" . ,(princ-to-string (cst:source cst)))))

(defmethod graphviz-node-properties append
    ((graph cst) (cst cst:atom-cst))
  `(("value" . ,(princ-to-string (cst:raw cst)))))

(defmethod graphviz-node-fillcolor
    ((graph cst) (cons-cst concrete-syntax-tree:cons-cst))
  (declare (ignore graph cons-cst))
  :gray)
