(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric cst-value-string (graph atom-cst))

(defgeneric cst-source-string (graph cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass cst (graph)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod cst-value-string
    ((graph cst) (cst cst:atom-cst))
  (princ-to-string (cst:raw cst)))

(defmethod cst-source-string
    ((graph cst) (cst cst:cst))
  (princ-to-string (cst:source cst)))

(defmethod graphviz-outgoing-edges append
    ((graph cst) (atom-cst concrete-syntax-tree:atom-cst))
  nil)

(defmethod graphviz-outgoing-edges append
    ((graph cst) (cons-cst cst:cons-cst))
  (list
   (make-edge (cst:first cons-cst) :label "first")
   (make-edge (cst:rest cons-cst) :label "rest")))

(defmethod graphviz-node-caption
    ((graph cst) (node cst:cons-cst))
  "cons")

(defmethod graphviz-node-caption
    ((graph cst) (node cst:atom-cst))
  "atom")

(defmethod graphviz-node-properties append
    ((graph cst) (cst cst:cst))
  `(("source" . ,(cst-source-string graph cst))))

(defmethod graphviz-node-properties append
    ((graph cst) (cst cst:atom-cst))
  `(("value" . ,(cst-value-string graph cst))))

(defmethod graphviz-node-fillcolor
    ((graph cst) (cons-cst concrete-syntax-tree:cons-cst))
  :gray)
