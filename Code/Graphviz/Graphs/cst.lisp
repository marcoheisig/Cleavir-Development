(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass cst (graph)
  ())

(defclass cst-edge (edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-node-attributes
    ((graph cst) (cst cst:cst))
  `(:shape :box
    :style :filled))

(defmethod graphviz-node-attributes
    ((graph cst) (cons-cst cst:cons-cst))
  `(:fillcolor :gray))

(defmethod graphviz-node-attributes
    ((graph cst) (atom-cst cst:atom-cst))
  `(:fillcolor :white))

(defmethod graphviz-edge-attributes
    ((graph cst) (edge cst-edge) from to edge-number)
  (let ((label (ecase edge-number
                 (0 "car")
                 (1 "cdr"))))
    `(:label ,label)))

(defmethod graphviz-node-caption
    ((graph cst) (node cst:cons-cst))
  "cons")

(defmethod graphviz-node-caption
    ((graph cst) (node cst:atom-cst))
  "atom")

(defmethod graphviz-node-properties append
    ((graph cst) (cst cst:atom-cst))
  `(("value" . ,(stringify (cst:raw cst)))))

(defmethod graphviz-potential-edges append
    ((graph cst) (node cst:cons-cst))
  (list (make-instance 'cst-edge)))

(defmethod graphviz-outgoing-edge-targets
    ((graph cst) (edge cst-edge) (cons-cst cst:cons-cst))
  (list (cst:first cons-cst)
        (cst:rest cons-cst)))
