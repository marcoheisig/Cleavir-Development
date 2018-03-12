(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANT-INPUT

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:constant-input))
  (princ-to-string (cleavir-ir:value datum)))

(defmethod graphviz-node-shape
    ((graph flowchart) (datum cleavir-ir:constant-input))
  :ellipse)

(defmethod graphviz-node-color
    ((graph flowchart) (datum cleavir-ir:constant-input))
  :green)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-LOCATION

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  (princ-to-string (cleavir-ir:name datum)))

(defmethod graphviz-node-shape
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  :ellipse)

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  :yellow)
