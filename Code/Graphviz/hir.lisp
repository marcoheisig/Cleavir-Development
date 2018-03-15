(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

;; Is there a name for the edges of unwind and enclose instructions?
(defclass pink-edge (edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for drawing data

;;; CONSTANT-INPUT

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:constant-input))
  (princ-to-string (cleavir-ir:value datum)))

(defmethod graphviz-node-attributes
    ((graph flowchart) (datum cleavir-ir:constant-input))
  '(:fillcolor :green))

;;; LEXICAL-LOCATION

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  (princ-to-string (cleavir-ir:name datum)))

(defmethod graphviz-node-attributes
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  '(:fillcolor :yellow))

;;; VALUES-LOCATION

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:values-location))
  '(:fillcolor :blue))

;;; IMMEDIATE-INPUT

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:immediate-input))
  (princ-to-string (cleavir-ir:value datum)))

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:immediate-input))
  '(:fillcolor :aquamarine))

;;; LOAD-TIME-VALUE-INPUT

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:load-time-value-input))
  (princ-to-string (cleavir-ir:form datum)))

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:load-time-value-input))
  '(:fillcolor :orange))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for drawing instructions

(defmethod graphviz-edge-attributes
    ((graph flowchart) (edge pink-edge) from to edge-number)
  '(:color :pink
    :style :dashed
    :weight 2
    :label ""))

;;; ENCLOSE-INSTRUCTION

(defmethod graphviz-potential-edges append
    ((graph flowchart) (node cleavir-ir:enclose-instruction))
  (list (make-instance 'pink-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph flowchart) (edge pink-edge) (instruction cleavir-ir:enclose-instruction))
  (list (cleavir-ir:code instruction)))

;;; UNWIND-INSTRUCTION

(defmethod graphviz-potential-edges append
    ((graph flowchart) (node cleavir-ir:unwind-instruction))
  (list (make-instance 'pink-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph flowchart) (edge pink-edge) (instruction cleavir-ir:unwind-instruction))
  (list (cleavir-ir:invocation instruction)))
