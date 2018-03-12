(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for drawing data

;;; CONSTANT-INPUT

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:constant-input))
  (princ-to-string (cleavir-ir:value datum)))

(defmethod graphviz-node-color
    ((graph flowchart) (datum cleavir-ir:constant-input))
  :green)

;;; LEXICAL-LOCATION

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  (princ-to-string (cleavir-ir:name datum)))

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  :yellow)

;;; VALUES-LOCATION

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:values-location))
  :blue)

;;; IMMEDIATE-INPUT

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:immediate-input))
  :aquamarine)

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:immediate-input))
  (princ-to-string (cleavir-ir:value datum)))

;;; LOAD-TIME-VALUE-INPUT

(defmethod graphviz-node-fillcolor
    ((graph flowchart) (datum cleavir-ir:load-time-value-input))
  :orange)

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:load-time-value-input))
  (princ-to-string (cleavir-ir:form datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods for drawing instructions

;;; ENCLOSE-INSTRUCTION

(defmethod graphviz-incoming-edges append
    ((graph flowchart) (instruction cleavir-ir:enclose-instruction))
  (list (make-edge (cleavir-ir:code instruction)
                   :color :pink
                   :style :dashed)))

;;; UNWIND-INSTRUCTION

(defmethod graphviz-outgoing-edges append
    ((graph flowchart) (instruction cleavir-ir:unwind-instruction))
  (list (make-edge (cleavir-ir:invocation instruction)
                   :color :pink
                   :style :dashed)))
