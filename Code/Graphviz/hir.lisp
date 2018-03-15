(cl:in-package #:cleavir-development)

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

;;; ENCLOSE-INSTRUCTION

#+nil
(defmethod graphviz-incoming-edges append
    ((graph flowchart) (instruction cleavir-ir:enclose-instruction))
  (list (make-edge (cleavir-ir:code instruction)
                   :color :pink
                   :weight 2
                   :style :dashed)))

;;; UNWIND-INSTRUCTION

#+nil
(defmethod graphviz-outgoing-edges append
    ((graph flowchart) (instruction cleavir-ir:unwind-instruction))
  (list (make-edge (cleavir-ir:invocation instruction)
                   :color :pink
                   :weight 2
                   :style :dashed)))
