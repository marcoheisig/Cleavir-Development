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
  (string-downcase (cleavir-ir:name datum)))

(defmethod graphviz-node-attributes
    ((graph flowchart) (datum cleavir-ir:lexical-location))
  '(:fillcolor :yellow))

;;; VALUES-LOCATION

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:values-location))
  "V")

(defmethod graphviz-node-attributes
    ((graph flowchart) (datum cleavir-ir:values-location))
  '(:fillcolor :blue))

;;; IMMEDIATE-INPUT

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:immediate-input))
  (princ-to-string (cleavir-ir:value datum)))

(defmethod graphviz-node-attributes
    ((graph flowchart) (datum cleavir-ir:immediate-input))
  '(:fillcolor :aquamarine))

;;; LOAD-TIME-VALUE-INPUT

(defmethod graphviz-node-caption
    ((graph flowchart) (datum cleavir-ir:load-time-value-input))
  (princ-to-string (cleavir-ir:form datum)))

(defmethod graphviz-node-attributes
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
    ((graph flowchart) (instruction cleavir-ir:enclose-instruction))
  (list (make-instance 'pink-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph flowchart) (edge pink-edge) (instruction cleavir-ir:enclose-instruction))
  (list (cleavir-ir:code instruction)))

;;; UNWIND-INSTRUCTION

(defmethod graphviz-potential-edges append
    ((graph flowchart) (instruction cleavir-ir:unwind-instruction))
  (list (make-instance 'pink-edge)))

(defmethod graphviz-incoming-edge-origins
    ((graph flowchart) (edge pink-edge) (instruction cleavir-ir:unwind-instruction))
  (list (cleavir-ir:invocation instruction)))

;;; ENTER-INSTRUCTION

(defmethod graphviz-node-properties append
    ((graph flowchart) (instruction cleavir-ir:enter-instruction))
  (labels ((simplify (item)
             (etypecase item
               (symbol item)
               (list (mapcar #'simplify item))
               (cleavir-ir:lexical-location
                (1+ (position item (cleavir-ir:outputs instruction)))))))
    `(("lambda-list"
       .
       ,(stringify
         (mapcar #'simplify (cleavir-ir:lambda-list instruction)))))))

;;; TOP-LEVEL-ENTER-INSTRUCTION

(defmethod graphviz-node-properties append
    ((graph flowchart) (instruction cleavir-ir:top-level-enter-instruction))
  (loop for form in (cleavir-ir:forms instruction)
        for index from 1
        collect
        (cons
         (format nil "form-~D" index)
         (stringify form))))

;;; THE-INSTRUCTION

(defmethod graphviz-node-properties append
    ((graph flowchart) (instruction cleavir-ir:the-instruction))
  `(("value-type" . ,(stringify (cleavir-ir:value-type instruction)))))

;;; THE-VALUES-INSTRUCTION

(defmethod graphviz-node-properties append
    ((graph flowchart) (instruction cleavir-ir:the-values-instruction))
  `(("required-types" . ,(stringify (cleavir-ir:required-types instruction)))
    ("optional-types" . ,(stringify (cleavir-ir:optional-types instruction)))
    ("rest-type" . ,(stringify (cleavir-ir:rest-type instruction)))))

;;; TYPEQ-INSTRUCTION

(defmethod graphviz-node-properties append
    ((graph flowchart) (instruction cleavir-ir:typeq-instruction))
  `(("value-type" . ,(stringify (cleavir-ir:value-type instruction)))))

;;; BOX-INSTRUCTION

(defmethod graphviz-node-properties append
    ((graph flowchart) (instruction cleavir-ir:box-instruction))
  `(("element-type" . ,(stringify (cleavir-ir:element-type instruction)))))

;;; UNBOX-INSTRUCTION

(defmethod graphviz-node-properties append
    ((graph flowchart) (instruction cleavir-ir:unbox-instruction))
  `(("element-type" . ,(stringify (cleavir-ir:element-type instruction)))))
