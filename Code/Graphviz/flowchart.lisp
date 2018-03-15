(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass flowchart (graph)
  ())

(defclass control-arc (edge)
  ())

(defclass data-arc (edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-node-attributes
    ((graph flowchart) node)
  '(:shape :box
    :style :filled))

(defmethod graphviz-node-attributes
    ((graph flowchart) (datum cleavir-ir:datum))
  '(:shape :ellipse))

(defmethod graphviz-edge-attributes
    ((graph flowchart) (edge edge) from to edge-number)
  `(:label ,(princ-to-string (1+ edge-number))))

(defmethod graphviz-edge-attributes
    ((graph flowchart)
     (edge control-arc)
     (from cleavir-ir:instruction)
     (to cleavir-ir:instruction)
     edge-number)
  '(:style :bold
    :weight 3))

(defmethod graphviz-edge-attributes
    ((graph flowchart)
     (edge data-arc)
     (from cleavir-ir:instruction)
     (to cleavir-ir:datum)
     edge-number)
  '(:color :red
    :fontcolor :red
    :style :dashed))

(defmethod graphviz-edge-attributes
    ((graph flowchart)
     (edge data-arc)
     (from cleavir-ir:datum)
     (to cleavir-ir:instruction)
     edge-number)
  '(:color :blue
    :fontcolor :blue
    :style :dashed))

(defmethod graphviz-node-caption
    ((graph flowchart)
     (instruction cleavir-ir:instruction))
  (strip-suffix (class-name (class-of instruction)) "-instruction"))

(defmethod graphviz-potential-edges append
    ((graph flowchart)
     (instruction cleavir-ir:instruction))
  (list (make-instance 'data-arc)
        (make-instance 'control-arc)))

(defmethod graphviz-potential-edges append
    ((graph flowchart)
     (datum cleavir-ir:datum))
  (list (make-instance 'data-arc)))

(defmethod graphviz-outgoing-edge-targets
    ((graph flowchart)
     (edge control-arc)
     (instruction cleavir-ir:instruction))
  (cleavir-ir:successors instruction))

(defmethod graphviz-outgoing-edge-targets
    ((graph flowchart)
     (edge data-arc)
     (instruction cleavir-ir:instruction))
  (cleavir-ir:outputs instruction))

(defmethod graphviz-incoming-edge-origins
    ((graph flowchart)
     (edge data-arc)
     (instruction cleavir-ir:instruction))
  (cleavir-ir:inputs instruction))

(defmethod graphviz-known-nodes append
    ((graph flowchart) (instruction cleavir-ir:instruction))
  (append (cleavir-ir:inputs instruction)
          (cleavir-ir:outputs instruction)
          (cleavir-ir:predecessors instruction)
          (cleavir-ir:successors instruction)))

(defmethod graphviz-known-nodes append
    ((graph flowchart) (datum cleavir-ir:datum))
  (append (cleavir-ir:defining-instructions datum)
          (cleavir-ir:using-instructions datum)))
