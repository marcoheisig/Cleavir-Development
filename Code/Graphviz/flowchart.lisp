(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass flowchart (graph)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-node-caption
    ((graph flowchart) (instruction cleavir-ir:instruction))
  ;; strip the -INSTRUCTION suffix
  (strip-suffix (class-name (class-of instruction)) "-instruction"))

(defmethod graphviz-outgoing-edges append
    ((graph flowchart) (instruction cleavir-ir:instruction))
  (append
   ;; outgoing control arcs
   (loop for successor in (cleavir-ir:successors instruction)
         for i from 1
         collect
         (make-edge successor
                    :style :bold
                    :label (princ-to-string i)))
   ;; outgoing data arcs
   (loop for output in (cleavir-ir:outputs instruction)
         for i from 1
         collect
         (make-edge output
                    :color :red
                    :fontcolor :red
                    :style :dashed
                    :label (princ-to-string i)))))

(defmethod graphviz-incoming-edges append
    ((graph flowchart) (instruction cleavir-ir:instruction))
    ;; incoming data arcs
    (loop for input in (cleavir-ir:inputs instruction)
          for i from 1
          collect
          (make-edge input
                     :color :blue
                     :fontcolor :blue
                     :style :dashed
                     :label (princ-to-string i))))

(defmethod graphviz-known-nodes append
    ((graph flowchart) (instruction cleavir-ir:instruction))
  (cleavir-ir:predecessors instruction))

(defmethod graphviz-known-nodes append
    ((graph flowchart) (datum cleavir-ir:datum))
  (append
   (cleavir-ir:defining-instructions datum)
   (cleavir-ir:using-instructions datum)))

(defmethod graphviz-node-shape
    ((graph flowchart) (datum cleavir-ir:datum))
  :ellipse)
