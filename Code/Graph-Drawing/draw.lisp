(in-package :cleavir-development)

(defparameter *graphviz-viewer* "evince")

(defgeneric draw (graph &rest roots))

(defgeneric coerce-to-graph-node (node graph))

(defmethod draw (graph &rest graph-roots)
  (uiop:with-temporary-file (:pathname output-file)
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots
      graph
      (loop for graph-root in graph-roots
            collect (coerce-to-graph-node graph-root graph)))
     output-file)
    (uiop:run-program
     (list *graphviz-viewer*
           (uiop:native-namestring output-file)))))
