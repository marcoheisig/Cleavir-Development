(in-package :cleavir-development)

(defparameter *graphviz-viewer* "evince")

(defparameter *default-graph* (make-instance 'cleavir-graph))

(defclass arbitrary-cleavir-graph
    (ast
     cst
     hir-flowchart
     mir-flowchart
     lir-flowchart)
  ())

(defun view (&rest graph-roots)
  (uiop:with-temporary-file (:pathname output-file)
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots *default-graph* graph-roots)
     output-file)
    (uiop:run-program
     (list *graphviz-viewer*
           (uiop:native-namestring output-file)))))
