(in-package :cleavir-development)

(defun program-in-path-p (program)
  (check-type program string)
  (multiple-value-bind (out err exit-code)
      (uiop:run-program
       (list "which" program)
       :ignore-error-status t)
    (declare (ignore out err))
    (zerop exit-code)))

(defparameter *graphviz-viewer*
  (find-if #'program-in-path-p '("evince" "okular" "xpdf")))

(defclass arbitrary-cleavir-graph
    (ast
     cst
     hir-flowchart
     mir-flowchart
     lir-flowchart)
  ())

(defun view (&rest graph-roots)
  (uiop:with-temporary-file (:pathname output-file :type "pdf")
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots
      (make-instance 'arbitrary-cleavir-graph)
      graph-roots)
     output-file :format :pdf)
    (uiop:run-program
     (list *graphviz-viewer*
           (uiop:native-namestring output-file)))))
