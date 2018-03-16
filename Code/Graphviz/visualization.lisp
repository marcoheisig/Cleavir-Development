(cl:in-package #:cleavir-development)

(defclass arbitrary-cleavir-graph
    (cst
     ast
     flowchart
     source-mixin
     policy-mixin)
  ())

(defparameter *graphviz-default-graph*
  (make-instance 'arbitrary-cleavir-graph))

(defparameter *graphviz-default-viewer*
  (flet ((program-in-path-p (program)
           (multiple-value-bind (out err exit-code)
               (uiop:run-program
                (list "which" program)
                :ignore-error-status t)
             (declare (ignore out err))
             (zerop exit-code))))
    (find-if #'program-in-path-p '("evince" "okular" "xpdf"))))

(defun draw (graph-root file &key
                               (format :pdf)
                               (graph *graphviz-default-graph*))
  (let ((graph (if (symbolp graph)
                   (make-instance graph)
                   graph)))
    (cl-dot:dot-graph
     (cl-dot:generate-graph-from-roots
      graph
      (list graph-root)
      (graphviz-graph-attributes graph))
     file :format format)))

(defun view (graph-root &key
                          (format :pdf)
                          (graph *graphviz-default-graph*)
                          (viewer *graphviz-default-viewer*))
  (uiop:with-temporary-file (:pathname file :type format)
    (draw graph-root file :format format
                          :graph graph)
    (uiop:run-program
     (list viewer (uiop:native-namestring file)))))
