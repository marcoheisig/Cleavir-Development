(cl:in-package #:cleavir-development)

(defclass arbitrary-cleavir-graph
    (ast
     cst
     flowchart)
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
                               (graph *graphviz-default-graph*)
                               (attributes '()))
  (cl-dot:dot-graph
   (cl-dot:generate-graph-from-roots graph (list graph-root) attributes)
   file :format format))

(defun view (graph-root &key
                          (format :pdf)
                          (graph *graphviz-default-graph*)
                          (viewer *graphviz-default-viewer*)
                          (attributes '()))
  (uiop:with-temporary-file (:pathname file :type format)
    (draw graph-root file :format format
                          :graph graph
                          :attributes attributes)
    (uiop:run-program
     (list viewer (uiop:native-namestring file)))))
