(cl:in-package #:cleavir-development)

(defun view-cst ()
  (view
   (with-input-from-string (stream "#1=(+ 2 #1#)")
     (eclector.concrete-syntax-tree:cst-read stream))))

(defmethod cleavir-env:optimize-info
    ((environment cleavir-bogus-test-environment:bogus-environment))
  (make-instance 'cleavir-env:optimize-info
    :optimize nil
    :policy nil))

(defun view-ast ()
  (view
   (cleavir-generate-ast:generate-ast
    '(+ 2 3)
    (make-instance 'cleavir-bogus-test-environment:bogus-environment)
    nil)))

(defun view-hir ()
  (values))
