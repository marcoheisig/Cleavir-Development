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

(defmethod cleavir-env:declarations
    ((environment cleavir-bogus-test-environment:bogus-environment))
  '())

(defun view-ast ()
  (view
   (cleavir-generate-ast:generate-ast
    `(progn
       (let ((a (make-array 5))
             (x 3))
         (incf (aref a x) 2)))
    (make-instance 'cleavir-bogus-test-environment:bogus-environment)
    nil)))

(defun view-hir ()
  (values))
