(cl:in-package #:cleavir-development-examples)

(defgeneric convert-to-ast (object))

(defmethod convert-to-ast ((cst cst:cst))
  (let ((cleavir-cst-to-ast:*compiler* 'cl:eval)
        (cleavir-generate-ast:*compiler* 'cl:eval))
    (cleavir-cst-to-ast:cst-to-ast cst *environment* nil)))

(defmethod convert-to-ast ((object t))
  (convert-to-ast
   (convert-to-cst object)))

(defun ast-example-1 ()
  (view
   (convert-to-ast "(+ 2 2)")))

(defun ast-example-2 ()
  (view
   (convert-to-ast "(defun foo (x) (* x x))")))
