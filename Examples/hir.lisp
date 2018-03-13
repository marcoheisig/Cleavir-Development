(cl:in-package #:cleavir-development-examples)

(defgeneric convert-to-hir (object))

(defmethod convert-to-hir ((ast cleavir-ast:ast))
  (cleavir-ast-to-hir:compile-toplevel
   (cleavir-ast-transformations:hoist-load-time-value ast)))

(defmethod convert-to-hir ((object t))
  (convert-to-hir
   (convert-to-ast object)))

(defun hir-example-1 ()
  (view
   (convert-to-hir "(+ 2 2)")))

(defun hir-example-2 ()
  (view
   (convert-to-hir "(defun foo (x) (* x x))")))
