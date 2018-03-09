(cl:in-package #:cleavir-development)

(defun run-examples ()
  (view-cst "(defun foo (x) (* x x))")

  (view-ast "(defun foo (x) (* x x))")

  (view-hir "(defun foo (x) (* x x))"))
