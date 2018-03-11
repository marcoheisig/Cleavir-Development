(cl:in-package #:cleavir-development)

(defun view-cst (object &rest arguments)
  (apply #'view (convert *context* object 'cst) arguments))

(defun view-ast (object &rest arguments)
  (apply #'view (convert *context* object 'ast) arguments))

(defun view-hir (object &rest arguments)
  (apply #'view (convert *context* object 'hir) arguments))

(defun view-mir (object &rest arguments)
  (apply #'view (convert *context* object 'mir) arguments))

(defun view-lir (object &rest arguments)
  (apply #'view (convert *context* object 'lir) arguments))
