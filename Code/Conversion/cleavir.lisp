(cl:in-package #:cleavir-development)

(defclass cleavir ()
  (;; See CLHS 3.2.1 for the definitions of the different environments.
   (%compilation-environment :initarg :compilation-environment
                             :reader compilation-environment)
   (%evaluation-environment :initarg :evaluation-environment
                            :reader evaluation-environment)
   ;; The value of cleavir-generate-ast:*compiler*
   (%mode :initarg :mode
          :reader mode
          :type (member cl:compile cl:compile-file cl:eval))))

;;; CST conversion

(defmethod convert
    ((context cleavir)
     (form t)
     (type (eql 'cst)))
  (declare (ignore context type))
  (concrete-syntax-tree:cst-from-expression form))

(defmethod convert
    ((context cleavir)
     (pathname pathname)
     (type (eql 'cst)))
  (declare (ignore context type))
  (with-open-file (stream pathname :direction :input)
    (eclector.concrete-syntax-tree:cst-read stream)))

(defmethod convert
    ((context cleavir)
     (string string)
     (type (eql 'cst)))
  (declare (ignore context type))
  (with-input-from-string (stream string)
    (eclector.concrete-syntax-tree:cst-read stream)))

;;; AST conversion

(defmethod convert
    ((context cleavir)
     (cst concrete-syntax-tree:cst)
     (type (eql 'ast)))
  (declare (ignore type))
  (let ((environment (compilation-environment context))
        (cleavir-generate-ast:*compiler* (mode context)))
    (cleavir-cst-to-ast:cst-to-ast cst environment nil)))

;;; HIR conversion

(defmethod convert
    ((context cleavir)
     (ast cleavir-ast:ast)
     (type (eql 'hir)))
  (declare (ignore type))
  (cleavir-ast-to-hir:compile-toplevel ast))

;;; MIR conversion

(defmethod convert
    ((context cleavir)
     (ast cleavir-ast:ast)
     (type (eql 'mir)))
  (declare (ignore context ast type))
  (error "Not implemented"))

;;; LIR conversion

(defmethod convert
    ((context cleavir)
     (ast cleavir-ast:ast)
     (type (eql 'lir)))
  (declare (ignore context ast type))
  (error "Not implemented"))
