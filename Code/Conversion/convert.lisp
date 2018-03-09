(cl:in-package #:cleavir-development)

;;; Code compiled by Cleavir passes through several internal
;;; representations. These representations are
;;;
;;; 1. Source code. Source code is either a forms or the content of some
;;;    file, depending on whether they are processed by COMPILE or EVAL or
;;;    by COMPILE-FILE.
;;;
;;; 2. Concrete Syntax Trees (CST). A CST is a graph of CST:CONS and
;;;    CST:ATOM instances that track both the original form and the source
;;;    location of that form.
;;;
;;; 3. Abstract Syntax Trees (AST). An AST is a graph of objects
;;;    corresponding roughly to the forms in the source code. The
;;;    difference to the source code is that macros are already expanded
;;;    and the different namespaces have been eliminated. Each AST node
;;;    tracks its corresponding source location and its compilation policy.
;;;
;;; 4. High-level Intermediate Representation (HIR). This representation is
;;;    a flow graph with both data and control flow arcs. The HIR is
;;;    completely independent from the target platform and all HIR values
;;;    are ordinary Common Lisp object.
;;;
;;; 5. Medium-level Intermediate Representation (MIR). This representation
;;;    is still a flowchart, but containing backend-specific details
;;;    regarding address computation and tagging.
;;;
;;; 6. Low-level Intermediate Representation (LIR). At this level, the
;;;    stack, registers and hardware instructions are explicitly exposed.
;;;
;;; The generic function CONVERT computes the desired low-level
;;; representation corresponding to a given high-level representation. To
;;; account for different conversion strategies, its first argument is some
;;; object denoting the compilation CONTEXT.

(defgeneric convert (context object type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trivial Conversion Methods

(defmethod convert (context (ast cleavir-ast:ast) (type (eql 'ast)))
  (declare (ignore context type))
  ast)

(defmethod convert (context (cst concrete-syntax-tree:cst) (type (eql 'cst)))
  (declare (ignore context type))
  cst)

(defmethod convert (context (hir cleavir-ir:instruction) (type (eql 'hir)))
  hir)

;;; TODO figure out how to differentiate between HIR, MIR and LIR.

(defmethod convert (context (mir cleavir-ir:instruction) (type (eql 'mir)))
  (declare (ignore context type))
  mir)

(defmethod convert (context (lir cleavir-ir:instruction) (type (eql 'lir)))
  (declare (ignore context type))
  lir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Chaining Methods

(defmethod convert (context (object t) (type (eql 'ast)))
  (convert context (convert context object 'cst) type))

(defmethod convert (context (object t) (type (eql 'hir)))
  (convert context (convert context object 'ast) type))

(defmethod convert (context (object t) (type (eql 'mir)))
  (convert context (convert context object 'hir) type))

(defmethod convert (context (object t) (type (eql 'lir)))
  (convert context (convert context object 'mir) type))
