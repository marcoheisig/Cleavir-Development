(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ast (graph)
  ())

(defclass ast-edge (edge)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-outgoing-edges append
    ((graph ast) (ast cleavir-ast:ast))
  (flet ((make-ast-edge (child)
           (make-edge graph 'ast-edge ast child)))
    (mapcar #'make-ast-edge (cleavir-ast:children ast))))

;;; The default label is the lower-case version of the name of the class
;;; (as a string) with suffix -ast stripped off.
(defmethod graphviz-node-label
    ((graph ast) (ast cleavir-ast:ast))
  (let ((name (string (class-name (class-of ast)))))
    (string-downcase (subseq name 0 (- (length name) 4)))))

;;; CONSTANT-AST Attributes

(defmethod graphviz-node-label
    ((graph ast) (ast cleavir-ast:constant-ast))
  (declare (ignore graph))
  (cleavir-ast:value ast))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:constant-ast))
  (declare (ignore graph ast))
  :green)

;;; LEXICAL-AST Attributes

(defmethod graphviz-node-label
    ((graph ast) (ast cleavir-ast:lexical-ast))
  (declare (ignore graph))
  (cleavir-ast:name ast))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:lexical-ast))
  (declare (ignore graph ast))
  :yellow)

;;; TAG-AST Attributes

(defmethod graphviz-node-label
    ((graph ast) (ast cleavir-ast:tag-ast))
  (declare (ignore graph))
  (cleavir-ast:name ast))

;;; TOP-LEVEL-FUNCTION-AST Attributes

(defmethod graphviz-node-label
    ((graph ast) (ast cleavir-ast:top-level-function-ast))
  (declare (ignore graph))
  (format nil "\"~a ~a\""
          (string-downcase (class-name (class-of ast)))
          (cleavir-ast:forms ast)))

;;; LOAD-TIME-VALUE-AST Attributes

(defmethod graphviz-node-label
    ((graph ast) (ast cleavir-ast:load-time-value-ast))
  (declare (ignore graph))
  (format nil "~a" (cleavir-ast:form ast)))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:load-time-value-ast))
  (declare (ignore graph ast))
  :pink)

;;; BIND-AST Attributes

(defmethod graphviz-node-shape
    ((graph ast) (ast cleavir-ast:bind-ast))
  (declare (ignore graph ast))
  :ellipse)

;;; THE-AST Attributes

(defmethod graphviz-node-label
    ((graph ast) (ast cleavir-ast:the-ast))
  (format nil "\"the (values ~{~s ~}~@[&optional ~{~s ~}~]&rest ~s)\""
          (cleavir-ast:required-types ast)
          (cleavir-ast:optional-types ast)
          (cleavir-ast:rest-type ast)))

;;; AREF-AST Attributes

;;; ASET-AST Attributes

;;; FLOAT-AST Attributes
