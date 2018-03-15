(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cleavir Specific Protocol Extensions

(defclass source-mixin ()
  ())

(defclass policy-mixin ()
  ())

(defgeneric source-string (graph node))

(defgeneric policy-string (graph node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod source-string
    ((graph graph) (cst cst:cst))
  (princ-to-string (cst:source cst)))

(defmethod source-string
    ((graph graph) (ast cleavir-ast:ast))
  (princ-to-string (cleavir-ast:origin ast)))

(defmethod policy-string
    ((graph graph) (ast cleavir-ast:ast))
  (princ-to-string (cleavir-ast:policy ast)))

(defmethod source-string
    ((graph graph) (instruction cleavir-ir:instruction))
  (princ-to-string (cleavir-ir:origin instruction)))

(defmethod policy-string
    ((graph graph) (instruction cleavir-ir:instruction))
  (princ-to-string (cleavir-ir:policy instruction)))

(defmethod graphviz-node-properties append
    ((graph source-mixin) (cst cst:cst))
  `(("source" . ,(source-string graph cst))))

(defmethod graphviz-node-properties append
    ((graph source-mixin) (ast cleavir-ast:ast))
  `(("source" . ,(source-string graph ast))))

(defmethod graphviz-node-properties append
    ((graph policy-mixin) (ast cleavir-ast:ast))
  `(("policy" . ,(policy-string graph ast))))

(defmethod graphviz-node-properties append
    ((graph policy-mixin) (instruction cleavir-ir:instruction))
  `(("policy" . ,(policy-string graph instruction))))
