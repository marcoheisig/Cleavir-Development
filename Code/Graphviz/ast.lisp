(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ast (graph)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod graphviz-outgoing-edges append
    ((graph ast) (ast cleavir-ast:ast))
  (loop for child in (cleavir-ast:children ast)
        for child-number from 1
        collect
        (make-instance 'edge
          :object child
          :label (princ-to-string child-number))))

;;; The default caption is the lower-case version of the name of the class
;;; (as a string) with suffix -ast stripped off.
(defmethod graphviz-node-caption
    ((graph ast) (ast cleavir-ast:ast))
  (declare (ignore graph))
  (let ((name (string (class-name (class-of ast)))))
    (string-downcase (subseq name 0 (- (length name) 4)))))

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:ast))
  (declare (ignore graph))
  `(("source" . ,(princ-to-string (cleavir-ast:origin ast)))
    ("policy" . ,(princ-to-string (cleavir-ast:policy ast)))))

;;; CONSTANT-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:constant-ast))
  (declare (ignore graph))
  `(("value" . ,(princ-to-string (cleavir-ast:value ast)))))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:constant-ast))
  (declare (ignore graph ast))
  :green)

;;; LEXICAL-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:lexical-ast))
  (declare (ignore graph))
  `(("name" . ,(princ-to-string (cleavir-ast:name ast)))))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:lexical-ast))
  (declare (ignore graph ast))
  :yellow)

;;; TAG-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:tag-ast))
  (declare (ignore graph))
  `(("name" . ,(princ-to-string (cleavir-ast:name ast)))))

;;; TOP-LEVEL-FUNCTION-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:top-level-function-ast))
  (declare (ignore graph))
  `(("forms" . ,(princ-to-string (cleavir-ast:forms ast)))))

;;; LOAD-TIME-VALUE-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:load-time-value-ast))
  (declare (ignore graph))
  `(("form" . ,(princ-to-string (cleavir-ast:form ast)))))

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

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:the-ast))
  `(("required-types" . ,(princ-to-string (cleavir-ast:required-types ast)))
    ("optional-types" . ,(princ-to-string (cleavir-ast:optional-types ast)))
    ("rest-type" . ,(princ-to-string (cleavir-ast:rest-type ast)))))

;;; FLOAT-AST Attributes

(macrolet ((defcaption (class caption)
             `(defmethod graphviz-node-caption
                  ((graph ast) (ast ,class))
                (declare (ignorable graph ast))
                ,caption)))
  (defcaption cleavir-ast:float-add-ast "float +")
  (defcaption cleavir-ast:float-sub-ast "float -")
  (defcaption cleavir-ast:float-mul-ast "float *")
  (defcaption cleavir-ast:float-div-ast "float /")
  (defcaption cleavir-ast:float-less-ast "float <")
  (defcaption cleavir-ast:float-not-greater-ast "float <=")
  (defcaption cleavir-ast:float-equal-ast "float =")
  (defcaption cleavir-ast:float-not-less-ast "float >=")
  (defcaption cleavir-ast:float-greater-ast "float >")
  (defcaption cleavir-ast:float-sin-ast "float sin")
  (defcaption cleavir-ast:float-cos-ast "float cos")
  (defcaption cleavir-ast:float-sqrt-ast "float sqrt"))
