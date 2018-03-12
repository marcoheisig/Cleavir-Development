(cl:in-package #:cleavir-development)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Functions

(defgeneric ast-source-string (graph ast))

(defgeneric ast-policy-string (graph ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes

(defclass ast (graph)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods

(defmethod ast-source-string
    ((graph ast) (ast cleavir-ast:ast))
  (princ-to-string (cleavir-ast:origin ast)))

(defmethod ast-policy-string
    ((graph ast) (ast cleavir-ast:ast))
  (princ-to-string (cleavir-ast:policy ast)))

(defmethod graphviz-outgoing-edges append
    ((graph ast) (ast cleavir-ast:ast))
  (loop for child in (cleavir-ast:children ast)
        for child-number from 1
        collect
        (make-edge child :label (princ-to-string child-number))))

;;; The default caption is the lower-case version of the name of the class
;;; (as a string) with suffix -ast stripped off.
(defmethod graphviz-node-caption
    ((graph ast) (ast cleavir-ast:ast))
  (let ((name (string (class-name (class-of ast)))))
    (string-downcase (subseq name 0 (- (length name) 4)))))

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:ast))
  `(("source" . ,(ast-source-string graph ast))
    ("policy" . ,(ast-policy-string graph ast))))

;;; CONSTANT-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:constant-ast))
  `(("value" . ,(princ-to-string (cleavir-ast:value ast)))))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:constant-ast))
  :green)

;;; LEXICAL-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:lexical-ast))
  `(("name" . ,(princ-to-string (cleavir-ast:name ast)))))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:lexical-ast))
  :yellow)

;;; TAG-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:tag-ast))
  `(("name" . ,(princ-to-string (cleavir-ast:name ast)))))

;;; TOP-LEVEL-FUNCTION-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:top-level-function-ast))
  `(("forms" . ,(princ-to-string (cleavir-ast:forms ast)))))

;;; LOAD-TIME-VALUE-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:load-time-value-ast))
  `(("form" . ,(princ-to-string (cleavir-ast:form ast)))))

(defmethod graphviz-node-fillcolor
    ((graph ast) (ast cleavir-ast:load-time-value-ast))
  :pink)

;;; BIND-AST Attributes

(defmethod graphviz-node-shape
    ((graph ast) (ast cleavir-ast:bind-ast))
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
                (declare (ignore graph ast))
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
