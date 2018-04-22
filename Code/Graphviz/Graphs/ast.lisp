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

(defmethod graphviz-node-attributes
    ((graph ast) (ast cleavir-ast:ast))
  '(:style :filled
    :fillcolor :white
    :shape :box))

(defmethod graphviz-edge-attributes
    ((graph ast) (edge ast-edge) from to edge-number)
  (let ((label (stringify (1+ edge-number))))
    `(:label ,label)))

(defmethod graphviz-node-caption
    ((graph ast) (ast cleavir-ast:ast))
  (strip-suffix (class-name (class-of ast)) "-ast"))

(defmethod graphviz-potential-edges append
    ((graph ast) (node cleavir-ast:ast))
  (list (make-instance 'ast-edge)))

(defmethod graphviz-outgoing-edge-targets
    ((graph ast) (edge ast-edge) (ast cleavir-ast:ast))
  (cleavir-ast:children ast))

;;; CONSTANT-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:constant-ast))
  `(("value" . ,(stringify (cleavir-ast:value ast)))))

(defmethod graphviz-node-attributes
    ((graph ast) (ast cleavir-ast:constant-ast))
  '(:fillcolor :green))

;;; LEXICAL-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:lexical-ast))
  `(("name" . ,(stringify (cleavir-ast:name ast)))))

(defmethod graphviz-node-attributes
    ((graph ast) (ast cleavir-ast:lexical-ast))
  '(:fillcolor :yellow))

;;; TAG-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:tag-ast))
  `(("name" . ,(stringify (cleavir-ast:name ast)))))

;;; TOP-LEVEL-FUNCTION-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:top-level-function-ast))
  `(("forms" . ,(stringify (cleavir-ast:forms ast)))))

;;; LOAD-TIME-VALUE-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:load-time-value-ast))
  `(("form" . ,(stringify (cleavir-ast:form ast)))))

(defmethod graphviz-node-attributes
    ((graph ast) (ast cleavir-ast:load-time-value-ast))
  '(:fillcolor :pink))

;;; BIND-AST Attributes

(defmethod graphviz-node-attributes
    ((graph ast) (ast cleavir-ast:bind-ast))
  '(:shape :ellipse))

;;; THE-AST Attributes

(defmethod graphviz-node-properties append
    ((graph ast) (ast cleavir-ast:the-ast))
  `(("required-types" . ,(stringify (cleavir-ast:required-types ast)))
    ("optional-types" . ,(stringify (cleavir-ast:optional-types ast)))
    ("rest-type" . ,(stringify (cleavir-ast:rest-type ast)))))

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
