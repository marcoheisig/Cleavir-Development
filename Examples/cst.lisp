(cl:in-package #:cleavir-development-examples)

(defgeneric convert-to-cst (object))

(defmethod convert-to-cst ((string string))
  (with-input-from-string (stream string)
    (eclector.concrete-syntax-tree:cst-read stream)))

(defmethod convert-to-cst ((form list))
  (concrete-syntax-tree:cst-from-expression form))

(defun cst-example-1 ()
  (view
   (convert-to-cst "(+ 2 2)")))

(defun cst-example-2 ()
  (view
   (convert-to-cst "(defun foo (x) (* x x))")))
