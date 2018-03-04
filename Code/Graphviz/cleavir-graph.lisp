(cl:in-package #:cleavir-development)

(defclass cleavir-graph
    (ast-graph
     cst-graph
     hir-graph
     mir-graph
     lir-graph)
  ())
