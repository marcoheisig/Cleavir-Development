(defsystem "cleavir-development"
  :description "Marco Heisig's Cleavir development tools."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "BSD"
  :depends-on ("uiop"
               "alexandria"
               "cl-dot"
               "eclector-concrete-syntax-tree"
               "concrete-syntax-tree"
               "cleavir-ast"
               "cleavir-io"
               "cleavir-generate-ast")
  :components
  ((:module "Code"
    :serial t
    :components ((:file "package")
                 (:module "Graphviz"
                  :components ((:file "generic-functions")
                               (:file "graph")
                               (:file "cst-graph")
                               (:file "ast-graph")
                               (:file "instruction-graph")
                               (:file "hir-graph")
                               (:file "mir-graph")
                               (:file "lir-graph")
                               (:file "cleavir-graph")
                               (:file "view")))))))
