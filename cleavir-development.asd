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
               "cleavir-environment"
               "cleavir-cst-to-ast"
               "cleavir-ast-to-hir"
               "cleavir-generate-ast"
               "cleavir-generate-ast-test")
  :components
  ((:module "Code"
    :serial t
    :components ((:file "package")
                 (:module "Graphviz"
                  :components ((:file "protocol")
                               (:file "cst")
                               (:file "ast")
                               (:file "flowchart")
                               (:file "hir-flowchart")
                               (:file "mir-flowchart")
                               (:file "lir-flowchart")
                               (:file "view")))))
   (:module "Examples"
    :components ((:file "view")))))
