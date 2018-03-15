(defsystem "cleavir-development"
  :description "Marco Heisig's Cleavir development tools."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "BSD"
  :depends-on ("uiop"
               "alexandria"
               "cl-dot"
               "concrete-syntax-tree"
               "cleavir-ast"
               "cleavir-ir")
  :components
  ((:module "Code"
    :serial t
    :components ((:file "package")
                 (:module "Graphviz"
                  :components ((:file "utilities")
                               (:file "protocol")
                               (:file "cleavir")
                               (:file "cst")
                               (:file "ast")
                               (:file "flowchart")
                               (:file "hir")
                               (:file "mir")
                               (:file "lir")
                               (:file "visualization")))))))

(defsystem "cleavir-development-examples"
  :description "Some illustrating examples for the Cleavir development tools."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "BSD"
  :depends-on ("cleavir-development"
               "sicl-extrinsic-environment"
               "eclector-concrete-syntax-tree"
               "cleavir-cst-to-ast")
  :components
  ((:module "Examples"
    :serial t
    :components ((:file "package")
                 (:file "environment")
                 (:file "cst")
                 (:file "ast")
                 (:file "hir")))))
