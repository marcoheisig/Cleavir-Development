(defsystem "cleavir-development"
  :description "Marco Heisig's Cleavir development tools."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "BSD"
  :depends-on ("uiop"
               "cl-dot"
               "eclector"
               "concrete-syntax-tree"
               "cleavir-ast"
               "cleavir-io"
               "cleavir-generate-ast")
  :components
  ((:module "Code"
    :serial t
    :components ((:file "package")
                 (:module "Graph-Drawing"
                  :components ((:file "draw")
                               (:file "cst")
                               (:file "ast")))))))
