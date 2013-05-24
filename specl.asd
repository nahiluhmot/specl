(asdf:defsystem #:specl
  :description "An RSpec-like testing framework for common lisp"
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.0.0"
  :serial t
  :components ((:file "src/package")
               (:file "src/util"    :depends-on ("src/package"))
               (:file "src/tree"    :depends-on ("src/util"))
               (:file "src/globals" :depends-on ("src/util"))
               (:file "src/env"     :depends-on ("src/util"))
               (:file "src/syntax"  :depends-on ("src/env" "src/tree" "src/globals"))
               (:file "src/runner"  :depends-on ("src/tree" "src/globals"))))

(asdf:defsystem #:specl-spec
  :description "Tests for specl"
  :around-compile nil
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.0.0"
  :serial t
  :depends-on (:specl)
  :components ((:file "spec/package")
               (:file "spec/shared-contexts")
               (:file "spec/behaviors")
               (:file "spec/util-spec")
               (:file "spec/tree-spec")
               (:file "spec/env-spec")
               (:file "spec/syntax-spec")))
