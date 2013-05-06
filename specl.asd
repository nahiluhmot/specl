(asdf:defsystem #:specl
  :description "An RSpec-like testing framework for common lisp"
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.0.0"
  :serial t
  :components ((:file "src/package")
               (:file "src/globals")
               (:file "src/util")
               (:file "src/env")
               (:file "src/context")
               (:file "src/shared-context")
               (:file "src/behavior")))
