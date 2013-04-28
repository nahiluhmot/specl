(asdf:defsystem #:specl
  :description "An RSpec-like testing framework for common lisp"
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.0.0"
  :serial t
  :components ((:file "src/package")
               (:file "src/util")
               (:file "src/env")))
