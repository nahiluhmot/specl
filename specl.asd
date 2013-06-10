(defsystem #:specl
  :description "An RSpec-like testing framework for Common Lisp"
  :author "Tom Hulihan"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "util"    :depends-on ("package"))
               (:file "tree"    :depends-on ("util"))
               (:file "globals" :depends-on ("util"))
               (:file "env"     :depends-on ("util"))
               (:file "test"    :depends-on ("util"))
               (:file "syntax"  :depends-on ("env" "tree" "globals"))
               (:file "runner"  :depends-on ("tree" "globals"))))

(defsystem #:specl-spec
  :description "Tests for specl"
  :author "Tom Hulihan"
  :license "MIT"
  :serial t
  :depends-on (:specl)
  :pathname "spec/"
  :components ((:file "package")
               (:file "shared-contexts" :depends-on ("package"))
               (:file "behaviors"       :depends-on ("package"))
               (:file "util-spec"       :depends-on ("package"))
               (:file "tree-spec"       :depends-on ("package"))
               (:file "env-spec"        :depends-on ("package"))
               (:file "test-spec"       :depends-on ("package"))
               (:file "syntax-spec"     :depends-on ("package"))))

(defmethod perform ((o test-op) (c (eql (find-system :specl))))
  (operate 'load-op :specl-spec)
  (operate 'test-op :specl-spec))

(defmethod perform ((o test-op) (c (eql (find-system :specl-spec))))
  (or (zerop (funcall (intern (string '#:run-all) :specl)))
      (error "Some specs failed!")))
