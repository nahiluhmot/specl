(in-package #:specl.spec)

(behavior "a function that returns a singleton env-tree" 
  (it "is a new tree with an env value"
    (is (env? (value subject))))
  (it "has no children"
    (is (null (tree-children subject)))))
