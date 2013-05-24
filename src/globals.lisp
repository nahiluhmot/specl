(in-package #:specl.globals)

(defvar *contexts* (new-tree :value '("" nil))
  "Contains all of the loaded contexts.")

(defvar *shared-contexts* (new-tree) 
  "Contains all of the loaded shared contexts.")

(defvar *behaviors* (make-hash-table :test #'equal)
  "Contains all of the loaded behaviors.")
