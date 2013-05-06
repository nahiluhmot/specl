(in-package #:specl-globals)

(defvar *contexts* nil
  "Contains all of the loaded contexts.")

(defvar *shared-contexts* (make-hash-table :test #'equal)
  "Contains all of the loaded shared contexts.")
