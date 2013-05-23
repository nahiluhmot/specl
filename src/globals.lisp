(in-package #:specl.globals)

(defvar *contexts* (new-tree :value '("" nil))
  "Contains all of the loaded contexts.")

(defvar *shared-contexts* (make-hash-table :test #'equal)
  "Contains all of the loaded shared contexts.")

(defvar *behaviors* (make-hash-table :test #'equal)
  "Contains all of the loaded behaviors.")

(defun clear-globals! ()
  "Resets every global to its default value."
  (setq *contexts* (new-tree :value '("" nil)))
  (clrhash *shared-contexts*)
  (clrhash *behaviors*)
  t)
