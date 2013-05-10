(in-package #:specl-globals)

(defvar *contexts* nil
  "Contains all of the loaded contexts.")

(defvar *shared-contexts* (make-hash-table :test #'equal)
  "Contains all of the loaded shared contexts.")

(defvar *behaviors* (make-hash-table :test #'equal)
  "Contains all of the loaded behaviors.")

(defvar *passes* nil
  "Holds the descriptions for each passing spec.")

(defvar *failures* nil
  "Holds the descriptions for each failing spec.")

(defun clear-globals! ()
  "Resets every global to its default value."
  (setq *contexts* nil)
  (clrhash *shared-contexts*)
  (clrhash *behaviors*)
  (setq *passes* nil)
  (setq *failures* nil)
  t)
