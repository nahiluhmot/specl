(in-package #:specl)

;; Contains all of the loaded contexts.
(defvar *contexts* nil)

;; Contains all of the loaded shared contexts.
(defvar *shared-contexts* (make-hash-table :test #'equal))
