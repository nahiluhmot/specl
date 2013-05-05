(in-package #:specl)

;; Contains all of the loaded shared contexts.
(defvar *shared-contexts* (make-hash-table :test #'equal))
