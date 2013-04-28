(in-package #:specl)

;; Alias a function / macro.
(defmacro alias (new original)
  `(defmacro ,new (&rest lambda-list)
     `(,',original ,@lambda-list)))

;; Because 'destructuring-bind' is a pretty big keyboard tax for a commonly used
;; macro.
(alias dbind destructuring-bind)

