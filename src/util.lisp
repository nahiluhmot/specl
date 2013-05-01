(in-package #:specl)

;; Alias a function / macro.
(defmacro alias (new original)
  `(defmacro ,new (&rest lambda-list)
     `(,',original ,@lambda-list)))

;; Because 'destructuring-bind' is a pretty big keyboard tax for a commonly used
;; macro.
(alias dbind destructuring-bind)

(labels ((lazy-let-expand-one (arg body)
           (dbind (sym . value) arg
             (let ((sym-set? (gensym))
                   (sym-val  (gensym)))
               `(let ((,sym-set? nil)
                      (,sym-val  nil))
                 (symbol-macrolet ((,sym (if ,sym-set?
                                           ,sym-val
                                           (progn (setf ,sym-set? t)
                                                  (setf ,sym-val (progn ,@value))))))
                   ,body))))))
  ;; Acts much like let, with the expection that all of the arguments are
  ;; evaluated lazily and then cached.
  (defmacro lazy-let (definitions &body body)
    (if (null definitions)
      body
      (reduce #'lazy-let-expand-one
              definitions
              :initial-value `(progn ,@body)
              :from-end t))))
