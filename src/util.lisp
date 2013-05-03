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

(labels ((replace-let*-with-lazy-let (form)
           (cond ((listp form) (mapcar #'replace-let*-with-lazy-let form))
                 ((eq 'let* form) 'lazy-let)
                 (t form))))
  ;; Lazily evaluate each argument to destructuring-bind by replacing `let*`
  ;; with `lazy-let`. Currently, this macro will replace every occurrence of
  ;; `let*` with `lazy-let`, regardless of whether or not it actually is a
  ;; function call. Thus, it should be used with caution.
  (defmacro lazy-dbind (&body args)
    (replace-let*-with-lazy-let (macroexpand-1 `(destructuring-bind ,@args)))))
