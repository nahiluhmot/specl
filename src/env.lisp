(in-package #:specl)

;; Create a new environment that holds all of the metadata for a test context.
(defun new-env (&key (desc "") befores afters funcs macros lets expectations children)
  (list 'ENV desc befores afters funcs macros lets expectations children))

;; Anaphoric macro to access each element in an environment.
(defmacro with-env (env &body body)
  `(dbind (desc befores afters funcs macros lets expectations children) (cdr ,env)
    ,@body))

;; Test if a value is an env.
(defun env? (env)
  (and (listp env)
       (= 9 (length env))
       (eq 'ENV (car env))
       (stringp (cadr env))
       (every #'listp (cddr env))))

;; Merge two envs together.
(defun env+ (env-a env-b)
  `(ENV ,(string-trim " " (concatenate 'string (cadr env-a) " " (cadr env-b)))
        ,@(mapcar (lambda (a b) (concatenate 'list a b))
                  (cddr env-a)
                  (cddr env-b))))

;; Given a parent and child environment, will add the parents befores and afters
;; to the child.
(defun inherit (parent child)
  (let ((env (with-env parent (new-env :befores befores :afters afters))))
    (env+ env child)))

;; Given a single form, will produce an env.
(defun form->env (form)
  (dbind (name . body) form
     (case name
       ('desc     (new-env :desc         (car body)))
       ('before   (new-env :befores      body))
       ('after    (new-env :afters       body))
       ('defun    (new-env :funcs        (list body)))
       ('defmacro (new-env :macros       (list body)))
       ('let      (new-env :lets         (list body)))
       ('it       (new-env :expectations (list body)))
       ('context  (new-env :children     (list (forms->env body))))
       ('include-context (or (gethash (car body) *shared-contexts*)
                             (error "Could not find shared context: ~A" (car body)))))))

;; Given an env and list of valid forms, creates an env by merging each form
;; together.
(defun forms->env (body)
  (reduce #'env+
          (mapcar #'form->env body)
          :initial-value (new-env)))
