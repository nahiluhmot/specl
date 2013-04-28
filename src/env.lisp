(in-package #:specl)

;; Create a new environment that holds all of the metadata for a test context.
(defun new-env (&key messages befores afters defs expectations children)
  (list 'ENV messages befores afters defs expectations children))

;; Anaphoric macro to access each element in an environment.
(defmacro with-env (env &body body)
  `(dbind (messages befores afters defs expectations children) (cdr ,env)
    ,@body))

;; Test if a value is an env.
(defun env? (env)
  (and (listp env)
       (= 7 (length env))
       (eq 'ENV (car env))
       (every #'listp (cdr env))))

;; Merge two envs together.
(defun env+ (env-a env-b)
  `(ENV ,@(mapcar (lambda (a b) (concatenate 'list a b))
                  (cdr env-a)
                  (cdr env-b))))

