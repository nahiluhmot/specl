(in-package #:specl)

;; Create a new environment that holds all of the metadata for a test context.
(defun new-env (&key (desc "") befores afters defs lets expectations children)
  (list 'ENV desc befores afters defs lets expectations children))

;; Anaphoric macro to access each element in an environment.
(defmacro with-env (env &body body)
  `(dbind (desc befores afters defs lets expectations children) (cdr ,env)
    ,@body))

;; Test if a value is an env.
(defun env? (env)
  (and (listp env)
       (= 8 (length env))
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
