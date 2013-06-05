(in-package #:specl.env)

(defun new-env (&key (desc "") tags befores afters funcs macros lets expectation)
  "Create a new environment that holds all of the metadata for a test context."
  (list 'ENV desc tags befores afters funcs macros lets expectation))

(defun env? (env)
  "Test if a value is an env."
  (and (listp env)
       (= 9 (length env))
       (string= 'ENV (car env))
       (stringp (cadr env))
       (every #'listp (cddr env))
       t))

(defmacro with-env (env &body body)
  "Anaphoric macro to access each element in an environment."
  (let ((evaluated-env (gensym)))
    `(let ((,evaluated-env ,env))
       (if (env? ,evaluated-env)
         (dbind (desc tags befores afters funcs macros lets expectation)
                (cdr ,evaluated-env)
                ,@body)
         (error "specl.env:with-env expected an env, got: '~A'" ,evaluated-env)))))

(defun env->new-env-syntax (env)
  (if (env? env)
    (with-env env
      `(new-env :desc ',desc :tags ',tags :befores ',befores :afters ',afters
                :funcs ',funcs :macros ',macros :lets ',lets
                :expectation ',expectation))
    (error "specl.env:env->new-env-syntax expected an env, got: ~A" env)))

(defun env+ (env-a env-b)
  "Merge two envs together."
  (if (every #'env? (list env-a env-b))
    `(ENV ,(string-trim " " (concatenate 'string (cadr env-a) " " (cadr env-b)))
          ,@(mapcar (lambda (a b) (concatenate 'list a b))
                    (cddr env-a)
                    (cddr env-b)))
    (error "specl.env:env+ expected two envs, got: '~A' and '~A'" env-a env-b)))

(defun inherit (parent child)
  "Given a parent and child environment, will add the parents befores and afters
   to the child."
  (let ((env (with-env parent (new-env :tags    tags
                                       :befores befores
                                       :afters  afters
                                       :funcs   funcs
                                       :macros  macros
                                       :lets    lets))))
    (env+ env child)))
