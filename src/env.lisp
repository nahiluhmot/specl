(in-package #:specl)

(defun new-env (&key (desc "") befores afters funcs macros lets expectations children)
  "Create a new environment that holds all of the metadata for a test context."
  (list 'ENV desc befores afters funcs macros lets expectations children))

(defmacro with-env (env &body body)
  "Anaphoric macro to access each element in an environment."
  `(dbind (desc befores afters funcs macros lets expectations children) (cdr ,env)
    ,@body))

(defun env? (env)
  "Test if a value is an env."
  (and (listp env)
       (= 9 (length env))
       (eq 'ENV (car env))
       (stringp (cadr env))
       (every #'listp (cddr env))))

(defun env+ (env-a env-b)
  "Merge two envs together."
  `(ENV ,(string-trim " " (concatenate 'string (cadr env-a) " " (cadr env-b)))
        ,@(mapcar (lambda (a b) (concatenate 'list a b))
                  (cddr env-a)
                  (cddr env-b))))

(defun inherit (parent child)
  "Given a parent and child environment, will add the parents befores and afters
   to the child."
  (let ((env (with-env parent (new-env :befores befores :afters afters))))
    (env+ env child)))

(defun form->env (form)
  "Given a single form, will produce an env."
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

(defun forms->env (body)
  "Given an env and list of valid forms, creates an env by merging each form together."
  (reduce #'env+
          (mapcar #'form->env body)
          :initial-value (new-env)))
