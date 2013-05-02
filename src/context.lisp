(in-package #:specl)

;; Given a form, is a no-op if the form is a valid context. Raises an error otherwise.
(defun validate-syntax (form)
  (and (or (stringp (car form))
           (error "The first element of a context must be a string, not: ~A" (car form)))
       (every (lambda (inner-form)
                 (and (or (listp inner-form)
                          (error "Each form after the description must be a non-null list, not ~A" inner-form))
                      (or (< 0 (length inner-form))
                          (error "Each form after the description must be a non-null list, not ~A" inner-form))
                      (or (member (car inner-form) '(before after def let it context))
                          (error "Expected one of (before after def let it context), got: " (car inner-form)))
                      (if (eq 'context (car inner-form))
                        (validate-syntax (cdr inner-form))
                        t)))
              (cdr form))))

;; Given a list formatted like this:
;; '("desc" (before b1) (after a1) (context "inner-desc"))
;; Will return:
;; '((desc "desc") (before b1) (after a1) (context (desc "inner-desc")))
;; This makes lexing contexts much easier since everything is uniform.
(defun normalize-descs (form)
  (dbind (desc &body body) form
    (cons `(desc ,desc)
          (mapcar (lambda (inner-form)
                    (or (and (listp inner-form)
                             (eq 'context (car inner-form))
                             `(context ,@(normalize-descs (cdr inner-form))))
                        inner-form))
                  body))))

;; Given a single form, will produce an env.
(defun form->env (form)
  (dbind (name . body) form
    (apply #'new-env
           (case name
             ('desc    `(:desc         ,(car body)))
             ('before  `(:befores      ,body))
             ('after   `(:afters       ,body))
             ('def     `(:defs         ,(list body)))
             ('let     `(:lets         ,(list body)))
             ('it      `(:expectations ,(list body)))
             ('context `(:children     ,(list (forms->env body))))))))

;; Given an env and list of valid forms, creates an env by merging each form togetger.
(defun forms->env (body)
  (reduce #'env+
          (mapcar #'form->env body)
          :initial-value (new-env)))

;; Given an env, produces Common Lisp code.
(defun env->cl (env)
  (with-env env
    `(symbol-macrolet ,(reverse lets)
       (labels ,(reverse defs)
         ,@(mapcar (lambda (it)
                     `(handler-case
                        (progn 
                          ,@befores
                          ,@(cdr it)
                          ,@afters
                          (format t "."))
                        (error (e) (format t "Failed ~A ~A~%" ,desc ,(car it)))))
                     expectations)
         ,@(mapcar (lambda (inner-env) (env->cl (inherit env inner-env))) children)))))

;; Create a new test context.
(defmacro context (&body body)
  (validate-syntax body) 
  (env->cl (forms->env (normalize-descs body))))
