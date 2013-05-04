(in-package #:specl)

;; Contains all of the loaded shared contexts.
(defvar *shared-contexts* (make-hash-table :test #'equal))

;; Given a form, is a no-op if the form is a valid context. Raises an error otherwise.
(defun validate-context-syntax (form)
  (and (or (listp form)
           (error "Expected a list, not: ~A" form)) 
       (or (stringp (car form))
           (error "The first element of a context must be a string, not: ~A" (car form)))
       (every (lambda (inner-form)
                 (and (or (listp inner-form)
                          (error "Each form after the description must be a non-null list, not ~A" inner-form))
                      (or (< 0 (length inner-form))
                          (error "Each form after the description must be a non-null list, not ~A" inner-form))
                      (or (member (car inner-form) '(before after def let it context include-context))
                          (error "Expected one of (before after def let it context include-context), got: ~A" (car inner-form)))
                      (if (eq 'context (car inner-form))
                        (validate-context-syntax (cdr inner-form))
                        t)))
              (cdr form))))

(defun validate-shared-context-syntax (form)
  (and (or (listp form)
           (error "Expected a list, not: ~A" form))
       (or (stringp (car form))
           (error "The first element of a shared-context must be a string, not: ~A" (car form)))
       (every (lambda (inner-form)
                (and (or (listp inner-form)
                         (error "Each form after the description must be a non-null list, not ~A" inner-form))
                     (or (< 0 (length inner-form))
                         (error "Each form after the description must be a non-null list, not ~A" inner-form))
                     (or (member (car inner-form) '(before after def let include-context))
                         (error "Expected one of (before after def let include-context), got: ~A" (car inner-form)))))
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
     (case name
       ('desc    (new-env :desc         (car body)))
       ('before  (new-env :befores      body))
       ('after   (new-env :afters       body))
       ('def     (new-env :defs         (list body)))
       ('let     (new-env :lets         (list body)))
       ('it      (new-env :expectations (list body)))
       ('context (new-env :children     (list (forms->env body))))
       ('include-context (or (gethash (car body) *shared-contexts*)
                             (error "Could not find shared context: ~A" (car body)))))))

;; Given an env and list of valid forms, creates an env by merging each form
;; together.
(defun forms->env (body)
  (reduce #'env+
          (mapcar #'form->env body)
          :initial-value (new-env)))

;; Given an env, produces Common Lisp code.
(defun env->cl (env)
  (with-env env
    `(lazy-let ,(reverse lets)
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

(defmacro shared-context (&body body)
  (validate-shared-context-syntax body)
  (setf (gethash (car body) *shared-contexts*)
        (forms->env (cdr body)))
  t)

;; Create a new test context.
(defmacro context (&body body)
  (validate-context-syntax body) 
  (env->cl (forms->env (normalize-descs body))))
