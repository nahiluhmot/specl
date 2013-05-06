(in-package #:specl-shared-context)

(defun validate-shared-context-syntax (form)
  "Given a form, will be a no-op if the form is a valid shared context. Otherwise, raises an error."
  (and (or (listp form)
           (error "Expected a list, not: ~A" form))
       (or (stringp (car form))
           (error "The first element of a shared-context must be a string, not: ~A" (car form)))
       (every (lambda (inner-form)
                (and (or (listp inner-form)
                         (error "Each form after the description must be a non-null list, not ~A" inner-form))
                     (or (< 0 (length inner-form))
                         (error "Each form after the description must be a non-null list, not ~A" inner-form))
                     (or (member (car inner-form) '(before after defun defmacro let include-context) :test #'string=)
                         (error "Expected one of (before after defun defmacro let include-context), got: ~A" (car inner-form)))))
              (cdr form))))

(defmacro shared-context (&body body)
  "Create a new shared context with a given body. This will add the environment
it creates to a global `*shared-contexts*` hash so that it may be included in
other contexts / context-like forms."
  (validate-shared-context-syntax body)
  (setf (gethash (car body) *shared-contexts*)
        (forms->env (cdr body)))
  t)

