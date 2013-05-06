(in-package #:specl-behavior)

(defun validate-behavior-syntax (form)
  "Given a form, will be a no-op if the form is a valid behavior. Otherwise, raises an error."
  (and (or (listp form)
           (error "Expected a list, not: ~A" form))
       (or (stringp (car form))
           (error "The first element of a shared-context must be a string, not: ~A" (car form)))
       (every (lambda (inner-form)
                (and (or (listp inner-form)
                         (error "Each form after the description must be a non-null list, not ~A" inner-form))
                     (or (< 0 (length inner-form))
                         (error "Each form after the description must be a non-null list, not ~A" inner-form))
                     (or (member (car inner-form) '(before after defun defmacro let it context include-context) :test #'string=)
                         (error "Expected one of (before after defun defmacro let it context include-context it-behaves-like), got: ~A" (car inner-form)))
                     (if (string= 'context (car inner-form))
                         (validate-context-syntax (cdr inner-form)))))
              (cdr form))))


(defmacro behavior (&body body)
  "Create a new shared context with a given body. This will add the environment
it creates to a global `*shared-contexts*` hash so that it may be included in
other contexts / context-like forms."
  (validate-behavior-syntax body)
  (setf (gethash (car body) *behaviors*)
        (env+ (new-env :desc "like") (forms->env (normalize-descs body))))
  t)

