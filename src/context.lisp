(in-package #:specl)

(defun validate-context-syntax (form)
  "Given a form, is a no-op if the form is a valid context. Raises an error otherwise."
  (and (or (listp form)
           (error "Expected a list, not: ~A" form)) 
       (or (stringp (car form))
           (error "The first element of a context must be a string, not: ~A" (car form)))
       (every (lambda (inner-form)
                 (and (or (listp inner-form)
                          (error "Each form after the description must be a non-null list, not ~A" inner-form))
                      (or (< 0 (length inner-form))
                          (error "Each form after the description must be a non-null list, not ~A" inner-form))
                      (or (member (car inner-form) '(before after defun defmacro let it context include-context) :test #'string=)
                          (error "Expected one of (before after func defmacro let it context include-context), got: ~A" (car inner-form)))
                      (if (string= 'context (car inner-form))
                        (validate-context-syntax (cdr inner-form))
                        t)))
              (cdr form))))

(defun normalize-descs (form)
  "Given a list formatted like this:
'(\"desc\" (before b1) (after a1) (context \"inner-desc\"))
Will return:
'((desc \"desc\") (before b1) (after a1) (context (desc \"inner-desc\")))
This makes lexing contexts much easier since everything is uniform."
  (dbind (desc &body body) form
    (cons `(desc ,desc)
          (mapcar (lambda (inner-form)
                    (or (and (listp inner-form)
                             (string= 'context (car inner-form))
                             `(context ,@(normalize-descs (cdr inner-form))))
                        inner-form))
                  body))))

(defmacro context (&body body)
  "Create a new test context. It will be added to a global *contexts* list that
holds all of the loaded contexts."
  (validate-context-syntax body) 
  (push (forms->env (normalize-descs body)) *contexts*)
  t)
