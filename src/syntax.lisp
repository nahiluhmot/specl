(in-package #:specl-syntax)

(defun validate-syntax (form children)
  "Given a form and list of possible children, is a no-op if the form is valid.
Raises an error otherwise."
  (and (or (listp form)
           (error "Expected a list, not: ~A" form)) 
       (or (stringp (car form))
           (error "The first element of a context must be a string, not: ~A"
                  (car form)))
       (every (lambda (inner-form)
                 (and (or (and (listp inner-form) (not (null inner-form))) 
                          (error "Expected a list, got: ~A" inner-form))
                      (or (member (car inner-form) children :test #'string=)
                          (error "Expected one of ~A, got: ~A"
                                 children (car inner-form)))
                      (if (string= 'context (car inner-form))
                          (validate-syntax (cdr inner-form))
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
  (validate-syntax body '(before after defun defmacro let it context
                          include-context it-behaves-like))
  (push (forms->env (normalize-descs body)) *contexts*)
  t)

(defmacro shared-context (&body body)
  "Create a new shared context with a given body. This will add the environment
it creates to a global `*shared-contexts*` hash so that it may be included in
other contexts / context-like forms."
  (validate-syntax body '(before after defun defmacro let include-context))
  (setf (gethash (car body) *shared-contexts*)
        (forms->env (cdr body)))
  t)

(defmacro behavior (&body body)
  "Creates a new behavior with the given body. The behavior will be added to a
global `*behaviors*` hash, with the description as the key and the env that is
generated as the body."
  (validate-syntax body '(before after defun defmacro let it context
                          include-context it-behaves-like))
  (setf (gethash (car body) *behaviors*)
        (env+ (new-env :desc "like") (forms->env (normalize-descs body))))
  t)
