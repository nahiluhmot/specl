(in-package #:specl-syntax)

(defun validate-syntax (form children)
  "Given a form and list of possible children, is a no-op if the form is valid.
Raises an error otherwise."
  (and (or (and (listp form) (not (null form))) 
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
                          (validate-syntax (cdr inner-form) children)
                        t)))
              (cdr form))
       t))

(defun normalize (form)
  "Given a list formatted like this:
'(\"desc\" (subject sub) (before b1) (after a1) (context \"inner-desc\"))
Will return:
'((desc \"desc\") (let subject sub) (before b1) (after a1)
  (context (desc \"inner-desc\")))
This makes lexing contexts much easier since everything is uniform."
  (dbind (desc &body body) form
    (cons `(desc ,desc)
          (mapcar (lambda (inner-form)
                    (or (and (string= 'context (car inner-form))
                             `(context ,@(normalize (cdr inner-form))))
                        (and (string= 'subject (car inner-form))
                             `(let ,(car inner-form) ,@(cdr inner-form)))
                        inner-form))
                  body))))

(defun form->env (form)
  "Given a single form, will produce an env."
  (dbind (name . body) form
     (string-case name
       ('desc     (new-env :desc        (car body)))
       ('before   (new-env :befores     body))
       ('after    (new-env :afters      body))
       ('defun    (new-env :funcs       (list body)))
       ('defmacro (new-env :macros      (list body)))
       ('let      (new-env :lets        (list body)))
       ('it       (new-env :children    (list (new-env :desc (car body)
                                                       :expectation (cdr body)))))
       ('context  (new-env :children    (list (forms->env body))))
       ('include-context (or (gethash (car body) *shared-contexts*)
                             (error "Could not find shared context: ~A" (car body))))
       ('it-behaves-like (let ((behavior (gethash (car body) *behaviors*)))
                           (or (and behavior
                                    (new-env :children behavior))
                               (error "Could not find behavior: ~A" (car body))))))))

(defun forms->env (body)
  "Given an env and list of valid forms, creates an env by merging each form together."
  (reduce #'env+
          (mapcar #'form->env body)
          :initial-value (new-env)))

(defun env->lambdas (env)
  "Given an env, will produce a tree of lambdas that return `pass` if the code
executed without an error. In the case of an error, the error itself is
returned."
  (with-env env
    (if (null expectation)
      `(new-tree
         :value (list ,desc nil)
         :children (list ,@(mapcar (lambda (child)
                                     (env->lambdas (inherit env child)))
                                   children)))
      (let ((e (gensym)))
        `(new-tree :value
           (list ,desc
             (lambda ()
               (handler-case
                 (lazy-let ,lets
                   (macrolet ,macros
                     (labels ,funcs
                       ,@befores
                       ,@expectation
                       ,@afters
                       'pass)))
                 (error (,e) ,e)))))))))

(defmacro context (&body body)
  "Create a new test context. It will be added to a global *contexts* list that
holds all of the loaded contexts."
  (validate-syntax body '(before after defun defmacro let it context
                          include-context it-behaves-like subject))
  `(setq *contexts* 
         (add-child ,(env->lambdas (forms->env (normalize body)))
                    *contexts*)))

(defmacro shared-context (&body body)
  "Create a new shared context with a given body. This will add the environment
it creates to a global `*shared-contexts*` hash so that it may be included in
other contexts / context-like forms."
  (validate-syntax body '(before after defun defmacro let include-context
                          subject))
  (setf (gethash (car body) *shared-contexts*)
        (cons 'ENV (cons "" (cddr (forms->env (normalize body))))))
  t)

(defmacro behavior (&body body)
  "Creates a new behavior with the given body. The behavior will be added to a
global `*behaviors*` hash, with the description as the key and the env that is
generated as the body."
  (validate-syntax body '(before after defun defmacro let it context
                          include-context it-behaves-like subject))
  (setf (gethash (car body) *behaviors*)
        (env+ (new-env :desc "like") (forms->env (normalize body))))
  t)
