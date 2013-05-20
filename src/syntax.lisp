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

(defun form->env-tree (form)
  "Given a single form, will produce an env-tree."
  (dbind (name . body) form
    (string-case name
      ('desc     (new-tree :value (new-env :desc    (car body))))
      ('before   (new-tree :value (new-env :befores body)))
      ('after    (new-tree :value (new-env :afters  body)))
      ('defun    (new-tree :value (new-env :funcs   (list body))))
      ('defmacro (new-tree :value (new-env :macros  (list body))))
      ('let      (new-tree :value (new-env :lets    (list body))))
      ('it       (new-tree :children
                           (list (new-tree :value (new-env :desc (car body)
                                                           :expectation (cdr body))))))
      ('context  (new-tree :children (list (forms->env-tree body))))
      ('include-context (or (gethash (car body) *shared-contexts*)
                            (error "Could not find matching shared context for: '~A'" (car body))))
      ('it-behaves-like (let ((behavior (gethash (car body) *behaviors*)))
                             (or (and (not (null behavior))
                                      (new-tree :children (list behavior)))
                                 (error "Could not find matching behavior for '~A'" (car body))))))))

(defun forms->env-tree (forms)
  "Given a list of forms, produces an env tree."
  (reduce (lambda (tree curr)
            (new-tree :value (env+ (or (value tree) (new-env))
                                   (or (value curr) (new-env)))
                      :children (mapcan #'tree-children (list tree curr))))
          (mapcar #'form->env-tree forms)
          :initial-value (new-tree)))

(defun env-tree->lambda-tree (tree)
  "Given a tree composed of envs, returns a tree where each value is structered
as such:
  `(,desc ,zero-arg-lambda)
This allows each spec to be transformed into code and cached."
  (map-with-accum
    (lambda (env acc)
      (let ((next-env (inherit acc env))
            (e (gensym)))
        (values
          (with-env next-env
            (if (null expectation)
              `(list ,desc nil)
              `(list ,desc
                 (lambda ()
                   (handler-case
                     (lazy-let ,lets
                       (macrolet ,macros
                         (labels ,funcs
                           ,@befores
                           ,@expectation
                           ,@afters
                           'pass)))
                     (error (,e) ,e))))))
          next-env)))
    tree
    :initial-value (new-env)))

(defmacro context (&body body)
  "Create a new test context. It will be added to a global *contexts* list that
holds all of the loaded contexts."
  (validate-syntax body '(before after defun defmacro let it context
                          include-context it-behaves-like subject))
  `(setf *contexts* 
         (add-child ,(tree->new-tree-syntax
                       (env-tree->lambda-tree
                         (forms->env-tree
                           (normalize body))))
                    *contexts*)))

(defmacro shared-context (&body body)
  "Create a new shared context with a given body. This will add the environment
it creates to a global `*shared-contexts*` hash so that it may be included in
other contexts / context-like forms."
  (validate-syntax body '(before after defun defmacro let include-context
                          subject))
  `(setf (gethash ,(car body) *shared-contexts*)
         ,(let ((env-tree (forms->env-tree (normalize body))))
            (tree->new-tree-syntax
              (map-tree #'env->new-env-syntax 
                        (set-value (append '(ENV "") (cddr (value env-tree)))
                                   env-tree))))))

(defmacro behavior (&body body)
  "Creates a new behavior with the given body. The behavior will be added to a
global `*behaviors*` hash, with the description as the key and the env that is
generated as the body."
  (validate-syntax body '(before after defun defmacro let it context
                          include-context it-behaves-like subject))
  `(setf (gethash ,(car body) *behaviors*)
         ,(let ((env-tree (forms->env-tree (normalize body))))
            (tree->new-tree-syntax
              (map-tree #'env->new-env-syntax 
                        (set-value (env+ (new-env :desc "like") (value env-tree))
                                   env-tree))))))
