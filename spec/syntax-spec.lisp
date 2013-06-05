(in-package #:specl.spec)

(context "validate-syntax"
  (include-context "error-helpers")

  (subject (validate-syntax form valid-children))
  (let valid-children '(before after defun defmacro let it context
                        include-context it-behaves-like subject))

  (context "when the form is not a list"
    (let form 1)
    (it "raises an error"
      (raises-error subject)))

  (context "when the form is null"
    (let form nil)
    (it "raises an error"
      (raises-error subject)))

  (context "when the form is a list"
    (let form (cons desc body))
    (let body nil)

    (context "but the first argument is not a string"
      (let desc 'not-a-string)
      (it "raises an error"
        (raises-error subject)))

    (context "and the first argument is a string"
      (let desc "sample-desc")

      (context "when at least one of the following forms is not a list"
        (let body '((before) after))
        (it "raises an error"
          (raises-error subject)))

      (context "when each following form is a list"
        (context "but at least one of the forms is not one of the expected children"
          (let body '((subject 1) (not-a-child)))
          (it "raises an error"
            (raises-error subject)))

        (context "and each of the following forms is one of the expected children"
          (context "with no child contexts"
            (let body '((after (format t "after~%")) (before (format t "before~%"))))
            (it "returns t"
              (is (eq t subject))))

          (context "with child contexts"
            (context "when at least one is invalid"
              (let body '((context "inner-context" this-breaks-it)))
              (it "raises an error"
                (raises-error subject)))

            (context "when they are all valid"
              (let body '((context "inner-context" (it "works" 1))))
              (it "returns t"
                (is (eq t subject))))))))))

(context "normalize"
  (subject (normalize '("suckerz" (subject 1))))

  (it "wraps the first value in the list into a `(desc ,value) structure"
    (is (equal '(desc "suckerz") (car subject))))

  (it "replaces `(subject ,form) with `(let subject ,form)"
    (is (equal '(let subject 1) (cadr subject)))))

(context "form->env-tree"
  (subject (form->env-tree form))

  (defun string-empty? (str)
    (and (stringp str) (string= str "")))
 
  (context "desc"
    (let test-desc "my-desc")
    (let form `(desc ,test-desc))

    (it-behaves-like "a function that returns a singleton env-tree")

    (context "the env"
      (it "has its desc set"
        (with-env (value subject) 
          (is (string= desc test-desc))))
      (it "has no other fields set"
        (is (every #'null (cddr subject))))))

  (context "before"
    (let test-befores '((before (format t "hi"))))
    (let form `(before ,@test-befores))

    (it-behaves-like "a function that returns a singleton env-tree")

    (context "the env"
      (let env (value subject))

      (it "has its befores set"
        (with-env env
          (is (equal test-befores befores))))

      (it "has nothing else set"
        (is (string-empty? (cadr env)))
        (is (null (caddr env)))
        (is (every #'null (cddddr env))))))
  
  (context "after"
    (let test-afters '((after (format t "hello"))))
    (let form `(after ,@test-afters))

    (it-behaves-like "a function that returns a singleton env-tree")

    (context "the env"
      (let env (value subject))
      (it "has its afters set"
        (with-env env
          (is (equal test-afters afters))))

      (it "has nothing else set"
        (with-env env
          (is (string-empty? desc))
          (is (null befores))
          (is (null funcs))
          (is (null macros))
          (is (null lets))
          (is (null expectation))))))

  (context "defun"
    (let test-func '(my-func (a b) (+ a b)))
    (let test-funcs (list test-func))
    (let form `(defun ,@test-func))

    (it-behaves-like "a function that returns a singleton env-tree")

    (context "the env"
      (let env (value subject))

      (it "has its funcs set"
        (with-env env
          (is (equal test-funcs funcs))))

      (it "has nothing else set"
        (with-env env
          (is (string-empty? desc))
          (is (null befores))
          (is (null afters))
          (is (null macros))
          (is (null lets))
          (is (null expectation))))))

  (context "defmacro"
    (let test-macro '(my-macro (&rest args) (reverse args)))
    (let test-macros (list test-macro))
    (let form `(defmacro ,@test-macro))

    (it-behaves-like "a function that returns a singleton env-tree")

    (context "the env"
      (let env (value subject))

      (it "has its macros set"
        (with-env env
          (is (equal test-macros macros))))

      (it "has nothing else set"
        (with-env env
          (is (string-empty? desc))
          (is (null befores))
          (is (null afters))
          (is (null funcs))
          (is (null lets))
          (is (null expectation))))))

  (context "let"
    (let test-let '(name 1))
    (let test-lets (list test-let))
    (let form `(let ,@test-let))

    (it-behaves-like "a function that returns a singleton env-tree")

    (context "the env"
      (let env (value subject))

      (it "has its lets set"
        (with-env env
          (is (equal test-lets lets))))

      (it "has nothing else set"
        (with-env env
          (is (string-empty? desc))
          (is (null befores))
          (is (null afters))
          (is (null funcs))
          (is (null macros))
          (is (null expectation))))))
  
  (context "it"
    (let test-it '("works" (is (null nil))))
    (let form `(it ,@test-it))
    
    (it "has no value"
      (is (null (value subject))))

    (it "has 1 child with no childern"
      (is (equal 1 (length (tree-children subject))))
      (is (null (tree-children (car (tree-children subject))))))

    (context "the child's env"
      (let env (value (car (tree-children subject))))

      (it "has a desc"
        (with-env env
          (is (string= (car test-it) desc))))

      (it "has an expectation"
        (with-env env
          (is (equal (cdr test-it) expectation))))

      (it "has null fields for everything else"
        (with-env env
          (is (null befores))
          (is (null afters))
          (is (null funcs))
          (is (null macros))
          (is (null lets))))))

  (context "context"
    (let test-desc "my-context")
    (let test-before '(format t "before"))
    (let test-let '(my-test-name (+ 1 2)))
    (let test-befores (list test-before))
    (let test-lets (list test-let))
    (let test-context `(context
                         (desc ,test-desc)
                         (before ,test-before)
                         (let ,@test-let)
                         ,@test-children))
    (let form test-context)

    (context "when the context has no inner children"
      (let test-children nil)

      (it "has no value"
        (is (null (value subject))))

      (it "has one child"
        (is (equal 1 (length (tree-children subject)))))

      (context "the child"
        (let child (car (tree-children subject)))
        (let env (value child))

        (it "has no children"
          (is (null (tree-children child))))

        (it "adds the expected data to the context"
          (with-env env
            (is (string= test-desc desc))
            (is (equal test-befores befores))
            (is (equal test-lets lets))
            (is (null afters))
            (is (null funcs))
            (is (null macros))))))

    (context "when the context has inner childern"
      (let inner-desc "inner")
      (let inner-context `(context
                            (desc ,inner-desc)))
      (let test-children (list inner-context))

      (it "has no value"
        (is (null (value subject))))

      (it "has one child"
        (is (= 1 (length (tree-children subject)))))

      (context "the child"
        (let child (car (tree-children subject)))
        (let env (value child))

        (it "adds the expected data to the context"
          (with-env env
            (is (string= test-desc desc))
            (is (equal test-befores befores))
            (is (equal test-lets lets))
            (is (null afters))
            (is (null funcs))
            (is (null macros))))  

        (it "has one child"
          (is (= 1 (length (tree-children child)))))

        (context "its child"
          (let child-child (car (tree-children child)))
          (let child-env (value child-child))

          (it "has no children"
            (is (null (tree-children child-child))))

          (it "sets the exepected data"
            (is (string= inner-desc (cadr child-env)))
            (is (every #'null (cddr child-env))))))))
  
  (context "include-context"
    (let shared-context-name "test-shared-context")
    (let form `(include-context ,shared-context-name))

    (defun remove-test-shared-context ()
      (setf *shared-contexts*
        (remove-children-if
          (lambda (child)
            (with-env (value child)
              (string= desc shared-context-name)))
          *shared-contexts*)))

    (context "when the shared context has not been defined"
      (include-context "error-helpers")
      (before (remove-test-shared-context))

      (it "raises an error"
        (raises-error subject)))

    (context "when the shared context has been defined"
      (let test-befores '((format t "before")))
      (let env-tree (new-tree :value (new-env :desc shared-context-name
                                              :befores test-befores)))

      (before (setf *shared-contexts*
                    (add-child env-tree *shared-contexts*)))
      (after (remove-test-shared-context))

      (it "has no children"
        (is (null (tree-children subject))))

      (it "returns that env-tree"
        (with-env (value subject)
          (is (string-empty? desc))
          (is (equal test-befores befores))
          (is (null afters))
          (is (null funcs))
          (is (null macros))
          (is (null lets))
          (is (null expectation))))))

  (context "it-behaves-like"
    (let behavior-name "test-behavior")
    (let form `(it-behaves-like ,behavior-name))

    (defun remove-test-behavior ()
      (setf *behaviors*
        (remove-children-if
          (lambda (child)
            (with-env (value child)
              (string= desc behavior-name)))
          *behaviors*)))

    (context "when the behavior has not been defined"
      (include-context "error-helpers")
      (before (remove-test-behavior))

      (it "raises an error"
        (raises-error subject)))

    (context "when the behavior has been defined"
      (let env-tree
        (new-tree :value    (new-env :desc "test-behavior")
                  :children (list (new-tree :value (new-env :desc "works"
                                                            :expectation '((+ 1 2)))))))
      (before (setf *behaviors*
                    (add-child env-tree *behaviors*)))
      (after (remove-test-behavior))

      (it "returns that env-tree as the child of a tree with no value"
        (is (equal (new-tree :children (list (set-value (env+ (new-env :desc "like")
                                                              (value env-tree))
                                                        env-tree)))
                   subject))))))

(context "forms->env-tree"
  (context "when at least one of the forms is invalid"
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error (forms->env-tree 'not-a-list))
      (raises-error (forms->env-tree '(not a doubly nested list)))
      (raises-error (forms->env-tree '((invalid-form) (sorry brah))))
      (raises-error (forms->env-tree '((before "lookin good") (fake "ugh"))))))

  (context "when all of the forms are valid"
    (it "builds the result from multiple form->env-tree calls"
      (is (equal (forms->env-tree '((before (format t "before"))))
                 (new-tree :value (new-env :befores '((format t "before"))))))

      (is (equal (forms->env-tree '((after (+ 1 2)) (it "works" (+ 3 4))))
                 (new-tree :value (new-env :afters '((+ 1 2)))
                           :children (list (new-tree :value (new-env :desc "works"
                                                                     :expectation '((+ 3 4)))))))))))

(context "env-tree->lambda-tree"
  (context "when the argument is not a tree"
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error (env-tree->lambda-tree 'not-even-close))
      (raises-error (env-tree->lambda-tree '(tree)))))

  (context "when the argument is a tree"
    (context "but at least one of its values is not an env"
      (include-context "error-helpers")
      (it "raises an error"
        (raises-error (env-tree->lambda-tree (new-tree :value 1)))
        (raises-error (env-tree->lambda-tree (new-tree :value (new-env)
                                                       :children (list (new-tree :value 2)))))))

    (context "and every value is an env"
      (let env-tree
        (new-tree :value (new-env :desc     "outer-desc"
                                  :befores '((format t "before"))
                                  :afters  '((format t "after")))
                  :children (list (new-tree :value (new-env :desc "test"
                                                            :expectation '((is-not nil)))))))
      (subject (env-tree->lambda-tree env-tree))

      (it "transforms the argument into a lambda tree"
        (is (string= "outer-desc" (cadr (value subject))))
        (is (null (caddr (value subject))))
        (is (= 1 (length (tree-children subject))))
        (is (string= "test" (cadr (value (car (tree-children subject))))))
        (is (functionp (eval (caddr (value (car (tree-children subject)))))))))))

(context "context"
  (include-context "error-helpers")
  (context "when the syntax is invalid"
    (it "raises an error"
      (raises-error (macroexpand-1 '(context 'non-string-desc (it "fails" 1))))
      (raises-error (macroexpand-1 '(context "non-paren-enclosed-children" it "fails" 1)))
      (raises-error (macroexpand-1 '(context "unrecognized-child" (not-real-child 1))))))
  (context "when the syntax is valid"
    (it "does not raise an error"
      (does-not-raise-error (macroexpand-1 '(context "valid context"
                                              (it "works" (is (+ 1 2)))))))))

(context "shared-context"
  (include-context "error-helpers")
  (context "when the syntax is invalid"
    (it "raises an error"
      (raises-error (macroexpand-1 '(shared-context 'not-string-desc)))
      (raises-error (macroexpand-1 '(shared-context "has-an-it" (it "should not be here" 1))))
      (raises-error (macroexpand-1 '(shared-context "non-paren-enclosed" before (format t "whoops"))))
      (raises-error (macroexpand-1 '(shared-context "no such child" (not-a-child "hi"))))))
  (context "when the syntax is valid"
    (it "does not raise an error"
      (does-not-raise-error (macroexpand-1 '(shared-context "valid-shared-context"
                                              (before (format t "yay!"))))))))

(context "behavior"
  (include-context "error-helpers")
  (context "when the syntax is invalid"
    (it "raises an error"
      (raises-error (macroexpand-1 '(behavior 'non-string-desc (it "fails" 1))))
      (raises-error (macroexpand-1 '(behavior "non-paren-enclosed-children" it "fails" 1)))
      (raises-error (macroexpand-1 '(behavior "unrecognized-child" (not-real-child 1))))))
  (context "when the syntax is valid"
    (it "does not raise an error"
      (does-not-raise-error (macroexpand-1 '(behavior "valid behavior"
                                              (it "works" (is (+ 1 2)))))))))
