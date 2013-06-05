(in-package #:specl.spec)

(context "new-env"
  (context "with no arguments"
    (subject (new-env))

    (it "sets the defaults"
      (with-env subject
        (is (equal "" desc))
        (is (null befores))
        (is (null afters))
        (is (null funcs))
        (is (null macros))
        (is (null lets))
        (is (null expectation))))
    (it "is an env"
      (is (env? subject))))


  (context "with keyword arguments"
    (include-context "sample-envs")
    (subject sample-context)

    (it "sets the arguments"
      (with-env subject
        (is (equal context-desc desc))
        (is (equal context-befores befores))
        (is (equal context-afters afters))
        (is (equal context-funcs funcs))
        (is (equal context-macros macros))
        (is (equal context-lets lets))
        (is (equal context-expectation expectation))))))

(context "env?"
  (context "when the given form is an env"
    (include-context "sample-envs")
    (it "returns t"
      (is (env? (new-env)))
      (is (env? sample-context))
      (is (env? sample-it))))
  (context "when the given form is not an env"
    (it "returns nil"
      (is-not (env? 'not-an-env))
      (is-not (env? '(ENV "desc")))
      (is-not (env? '(ENV nil nil nil nil nil nil nil nil))))))

(context "with-env"
  (context "when the given form does not evaluate to an env"
    (include-context "error-helpers")
    (subject 'not-an-env)

    (it "raises an error"
      (raises-error (with-env subject
                      (format t "desc: ~A" desc)))))
  
  (context "when the given form is an env"
    (include-context "sample-envs")

    (it "binds every value in the env to a variable"
      (with-env sample-context
        (is (equal context-desc desc))
        (is (equal context-befores befores))
        (is (equal context-afters afters))
        (is (equal context-funcs funcs))
        (is (equal context-macros macros))
        (is (equal context-lets lets))
        (is (equal context-expectation expectation))))))

(context "env->new-env-syntax"
  (subject (env->new-env-syntax form))

  (context "when the argument is not an env"
    (let form 'not-an-env)
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error subject)))

  (context "when the argument is an env"
    (let env (new-env :desc "my-env" :befores '((format t "befores"))
                      :afters '((format t "afters")) :funcs '((my-func () 1))
                      :macros '((my-macro () nil)) :lets '((name 1))
                      :expectation '((is (= 1 1)))))
    (let form env)
    (it "returns the syntax to create an identical env"
      (with-env env
        (is (equal env (eval subject)))))))

(context "env+"
  (include-context "error-helpers")

  (context "when at least one of the arguments is not an env"
    (it "raises an error"
      (raises-error (env+ 'not-env-1 'not-env-2))
      (raises-error (env+ (new-env) 'not-env-3))
      (raises-error (env+ 'not-env-3 (new-env)))
      (raises-error (env+ (new-env) '(ENV "desc" nil)))))

  (context "when both arguments are envs"
    (let desc-a "first-desc")
    (let tags-a '(:yolo :swag))
    (let befores-a '((format t "befores")))
    (let afters-a '((format t "afters")))
    (let env-a (new-env :desc desc-a
                        :tags tags-a
                        :befores befores-a
                        :afters afters-a))
    (let desc-b "")
    (let befores-b '((print 1)))
    (let lets-b '((name 1)))
    (let env-b (new-env :desc desc-b
                        :befores befores-b
                        :lets lets-b))
    (subject (env+ env-a env-b))

    (it "trims trailing whitespace off of the new desc"
      (with-env subject
        (is (equal desc desc-a))))

    (it "concatenates all of the lists"
      (with-env subject
        (is (equal befores '((format t "befores") (print 1))))
        (is (equal afters afters-a))
        (is (equal lets lets-b))
        (is (null funcs))
        (is (null macros))
        (is (null expectation))))))

(context "inherit"
  (context "when at least one argument is not an env"
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error (inherit (new-env) 1))
      (raises-error (inherit 1 (new-env)))
      (raises-error (inherit 1 2))
      (raises-error (inherit (new-env) '(ENV "yolo" nil nil)))))

  (context "when both arguments are envs"
    (include-context "sample-envs")
    (subject (inherit sample-context sample-it))

    (it "adds all of the runtime metadata"
      (with-env subject
        (is (equal desc it-desc))
        (is (equal tags context-tags))
        (is (equal expectation it-expectation))
        (is (equal befores context-befores))
        (is (equal afters context-afters))
        (is (equal lets context-lets))
        (is (equal funcs context-funcs))
        (is (equal macros context-macros))))))
