(in-package #:specl.spec)

(shared-context "error-helpers"
  (defmacro raises-error (&body body)
    (let ((e (gensym)))
      `(is (handler-case (progn ,@body nil) (error (,e) t)))))
  (defmacro does-not-raise-error (&body body)
    (let ((e (gensym)))
      `(is (handler-case (progn ,@body t) (error (,e) nil))))))

(shared-context "sample-envs"
  (let context-desc "my-description")
  (let context-tags '(:slow))
  (let context-befores '((format t "befores~%")))
  (let context-afters '((format t "afters~%")))
  (let context-funcs '((func (name) (* 2 (1+ name)))))
  (let context-macros '((macro () (null nil))))
  (let context-lets '((test-let 14)))
  (let context-expectation nil)
  (let sample-context (new-env :desc context-desc
                               :tags context-tags
                               :befores context-befores
                               :afters context-afters
                               :funcs context-funcs
                               :macros context-macros
                               :lets context-lets
                               :expectation context-expectation))

  (let it-desc "works")
  (let it-expectation '((is (env? sample-context))))
  (let sample-it (new-env :desc it-desc 
                          :expectation it-expectation)))
