(in-package #:specl.spec)

(context "new-test"
  (include-context "error-helpers")

  (it "accepts :desc, :tags, and :expectation as keyword arguments"
    (does-not-raise-error (specl.test:new-test))
    (does-not-raise-error
      (specl.test:new-test :desc "my test"))
    (does-not-raise-error
      (specl.test:new-test :tags (list :swag :yolo)))
    (does-not-raise-error
      (specl.test:new-test :expectation (lambda () (is (null nil))))))

  (it "creates a new test"
    (is (specl.test:test? (specl.test:new-test)))
    (is (specl.test:test? (specl.test:new-test :desc "swag")))
    (is (specl.test:test? (specl.test:new-test :tags '(:hey :gurl))))
    (is (specl.test:test? (specl.test:new-test :expectation (lambda () t))))
    (is (specl.test:test? (specl.test:new-test :desc "dat test"
                                               :tags '(:ya :nasty)
                                               :expectation (lambda () t))))))

(context "test?"
  (subject (specl.test:test? form))
  (before (format t "form: ~A~%" form))

  (context "when the argument is not a list"
    (let form 'not-a-list)
    (it "returns nil"
      (is-not subject)))

  (context "when the argument is a list"
    (context "but its length is not 4"
      (let form '(length is 3))
      (it "returns nil"
        (is-not subject)))

    (context "and its length is 4"
      (context "but the car is not TEST"
        (let form '(lol "hey" nil nil))
        (it "returns false"
          (is-not subject)))

      (context "and the car is TEST"
        (context "but the cadr is not a string"
          (let form '(test nil nil nil))
          (it "returns false"
            (is-not subject)))

        (context "and the cadr is a string"
          (context "but at least one of the tags is not a keyword"
            (let form '(test "yolo" (:tag "swerve") nil))
            (it "returns false"
              (is-not subject)))

          (context "and all of the tags are keywords"
            (context "and the expectation is not nil or a function"
              (let form '(TEST "swerve" (:hello) 1))
              (it "returns false"
                (is-not subject)))

            (context "and the expectation is nil"
              (let form '(TEST "swerve" (:hello) nil))
              (it "returns true"
                (is subject)))

            (context "and the expectation is a function"
              (let form `(TEST "swerve" (:hello) ,(lambda () t)))
              (it "returns true"
                (is subject)))))))))

(context "with-test"

  (context "when the argument evaluates to a test"
    (let test-desc "my description")
    (let test-tags '(:hey :there))
    (let test-expectation nil)
    (let test-test (specl.test:new-test :desc test-desc
                                        :tags test-tags
                                        :expectation test-expectation))
    (it "binds desc, tags, and expectation to symbols"
      (specl.test:with-test test-test
        (is (equal specl.test:desc test-desc))
        (is (equal specl.test:tags test-tags))
        (is (equal specl.test:expectation test-expectation)))))

  (context "when the argument does not evaluate to a test"
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error (specl.test:with-test 1 2))
      (raises-error (specl.test:with-test '(not a test) nil)))))

(context "test->new-test-syntax"
  (context "when the argument is not a test"
    (include-context "error-helpers")
    (it "raises an error"
      (raises-error (specl.test:test->new-test-syntax 'not-a-test)))) 

  (context "when the argument is a test"
    (let test (specl.test:new-test :desc "my-test"
                                   :tags '(:tag-a :tag-b)
                                   :expectation (lambda () 1)))
    (subject (eval (specl.test:test->new-test-syntax test)))
    (it "returns the syntax to create an identical test"
      (is (specl.test:test? subject))
      (specl.test:with-test subject
        (is (string= specl.test:desc (cadr test)))
        (is (equal specl.test:tags (caddr test)))
        (is (= 1 (funcall (cadddr test))))))))
