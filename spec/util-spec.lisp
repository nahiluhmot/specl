(in-package #:specl.spec)

;; Originally, this alias lived inside of a `before` of the following context.
;; Unfortunately, this raised a style error that was causing the spec to fail.
(alias all and)
(context "alias"
  (it "defines a macro that expands to another operation"
    (is (all 1 2 3))
    (is-not (all nil 'sym))))

(context "dbind"
  (it "is an alias for `destructuring-bind`"
    (is (eq 'destructuring-bind
            (car (macroexpand-1 '(dbind)))))))


(context "string-case"
  (it "acts just like `case`, but uses `string=` for comparisons"
    (is (eq 'a (string-case "my-sweet-string"
                 ("yolo" 'z)
                 ("hiya" 'm)
                 ("my-sweet-string" 'a))))))

(context "lazy-let"
  (before (setq test-value 0))

  (context "when its argument is never used"
    (it "never executes the block"
      (is (zerop (lazy-let ((value (incf test-value)))
                   test-value)))))

  (context "when the argument is used for the first time"
    (it "executes the block"
      (is (eq 1 (lazy-let ((value (incf test-value)))
                  value
                  test-value)))))

  (context "when the argument is used two or more times"
    (it "returns the cached value"
      (is (eq 1 (lazy-let ((value (incf test-value)))
                  value
                  value
                  test-value))))))

(context "first-by"
  (subject (first-by #'pred lst))

  (defun pred (x) (oddp x))

  (context "when the predicate is never satisfied"
    (let lst '(2 4 6 8))
    (it "returns nil"
      (is (null subject))))

  (context "when the predicate is satisfied"
    (let lst '(2 4 1 3 5))
    (it "returns the first value that satisfies it"
      (is (= 1 subject)))))

(context "flatten"
  (subject (flatten arg))

  (context "when the argument is nil"
    (let arg nil)
    (it "returns nil"
      (is (null subject))))

  (context "when the is an atom"
    (let arg 15)
    (it "wraps the argument in a list"
      (is (equal `(,arg) subject))))

  (context "when the argument is a non-nested list"
    (let arg '(1 2 3 4 5))
    (it "returns the list"
      (is (equal arg subject))))

  (context "when the argument is a nested list"
    (let arg '((1 2) (3 (4 5)) 6))
    (let expected '(1 2 3 4 5 6))
    (it "returns a non-nested list"
      (is (equal expected subject)))))

(context "is"
  (include-context "error-helpers")

  (context "when the form returns nil"
    (it "raises an error"
      (raises-error (is nil))))

  (context "when the form does not return nil"
    (it "does nothing"
      (does-not-raise-error (is t)))))

(context "is-not"
  (include-context "error-helpers")

  (context "when the form returns nil"
    (it "does nothing"
      (does-not-raise-error (is-not nil))))

  (context "when the form does not return nil"
    (it "raises an error"
      (raises-error (is-not t)))))

