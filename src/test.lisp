(in-package #:specl.test)

;;; A test represents the runtime datastructure that holds the following:
;;;  - a manditory description
;;;  - an optional list of tags
;;;  - an optional expectation to run

(defun new-test (&key (desc "") tags expectation)
  "Given a description, tags, and expectation creates a test."
  `(TEST ,desc ,tags ,expectation))

(defun test? (test)
  "Given a form, returns t if it is a test, nil otherwise."
  (and (listp test)
       (= 4 (length test))
       (string= 'TEST (car test))
       (stringp (cadr test))
       (every #'keywordp (caddr test))
       (or (functionp (cadddr test))
           (null (cadddr test)))))

(defmacro with-test (test &body body)
  "Given a test and body, will bind the symbols desc, tags, and expectation
and evaluate the body."
  (let ((evaluated-test (gensym "WITH-TEST-")))
    `(let ((,evaluated-test ,test))
       (if (test? ,evaluated-test)
         (dbind (desc tags expectation) (cdr ,evaluated-test)
            ,@body)
         (error "specl.test:with-test expected a test, got: ~A~%" ,evaluated-test)))))

(defun test->new-test-syntax (test)
  "Given a test, returns the Common Lisp syntax to create an identical test."
  (if (test? test)
    (with-test test
      `(new-test :desc ',desc :tags ',tags :expectation ',expectation))
    (error "specl.test:test->new-test-syntax expected a test, got: ~A~%" test)))
