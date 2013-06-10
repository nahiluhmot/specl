(in-package #:specl.test)

;;; A test represents the runtime datastructure that holds the following:
;;;  - a manditory description
;;;  - an optional list of tags
;;;  - an optional expectation to run

(defun new-test (&key (desc "") tags expectation)
  "Given a description, tags, and expectation creates a test."
  `(TEST ,desc ,tags ,expectation))

(defun test? (test)
  (and (listp test)
       (= 4 (length test))
       (string= 'TEST (car test))
       (stringp (cadr test))
       (every #'keywordp (caddr test))
       (or (functionp (cadddr test))
           (null (cadddr test)))))

(defmacro with-test (test &body body)
  (let ((evaluated-test (gensym "WITH-TEST-")))
    `(let ((,evaluated-test ,test))
       (if (test? ,evaluated-test)
         (dbind (desc tags expectation) (cdr ,evaluated-test)
            ,@body)
         (error "specl.test:with-test expected a test, got: ~A~%" ,evaluated-test)))))
