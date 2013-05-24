(in-package #:specl.util)

(defmacro alias (new original)
  "Alias a function / macro."
  (let ((lambda-list (gensym)))
    `(defmacro ,new (&rest ,lambda-list)
       `(,',original ,@,lambda-list))))

;; Because 'destructuring-bind' is a pretty big keyboard tax for a commonly used
;; macro.
(alias dbind destructuring-bind)

(labels ((lazy-let-expand-one (arg body)
           (dbind (sym . value) arg
             (let ((sym-set? (gensym))
                   (sym-val  (gensym)))
               `(let ((,sym-set? nil)
                      (,sym-val  nil))
                 (symbol-macrolet ((,sym (if ,sym-set?
                                           ,sym-val
                                           (progn (setf ,sym-set? t)
                                                  (setf ,sym-val (progn ,@value))))))
                   ,body))))))
  (defmacro lazy-let (definitions &body body)
    "Acts much like let, with the expection that all of the arguments are
evaluated lazily and then cached."
    (if (null definitions)
      `(progn ,@body)
      (reduce #'lazy-let-expand-one
              definitions
              :initial-value `(progn ,@body)
              :from-end t))))

(defmacro string-case (value &body forms)
  (let ((name (gensym)))
    `(let ((,name ,value))
      (cond ,@(mapcar (lambda (form)
                        (dbind (val . body) form
                          `((string= ,name ,val) ,@body)))
                      forms)))))

(defun first-by (pred lst)
  "Given a predicate and list, returns the first value that satisfies that
predicate."
  (cond ((null lst) nil)
        ((funcall pred (car lst)) (car lst))
        (t (first-by pred (cdr lst)))))

(defun flatten (struct)
  "Given a structure, will return a non-nested list."
  (cond ((null struct) nil)
        ((atom struct) (list struct))
        ((listp struct) (mapcan #'flatten struct))))

(defmacro is (form)
  "Raises an error if the given form returns nil."
  `(unless ,form
    (error "Expected ~A to not return nil" (quote ,@(list form)))))

(defmacro is-not (form)
  "Raises an error unless the given form returns nil."
  `(when ,form
    (error "Expected ~A to return nil" (quote ,@(list form)))))
