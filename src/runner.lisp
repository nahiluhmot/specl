(in-package #:specl-runner)

(declaim #+sbcl (sb-ext:muffle-conditions style-warning))

(defun expand-it (it befores afters funcs macros lets)
  "Given an expectation and its necessary callbacks, expands them into Common
Lisp. The code will return `t` on success and `nil` on failure."
  `(handler-case
     (progn
       (lazy-let ,lets
         (macrolet ,macros
           (labels ,funcs
             ,@befores
             ,@it
             ,@afters)))
       t)
     (error (e) nil)))

(defun expand-env (env &optional (parent-desc ""))
  "Given an environment, expands it into Common Lisp code. The code returns a
cons cell with the `car` being the passes, and `cdr` being the failures."
  (let ((passes (gensym)) (failures (gensym)))
    (with-env env
      `(let ((,passes nil) (,failures nil))
         ,@(mapcar
             (lambda (it)
               (let ((full-desc (string-trim " " (concatenate 'string parent-desc " " desc " " (car it)))))
                 `(if ,(expand-it (cdr it) befores afters funcs macros lets)
                    (progn (format t ".")
                           (push ,full-desc ,passes)) 
                    (progn (format t "F")
                           (push ,full-desc ,failures)))))
             expectations)
         ,@(mapcar
             (lambda (child)
               (let ((ps (gensym)) (fs (gensym)))
                 `(dbind (,ps . ,fs) ,(expand-env (inherit env child)
                                                  (string-trim " " (concatenate 'string parent-desc " " desc)))
                    (nconc ,passes ,ps) (nconc ,failures ,fs))))
             children)
         (cons ,passes ,failures)))))

(defmacro run! ()
  (let ((results (gensym))
        (passes (gensym))
        (failures (gensym))
        (desc (gensym)))
    `(let* ((,results (list ,@(mapcar #'expand-env *contexts*)))
            (,passes (remove nil (mapcar #'car ,results)))
            (,failures (remove nil (mapcar #'cdr ,results))))
       (format t "~%The following passed:~%")
       (mapcar (lambda (,desc) (format t "  ~A~%" ,desc)) ,passes)
       (format t "~%The following failed:~%")
       (mapcar (lambda (,desc) (format t "  ~A~%" ,desc)) ,failures))))
