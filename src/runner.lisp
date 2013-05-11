(in-package #:specl-runner)

(declaim #+sbcl (sb-ext:muffle-conditions style-warning))

(defun expand-it (desc it befores afters funcs macros lets)
  "Given an expectation and its necessary callbacks, expands them into Common
Lisp. The code will return `t` on success and `nil` on failure."
  (let ((expanded-desc (gensym)) (e (gensym)))
    `(handler-case
       (progn
         (lazy-let ,lets
           (macrolet ,macros
             (labels ,funcs
               ,@befores
               ,@(cdr it)
               ,@afters)))
         (push ,desc *passes*))
       (error (,e)
         (let ((,expanded-desc (format nil "~A: ~A" ,desc ,e)))
         (push ,expanded-desc *failures*))))))

(defun expand-env (env &optional (parent-desc ""))
  "Given an environment, expands it into Common Lisp code."
  (with-env env
    `(progn
       ,@(mapcar
           (lambda (it)
             (let ((full-desc (string-trim " " (concatenate 'string parent-desc " " desc " " (car it)))))
               (expand-it full-desc (cdr it) befores afters funcs macros lets)))
           expectations)
       ,@(mapcar
           (lambda (child)
             (expand-env (inherit env child)
                         (string-trim " " (concatenate 'string parent-desc " " desc))))
           children))))

(defmacro run! ()
  (let ((desc (gensym)))
    `(progn 
       ,@(mapcar #'expand-env *contexts*)
       (format t "~%The following passed:~%")
       (mapcar (lambda (,desc) (format t "  ~A~%" ,desc)) *passes*)
       (format t "~%The following failed:~%")
       (mapcar (lambda (,desc) (format t "  ~A~%" ,desc)) *failures*)
       (setq *passes* nil)
       (setq *failures* nil))))
