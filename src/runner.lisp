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
  "Given an environment, expands it into Common Lisp code."
  (let ((passes (gensym)) (failures (gensym)))
    (with-env env
      `(progn
         ,@(mapcar
             (lambda (it)
               (let ((full-desc (string-trim " " (concatenate 'string parent-desc " " desc " " (car it)))))
                 `(if ,(expand-it (cdr it) befores afters funcs macros lets)
                   (push ,full-desc *passes*)
                   (push ,full-desc *failures*))))
             expectations)
         ,@(mapcar
             (lambda (child)
               (expand-env (inherit env child)
                           (string-trim " " (concatenate 'string parent-desc " " desc))))
             children)))))

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
