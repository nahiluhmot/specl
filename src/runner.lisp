(in-package #:specl-runner)

(defun run-tree (tree &optional (spaces ""))
  (dbind (desc test) (value tree)
    (let ((next-spaces (concatenate 'string "  " spaces)))
      (if (null test)
        (format t "~A~A~%" spaces desc)
        (let ((result (funcall test)))
          (if (and (symbolp result) (string= 'pass result))
            (format t "~A~C[0;32;49m~A~C[0m~%" spaces #\ESC desc #\ESC)
            (format t "~A~C[0;31;49m~A~%~A~A~C[0m~%" spaces #\ESC desc next-spaces result #\ESC))))
      (mapcar (lambda (child) (run-tree child next-spaces))
              (tree-children tree)))))

(defun run-all ()
  (run-tree *contexts*)
  t)
