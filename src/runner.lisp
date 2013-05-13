(in-package #:specl-runner)

(defun run-tree (tree &optional (spaces "") (parent-desc ""))
  (dbind (desc test) (value tree)
    (let ((passes nil)
          (failures nil)
          (full-desc (string-trim " " (concatenate 'string parent-desc " " desc)))
          (next-spaces (concatenate 'string "  " spaces)))
      (if (null test)
        (format t "~A~A~%" spaces desc)
        (let ((result (funcall test)))
          (if (and (symbolp result) (string= 'pass result))
            (progn
              (push full-desc passes)
              (format t "~A~C[0;32;49m~A~C[0m~%" spaces #\ESC desc #\ESC)) 
            (progn
              (push full-desc failures)
              (format t "~A~C[0;31;49m~A~%~A~A~C[0m~%" spaces #\ESC desc next-spaces result #\ESC)))))
      (unless (null (tree-children tree))
        (dbind (child-passes child-failures)
               (apply #'mapcar #'list
                 (mapcar (lambda (child) (run-tree child next-spaces full-desc))
                         (tree-children tree)))
          (setq passes (remove nil (flatten (append passes child-passes))))
          (setq failures (remove nil (flatten (append failures child-failures))))))
      (list passes failures))))

(defun run-all ()
  "Runs all of the loaded specs. It will return the number of specs that failed."
  (dbind (passes failures) (run-tree *contexts*)
    (if (null failures)
      (format t "All ~A passed!~%" (length passes))
      (progn
        (format t "~A passed~%" (length passes))
        (format t "The following ~A failed:~%" (length failures))
        (mapcar (lambda (failure) (format t "  ~A~%" failure)) failures)))
    (length failures)))
