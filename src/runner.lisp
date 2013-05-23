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
              (format t "~A~C[0;31;49m~A~%~AFAIL: ~A~C[0m~%" spaces #\ESC desc next-spaces result #\ESC)))))
      (unless (null (tree-children tree))
        (dbind (child-passes child-failures)
               (apply #'mapcar #'list
                 (mapcar (lambda (child) (run-tree child next-spaces full-desc))
                         (reverse (tree-children tree))))
          (setq passes (remove nil (flatten (append passes child-passes))))
          (setq failures (remove nil (flatten (append failures child-failures))))))
      (list passes failures))))

(defun print-results (passes failures)
  "Given a list of passes and failures, will print out the results of the
tests. Returns the number of failing test cases."
  (if (null failures)
    (format t "All ~A passed!~%" (length passes))
    (progn
      (format t "~A passed~%" (length passes))
      (format t "The following ~A failed:~%" (length failures))
      (mapcar (lambda (failure) (format t "  ~A~%" failure)) failures)))
  (length failures))

(defun run-all ()
  "Runs each test context, prints the reluts, and returns the number of failing
test cases."
  (apply #'print-results (run-tree *contexts*)))

(defun run-by-desc (desc)
  "Runs each context whose desc is `string=` to the argument."
  (apply #'print-results
         (run-tree (filter-tree (lambda (val)
                                  (dbind (val-desc _) val
                                    (string= val-desc desc)))
                                *contexts*))))
