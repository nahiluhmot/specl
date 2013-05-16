(in-package #:specl-cli)

(defun run-specs ()
  (let ((ql-package (cadr sb-ext:*posix-argv*)))
    (load (concatenate 'string  ql-package ".asd"))
    (ql:quickload (intern ql-package))
    (run-all)))
