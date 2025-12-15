;;;; eval.lisp - Run SBCL code/file with concise output
;;;; Usage: sbcl --script tools/eval.lisp [file.lisp | -e "code"]

(defun main ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (cond
      ((null args)
       (format t "Usage: tools/eval file.lisp~%")
       (format t "       tools/eval -e \"(+ 1 2)\"~%")
       (sb-ext:exit :code 1))
      ((string= (car args) "-e")
       (eval-string (cadr args)))
      (t
       (eval-file (car args))))))

(defun eval-string (code)
  (handler-case
      (let ((result (eval (read-from-string code))))
        (format t "~S~%" result))
    (error (c)
      (format t "ERROR: ~A~%" c)
      (sb-ext:exit :code 1))))

(defun eval-file (path)
  (handler-case
      (load path :verbose nil :print nil)
    (error (c)
      (format t "ERROR: ~A~%" c)
      (sb-ext:exit :code 1))))

(main)
