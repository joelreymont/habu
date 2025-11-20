;;;; Print IR in readable format for backend

(defun compile-expr (expr)
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (cons? expr)
      (let ((op (car expr)))
        (let ((args (cdr expr)))
          (if (cons? args)
            (let ((arg1 (car args)))
              (let ((rest (cdr args)))
                (if (cons? rest)
                  (list (quote call) op
                        (compile-expr arg1)
                        (compile-expr (car rest)))
                  (list (quote call) op (compile-expr arg1)))))
            (list (quote call) op))))
      expr)))

;;; Test it
(compile-expr 42)
(compile-expr (quote (+ 10 15)))
(compile-expr (quote (* 6 7)))
