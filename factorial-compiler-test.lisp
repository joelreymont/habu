;;;; End-to-End Compiler Test
;;;; Compile factorial function to IR

(defun compile-expr (expr)
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (symbol? expr)
      (list (quote var) expr)
      (if (cons? expr)
        (let ((op (car expr)))
          (let ((args (cdr expr)))
            (if (symbol=? op (quote if))
              (compile-if args)
              (if (symbol=? op (quote defun))
                (compile-defun args)
                (compile-call op args)))))
        expr))))

(defun compile-if (args)
  (list (quote if-expr)
        (compile-expr (car args))
        (compile-expr (car (cdr args)))
        (compile-expr (car (cdr (cdr args))))))

(defun compile-defun (args)
  (list (quote defun-expr)
        (car args)
        (car (cdr args))
        (compile-expr (car (cdr (cdr args))))))

(defun compile-call (op args)
  (if (cons? args)
    (let ((arg1 (car args)))
      (let ((rest (cdr args)))
        (if (cons? rest)
          (list (quote call) op
                (compile-expr arg1)
                (compile-expr (car rest)))
          (list (quote call) op (compile-expr arg1)))))
    (list (quote call) op)))

;;; Compile factorial!
(compile-expr (quote (defun factorial (n)
                       (if (= n 0)
                         1
                         (* n (factorial (- n 1)))))))
