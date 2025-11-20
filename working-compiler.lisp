;;;; Habu Self-Hosting Compiler - Working Version!
;;;; Compiles Habu expressions to S-expression IR

(defun compile-expr (expr)
  (if (fixnum? expr)
    (list (quote lit) expr)
    (if (symbol? expr)
      (list (quote var) expr)
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
        expr))))

;;; Test Suite
(compile-expr 42)
(compile-expr (quote x))
(compile-expr (quote (+ 1 2)))
(compile-expr (quote (* 3 (+ 4 5))))
