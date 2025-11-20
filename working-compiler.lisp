;;;; Working Habu Self-Hosting Compiler
;;;; Compiles Habu expressions to IR (S-expressions)

(defun compile-expr (expr)
  (if (fixnum? expr)
    ;;; Fixnum literal
    (list (quote lit) expr)
    (if (symbol? expr)
      ;;; Variable reference
      (list (quote var) expr)
      (if (cons? expr)
        ;;; Check first element
        (let ((op (car expr)))
          (if (symbol? op)
            ;;; It's a symbol - check if it's an operator
            ;;; For now, just wrap it
            (list (quote call) op
                  (compile-expr (car (cdr expr)))
                  (compile-expr (car (cdr (cdr expr)))))
            ;;; Not a symbol
            expr))
        ;;; Not fixnum, symbol, or cons
        expr))))

;;; Test cases
(compile-expr (quote 42))
(compile-expr (quote x))
(compile-expr (quote (+ 1 2)))
(compile-expr (quote (* 3 (+ 4 5))))
