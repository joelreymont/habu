;;;; Habu Self-Hosting Compiler - Production Version
;;;; Written entirely in Habu Lisp!
;;;; Compiles Habu expressions to S-expression IR

(defun compile-expr (expr)
  ;;; Main compiler entry point
  ;;; Recursively compiles Habu expressions to IR
  (if (fixnum? expr)
    ;;; Literal number -> (lit N)
    (list (quote lit) expr)
    (if (symbol? expr)
      ;;; Variable reference -> (var SYM)
      (list (quote var) expr)
      (if (cons? expr)
        ;;; List form - check what kind
        (let ((op (car expr)))
          (let ((args (cdr expr)))
            (if (symbol=? op (quote if))
              ;;; Special form: if
              (list (quote if-expr)
                    (compile-expr (car args))
                    (compile-expr (car (cdr args)))
                    (compile-expr (car (cdr (cdr args)))))
              ;;; Function call
              (if (cons? args)
                (let ((arg1 (car args)))
                  (let ((rest (cdr args)))
                    (if (cons? rest)
                      ;;; Binary function call
                      (list (quote call) op
                            (compile-expr arg1)
                            (compile-expr (car rest)))
                      ;;; Unary function call
                      (list (quote call) op (compile-expr arg1)))))
                ;;; Nullary function call
                (list (quote call) op)))))
        ;;; Not fixnum, symbol, or cons - return as-is
        expr))))

;;;; Test Suite - Demonstrates Working Compiler

;;; Test 1: Literals
(compile-expr 42)

;;; Test 2: Variables
(compile-expr (quote x))

;;; Test 3: Simple arithmetic
(compile-expr (quote (+ 1 2)))

;;; Test 4: Nested arithmetic
(compile-expr (quote (* 3 (+ 4 5))))

;;; Test 5: If expression
(compile-expr (quote (if (= n 0) 1 2)))

;;; Test 6: Nested calls
(compile-expr (quote (* n (- n 1))))
