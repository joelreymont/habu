;;;; Habu Self-Hosting Compiler v2
;;;; Now with working type predicates!
;;;; Compiles Habu expressions to S-expression IR

;;;; Strategy: Generate S-expression IR that can be:
;;;;  1. Pretty-printed for inspection
;;;;  2. Converted to C code
;;;;  3. Interpreted directly

;;;; ============================================
;;;; Part 1: Compiler
;;;; ============================================

(defun compile-expr (expr)
  ;;; Compile expression to IR (as S-expression)
  (if (fixnum? expr)
    ;;; Fixnum literal -> just return it
    expr
    (if (symbol? expr)
      ;;; Symbol (variable) -> wrap in var node
      (list (quote var) expr)
      (if (cons? expr)
        ;;; Function application
        (let ((op (car expr)))
          ;;; Check if it's a recognized operator
          (if (symbol? op)
            ;;; Check for arithmetic ops
            (if (= (car (car expr)) (quote +))
              (list (quote add)
                   (compile-expr (car (cdr expr)))
                   (compile-expr (car (cdr (cdr expr)))))
              (if (= (car (car expr)) (quote -))
                (list (quote sub)
                     (compile-expr (car (cdr expr)))
                     (compile-expr (car (cdr (cdr expr)))))
                (if (= (car (car expr)) (quote *))
                  (list (quote mul)
                       (compile-expr (car (cdr expr)))
                       (compile-expr (car (cdr (cdr expr)))))
                  (if (= (car (car expr)) (quote /))
                    (list (quote div)
                         (compile-expr (car (cdr expr)))
                         (compile-expr (car (cdr (cdr expr)))))
                    ;;; Unknown operator -> return as-is
                    expr))))
            ;;; Non-symbol operator
            expr))
        ;;; Not fixnum, symbol, or cons -> return as-is
        expr))))

;;;; ============================================
;;;; Part 2: Test Cases
;;;; ============================================

(defun test-compiler ()
  ;;; Run test suite
  (let ((t1 (compile-expr (quote 42))))
    (let ((t2 (compile-expr (quote (+ 1 2)))))
      (let ((t3 (compile-expr (quote (* 3 (+ 4 5))))))
        (list t1 t2 t3)))))

;;;; Run tests
(test-compiler)
