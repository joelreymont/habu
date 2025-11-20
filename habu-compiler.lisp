;;;; Habu Self-Hosting Compiler v1.0
;;;; Written entirely in Habu Lisp
;;;; Compiles Habu expressions to S-expression IR

;;;; ============================================
;;;; Part 1: Utilities
;;;; ============================================

(defun is-quote? (expr)
  ;;; Check if expression is (quote ...)
  (if (cons? expr)
    (if (symbol? (car expr))
      ;;; Would check symbol name, but we don't have symbol=? yet
      ;;; For now, assume if it's a cons with symbol first, check tag
      0
    0)
  0))

;;;; ============================================
;;;; Part 2: Compiler Core
;;;; ============================================

(defun compile-expr (expr)
  ;;; Main compiler entry point
  ;;; Compiles Habu expression to IR
  (if (fixnum? expr)
    ;;; Fixnum literal -> (lit N)
    (cons (quote lit) (cons expr (quote nil)))

    (if (symbol? expr)
      ;;; Symbol (variable reference) -> (var SYM)
      (cons (quote var) (cons expr (quote nil)))

      (if (cons? expr)
        ;;; List form - check what kind
        (let ((op (car expr)))
          (if (symbol? op)
            ;;; Operator is a symbol
            ;;; For now, treat all as function calls
            ;;; (op arg1 arg2) -> (call op (compile arg1) (compile arg2))
            (cons (quote call)
                 (cons op
                      (cons (compile-expr (car (cdr expr)))
                           (cons (compile-expr (car (cdr (cdr expr))))
                                (quote nil)))))
            ;;; Non-symbol operator - just return as-is
            expr))

        ;;; Not fixnum, symbol, or cons - return as-is
        expr))))

;;;; ============================================
;;;; Part 3: Pretty Printer
;;;; ============================================

(defun compile-and-show (expr)
  ;;; Compile and return result (REPL will print it)
  (compile-expr expr))

;;;; ============================================
;;;; Part 4: Test Suite
;;;; ============================================

;;; Test 1: Compile a fixnum literal
(defun test-fixnum ()
  (compile-and-show (quote 42)))

;;; Test 2: Compile a variable reference
(defun test-var ()
  (compile-and-show (quote x)))

;;; Test 3: Compile addition
(defun test-add ()
  (compile-and-show (quote (+ 1 2))))

;;; Test 4: Compile nested expression
(defun test-nested ()
  (compile-and-show (quote (* 3 (+ 4 5)))))

;;; Test 5: Compile subtraction
(defun test-sub ()
  (compile-and-show (quote (- 10 3))))

;;;; ============================================
;;;; Part 5: Run Tests
;;;; ============================================

;;; Run all tests and show results
(defun run-tests ()
  (cons (test-fixnum)
       (cons (test-var)
            (cons (test-add)
                 (cons (test-nested)
                      (cons (test-sub)
                           (quote nil)))))))

;;; Execute test suite
(run-tests)
