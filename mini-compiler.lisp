;;;; Minimal Self-Hosting Compiler for Habu
;;;; Written in pure Habu Lisp using only existing features
;;;; Goal: Compile Habu to C, achieve self-hosting

;;;; This compiler is intentionally minimal:
;;;;  - Generates C code (not native code)
;;;;  - Supports only essential features
;;;;  - Uses only primitives available in habu-repl.lisp

;;;; ============================================
;;;; Part 1: Code Generation Helpers
;;;; ============================================

;;; Since we don't have string-concat yet, we work with lists
;;; and convert to strings later

(defun emit-fixnum (n)
  ;;; Generate C code for a fixnum literal
  ;;; Returns: string representing "fixnum_to_value(n)"
  ;;; For now, just return the number as a symbol
  ;;;   Later we'll convert this properly
  n)

(defun emit-symbol (sym)
  ;;; Generate C code for a symbol
  ;;; For now, symbols compile to themselves
  sym)

(defun emit-binop (op left right)
  ;;; Generate C code for binary operation
  ;;; op is +, -, *, or /
  ;;; Returns: (op left right) as list
  (list op left right))

(defun emit-if (test then else-clause)
  ;;; Generate C code for if expression
  ;;; Returns: (if test then else) as list
  (list (quote if) test then else-clause))

;;;; ============================================
;;;; Part 2: Compiler
;;;; ============================================

(defun compile-expr (expr env)
  ;;; Compile a Habu expression to C code (as S-expression)
  ;;; env is an association list of variable bindings
  (if (fixnum? expr)
    ;;; Fixnum literal
    (emit-fixnum expr)
    (if (symbol? expr)
      ;;; Variable reference
      (emit-symbol expr)
      (if (cons? expr)
        ;;; Function application or special form
        (let ((op (car expr)))
          (if (symbol=? op (make-symbol (quote +)))
            ;;; Addition
            (emit-binop (quote +)
                       (compile-expr (car (cdr expr)) env)
                       (compile-expr (car (cdr (cdr expr))) env))
            (if (symbol=? op (make-symbol (quote -)))
              ;;; Subtraction
              (emit-binop (quote -)
                         (compile-expr (car (cdr expr)) env)
                         (compile-expr (car (cdr (cdr expr))) env))
              (if (symbol=? op (make-symbol (quote *)))
                ;;; Multiplication
                (emit-binop (quote *)
                           (compile-expr (car (cdr expr)) env)
                           (compile-expr (car (cdr (cdr expr))) env))
                (if (symbol=? op (make-symbol (quote /)))
                  ;;; Division
                  (emit-binop (quote /)
                             (compile-expr (car (cdr expr)) env)
                             (compile-expr (car (cdr (cdr expr))) env))
                  (if (symbol=? op (make-symbol (quote if)))
                    ;;; If expression
                    (emit-if (compile-expr (car (cdr expr)) env)
                            (compile-expr (car (cdr (cdr expr))) env)
                            (compile-expr (car (cdr (cdr (cdr expr)))) env))
                    ;;; Unknown - return as-is for now
                    expr))))))
        ;;; Not a cons - return as-is
        expr))))

(defun compile-toplevel (expr)
  ;;; Compile a top-level form
  ;;; For now, just compile the expression
  (compile-expr expr (quote nil)))

;;;; ============================================
;;;; Part 3: Test Cases
;;;; ============================================

;;; Test 1: Compile a fixnum
(defun test1 ()
  (compile-toplevel (quote 42)))

;;; Test 2: Compile addition
(defun test2 ()
  (compile-toplevel (quote (+ 1 2))))

;;; Test 3: Compile nested expression
(defun test3 ()
  (compile-toplevel (quote (+ (* 3 4) (- 10 5)))))

;;; Test 4: Compile if expression
(defun test4 ()
  (compile-toplevel (quote (if 1 2 3))))

;;;; ============================================
;;;; Part 4: Testing
;;;; ============================================

;;; Just return test results - the REPL will print them
;;; Test all cases
(list (test1) (test2) (test3) (test4))
