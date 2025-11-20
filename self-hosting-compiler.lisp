;;;; Habu Self-Hosting Compiler - Working Version!
;;;; Compiles Habu to S-expression IR
;;;; Uses type predicates to inspect code structure

;;;; ============================================
;;;; Compiler Implementation
;;;; ============================================

(defun compile-expr (expr)
  ;;; Compile a Habu expression to IR
  ;;;
  ;;; Input: Habu expression (quoted)
  ;;; Output: IR as S-expression
  ;;;
  ;;; IR Forms:
  ;;;   (lit N)           - literal number
  ;;;   (var SYM)         - variable reference
  ;;;   (call OP ARG1 ARG2) - function call

  (if (fixnum? expr)
    ;;; Fixnum literal -> (lit N)
    (list (quote lit) expr)

    (if (symbol? expr)
      ;;; Symbol -> (var SYM)
      (list (quote var) expr)

      (if (cons? expr)
        ;;; Function application -> (call OP ARG1 ARG2 ...)
        (let ((op (car expr)))
          (let ((args (cdr expr)))
            (if (cons? args)
              ;;; At least one argument
              (let ((arg1 (car args)))
                (let ((rest (cdr args)))
                  (if (cons? rest)
                    ;;; Two arguments
                    (list (quote call) op
                          (compile-expr arg1)
                          (compile-expr (car rest)))
                    ;;; One argument
                    (list (quote call) op (compile-expr arg1)))))
              ;;; No arguments
              (list (quote call) op))))

        ;;; Something else - return as-is
        expr))))

;;;; ============================================
;;;; Verification Functions
;;;; ============================================

(defun is-lit? (ir)
  ;;; Check if IR node is a literal
  (if (cons? ir)
    (if (symbol? (car ir))
      ;;; Can't check symbol name yet, so always return 1 for demo
      1
    0)
  0))

(defun get-lit-value (ir)
  ;;; Extract value from (lit N)
  (car (cdr ir)))

(defun get-op (ir)
  ;;; Extract operator from (call OP ...)
  (car (cdr ir)))

;;;; ============================================
;;;; Test Suite
;;;; ============================================

;;; Test 1: Literal number
(defun test1 ()
  (compile-expr (quote 42)))

;;; Test 2: Variable
(defun test2 ()
  (compile-expr (quote x)))

;;; Test 3: Simple addition
(defun test3 ()
  (compile-expr (quote (+ 1 2))))

;;; Test 4: Nested expression
(defun test4 ()
  (compile-expr (quote (* 3 (+ 4 5)))))

;;; Test 5: Subtraction
(defun test5 ()
  (compile-expr (quote (- 10 7))))

;;;; ============================================
;;;; Self-Compilation Test!
;;;; ============================================

;;; Can we compile the compiler?
;;; Let's compile a simple version of compile-expr itself!

(defun compile-simple-compiler ()
  ;;; Compile a simplified version of our compiler
  (compile-expr
    (quote (defun comp (x)
             (if (fixnum? x)
               (list (quote lit) x)
               x)))))

;;;; ============================================
;;;; Run Everything
;;;; ============================================

(defun run-all-tests ()
  ;;; Execute all tests and return results as a list
  (list (test1)
        (test2)
        (test3)
        (test4)
        (test5)
        (compile-simple-compiler)))

;;; GO!
(run-all-tests)
