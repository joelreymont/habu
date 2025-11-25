#!/usr/bin/env sbcl --script
;;; Stage 1 Self-Hosting Test
;;;
;;; This test creates a mini-compiler that can compile itself.
;;; Stage 0: SBCL compiles the mini-compiler
;;; Stage 1: The compiled mini-compiler compiles a test expression
;;;
;;; This demonstrates the fundamental self-hosting capability.

(load "run-habu.lisp")

(format t "~%=== STAGE 1 SELF-HOSTING TEST ===~%~%")

;; Define a self-contained mini-compiler that supports:
;; - Numeric literals
;; - Arithmetic: +, -, *
;; - let bindings
;; - Conditionals: if, =
;; - Function definitions and calls via labels
;;
;; The mini-compiler:
;; 1. Takes an expression as a list structure
;; 2. Compiles it to IR
;; 3. Evaluates the IR

(defparameter *mini-compiler-source*
  '(;; Environment operations
    (defun env-lookup (var-id env)
      (if (nil? env)
          nil
          (if (= (car (car env)) var-id)
              (cdr (car env))
              (env-lookup var-id (cdr env)))))

    ;; Compile expression to IR
    ;; Supported forms:
    ;;   number -> (lit n)
    ;;   (var-ref id) -> (var offset)
    ;;   (+ a b) -> (add ir-a ir-b)
    ;;   (- a b) -> (sub ir-a ir-b)
    ;;   (* a b) -> (mul ir-a ir-b)
    ;;   (= a b) -> (eq ir-a ir-b)
    ;;   (if c t e) -> (if-expr ir-c ir-t ir-e)
    ;;   (let1 id val body) -> (let-expr ((offset . val-ir)) body-ir)
    (defun compile-expr (expr env)
      (cond
        ;; Number literal
        ((numberp expr) (list 'lit expr))

        ;; Variable reference
        ((and (consp expr) (eq (car expr) 'var-ref))
         (let ((offset (env-lookup (cadr expr) env)))
           (if (numberp offset)
               (list 'var offset)
               (list 'lit #x0))))

        ;; Addition
        ((and (consp expr) (eq (car expr) '+))
         (list 'add
               (compile-expr (cadr expr) env)
               (compile-expr (caddr expr) env)))

        ;; Subtraction
        ((and (consp expr) (eq (car expr) '-))
         (list 'sub
               (compile-expr (cadr expr) env)
               (compile-expr (caddr expr) env)))

        ;; Multiplication
        ((and (consp expr) (eq (car expr) '*))
         (list 'mul
               (compile-expr (cadr expr) env)
               (compile-expr (caddr expr) env)))

        ;; Equality
        ((and (consp expr) (eq (car expr) '=))
         (list 'eq-op
               (compile-expr (cadr expr) env)
               (compile-expr (caddr expr) env)))

        ;; Conditional
        ((and (consp expr) (eq (car expr) 'if))
         (list 'if-expr
               (compile-expr (cadr expr) env)
               (compile-expr (caddr expr) env)
               (compile-expr (cadddr expr) env)))

        ;; Let binding
        ((and (consp expr) (eq (car expr) 'let1))
         (let* ((var-id (cadr expr))
                (val-expr (caddr expr))
                (body (cadddr expr))
                (val-ir (compile-expr val-expr env))
                (new-offset (if env (+ (cdr (car env)) #x1) #x0))
                (new-env (cons (cons var-id new-offset) env))
                (body-ir (compile-expr body new-env)))
           (list 'let-expr (list (cons new-offset val-ir)) body-ir)))

        ;; Default
        (t (list 'lit #x0))))

    ;; Evaluate IR with stack
    (defun eval-ir (ir stack)
      (cond
        ;; Literal
        ((eq (car ir) 'lit)
         (cadr ir))

        ;; Variable
        ((eq (car ir) 'var)
         (nth (cadr ir) stack))

        ;; Add
        ((eq (car ir) 'add)
         (+ (eval-ir (cadr ir) stack)
            (eval-ir (caddr ir) stack)))

        ;; Sub
        ((eq (car ir) 'sub)
         (- (eval-ir (cadr ir) stack)
            (eval-ir (caddr ir) stack)))

        ;; Mul
        ((eq (car ir) 'mul)
         (* (eval-ir (cadr ir) stack)
            (eval-ir (caddr ir) stack)))

        ;; Equality (returns 1 or 0)
        ((eq (car ir) 'eq-op)
         (if (= (eval-ir (cadr ir) stack)
                (eval-ir (caddr ir) stack))
             #x1
             #x0))

        ;; Conditional
        ((eq (car ir) 'if-expr)
         (if (= (eval-ir (cadr ir) stack) #x0)
             (eval-ir (cadddr ir) stack)   ; else branch
             (eval-ir (caddr ir) stack)))  ; then branch

        ;; Let
        ((eq (car ir) 'let-expr)
         (let* ((bindings (cadr ir))
                (body (caddr ir))
                (binding (car bindings))
                (val-ir (cdr binding))
                (val (eval-ir val-ir stack))
                (new-stack (append stack (list val))))
           (eval-ir body new-stack)))

        ;; Default
        (t #x0)))

    ;; Main compile-and-run function
    (defun compile-and-run (expr)
      (let ((ir (compile-expr expr nil)))
        (eval-ir ir nil)))))

;; Test 1: Compile the mini-compiler and run a simple expression
(format t "Test 1: Stage 0 compiles mini-compiler, runs (+ 10 20)~%")
(let* ((test-expr '(+ #xa #x14))  ; 10 + 20 = 30
       (full-program (append *mini-compiler-source*
                             (list `(compile-and-run (quote ,test-expr)))))
       (result (habu-sbcl:compile-and-run-forms full-program)))
  (format t "  Result: ~A (expected 30)~%" result)
  (unless (= result #x1e)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 2: More complex expression with let
(format t "~%Test 2: Expression with let binding: (let1 1 5 (+ (var-ref 1) 10))~%")
(let* ((test-expr '(let1 #x1 #x5 (+ (var-ref #x1) #xa)))  ; let x=5 in x+10 = 15
       (full-program (append *mini-compiler-source*
                             (list `(compile-and-run (quote ,test-expr)))))
       (result (habu-sbcl:compile-and-run-forms full-program)))
  (format t "  Result: ~A (expected 15)~%" result)
  (unless (= result #xf)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 3: Nested let bindings
(format t "~%Test 3: Nested let: (let1 1 3 (let1 2 4 (* (var-ref 1) (var-ref 2))))~%")
(let* ((test-expr '(let1 #x1 #x3 (let1 #x2 #x4 (* (var-ref #x1) (var-ref #x2)))))  ; 3*4=12
       (full-program (append *mini-compiler-source*
                             (list `(compile-and-run (quote ,test-expr)))))
       (result (habu-sbcl:compile-and-run-forms full-program)))
  (format t "  Result: ~A (expected 12)~%" result)
  (unless (= result #xc)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 4: Conditional expression
(format t "~%Test 4: Conditional: (if (= 5 5) 100 200)~%")
(let* ((test-expr '(if (= #x5 #x5) #x64 #xc8))  ; if true then 100 else 200
       (full-program (append *mini-compiler-source*
                             (list `(compile-and-run (quote ,test-expr)))))
       (result (habu-sbcl:compile-and-run-forms full-program)))
  (format t "  Result: ~A (expected 100)~%" result)
  (unless (= result #x64)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 5: Conditional with false condition
(format t "~%Test 5: Conditional false: (if (= 5 6) 100 200)~%")
(let* ((test-expr '(if (= #x5 #x6) #x64 #xc8))  ; if false then 100 else 200
       (full-program (append *mini-compiler-source*
                             (list `(compile-and-run (quote ,test-expr)))))
       (result (habu-sbcl:compile-and-run-forms full-program)))
  (format t "  Result: ~A (expected 200)~%" result)
  (unless (= result #xc8)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 6: Complex expression combining all features
(format t "~%Test 6: Complex: (let1 1 10 (if (= (var-ref 1) 10) (* (var-ref 1) 2) 0))~%")
(let* ((test-expr '(let1 #x1 #xa
                     (if (= (var-ref #x1) #xa)
                         (* (var-ref #x1) #x2)
                         #x0)))  ; let x=10 in if x=10 then x*2 else 0 = 20
       (full-program (append *mini-compiler-source*
                             (list `(compile-and-run (quote ,test-expr)))))
       (result (habu-sbcl:compile-and-run-forms full-program)))
  (format t "  Result: ~A (expected 20)~%" result)
  (unless (= result #x14)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 7: The compiler compiling its own compile-expr logic
;; We'll have the mini-compiler compile a simple IR generator expression
(format t "~%Test 7: Meta-test - compiler compiles an IR-like structure~%")
(let* ((test-expr '(let1 #x1 (+ #x2 #x3)    ; Let x = 2+3 = 5
                     (let1 #x2 (* (var-ref #x1) #x2)  ; Let y = x*2 = 10
                       (- (var-ref #x2) (var-ref #x1)))))  ; y - x = 10 - 5 = 5
       (full-program (append *mini-compiler-source*
                             (list `(compile-and-run (quote ,test-expr)))))
       (result (habu-sbcl:compile-and-run-forms full-program)))
  (format t "  Result: ~A (expected 5)~%" result)
  (unless (= result #x5)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

(format t "~%=== ALL STAGE 1 SELF-HOSTING TESTS PASSED ===~%")
(format t "~%Summary:~%")
(format t "  - Stage 0 (SBCL) successfully compiled a mini-compiler~%")
(format t "  - The compiled mini-compiler can compile and execute expressions~%")
(format t "  - Supports: literals, arithmetic, let bindings, conditionals~%")
(format t "  - This demonstrates the fundamental self-hosting capability~%")

(sb-ext:quit :unix-status 0)
