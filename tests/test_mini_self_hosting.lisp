#!/usr/bin/env sbcl --script
;;; Mini Self-Hosting Test
;;; Compiles a simple expression evaluator and runs it to evaluate an expression
;;; This is a stepping stone towards full self-hosting

(load "run-habu.lisp")

(format t "~%=== MINI SELF-HOSTING TEST ===~%~%")

;; Test 1: Compile a simple evaluator for add expressions
;; The evaluator takes a tagged list representation and evaluates it
(format t "Test 1: Compile and run a simple evaluator~%")
(format t "  - Compiling a mini-evaluator for (+ a b) expressions~%")

;; Expression format: (list 'add a b) where a, b are fixnums
;; The evaluator checks if car is 'add symbol and returns a + b
(let ((result (habu-sbcl:compile-and-run-forms
               '(;; Simple evaluator: takes list (add x y), returns x + y
                 (defun eval-expr (expr)
                   (let ((op (car expr))
                         (a (cadr expr))
                         (b (caddr expr)))
                     ;; Check if op is 'add (we use eq with quoted symbol)
                     (if (eq op 'add)
                         (+ a b)
                         #x0)))

                 ;; Test: create expression (add 10 20) and evaluate it
                 (eval-expr (list 'add #x10 #x20))))))
  (format t "  - Result: ~A (expected 48 = 0x30)~%" result)
  (unless (= result #x30)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 2: Compile a recursive evaluator with multiple ops
(format t "~%Test 2: Recursive evaluator with nested expressions~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '(;; Evaluator supporting add, sub, lit
                 (defun eval-expr (expr)
                   (cond
                     ;; Literal: just the value
                     ((not (consp expr)) expr)
                     ;; (lit n)
                     ((eq (car expr) 'lit) (cadr expr))
                     ;; (add e1 e2)
                     ((eq (car expr) 'add)
                      (+ (eval-expr (cadr expr))
                         (eval-expr (caddr expr))))
                     ;; (sub e1 e2)
                     ((eq (car expr) 'sub)
                      (- (eval-expr (cadr expr))
                         (eval-expr (caddr expr))))
                     ;; (mul e1 e2)
                     ((eq (car expr) 'mul)
                      (* (eval-expr (cadr expr))
                         (eval-expr (caddr expr))))
                     (t #x0)))

                 ;; Test: (add (mul 3 4) (sub 10 5)) = 12 + 5 = 17
                 (eval-expr (list 'add
                                  (list 'mul #x3 #x4)
                                  (list 'sub #xa #x5)))))))
  (format t "  - Expression: (add (mul 3 4) (sub 10 5))~%")
  (format t "  - Result: ~A (expected 17)~%" result)
  (unless (= result #x11)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 3: Compile an IR generator (like compile-expr)
(format t "~%Test 3: Mini IR generator (simulates compile-expr)~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '(;; Mini IR generator: converts expressions to IR representation
                 (defun gen-ir (expr)
                   (cond
                     ;; Literal number
                     ((numberp expr) (list 'lit expr))
                     ;; (+ a b)
                     ((and (consp expr) (eq (car expr) '+))
                      (list 'add (gen-ir (cadr expr)) (gen-ir (caddr expr))))
                     ;; (- a b)
                     ((and (consp expr) (eq (car expr) '-))
                      (list 'sub (gen-ir (cadr expr)) (gen-ir (caddr expr))))
                     ;; (* a b)
                     ((and (consp expr) (eq (car expr) '*))
                      (list 'mul (gen-ir (cadr expr)) (gen-ir (caddr expr))))
                     (t nil)))

                 ;; Evaluator for IR
                 (defun eval-ir (ir)
                   (cond
                     ((eq (car ir) 'lit) (cadr ir))
                     ((eq (car ir) 'add)
                      (+ (eval-ir (cadr ir)) (eval-ir (caddr ir))))
                     ((eq (car ir) 'sub)
                      (- (eval-ir (cadr ir)) (eval-ir (caddr ir))))
                     ((eq (car ir) 'mul)
                      (* (eval-ir (cadr ir)) (eval-ir (caddr ir))))
                     (t #x0)))

                 ;; Test: generate IR for (+ (* 2 3) 4) and evaluate
                 ;; IR should be: (add (mul (lit 2) (lit 3)) (lit 4))
                 (let ((ir (gen-ir (list '+ (list '* #x2 #x3) #x4))))
                   (eval-ir ir))))))
  (format t "  - Source: (+ (* 2 3) 4)~%")
  (format t "  - Result: ~A (expected 10)~%" result)
  (unless (= result #xa)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 4: Full compile + eval pipeline (simulates self-hosting)
;; Use numeric identifiers instead of symbols to avoid interning issues
(format t "~%Test 4: Complete mini-compiler pipeline~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '(;; Environment lookup (using numeric var IDs)
                 (defun env-lookup (var-id env)
                   (if (nil? env)
                       nil
                       (if (= (car (car env)) var-id)
                           (cdr (car env))
                           (env-lookup var-id (cdr env)))))

                 ;; IR generator with variable support
                 ;; Variables are represented as (var-ref id) where id is a fixnum
                 (defun compile-to-ir (expr env)
                   (cond
                     ;; Number literal
                     ((numberp expr) (list 'lit expr))
                     ;; (var-ref id) - variable reference
                     ((and (consp expr) (eq (car expr) 'var-ref))
                      (let ((offset (env-lookup (cadr expr) env)))
                        ;; Use numberp because nil? considers 0 as nil-like in Habu
                        (if (numberp offset)
                            (list 'var offset)
                            (list 'lit #x0))))
                     ;; (+ a b)
                     ((and (consp expr) (eq (car expr) '+))
                      (list 'add
                            (compile-to-ir (cadr expr) env)
                            (compile-to-ir (caddr expr) env)))
                     ;; (let1 id val body) - single var let
                     ((and (consp expr) (eq (car expr) 'let1))
                      (let* ((var-id (cadr expr))
                             (val-expr (caddr expr))
                             (body (cadddr expr))
                             (val-ir (compile-to-ir val-expr env))
                             (new-offset (if env (+ (cdr (car env)) #x1) #x0))
                             (new-env (cons (cons var-id new-offset) env))
                             (body-ir (compile-to-ir body new-env)))
                        (list 'let-expr (list (cons new-offset val-ir)) body-ir)))
                     (t (list 'lit #x0))))

                 ;; IR evaluator with let support
                 (defun eval-ir (ir stack)
                   (cond
                     ((eq (car ir) 'lit) (cadr ir))
                     ((eq (car ir) 'var)
                      (let ((offset (cadr ir)))
                        (nth offset stack)))
                     ((eq (car ir) 'add)
                      (+ (eval-ir (cadr ir) stack)
                         (eval-ir (caddr ir) stack)))
                     ((eq (car ir) 'let-expr)
                      (let* ((bindings (cadr ir))
                             (body (caddr ir))
                             (binding (car bindings))
                             (offset (car binding))
                             (val-ir (cdr binding))
                             (val (eval-ir val-ir stack))
                             (new-stack (append stack (list val))))
                        (eval-ir body new-stack)))
                     (t #x0)))

                 ;; Test: compile and evaluate (let1 1 5 (+ (var-ref 1) 10))
                 ;; Equivalent to: (let ((x 5)) (+ x 10)) with x = var-id 1
                 (let* ((expr (list 'let1 #x1 #x5
                                    (list '+ (list 'var-ref #x1) #xa)))
                        (ir (compile-to-ir expr nil)))
                   (eval-ir ir nil))))))
  (format t "  - Expression: (let1 1 5 (+ (var-ref 1) 10)) [like (let ((x 5)) (+ x 10))]~%")
  (format t "  - Result: ~A (expected 15)~%" result)
  (unless (= result #xf)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 5: Higher-order function in mini-compiler
(format t "~%Test 5: Mini-compiler with mapcar code generation~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '(;; Simulate code generation: compile a list of expressions
                 (defun compile-exprs (exprs)
                   (mapcar (lambda (e) (list 'lit e)) exprs))

                 ;; Sum up the literals
                 (defun sum-literals (irs)
                   (reduce (lambda (acc ir) (+ acc (cadr ir)))
                           irs
                           #x0))

                 ;; Test: compile (1 2 3 4 5) and sum = 15
                 (sum-literals (compile-exprs (list #x1 #x2 #x3 #x4 #x5)))))))
  (format t "  - Compile and sum literals from list (1 2 3 4 5)~%")
  (format t "  - Result: ~A (expected 15)~%" result)
  (unless (= result #xf)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

(format t "~%=== ALL MINI SELF-HOSTING TESTS PASSED ===~%")
(format t "~%The compiler can:~%")
(format t "  - Compile expression evaluators~%")
(format t "  - Compile recursive evaluators~%")
(format t "  - Compile IR generators~%")
(format t "  - Compile full compile+eval pipelines~%")
(format t "  - Handle higher-order functions in mini-compilers~%")
(format t "~%This demonstrates the compiler can generate code that generates/evaluates code.~%")

(sb-ext:quit :unix-status 0)
