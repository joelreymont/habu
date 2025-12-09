;;; Test habu0: A minimal self-hosted compiler test
;;; Compiles a mini-compiler to native, then uses it to compile and run programs
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-habu0-compile
  (:use :cl)
  (:import-from :habu #:deliver))
(in-package :habu-test-habu0-compile)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test habu0: Self-Hosted Compiler ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/habu0_~A" name)))
    (handler-case
        (progn
          (habu:deliver source path)
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" path)
                              :output nil :error nil :wait t)
          (let* ((proc (sb-ext:run-program path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn
                  (format t "[PASS] ~A = ~A~%" name code)
                  (incf *tests-passed*))
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
                  (incf *tests-failed*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *tests-failed*)))))

;;; Test 1: Simple compile + eval (using quoted data instead of reader)
;;; This demonstrates the core compile-eval cycle without reader complexity
(test-native "simple-compile-eval"
  ";; Compile an expression represented as nested lists to IR and evaluate
   (defun compile-expr (expr)
     (if (numberp expr)
         (list 'lit expr)
         (if (eq (car expr) '+)
             (list 'add (compile-expr (cadr expr)) (compile-expr (caddr expr)))
             (if (eq (car expr) '*)
                 (list 'mul (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                 (list 'lit 0)))))

   (defun eval-ir (ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'add)
             (+ (eval-ir (cadr ir)) (eval-ir (caddr ir)))
             (if (eq (car ir) 'mul)
                 (* (eval-ir (cadr ir)) (eval-ir (caddr ir)))
                 0))))

   ;; Compile and evaluate: (+ 10 (* 4 8)) = 10 + 32 = 42
   (eval-ir (compile-expr (list '+ 10 (list '* 4 8))))"
  42)

;;; Test 2: A compiler that generates tagged IR and evaluates it
(test-native "ir-compiler"
  ";; IR compiler with multiple expression types
   (defun compile-expr (expr)
     (if (numberp expr)
         (list 'lit expr)
         (if (eq (car expr) '+)
             (list 'add-ir (compile-expr (cadr expr)) (compile-expr (caddr expr)))
             (if (eq (car expr) '-)
                 (list 'sub-ir (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                 (if (eq (car expr) '*)
                     (list 'mul-ir (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                     (if (eq (car expr) 'if)
                         (list 'if-ir
                               (compile-expr (cadr expr))
                               (compile-expr (caddr expr))
                               (compile-expr (cadddr expr)))
                         (list 'lit 0)))))))

   (defun eval-ir (ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'add-ir)
             (+ (eval-ir (cadr ir)) (eval-ir (caddr ir)))
             (if (eq (car ir) 'sub-ir)
                 (- (eval-ir (cadr ir)) (eval-ir (caddr ir)))
                 (if (eq (car ir) 'mul-ir)
                     (* (eval-ir (cadr ir)) (eval-ir (caddr ir)))
                     (if (eq (car ir) 'if-ir)
                         (if (not (= (eval-ir (cadr ir)) 0))
                             (eval-ir (caddr ir))
                             (eval-ir (cadddr ir)))
                         0))))))

   ;; Test: (+ (* 3 4) (- 10 (if 1 2 0))) = 12 + 8 = 20
   ;; Actually: 12 + (10 - 2) = 20
   (let ((ir (compile-expr (list '+ (list '* 3 4)
                                    (list '- 10 (list 'if 1 2 0))))))
     (eval-ir ir))"
  20)

;;; Test 3: Compiler with variable binding (environment)
(test-native "env-compiler"
  ";; Compiler with environment for variable lookup
   (defun env-lookup (name env)
     (if (null env)
         nil
         (if (eq name (car (car env)))
             (cdr (car env))
             (env-lookup name (cdr env)))))

   (defun env-extend (name val env)
     (cons (cons name val) env))

   (defun compile-expr (expr env)
     (if (numberp expr)
         (list 'lit expr)
         (if (symbolp expr)
             (let ((val (env-lookup expr env)))
               (if val (list 'lit val) (list 'lit 0)))
             (if (eq (car expr) '+)
                 (list 'add-ir (compile-expr (cadr expr) env)
                               (compile-expr (caddr expr) env))
                 (if (eq (car expr) 'let)
                     (let* ((binding (car (cadr expr)))
                            (name (car binding))
                            (val (cadr binding))
                            (val-result (eval-ir (compile-expr val env)))
                            (new-env (env-extend name val-result env)))
                       (compile-expr (caddr expr) new-env))
                     (list 'lit 0))))))

   (defun eval-ir (ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'add-ir)
             (+ (eval-ir (cadr ir)) (eval-ir (caddr ir)))
             0)))

   ;; Test: (let ((x 10)) (let ((y 32)) (+ x y)))
   (let ((ir (compile-expr (list 'let (list (list 'x 10))
                                 (list 'let (list (list 'y 32))
                                       (list '+ 'x 'y)))
                          nil)))
     (eval-ir ir))"
  42)

;;; Test 4: Full read-compile-codegen-execute simulation
(test-native "full-pipeline"
  ";; Simulates the full compiler pipeline with bytecode
   ;; 1. Parser: reads tokens
   ;; 2. Compiler: generates bytecode IR
   ;; 3. Linker: assigns addresses
   ;; 4. Executor: runs bytecode

   (defun compile-to-bc (expr)
     (if (numberp expr)
         (list (list 'push expr))
         (if (eq (car expr) '+)
             (append (compile-to-bc (cadr expr))
                     (compile-to-bc (caddr expr))
                     (list (list 'add)))
             (if (eq (car expr) '*)
                 (append (compile-to-bc (cadr expr))
                         (compile-to-bc (caddr expr))
                         (list (list 'mul)))
                 nil))))

   (defun link-bc (bc addr)
     (if (null bc)
         nil
         (cons (cons addr (car bc))
               (link-bc (cdr bc) (+ addr 4)))))

   (defun exec-bc (linked stack)
     (if (null linked)
         (car stack)
         (let ((instr (cdr (car linked)))
               (rest (cdr linked)))
           (if (eq (car instr) 'push)
               (exec-bc rest (cons (cadr instr) stack))
               (if (eq (car instr) 'add)
                   (exec-bc rest (cons (+ (car stack) (cadr stack)) (cddr stack)))
                   (if (eq (car instr) 'mul)
                       (exec-bc rest (cons (* (car stack) (cadr stack)) (cddr stack)))
                       (exec-bc rest stack)))))))

   ;; Test: (* (+ 2 3) (+ 3 5)) = 5 * 8 = 40
   (let* ((expr (list '* (list '+ 2 3) (list '+ 3 5)))
          (bc (compile-to-bc expr))
          (linked (link-bc bc 0)))
     (exec-bc linked nil))"
  40)

;;; Test 5: Self-similar compiler - compiles expressions like itself
(test-native "meta-compile"
  ";; A compiler that can represent its own structure
   ;; Uses tagged IR that mirrors the compiler's own operations

   (defun compile (expr)
     (if (numberp expr)
         (list 'const expr)
         (if (eq (car expr) 'if)
             (list 'branch
                   (compile (cadr expr))
                   (compile (caddr expr))
                   (compile (cadddr expr)))
             (if (eq (car expr) 'call)
                 (list 'invoke (cadr expr)
                       (mapcar #'compile (cddr expr)))
                 (if (eq (car expr) '+)
                     (list 'binop 'plus
                           (compile (cadr expr))
                           (compile (caddr expr)))
                     (if (eq (car expr) '*)
                         (list 'binop 'times
                               (compile (cadr expr))
                               (compile (caddr expr)))
                         (list 'const 0)))))))

   (defun run (ir)
     (if (eq (car ir) 'const)
         (cadr ir)
         (if (eq (car ir) 'binop)
             (let ((op (cadr ir))
                   (left (run (caddr ir)))
                   (right (run (cadddr ir))))
               (if (eq op 'plus) (+ left right) (* left right)))
             (if (eq (car ir) 'branch)
                 (if (not (= (run (cadr ir)) 0))
                     (run (caddr ir))
                     (run (cadddr ir)))
                 0))))

   ;; Test: compile and run (if 1 (+ 20 22) 0) = 42
   (let ((ir (compile (list 'if 1 (list '+ 20 22) 0))))
     (run ir))"
  42)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
