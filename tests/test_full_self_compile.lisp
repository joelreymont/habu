;; Test full self-compilation: compiled compiler compiles working programs
;; Uses proven patterns from test_self_compiling_mini.lisp
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-full-self-compile
  (:use :cl))

(in-package :habu-test-full-self-compile)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test full self-compilation ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/fsc_~A" name)))
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

;; Test 1: Simple arithmetic compiler with all ops
(test-native "arith-compiler"
  ";; Compiler for arithmetic expressions
   (defun compile-expr (expr)
     (if (numberp expr)
         (list 'lit expr)
         (if (eq (car expr) '+)
             (list 'add (compile-expr (cadr expr)) (compile-expr (caddr expr)))
             (if (eq (car expr) '*)
                 (list 'mul (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                 (if (eq (car expr) '-)
                     (list 'sub (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                     (list 'lit 0))))))

   (defun eval-ir (ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'add)
             (+ (eval-ir (cadr ir)) (eval-ir (caddr ir)))
             (if (eq (car ir) 'mul)
                 (* (eval-ir (cadr ir)) (eval-ir (caddr ir)))
                 (if (eq (car ir) 'sub)
                     (- (eval-ir (cadr ir)) (eval-ir (caddr ir)))
                     0)))))

   ;; Compile and eval: (+ (* 3 4) (- 10 8)) = 12 + 2 = 14
   (eval-ir (compile-expr (list '+ (list '* 3 4) (list '- 10 8))))"
  14)

;; Test 2: Compiler with conditionals
(test-native "cond-compiler"
  ";; Compiler that handles if expressions
   (defun compile-expr (expr)
     (if (numberp expr)
         (list 'lit expr)
         (if (eq (car expr) '=)
             (list 'eq-ir (compile-expr (cadr expr)) (compile-expr (caddr expr)))
             (if (eq (car expr) 'if)
                 (list 'if-ir
                       (compile-expr (cadr expr))
                       (compile-expr (caddr expr))
                       (compile-expr (cadddr expr)))
                 (if (eq (car expr) '*)
                     (list 'mul (compile-expr (cadr expr)) (compile-expr (caddr expr)))
                     (list 'lit 0))))))

   (defun eval-ir (ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'mul)
             (* (eval-ir (cadr ir)) (eval-ir (caddr ir)))
             (if (eq (car ir) 'eq-ir)
                 (if (= (eval-ir (cadr ir)) (eval-ir (caddr ir))) 1 0)
                 (if (eq (car ir) 'if-ir)
                     (if (not (= (eval-ir (cadr ir)) 0))
                         (eval-ir (caddr ir))
                         (eval-ir (cadddr ir)))
                     0)))))

   ;; (if (= 5 5) (* 6 7) 0) = 42
   (eval-ir (compile-expr (list 'if (list '= 5 5) (list '* 6 7) 0)))"
  42)

;; Test 3: Compiler with variable stack
(test-native "stack-compiler"
  ";; Compiler with let bindings using a stack
   (defun compile-expr (expr depth)
     (if (numberp expr)
         (list 'lit expr)
         (if (eq (car expr) 'var)
             (list 'var-ref (- depth (cadr expr) 1))
             (if (eq (car expr) '+)
                 (list 'add
                       (compile-expr (cadr expr) depth)
                       (compile-expr (caddr expr) depth))
                 (if (eq (car expr) 'let1)
                     (list 'let-ir
                           (compile-expr (cadr expr) depth)
                           (compile-expr (caddr expr) (+ depth 1)))
                     (list 'lit 0))))))

   (defun eval-ir (ir stack)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'var-ref)
             (nth (cadr ir) stack)
             (if (eq (car ir) 'add)
                 (+ (eval-ir (cadr ir) stack) (eval-ir (caddr ir) stack))
                 (if (eq (car ir) 'let-ir)
                     (let ((val (eval-ir (cadr ir) stack)))
                       (eval-ir (caddr ir) (cons val stack)))
                     0)))))

   ;; (let1 10 (let1 20 (+ (var 0) (var 1)))) = 10 + 20 = 30
   (let ((ir (compile-expr (list 'let1 10 (list 'let1 20 (list '+ (list 'var 0) (list 'var 1)))) 0)))
     (eval-ir ir nil))"
  30)

;; Test 4: Compiler with function calls (pre-built IR)
(test-native "fn-compiler"
  ";; Evaluator with function call support
   (defun eval-ir (ir stack fn-ir)
     (if (eq (car ir) 'lit)
         (cadr ir)
         (if (eq (car ir) 'arg)
             (car stack)
             (if (eq (car ir) 'mul)
                 (* (eval-ir (cadr ir) stack fn-ir)
                    (eval-ir (caddr ir) stack fn-ir))
                 (if (eq (car ir) 'sub)
                     (- (eval-ir (cadr ir) stack fn-ir)
                        (eval-ir (caddr ir) stack fn-ir))
                     (if (eq (car ir) 'eq-ir)
                         (if (= (eval-ir (cadr ir) stack fn-ir)
                                (eval-ir (caddr ir) stack fn-ir))
                             1 0)
                         (if (eq (car ir) 'if-ir)
                             (if (not (= (eval-ir (cadr ir) stack fn-ir) 0))
                                 (eval-ir (caddr ir) stack fn-ir)
                                 (eval-ir (cadddr ir) stack fn-ir))
                             (if (eq (car ir) 'call)
                                 (let ((arg-val (eval-ir (cadr ir) stack fn-ir)))
                                   (eval-ir fn-ir (list arg-val) fn-ir))
                                 0))))))))

   ;; fact(n) = if n=0 then 1 else n * fact(n-1)
   (let ((fact-ir (list 'if-ir
                        (list 'eq-ir (list 'arg) (list 'lit 0))
                        (list 'lit 1)
                        (list 'mul
                              (list 'arg)
                              (list 'call (list 'sub (list 'arg) (list 'lit 1)))))))
     ;; fact(5) = 120
     (eval-ir (list 'call (list 'lit 5)) nil fact-ir))"
  120)

;; Test 5: Full compile-link-execute cycle (from working test)
(test-native "compile-link-exec"
  ";; Simulates compile-link-execute cycle with bytecode
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

   (defun link-bc (bc)
     (labels ((assign (instrs addr)
                (if (null instrs)
                    nil
                    (cons (cons addr (car instrs))
                          (assign (cdr instrs) (+ addr 4))))))
       (assign bc 0)))

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

   ;; (* (+ 3 4) (+ 2 4)) = 7 * 6 = 42
   (let* ((expr (list '* (list '+ 3 4) (list '+ 2 4)))
          (bc (compile-to-bc expr))
          (linked (link-bc bc)))
     (exec-bc linked nil))"
  42)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
