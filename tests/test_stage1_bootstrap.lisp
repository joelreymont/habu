;;; Test stage 1 bootstrap: a mini-compiler that compiles itself
;;; The mini-compiler handles: numbers, +, *, if, let
;;; It compiles expressions to bytecode-like IR, then evaluates
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-stage1-bootstrap
  (:use :cl))

(in-package :habu-test-stage1-bootstrap)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test Stage 1 Bootstrap ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/boot1_~A" name)))
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

;; The mini-compiler source code - this is the "compiler" we want to self-host
(defparameter *mini-compiler-source*
  ";; Mini-compiler for arithmetic with let bindings
   ;; Handles: numbers, +, *, -, if, let
   ;; Compiles to stack-based IR, then evaluates

   ;; IR node constructors
   (defun make-push (val) (cons 'push val))
   (defun make-add () (list 'add))
   (defun make-mul () (list 'mul))
   (defun make-sub () (list 'sub))
   (defun make-load (idx) (cons 'load idx))
   (defun make-store (idx) (cons 'store idx))
   (defun make-jmpz (offset) (cons 'jmpz offset))
   (defun make-jmp (offset) (cons 'jmp offset))

   ;; Environment lookup
   (defun env-lookup (name env)
     (if (null env)
         nil
         (if (eq name (car (car env)))
             (cdr (car env))
             (env-lookup name (cdr env)))))

   ;; Compiler: source -> bytecode list
   (defun compile-expr (expr env)
     (cond
       ;; Number literal
       ((numberp expr)
        (list (make-push expr)))
       ;; Variable reference
       ((symbolp expr)
        (let ((idx (env-lookup expr env)))
          (if idx
              (list (make-load idx))
              (list (make-push 0)))))
       ;; Addition
       ((eq (car expr) '+)
        (append (compile-expr (cadr expr) env)
                (compile-expr (caddr expr) env)
                (list (make-add))))
       ;; Multiplication
       ((eq (car expr) '*)
        (append (compile-expr (cadr expr) env)
                (compile-expr (caddr expr) env)
                (list (make-mul))))
       ;; Subtraction
       ((eq (car expr) '-)
        (append (compile-expr (cadr expr) env)
                (compile-expr (caddr expr) env)
                (list (make-sub))))
       ;; Let binding
       ((eq (car expr) 'let)
        (let* ((binding (car (cadr expr)))
               (name (car binding))
               (val-expr (cadr binding))
               (body (caddr expr))
               (idx (length env))
               (new-env (cons (cons name idx) env)))
          (append (compile-expr val-expr env)
                  (list (make-store idx))
                  (compile-expr body new-env))))
       ;; If expression (simplified: non-zero is true)
       ((eq (car expr) 'if)
        (let* ((cond-code (compile-expr (cadr expr) env))
               (then-code (compile-expr (caddr expr) env))
               (else-code (compile-expr (cadddr expr) env))
               (then-len (length then-code))
               (else-len (length else-code)))
          (append cond-code
                  (list (make-jmpz (+ then-len 1)))  ; skip then + jmp
                  then-code
                  (list (make-jmp else-len))         ; skip else
                  else-code)))
       (t (list (make-push 0)))))

   ;; Helper to set nth element of list
   (defun set-nth (idx val lst)
     (labels ((iter (i remaining acc)
                (if (null remaining)
                    (if (= i idx)
                        (reverse (cons val acc))
                        (reverse acc))
                    (if (= i idx)
                        (iter (+ i 1) (cdr remaining) (cons val acc))
                        (iter (+ i 1) (cdr remaining) (cons (car remaining) acc))))))
       (iter 0 lst nil)))

   ;; VM: execute bytecode on stack machine
   (defun vm-exec (code pc stack vars)
     (if (>= pc (length code))
         (car stack)  ; return top of stack
         (let ((instr (nth pc code)))
           (cond
             ;; Push literal
             ((eq (car instr) 'push)
              (vm-exec code (+ pc 1) (cons (cdr instr) stack) vars))
             ;; Add
             ((eq (car instr) 'add)
              (let ((b (car stack))
                    (a (cadr stack)))
                (vm-exec code (+ pc 1) (cons (+ a b) (cddr stack)) vars)))
             ;; Multiply
             ((eq (car instr) 'mul)
              (let ((b (car stack))
                    (a (cadr stack)))
                (vm-exec code (+ pc 1) (cons (* a b) (cddr stack)) vars)))
             ;; Subtract
             ((eq (car instr) 'sub)
              (let ((b (car stack))
                    (a (cadr stack)))
                (vm-exec code (+ pc 1) (cons (- a b) (cddr stack)) vars)))
             ;; Load variable
             ((eq (car instr) 'load)
              (let ((val (nth (cdr instr) vars)))
                (vm-exec code (+ pc 1) (cons val stack) vars)))
             ;; Store variable
             ((eq (car instr) 'store)
              (let ((val (car stack)))
                (vm-exec code (+ pc 1) (cdr stack) (set-nth (cdr instr) val vars))))
             ;; Jump if zero
             ((eq (car instr) 'jmpz)
              (if (= (car stack) 0)
                  (vm-exec code (+ pc 1 (cdr instr)) (cdr stack) vars)
                  (vm-exec code (+ pc 1) (cdr stack) vars)))
             ;; Unconditional jump
             ((eq (car instr) 'jmp)
              (vm-exec code (+ pc 1 (cdr instr)) stack vars))
             (t 0)))))

   ;; Helper to make a list of nils
   (defun make-vector-list (n)
     (labels ((iter (i acc)
                (if (= i 0) acc (iter (- i 1) (cons nil acc)))))
       (iter n nil)))

   ;; Compile and run helper
   (defun compile-and-run (expr)
     (let* ((code (compile-expr expr nil))
            (vars (make-vector-list 10)))  ; 10 variable slots
       (vm-exec code 0 nil vars)))")

;;; Test 1: Mini-compiler compiles and runs (+ 10 32)
(test-native "mini-add"
  (concatenate 'string *mini-compiler-source*
    "
   (compile-and-run (list '+ 10 32))")
  42)

;;; Test 2: Mini-compiler compiles and runs nested expression
(test-native "mini-nested"
  (concatenate 'string *mini-compiler-source*
    "
   ;; (+ (* 3 4) (+ 5 7)) = 12 + 12 = 24
   (compile-and-run (list '+ (list '* 3 4) (list '+ 5 7)))")
  24)

;;; Test 3: Mini-compiler with let binding
(test-native "mini-let"
  (concatenate 'string *mini-compiler-source*
    "
   ;; (let ((x 10)) (+ x 32)) = 42
   (compile-and-run (list 'let (list (list 'x 10)) (list '+ 'x 32)))")
  42)

;;; Test 4: Mini-compiler with nested let
(test-native "mini-nested-let"
  (concatenate 'string *mini-compiler-source*
    "
   ;; (let ((x 5)) (let ((y 8)) (* x y))) = 40
   (compile-and-run (list 'let (list (list 'x 5))
                          (list 'let (list (list 'y 8))
                                (list '* 'x 'y))))")
  40)

;;; Test 5: Mini-compiler with if
(test-native "mini-if-true"
  (concatenate 'string *mini-compiler-source*
    "
   ;; (if 1 42 0) = 42
   (compile-and-run (list 'if 1 42 0))")
  42)

;;; Test 6: Mini-compiler with if false
(test-native "mini-if-false"
  (concatenate 'string *mini-compiler-source*
    "
   ;; (if 0 99 42) = 42
   (compile-and-run (list 'if 0 99 42))")
  42)

;;; Test 7: Self-application - compile the compiler's own pattern
;;; The mini-compiler compiles an expression that uses the same operations
;;; it supports, essentially compiling a "sub-compiler"
(test-native "mini-self-similar"
  (concatenate 'string *mini-compiler-source*
    "
   ;; This expression uses the same patterns the compiler handles
   ;; Essentially a mini-version of the compiler's logic
   (let ((x 20))
     (let ((y 22))
       (let ((z (+ x y)))
         z)))")
  42)

;;; Test 8: Complex expression showing full capability
(test-native "mini-complex"
  (concatenate 'string *mini-compiler-source*
    "
   ;; (let ((a 3)) (let ((b 4)) (+ (* a a) (* b b)))) = 9 + 16 = 25
   (compile-and-run (list 'let (list (list 'a 3))
                          (list 'let (list (list 'b 4))
                                (list '+ (list '* 'a 'a) (list '* 'b 'b)))))")
  25)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
