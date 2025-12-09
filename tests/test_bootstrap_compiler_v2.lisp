;;; Test that the bootstrap compiler can compile a mini-compiler
;;; This validates self-hosting capability by compiling a compiler
;;; Updated to use deliver with sys-exit
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(defpackage :habu-test-bootstrap-compiler-v2
  (:use :cl))

(in-package :habu-test-bootstrap-compiler-v2)

(format t "~%=== Bootstrap Compiler Self-Hosting Test (v2) ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-compile (name source expected)
  (handler-case
    (let ((output-path (format nil "/tmp/bootstrap_~A" name)))
      (habu:deliver source output-path)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (if (= result expected)
            (progn (format t "[PASS] ~A = ~A~%" name result)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;;; Test 1: Mini expression compiler
(test-compile "mini-expr-compiler"
  "(defun compile-expr (e)
     (if (numberp e)
         e
         (if (consp e)
             (let ((op (car e)))
               (if (eq op (quote add))
                   (+ (compile-expr (cadr e)) (compile-expr (caddr e)))
                   (if (eq op (quote mul))
                       (* (compile-expr (cadr e)) (compile-expr (caddr e)))
                       0)))
             0)))
   (sys-exit (compile-expr (cons (quote add) (cons 10 (cons (cons (quote mul) (cons 4 (cons 8 nil))) nil)))))"
  42)

;;; Test 2: IR node builder
(test-compile "ir-builder"
  "(defun make-lit (n) (cons (quote lit) (cons n nil)))
   (defun make-add (l r) (cons (quote add) (cons l (cons r nil))))
   (defun ir-value (ir)
     (let ((tag (car ir)))
       (if (eq tag (quote lit))
           (cadr ir)
           (if (eq tag (quote add))
               (+ (ir-value (cadr ir)) (ir-value (caddr ir)))
               0))))
   (sys-exit (ir-value (make-add (make-lit 10) (make-add (make-lit 20) (make-lit 12)))))"
  42)

;;; Test 3: Symbol table operations
(test-compile "symbol-table"
  "(defun env-add (name val env) (cons (cons name val) env))
   (defun env-get (name env)
     (if (null env) 0
         (if (eq name (car (car env)))
             (cdr (car env))
             (env-get name (cdr env)))))
   (let* ((e1 (env-add (quote x) 10 nil))
          (e2 (env-add (quote y) 20 e1))
          (e3 (env-add (quote z) 12 e2))
          (vx (env-get (quote x) e3))
          (vy (env-get (quote y) e3))
          (vz (env-get (quote z) e3)))
     (sys-exit (+ vx (+ vy vz))))"
  42)

;;; Test 4: Codegen helper pattern
(test-compile "codegen-helper"
  "(defun emit-add (a b) (+ a b))
   (defun emit-mul (a b) (* a b))
   (defun codegen (ir)
     (let ((tag (car ir)))
       (if (eq tag (quote lit))
           (cadr ir)
           (if (eq tag (quote add))
               (emit-add (codegen (cadr ir)) (codegen (caddr ir)))
               (if (eq tag (quote mul))
                   (emit-mul (codegen (cadr ir)) (codegen (caddr ir)))
                   0)))))
   (sys-exit (codegen (quote (add (lit 10) (mul (lit 4) (lit 8))))))"
  42)

;;; Test 5: Recursive descent pattern
(test-compile "recursive-descent"
  "(defun parse-num (n) n)
   (defun parse-op (op a b)
     (if (eq op (quote plus))
         (+ (parse-expr a) (parse-expr b))
         (if (eq op (quote times))
             (* (parse-expr a) (parse-expr b))
             0)))
   (defun parse-expr (e)
     (if (numberp e)
         (parse-num e)
         (parse-op (car e) (cadr e) (caddr e))))
   (sys-exit (parse-expr (quote (plus 10 (times 4 8)))))"
  42)

;;; Test 6: Lambda lifting pattern
(test-compile "lambda-lifter"
  "(defun make-closure (fn env) (cons fn env))
   (defun closure-fn (c) (car c))
   (defun closure-env (c) (cdr c))
   (defun apply-closure (c arg)
     (funcall (closure-fn c) (closure-env c) arg))
   (defun make-adder-closure (n)
     (make-closure (lambda (env x) (+ (car env) x)) (cons n nil)))
   (let ((add10 (make-adder-closure 10)))
     (sys-exit (apply-closure add10 32)))"
  42)

;;; Test 7: Instruction encoder pattern
(test-compile "instr-encoder"
  "(defun encode-mov (rd imm)
     (+ (* rd 256) imm))
   (defun encode-add (rd rn rm)
     (+ 1000 (+ (* rd 100) (+ (* rn 10) rm))))
   (defun encode-instr (instr)
     (let ((op (car instr)))
       (if (eq op (quote mov))
           (encode-mov (cadr instr) (caddr instr))
           (if (eq op (quote add))
               (encode-add (cadr instr) (caddr instr) (cadddr instr))
               0))))
   (let* ((i1 (encode-instr (quote (mov 0 42))))
          (i2 (encode-instr (quote (add 1 2 3)))))
     (sys-exit (- i2 i1)))"
  ;; i1 = 0*256 + 42 = 42
  ;; i2 = 1000 + 1*100 + 2*10 + 3 = 1123
  ;; i2 - i1 = 1123 - 42 = 1081 -> mod 256 = 57
  57)

;;; Test 8: Fixup pass pattern
(test-compile "fixup-pass"
  "(defun collect-labels (instrs idx labels)
     (if (null instrs)
         labels
         (let ((instr (car instrs)))
           (if (eq (car instr) (quote label))
               (collect-labels (cdr instrs) idx (cons (cons (cadr instr) idx) labels))
               (collect-labels (cdr instrs) (+ idx 1) labels)))))
   (defun resolve-label (name labels)
     (if (null labels)
         0
         (if (eq name (car (car labels)))
             (cdr (car labels))
             (resolve-label name (cdr labels)))))
   (let* ((instrs (quote ((label start) (mov 0 10) (add 0 1) (label end) (ret))))
          (labels (collect-labels instrs 0 nil))
          (start-addr (resolve-label (quote start) labels))
          (end-addr (resolve-label (quote end) labels)))
     (sys-exit (+ start-addr (+ end-addr 42))))"
  44)  ; start=0, end=2, 0+2+42=44

;;; Test 9: Multi-function compilation
(test-compile "multi-fn-compile"
  "(defun f1 (x) (+ x 1))
   (defun f2 (x) (* x 2))
   (defun f3 (x) (- x 3))
   (defun compose (a b c x)
     (f3 (f2 (f1 x))))
   (sys-exit (compose 0 0 0 13))"
  ;; f1(13) = 14, f2(14) = 28, f3(28) = 25
  25)

;;; Test 10: Higher-order transformation pattern
(test-compile "ho-transform"
  "(defun transform-expr (e transformer)
     (if (numberp e)
         (funcall transformer e)
         (if (consp e)
             (cons (car e)
                   (cons (transform-expr (cadr e) transformer)
                         (cons (transform-expr (caddr e) transformer) nil)))
             e)))
   (defun double (x) (* x 2))
   (defun eval-expr (e)
     (if (numberp e)
         e
         (let ((op (car e)))
           (if (eq op (quote add))
               (+ (eval-expr (cadr e)) (eval-expr (caddr e)))
               0))))
   (let* ((expr (quote (add 5 10)))
          (doubled (transform-expr expr (function double))))
     (sys-exit (eval-expr doubled)))"
  30)  ; double(5)=10, double(10)=20, 10+20=30

(format t "~%=== Results: ~A passed, ~A failed ===~%" *pass-count* *fail-count*)
