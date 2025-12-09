;; Test end-to-end compilation pipeline: parse -> compile -> eval
;; These tests demonstrate the complete compilation cycle needed for self-hosting
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-end-to-end-compile
  (:use :cl)
  (:import-from :habu #:deliver))
(in-package :habu-test-end-to-end-compile)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test end-to-end compile pipeline ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  (let ((path (format nil "/tmp/e2e_~A" name)))
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

;; Test 1: Parse s-expression structure
;; Demonstrates: parsing lists, extracting elements
(test-native "parse-sexp"
  "(let* ((expr (list '+ (list '* 2 3) (list '- 10 5)))
          (op (car expr))
          (arg1 (cadr expr))
          (arg2 (caddr expr)))
     ;; Check structure: (+ (* 2 3) (- 10 5))
     (if (and (eq op '+)
              (eq (car arg1) '*)
              (eq (car arg2) '-))
         42
         0))"
  42)

;; Test 2: Expression type classification
;; Demonstrates: type dispatch for compiler (using cond)
(test-native "type-classify"
  "(defun classify (x)
     (if (consp x)
         30
         (if (null x)
             12
             0)))
   ;; Test cons and nil type checking
   (+ (classify (cons 1 2)) (classify nil))"
  42)  ; 30 + 12 = 42

;; Test 3: Recursive IR traversal
;; Demonstrates: tree walking pattern for compilation
(test-native "ir-traverse"
  "(defun count-nodes (ir)
     (if (consp ir)
         (+ 1 (count-nodes (car ir)) (count-nodes (cdr ir)))
         0))
   ;; Count cons cells in (add (lit 1) (lit 2))
   ;; Structure: (add . ((lit . (1 . nil)) . ((lit . (2 . nil)) . nil)))
   ;; 7 cons cells total
   (count-nodes (list 'add (list 'lit 1) (list 'lit 2)))"
  7)

;; Test 4: Symbol table operations
;; Demonstrates: environment management for variable binding
(test-native "symtab-ops"
  "(defun my-assoc (key alist)
     (if (null alist)
         nil
         (if (eq key (car (car alist)))
             (car alist)
             (my-assoc key (cdr alist)))))
   (defun extend-env (name val env)
     (cons (cons name val) env))
   (let* ((env nil)
          (env (extend-env 'x 10 env))
          (env (extend-env 'y 20 env))
          (env (extend-env 'z 12 env)))
     (let ((entry (my-assoc 'z env)))
       (if entry (cdr entry) 0)))"
  12)

;; Test 5: Code generation pattern - instruction encoding
;; Demonstrates: ARM64-style instruction encoding (simplified for exit code range)
(test-native "instr-encode"
  "(defun encode-add (rd rn rm)
     ;; Simplified encoding: combine rd, rn, rm into single value
     (+ rd (* rn 4) (* rm 16)))
   ;; Encode: ADD x0, x1, x2 = 0 + 4 + 32 = 36
   ;; Then add 6 to get 42
   (+ (encode-add 0 1 2) 6)"
  42)

;; Test 6: Multi-pass compilation pattern
;; Demonstrates: collect then process pattern
(test-native "multi-pass"
  "(defun collect-names (forms)
     (if (null forms)
         nil
         (if (and (consp (car forms)) (eq (car (car forms)) 'defun))
             (cons (cadr (car forms)) (collect-names (cdr forms)))
             (collect-names (cdr forms)))))
   ;; Collect function names from a list of forms
   (let ((forms (list (list 'defun 'foo nil)
                      (list 'defun 'bar nil)
                      (list 'let nil 42))))
     (length (collect-names forms)))"
  2)

;; Test 7: Closure-based compilation
;; Demonstrates: higher-order patterns for code generation
(test-native "closure-compile"
  "(defun make-adder (n)
     (lambda (x) (+ x n)))
   (let* ((add5 (make-adder 5))
          (add10 (make-adder 10)))
     (+ (funcall add5 20) (funcall add10 7)))"
  42)  ; (20+5) + (7+10) = 25 + 17 = 42

;; Test 8: IR optimization pattern - constant folding
;; Demonstrates: optimization pass in compiler
(test-native "const-fold"
  "(defun fold-ir (ir)
     (cond
       ((not (consp ir)) ir)
       ((eq (car ir) 'lit) ir)
       ((eq (car ir) 'add)
        (let ((left (fold-ir (cadr ir)))
              (right (fold-ir (caddr ir))))
          (if (and (consp left) (eq (car left) 'lit)
                   (consp right) (eq (car right) 'lit))
              (list 'lit (+ (cadr left) (cadr right)))
              (list 'add left right))))
       (t ir)))
   ;; Fold (add (lit 10) (lit 32)) -> (lit 42)
   (let ((result (fold-ir (list 'add (list 'lit 10) (list 'lit 32)))))
     (if (and (eq (car result) 'lit)
              (= (cadr result) 42))
         42
         0))"
  42)

;; Test 9: Stack-based evaluation
;; Demonstrates: VM-style evaluation with operand stack
(test-native "stack-eval"
  "(defun stack-eval (instrs stack)
     (if (null instrs)
         (car stack)
         (let ((instr (car instrs)))
           (cond
             ((eq (car instr) 'push)
              (stack-eval (cdr instrs) (cons (cadr instr) stack)))
             ((eq (car instr) 'add)
              (let ((a (car stack))
                    (b (cadr stack))
                    (rest (cddr stack)))
                (stack-eval (cdr instrs) (cons (+ a b) rest))))
             ((eq (car instr) 'mul)
              (let ((a (car stack))
                    (b (cadr stack))
                    (rest (cddr stack)))
                (stack-eval (cdr instrs) (cons (* a b) rest))))
             (t (stack-eval (cdr instrs) stack))))))
   ;; Evaluate: push 6, push 7, mul -> 42
   (stack-eval (list (list 'push 6) (list 'push 7) (list 'mul)) nil)"
  42)

;; Test 10: Full compile-eval cycle with conditionals
;; Demonstrates: control flow compilation
(test-native "compile-if"
  "(defun compile-expr (expr env)
     (cond
       ((numberp expr) (list 'lit expr))
       ((symbolp expr)
        (let ((idx (position expr env)))
          (if idx (list 'var idx) (list 'lit 0))))
       ((eq (car expr) '+)
        (list 'add (compile-expr (cadr expr) env)
                   (compile-expr (caddr expr) env)))
       ((eq (car expr) 'if)
        (list 'if-ir (compile-expr (cadr expr) env)
                     (compile-expr (caddr expr) env)
                     (compile-expr (cadddr expr) env)))
       (t (list 'lit 0))))
   (defun position (item lst)
     (labels ((pos (l i)
                (if (null l)
                    nil
                    (if (eq item (car l))
                        i
                        (pos (cdr l) (+ i 1))))))
       (pos lst 0)))
   (defun eval-ir (ir stack)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'var) (nth (cadr ir) stack))
       ((eq (car ir) 'add) (+ (eval-ir (cadr ir) stack)
                              (eval-ir (caddr ir) stack)))
       ((eq (car ir) 'if-ir)
        (if (not (= (eval-ir (cadr ir) stack) 0))
            (eval-ir (caddr ir) stack)
            (eval-ir (cadddr ir) stack)))
       (t 0)))
   ;; Compile and eval: (if 1 42 0)
   (let ((ir (compile-expr (list 'if 1 42 0) nil)))
     (eval-ir ir nil))"
  42)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
