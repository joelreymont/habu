;; Test native compilation of compile-expr - core of Habu compiler
;; These tests build up to compiling the actual expression compiler
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-native-compile-expr
  (:use :cl)
  (:import-from :habu #:deliver))
(in-package :habu-test-native-compile-expr)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test native compile-expr patterns ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/nce_~A" name)))
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

;; Test 1: IR node construction with cons
(test-native "ir-node"
  "(defun make-lit (val) (cons 'lit (cons val nil)))
   (let ((ir (make-lit 42)))
     (car (cdr ir)))"
  42)

;; Test 2: Compile number to IR
(test-native "compile-num"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         nil))
   (let ((ir (compile-expr 42)))
     (car (cdr ir)))"
  42)

;; Test 3: Compile arithmetic with nested calls
(test-native "compile-add"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+)
                   (cons 'add (cons (compile-expr (car (cdr expr)))
                                   (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                   nil))
             nil)))
   (let ((ir (compile-expr (cons '+ (cons 10 (cons 20 nil))))))
     (if (eq (car ir) 'add) 42 0))"
  42)

;; Test 4: Evaluate IR for lit nodes
(test-native "eval-lit"
  "(defun eval-ir (ir)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           0)))
   (eval-ir (cons 'lit (cons 42 nil)))"
  42)

;; Test 5: Evaluate IR for add nodes
(test-native "eval-add"
  "(defun eval-ir (ir)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           (if (eq tag 'add)
               (+ (eval-ir (car (cdr ir)))
                  (eval-ir (car (cdr (cdr ir)))))
               0))))
   (eval-ir (cons 'add (cons (cons 'lit (cons 10 nil))
                             (cons (cons 'lit (cons 32 nil)) nil))))"
  42)

;; Test 6: Full compile + eval for arithmetic
(test-native "compile-eval"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+)
                   (cons 'add (cons (compile-expr (car (cdr expr)))
                                   (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                   (if (eq op '-)
                       (cons 'sub (cons (compile-expr (car (cdr expr)))
                                       (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                       (if (eq op '*)
                           (cons 'mul (cons (compile-expr (car (cdr expr)))
                                           (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                           (cons 'lit (cons 0 nil))))))
             (cons 'lit (cons 0 nil)))))
   (defun eval-ir (ir)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           (if (eq tag 'add)
               (+ (eval-ir (car (cdr ir)))
                  (eval-ir (car (cdr (cdr ir)))))
               (if (eq tag 'sub)
                   (- (eval-ir (car (cdr ir)))
                      (eval-ir (car (cdr (cdr ir)))))
                   (if (eq tag 'mul)
                       (* (eval-ir (car (cdr ir)))
                          (eval-ir (car (cdr (cdr ir)))))
                       0))))))
   ;; (* 6 7) = 42
   (eval-ir (compile-expr (cons '* (cons 6 (cons 7 nil)))))"
  42)

;; Test 7: Variable lookup with environment
(test-native "var-lookup"
  "(defun my-assoc (key alist)
     (if (null alist)
         nil
         (if (eq key (car (car alist)))
             (car alist)
             (my-assoc key (cdr alist)))))
   (defun compile-expr (expr env)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (if (symbolp expr)
             (let ((binding (my-assoc expr env)))
               (if binding
                   (cons 'var-ref (cons (cdr binding) nil))
                   (cons 'lit (cons 0 nil))))
             (cons 'lit (cons 0 nil)))))
   (defun eval-ir (ir stack)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           (if (eq tag 'var-ref)
               (nth (car (cdr ir)) stack)
               0))))
   (let* ((env (cons (cons 'x 0) (cons (cons 'y 1) nil)))
          (stack (cons 10 (cons 32 nil)))
          (ir (compile-expr 'y env)))
     (eval-ir ir stack))"
  32)

;; Test 8: Nested expression compilation
(test-native "nested-expr"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+)
                   (cons 'add (cons (compile-expr (car (cdr expr)))
                                   (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                   (if (eq op '*)
                       (cons 'mul (cons (compile-expr (car (cdr expr)))
                                       (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                       (cons 'lit (cons 0 nil)))))
             (cons 'lit (cons 0 nil)))))
   (defun eval-ir (ir)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           (if (eq tag 'add)
               (+ (eval-ir (car (cdr ir)))
                  (eval-ir (car (cdr (cdr ir)))))
               (if (eq tag 'mul)
                   (* (eval-ir (car (cdr ir)))
                      (eval-ir (car (cdr (cdr ir)))))
                   0)))))
   ;; (+ (* 3 4) (+ 5 7)) = 12 + 12 = 24
   (eval-ir (compile-expr (cons '+ (cons (cons '* (cons 3 (cons 4 nil)))
                                         (cons (cons '+ (cons 5 (cons 7 nil))) nil)))))"
  24)

;; Test 9: Multiple operator support with compile and eval
(test-native "multi-op"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+) (cons 'add (cons (compile-expr (car (cdr expr)))
                                              (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                   (if (eq op '-) (cons 'sub (cons (compile-expr (car (cdr expr)))
                                                  (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                       (if (eq op '*) (cons 'mul (cons (compile-expr (car (cdr expr)))
                                                      (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                           (if (eq op '/) (cons 'div (cons (compile-expr (car (cdr expr)))
                                                          (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                               (cons 'lit (cons 0 nil)))))))
             (cons 'lit (cons 0 nil)))))
   (defun eval-ir (ir)
     (let ((tag (car ir)))
       (if (eq tag 'lit) (car (cdr ir))
           (if (eq tag 'add) (+ (eval-ir (car (cdr ir))) (eval-ir (car (cdr (cdr ir)))))
               (if (eq tag 'sub) (- (eval-ir (car (cdr ir))) (eval-ir (car (cdr (cdr ir)))))
                   (if (eq tag 'mul) (* (eval-ir (car (cdr ir))) (eval-ir (car (cdr (cdr ir)))))
                       (if (eq tag 'div) (/ (eval-ir (car (cdr ir))) (eval-ir (car (cdr (cdr ir)))))
                           0)))))))
   ;; (- (* 7 8) (/ 28 2)) = 56 - 14 = 42
   (eval-ir (compile-expr (cons '- (cons (cons '* (cons 7 (cons 8 nil)))
                                         (cons (cons '/ (cons 28 (cons 2 nil))) nil)))))"
  42)

;; Test 10: Let binding compilation
;; Directly build IR and evaluate: (let ((x 32)) (+ x 10)) = 42
;; IR: (let-ir (lit 32) (add (var-ref) (lit 10)))
(test-native "compile-let"
  "(defun eval-ir (ir stack)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           (if (eq tag 'var-ref)
               (car stack)
               (if (eq tag 'add)
                   (+ (eval-ir (car (cdr ir)) stack)
                      (eval-ir (car (cdr (cdr ir))) stack))
                   (if (eq tag 'let-ir)
                       (let* ((val (eval-ir (car (cdr ir)) stack))
                              (new-stack (cons val stack)))
                         (eval-ir (car (cdr (cdr ir))) new-stack))
                       0))))))
   ;; IR: (let-ir (lit 32) (add (var-ref) (lit 10)))
   (let ((ir (cons 'let-ir (cons (cons 'lit (cons 32 nil))
                                  (cons (cons 'add (cons (cons 'var-ref nil)
                                                          (cons (cons 'lit (cons 10 nil)) nil)))
                                        nil)))))
     (eval-ir ir nil))"
  42)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
