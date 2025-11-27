;; Test native compilation of expression compiler patterns
;; These tests verify that patterns used in the Habu compiler work natively
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Test native expression compiler patterns ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/nec_~A" name)))
    (handler-case
        (progn
          (habu:deliver-with-libsystem source path)
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

;; Test 1: Tag checking (like has-tag? in compiler)
(test-native "tag-check"
  "(defun has-tag (ir tag)
     (if (consp ir)
         (if (eq (car ir) tag) 1 0)
         0))
   (+ (has-tag (cons 'lit (cons 42 nil)) 'lit)
      (has-tag (cons 'add nil) 'lit))"
  1)  ; First returns 1, second returns 0

;; Test 2: IR node accessors (like ir-arg1, ir-arg2)
(test-native "ir-accessors"
  "(defun ir-tag (ir) (car ir))
   (defun ir-arg1 (ir) (car (cdr ir)))
   (defun ir-arg2 (ir) (car (cdr (cdr ir))))
   (let ((ir (cons 'add (cons 10 (cons 20 nil)))))
     (+ (ir-arg1 ir) (ir-arg2 ir)))"
  30)

;; Test 3: Compile number expression
(test-native "compile-num"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (cons 'unknown nil)))
   (let ((ir (compile-expr 42)))
     (car (cdr ir)))"
  42)

;; Test 4: Compile arithmetic with symbol dispatch
(test-native "compile-arith"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+)
                   (cons 'add (cons (compile-expr (car (cdr expr)))
                                   (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                   (cons 'unknown nil)))
             (cons 'unknown nil))))
   (let ((ir (compile-expr (cons '+ (cons 10 (cons 20 nil))))))
     (if (eq (car ir) 'add) 42 0))"
  42)

;; Test 5: Environment lookup (key pattern in compiler)
(test-native "env-lookup"
  "(defun my-assoc (key alist)
     (if (null alist)
         nil
         (if (eq key (car (car alist)))
             (car alist)
             (my-assoc key (cdr alist)))))
   (defun env-lookup (name env)
     (let ((binding (my-assoc name env)))
       (if binding (cdr binding) 0)))
   (let ((env (cons (cons 'x 10) (cons (cons 'y 20) nil))))
     (+ (env-lookup 'x env) (env-lookup 'y env)))"
  30)

;; Test 6: Environment extension (like compile-env binding)
(test-native "env-extend"
  "(defun env-extend (name val env)
     (cons (cons name val) env))
   (defun my-assoc (key alist)
     (if (null alist)
         nil
         (if (eq key (car (car alist)))
             (car alist)
             (my-assoc key (cdr alist)))))
   (let* ((env nil)
          (env (env-extend 'x 10 env))
          (env (env-extend 'y 20 env)))
     (cdr (my-assoc 'y env)))"
  20)

;; Test 7: Compile and evaluate IR (simplified)
(test-native "compile-eval"
  "(defun compile-expr (expr)
     (if (numberp expr)
         (cons 'lit (cons expr nil))
         (if (consp expr)
             (let ((op (car expr)))
               (if (eq op '+)
                   (cons 'add (cons (compile-expr (car (cdr expr)))
                                   (cons (compile-expr (car (cdr (cdr expr)))) nil)))
                   (cons 'lit (cons 0 nil))))
             (cons 'lit (cons 0 nil)))))
   (defun eval-ir (ir)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           (if (eq tag 'add)
               (+ (eval-ir (car (cdr ir)))
                  (eval-ir (car (cdr (cdr ir)))))
               0))))
   (eval-ir (compile-expr (cons '+ (cons 10 (cons 32 nil)))))"
  42)

;; Test 8: Nested compilation (compile nested expressions)
(test-native "nested-compile"
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
   ;; (* 3 (+ 4 10)) = 3 * 14 = 42
   (eval-ir (compile-expr (cons '* (cons 3 (cons (cons '+ (cons 4 (cons 10 nil))) nil)))))"
  42)

;; Test 9: Let binding compilation pattern
(test-native "compile-let"
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
             (cons 'unknown nil))))
   (defun eval-ir (ir stack)
     (let ((tag (car ir)))
       (if (eq tag 'lit)
           (car (cdr ir))
           (if (eq tag 'var-ref)
               (nth (car (cdr ir)) stack)
               0))))
   (let* ((env (cons (cons 'x 0) (cons (cons 'y 1) nil)))
          (ir (compile-expr 'y env))
          (stack (cons 10 (cons 32 nil))))
     (eval-ir ir stack))"
  32)

;; Test 10: Multiple operator support
(test-native "multi-ops"
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
   ;; (- (* 7 8) (+ 10 4)) = 56 - 14 = 42
   (eval-ir (compile-expr (cons '- (cons (cons '* (cons 7 (cons 8 nil)))
                                         (cons (cons '+ (cons 10 (cons 4 nil))) nil)))))"
  42)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
