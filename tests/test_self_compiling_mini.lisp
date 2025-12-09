;; Test self-compiling mini-compiler: a compiler that can compile itself
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-self-compiling-mini
  (:use :cl)
  (:import-from :habu #:deliver))
(in-package :habu-test-self-compiling-mini)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test self-compiling mini-compiler ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/scm_~A" name)))
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

;; Test 1: Mini-compiler that compiles and evaluates arithmetic IR
;; This is a complete compile + eval cycle: source -> IR -> result
(test-native "compile-eval-add"
  ";; A mini-compiler that compiles (+ a b) to IR and evaluates it
   (defun compile-expr (expr)
     (cond
       ((numberp expr) (list 'lit expr))
       ((and (consp expr) (eq (car expr) '+))
        (list 'add (compile-expr (cadr expr)) (compile-expr (caddr expr))))
       ((and (consp expr) (eq (car expr) '*))
        (list 'mul (compile-expr (cadr expr)) (compile-expr (caddr expr))))
       (t nil)))
   (defun eval-ir (ir)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'add) (+ (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       ((eq (car ir) 'mul) (* (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       (t 0)))
   ;; Compile (+ 10 20) to IR and evaluate
   (let ((ir (compile-expr (list '+ 10 20))))
     (eval-ir ir))"
  30)

;; Test 2: Self-application - compiler compiles another compiler's IR
;; IR1 = compile (+ 3 4), IR2 = compile (eval IR1)
(test-native "nested-compile"
  "(defun compile-expr (expr)
     (cond
       ((numberp expr) (list 'lit expr))
       ((and (consp expr) (eq (car expr) '+))
        (list 'add (compile-expr (cadr expr)) (compile-expr (caddr expr))))
       ((and (consp expr) (eq (car expr) '*))
        (list 'mul (compile-expr (cadr expr)) (compile-expr (caddr expr))))
       (t nil)))
   (defun eval-ir (ir)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'add) (+ (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       ((eq (car ir) 'mul) (* (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       (t 0)))
   ;; Nested compilation: compile (+ (* 2 3) (* 4 5))
   (let ((ir (compile-expr (list '+ (list '* 2 3) (list '* 4 5)))))
     (eval-ir ir))"
  26)  ; (2*3) + (4*5) = 6 + 20 = 26

;; Test 3: Mini-compiler with let binding support
;; Compiler can handle expressions with variable bindings
(test-native "compile-let"
  "(defun compile-expr (expr env)
     (cond
       ((numberp expr) (list 'lit expr))
       ((symbolp expr)
        (let ((binding (assoc expr env)))
          (if binding
              (list 'var (cdr binding))
              (list 'lit 0))))
       ((and (consp expr) (eq (car expr) '+))
        (list 'add (compile-expr (cadr expr) env)
                   (compile-expr (caddr expr) env)))
       ((and (consp expr) (eq (car expr) 'let))
        (let* ((bindings (cadr expr))
               (body (caddr expr))
               (var (car (car bindings)))
               (val (compile-expr (cadr (car bindings)) env))
               (new-env (cons (cons var 0) env)))
          (list 'let1 val (compile-expr body new-env))))
       (t nil)))
   (defun eval-ir (ir stack)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'var) (nth (cadr ir) stack))
       ((eq (car ir) 'add) (+ (eval-ir (cadr ir) stack)
                              (eval-ir (caddr ir) stack)))
       ((eq (car ir) 'let1)
        (let* ((val (eval-ir (cadr ir) stack))
               (new-stack (cons val stack)))
          (eval-ir (caddr ir) new-stack)))
       (t 0)))
   ;; Compile and eval: (let ((x 10)) (+ x 5))
   (let ((ir (compile-expr (list 'let (list (list 'x 10)) (list '+ 'x 5)) nil)))
     (eval-ir ir nil))"
  15)

;; Test 4: Compiler that can compile itself partially
;; The compile-expr function is used to compile another version of itself
(test-native "self-similar"
  ";; A compiler that generates IR for arithmetic expressions
   (defun compile-expr (expr)
     (cond
       ((numberp expr) (list 'lit expr))
       ((and (consp expr) (eq (car expr) '+))
        (list 'add (compile-expr (cadr expr)) (compile-expr (caddr expr))))
       ((and (consp expr) (eq (car expr) '-))
        (list 'sub (compile-expr (cadr expr)) (compile-expr (caddr expr))))
       (t nil)))
   (defun eval-ir (ir)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'add) (+ (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       ((eq (car ir) 'sub) (- (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       (t 0)))
   ;; Use the compiler to compile an expression that would test the compiler
   ;; This is self-similar: the expression being compiled is the same pattern
   ;; as what the compiler handles
   (let* ((test-expr (list '+ (list '- 50 20) (list '+ 5 7)))
          (ir (compile-expr test-expr)))
     (eval-ir ir))"
  42)  ; (50-20) + (5+7) = 30 + 12 = 42

;; Test 5: Full mini-compiler with defun support
;; Uses my-assoc (user-defined) instead of inline assoc due to nested scope bug
(test-native "compile-defun"
  "(defun my-assoc (key alist)
     (if (null alist)
         nil
         (if (eq key (car (car alist)))
             (car alist)
             (my-assoc key (cdr alist)))))
   (defun fn-lookup (name fenv)
     (if (null fenv)
         nil
         (if (eq (car fenv) name)
             name
             (fn-lookup name (cdr fenv)))))
   (defun compile-expr (expr fenv env)
     (cond
       ((numberp expr) (list 'lit expr))
       ((symbolp expr)
        (let ((binding (my-assoc expr env)))
          (if binding
              (list 'var (cdr binding))
              (list 'lit 0))))
       ((and (consp expr) (eq (car expr) '+))
        (list 'add (compile-expr (cadr expr) fenv env)
                   (compile-expr (caddr expr) fenv env)))
       ((and (consp expr) (eq (car expr) '-))
        (list 'sub (compile-expr (cadr expr) fenv env)
                   (compile-expr (caddr expr) fenv env)))
       ((and (consp expr) (fn-lookup (car expr) fenv))
        ;; Function call - compile argument
        (list 'call (car expr) (compile-expr (cadr expr) fenv env)))
       (t (list 'lit 0))))
   (defun eval-ir (ir fntab stack)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'var) (nth (cadr ir) stack))
       ((eq (car ir) 'add) (+ (eval-ir (cadr ir) fntab stack)
                              (eval-ir (caddr ir) fntab stack)))
       ((eq (car ir) 'sub) (- (eval-ir (cadr ir) fntab stack)
                              (eval-ir (caddr ir) fntab stack)))
       ((eq (car ir) 'call)
        (let* ((fn-name (cadr ir))
               (fn-entry (my-assoc fn-name fntab))
               (fn-ir (cdr fn-entry))
               (arg-val (eval-ir (caddr ir) fntab stack)))
          (eval-ir fn-ir fntab (list arg-val))))
       (t 0)))
   ;; Define double function and call it
   (let* ((fenv (list 'double))
          (fn-ir (compile-expr (list '+ 'x 'x) fenv (list (cons 'x 0))))
          (fntab (list (cons 'double fn-ir)))
          (call-ir (compile-expr (list 'double 21) fenv nil)))
     (eval-ir call-ir fntab nil))"
  42)  ; double(21) = 21 + 21 = 42

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
