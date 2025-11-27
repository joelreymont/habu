;; Test native mini self-compiler: compiles expression evaluators to native executables
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Test native mini self-compiler ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/nsc_~A" name)))
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

;; Test 1: Native evaluator for tagged add expressions
(test-native "eval-add"
  "(defun eval-expr (expr)
     (let ((op (car expr))
           (a (cadr expr))
           (b (caddr expr)))
       (if (eq op 'add)
           (+ a b)
           0)))
   (eval-expr (list 'add 10 20))"
  30)

;; Test 2: Native recursive evaluator supporting add/sub/mul
(test-native "eval-recursive"
  "(defun eval-expr (expr)
     (cond
       ((not (consp expr)) expr)
       ((eq (car expr) 'lit) (cadr expr))
       ((eq (car expr) 'add)
        (+ (eval-expr (cadr expr))
           (eval-expr (caddr expr))))
       ((eq (car expr) 'sub)
        (- (eval-expr (cadr expr))
           (eval-expr (caddr expr))))
       ((eq (car expr) 'mul)
        (* (eval-expr (cadr expr))
           (eval-expr (caddr expr))))
       (t 0)))
   (eval-expr (list 'add
                    (list 'mul (list 'lit 3) (list 'lit 4))
                    (list 'sub (list 'lit 10) (list 'lit 5))))"
  17)  ; (3*4) + (10-5) = 12 + 5 = 17

;; Test 3: Native IR generator converting source to IR
(test-native "ir-gen"
  "(defun gen-ir (expr)
     (cond
       ((numberp expr) (list 'lit expr))
       ((and (consp expr) (eq (car expr) '+))
        (list 'add (gen-ir (cadr expr)) (gen-ir (caddr expr))))
       ((and (consp expr) (eq (car expr) '-))
        (list 'sub (gen-ir (cadr expr)) (gen-ir (caddr expr))))
       ((and (consp expr) (eq (car expr) '*))
        (list 'mul (gen-ir (cadr expr)) (gen-ir (caddr expr))))
       (t nil)))
   (defun eval-ir (ir)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'add)
        (+ (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       ((eq (car ir) 'sub)
        (- (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       ((eq (car ir) 'mul)
        (* (eval-ir (cadr ir)) (eval-ir (caddr ir))))
       (t 0)))
   (let ((ir (gen-ir (list '+ (list '* 2 3) 4))))
     (eval-ir ir))"
  10)  ; (2*3) + 4 = 10

;; Test 4: Native environment lookup (core compiler pattern)
(test-native "env-lookup"
  "(defun env-lookup (var-id env)
     (if (null env)
         nil
         (if (= (car (car env)) var-id)
             (cdr (car env))
             (env-lookup var-id (cdr env)))))
   (let ((env (cons (cons 1 100) (cons (cons 2 200) nil))))
     (env-lookup 2 env))"
  200)

;; Test 5: Native IR evaluator with variable bindings (simplified)
(test-native "eval-with-stack"
  "(defun eval-ir (ir stack)
     (cond
       ((eq (car ir) 'lit) (cadr ir))
       ((eq (car ir) 'var)
        (nth (cadr ir) stack))
       ((eq (car ir) 'add)
        (+ (eval-ir (cadr ir) stack)
           (eval-ir (caddr ir) stack)))
       (t 0)))
   (let* ((stack (list 5))
          (ir (list 'add (list 'var 0) (list 'lit 10))))
     (eval-ir ir stack))"
  15)  ; stack[0]=5, 5 + 10 = 15

;; Test 6: Native mapcar for code generation patterns
(test-native "mapcar-codegen"
  "(defun compile-exprs (exprs)
     (mapcar (lambda (e) (list 'lit e)) exprs))
   (defun sum-literals (irs)
     (labels ((iter (lst acc)
                (if (null lst)
                    acc
                    (iter (cdr lst) (+ acc (cadr (car lst)))))))
       (iter irs 0)))
   (sum-literals (compile-exprs (list 1 2 3 4 5)))"
  15)

;; Test 7: Native tree traversal (like IR walking in compiler)
(test-native "tree-walk"
  "(defun count-nodes (tree)
     (if (consp tree)
         (+ 1 (count-nodes (car tree)) (count-nodes (cdr tree)))
         0))
   (count-nodes (cons (cons 1 2) (cons 3 4)))"
  3)

;; Test 8: Native symbol table building (compiler pattern)
(test-native "symbol-table"
  "(defun add-binding (name offset table)
     (cons (cons name offset) table))
   (defun lookup (name table)
     (if (null table)
         nil
         (if (eq (car (car table)) name)
             (cdr (car table))
             (lookup name (cdr table)))))
   (let* ((tbl nil)
          (tbl (add-binding 'x 0 tbl))
          (tbl (add-binding 'y 1 tbl))
          (tbl (add-binding 'z 2 tbl)))
     (lookup 'y tbl))"
  1)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
