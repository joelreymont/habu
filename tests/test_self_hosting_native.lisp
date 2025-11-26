;;; Self-hosting tests for the native compiler
;;; Tests that compile compiler-like code patterns to native executables
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Self-Hosting Native Compiler Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-self-host (name source expected)
  (handler-case
    (let* ((forms (nc-read-all source))
           (bytes (nc-compile-program forms nil))
           (output-path (format nil "/tmp/selfhost_~A" name)))
      (habu-macho:deliver-native-with-heap output-path bytes)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
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

;;; Test 1: Tree traversal - count nodes (like counting IR nodes)
;;; Note: Use let* to sequence recursive calls (known bug with nested calls in + args)
(test-self-host "count-nodes"
  "(defun count-nodes (tree)
     (if (consp tree)
         (let* ((a (count-nodes (car tree)))
                (b (count-nodes (cdr tree))))
           (+ 1 a b))
         (if (null tree) 0 1)))
   (count-nodes (cons (cons 1 2) (cons 3 nil)))"
  6)  ; 3 cons cells + 3 atoms (1, 2, 3) = 6

;;; Test 2: Pattern matching - like IR tag matching
(test-self-host "match-tag"
  "(defun get-tag (ir)
     (if (consp ir) (car ir) nil))
   (defun is-add (ir)
     (if (eq (get-tag ir) (quote add)) 1 0))
   (is-add (cons (quote add) (cons 1 (cons 2 nil))))"
  1)

;;; Test 3: Environment lookup - like variable resolution
(test-self-host "env-lookup"
  "(defun env-lookup (name env)
     (if (null env)
         0
         (if (eq name (car (car env)))
             (cdr (car env))
             (env-lookup name (cdr env)))))
   (let ((env (cons (cons (quote x) 10)
                    (cons (cons (quote y) 32) nil))))
     (+ (env-lookup (quote x) env) (env-lookup (quote y) env)))"
  42)

;;; Test 4: List transformation - like IR transformation
(test-self-host "double-list"
  "(defun double-each (lst)
     (if (null lst)
         nil
         (cons (* 2 (car lst)) (double-each (cdr lst)))))
   (defun sum (lst acc)
     (if (null lst) acc (sum (cdr lst) (+ acc (car lst)))))
   (sum (double-each (cons 1 (cons 2 (cons 3 nil)))) 0)"
  12)  ; 2 + 4 + 6 = 12

;;; Test 5: Nested recursion - like nested IR processing
(test-self-host "nested-map"
  "(defun map-add1 (lst)
     (if (null lst)
         nil
         (cons (+ 1 (car lst)) (map-add1 (cdr lst)))))
   (defun reduce-sum (lst acc)
     (if (null lst) acc (reduce-sum (cdr lst) (+ acc (car lst)))))
   (reduce-sum (map-add1 (map-add1 (cons 1 (cons 2 (cons 3 nil))))) 0)"
  12)  ; (3 + 4 + 5) = 12

;;; Test 6: Association list operations - like symbol table lookup
(test-self-host "alist-ops"
  "(defun alist-get (key alist)
     (if (null alist)
         0
         (if (eq key (car (car alist)))
             (cdr (car alist))
             (alist-get key (cdr alist)))))
   (defun alist-set (key val alist)
     (cons (cons key val) alist))
   (let* ((a1 (alist-set (quote x) 10 nil))
          (a2 (alist-set (quote y) 32 a1)))
     (+ (alist-get (quote x) a2) (alist-get (quote y) a2)))"
  42)

;;; Test 7: Compiler-style expression evaluation (mini interpreter)
(test-self-host "mini-eval"
  "(defun mini-eval (expr)
     (if (consp expr)
         (let ((op (car expr)))
           (if (eq op (quote add))
               (+ (mini-eval (car (cdr expr)))
                  (mini-eval (car (cdr (cdr expr)))))
               (if (eq op (quote mul))
                   (* (mini-eval (car (cdr expr)))
                      (mini-eval (car (cdr (cdr expr)))))
                   0)))
         expr))
   (mini-eval (cons (quote add)
                    (cons 10
                          (cons (cons (quote mul)
                                      (cons 4 (cons 8 nil)))
                                nil))))"
  42)  ; 10 + (4 * 8) = 42

;;; Test 8: Free variable collection (like closure analysis)
(test-self-host "collect-vars"
  "(defun in-list (x lst)
     (if (null lst)
         nil
         (if (eq x (car lst))
             t
             (in-list x (cdr lst)))))
   (defun collect-vars (expr bound)
     (if (consp expr)
         (append (collect-vars (car expr) bound)
                 (collect-vars (cdr expr) bound))
         (if (null expr)
             nil
             (if (in-list expr bound)
                 nil
                 (cons expr nil)))))
   (length (collect-vars (cons (quote x) (cons (quote y) (cons (quote z) nil)))
                        (cons (quote x) nil)))"
  2)  ; y and z are free

;;; Test 9: Code generation helper - offset calculation
(test-self-host "calc-offsets"
  "(defun calc-offset (vars name idx)
     (if (null vars)
         0
         (if (eq name (car vars))
             idx
             (calc-offset (cdr vars) name (+ idx 1)))))
   (calc-offset (cons (quote a) (cons (quote b) (cons (quote c) nil)))
                (quote c) 0)"
  2)

;;; Test 10: Recursive descent - like parsing
(test-self-host "parse-sum"
  "(defun parse-nums (lst)
     (if (null lst)
         0
         (+ (car lst) (parse-nums (cdr lst)))))
   (parse-nums (cons 10 (cons 20 (cons 12 nil))))"
  42)

(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *pass-count* *fail-count*)
(sb-ext:exit :code *fail-count*)
