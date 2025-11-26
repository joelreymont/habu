#!/usr/bin/env sbcl --script
;;; Test bootstrap compiler IR evaluation
;;; Tests the HABU package bootstrap compiler using the IR evaluator path

(load "bootstrap/compiler.lisp")

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun run-nc-test (name source expected)
  "Test bootstrap compiler by evaluating source via IR evaluator"
  (let* ((forms (habu:nc-read-all source))
         (result (habu:nc-eval-forms forms)))
    (if (= result expected)
        (progn
          (format t "[PASS] ~A: ~A = ~A~%" name source expected)
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] ~A: ~A expected ~A got ~A~%" name source expected result)
          (incf *tests-failed*)))))

(format t "~%=== Native Compiler Tests ===~%~%")

;; Test 1: Simple addition
(run-nc-test "add" "(+ 10 7)" 17)

;; Test 2: Simple multiplication
(run-nc-test "mul" "(* 3 4)" 12)

;; Test 3: Nested arithmetic
(run-nc-test "nested" "(+ (* 3 4) 5)" 17)

;; Test 4: Subtraction
(run-nc-test "sub" "(- 20 8)" 12)

;; Test 5: Complex expression
(run-nc-test "complex" "(+ (- 100 50) (* 2 10))" 70)

;; Test 6: Let binding
(run-nc-test "let" "(let ((x 5)) (+ x 3))" 8)

;; Test 7: Let with multiple bindings
(run-nc-test "let-multi" "(let ((a 3) (b 4)) (* a b))" 12)

;; Test 8: Comparison equals
(run-nc-test "cmp-eq" "(if (= 5 5) 1 0)" 1)

;; Test 9: Comparison less than
(run-nc-test "cmp-lt" "(if (< 3 5) 10 20)" 10)

;; Test 10: Nested let
(run-nc-test "let-nested" "(let ((x 2)) (let ((y 3)) (* x y)))" 6)

;; Test 11: User-defined function
(run-nc-test "defun-simple" "(defun double (x) (* x 2)) (double 7)" 14)

;; Test 12: Recursive function (factorial)
(run-nc-test "defun-recursive" "(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)" 120)

;; Test 13: Multiple functions
(run-nc-test "defun-multi" "(defun add1 (x) (+ x 1)) (defun add2 (x) (add1 (add1 x))) (add2 10)" 12)

;; Test 14: Greater than comparison
(run-nc-test "cmp-gt" "(if (> 10 5) 1 0)" 1)

;; Test 15: Less than or equal
(run-nc-test "cmp-le" "(if (<= 5 5) 1 0)" 1)

;; Test 16: Mutual recursion (forward references)
(run-nc-test "mutual-rec" "(defun odd? (n) (if (= n 0) 0 (even? (- n 1)))) (defun even? (n) (if (= n 0) 1 (odd? (- n 1)))) (even? 10)" 1)

;; Test 17: Fibonacci
(run-nc-test "fibonacci" "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)" 55)

;; Test 18: Higher-order function (funcall)
(run-nc-test "higher-order" "(defun apply-twice (f x) (f (f x))) (defun inc (n) (+ n 1)) (apply-twice inc 5)" 7)

;; Test 19: Explicit funcall
(run-nc-test "funcall" "(defun call-with-3 (f) (funcall f 3)) (defun double (x) (* x 2)) (call-with-3 double)" 6)

;; Test 20: #'function syntax
(run-nc-test "function-syntax" "(defun apply-it (f x) (funcall f x)) (defun triple (n) (* n 3)) (apply-it #'triple 4)" 12)

;; Test 21: progn
(run-nc-test "progn" "(progn 1 2 3)" 3)

;; Test 22: progn with side effects
(run-nc-test "progn-let" "(let ((x 0)) (progn (+ x 1) (+ x 2) (+ x 3)))" 3)

;; Test 23: and - all true
(run-nc-test "and-true" "(and 1 2 3)" 3)

;; Test 24: and - with false
(run-nc-test "and-false" "(and 1 0 3)" 0)

;; Test 25: or - first true
(run-nc-test "or-first" "(or 1 0 0)" 1)

;; Test 26: or - all false
(run-nc-test "or-false" "(or 0 0 0)" 0)

;; Test 27: or - last true (returns truthy value, not 1)
(run-nc-test "or-last" "(or 0 0 5)" 5)

;; Test 28: not
(run-nc-test "not-true" "(not 0)" 1)

;; Test 29: not false
(run-nc-test "not-false" "(not 1)" 0)

;; Test 30: cond with first match
(run-nc-test "cond-first" "(cond ((= 1 1) 100) ((= 2 2) 200) (t 300))" 100)

;; Test 31: cond with second match
(run-nc-test "cond-second" "(cond ((= 1 2) 100) ((= 2 2) 200) (t 300))" 200)

;; Test 32: cond with t clause
(run-nc-test "cond-t" "(cond ((= 1 2) 100) ((= 2 3) 200) (t 300))" 300)

;; Test 33: when - true
(run-nc-test "when-true" "(when (= 1 1) 42)" 42)

;; Test 34: when - false
(run-nc-test "when-false" "(when (= 1 2) 42)" 0)

;; Test 35: unless - true (should not execute)
(run-nc-test "unless-true" "(unless (= 1 1) 42)" 0)

;; Test 36: unless - false (should execute)
(run-nc-test "unless-false" "(unless (= 1 2) 42)" 42)

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
