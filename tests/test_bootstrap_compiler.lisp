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

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
