#!/usr/bin/env sbcl --script
;;; Test bootstrap compiler full pipeline
;;; Compiles Lisp source to ARM64 bytecode and verifies output

(load "bootstrap/compiler.lisp")
(in-package :habu)

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun run-pipeline-test (name source)
  "Compile source through full pipeline and verify bytecode is generated"
  (handler-case
      (let* ((forms (nc-read-all source))
             (compiled (nc-compile-forms forms))
             (fns (car compiled))
             (mir (cadr compiled))
             (main-code (nc-codegen-main mir nil)))
        (if (and (listp main-code) (> (length main-code) 0))
            (progn
              (format t "[PASS] ~A: ~A bytes~%" name (length main-code))
              (incf *tests-passed*))
            (progn
              (format t "[FAIL] ~A: no bytecode generated~%" name)
              (incf *tests-failed*))))
    (error (e)
      (format t "[FAIL] ~A: ~A~%" name e)
      (incf *tests-failed*))))

(format t "~%=== Bootstrap Pipeline Tests ===~%~%")

;; Test 1: Simple literal
(run-pipeline-test "literal" "42")

;; Test 2: Arithmetic
(run-pipeline-test "arithmetic" "(+ 10 20)")

;; Test 3: Nested arithmetic
(run-pipeline-test "nested-arith" "(+ (* 3 4) (- 10 5))")

;; Test 4: Let binding
(run-pipeline-test "let" "(let ((x 5)) (+ x 3))")

;; Test 5: If expression
(run-pipeline-test "if" "(if (< 3 5) 10 20)")

;; Test 6: Defun and call
(run-pipeline-test "defun" "(defun double (x) (* x 2)) (double 7)")

;; Test 7: Recursive function
(run-pipeline-test "recursive" "(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)")

;; Test 8: List operations
(run-pipeline-test "list-ops" "(car (cons 1 2))")

;; Test 9: Cond
(run-pipeline-test "cond" "(cond ((= 1 2) 100) (t 200))")

;; Test 10: Progn
(run-pipeline-test "progn" "(progn 1 2 3)")

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
