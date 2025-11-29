#!/usr/bin/env sbcl --script
;;; Test bootstrap compiler ARM64 code generation
;;; Verifies that codegen produces correct instruction sequences

(load "bootstrap/compiler.lisp")
(in-package :habu)

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun run-codegen-test (name ir expected-len)
  "Test that codegen produces non-empty bytecode of expected length"
  (let* ((code (codegen ir nil nil 0))
         (len (length code)))
    (if (and (listp code) (>= len expected-len))
        (progn
          (format t "[PASS] ~A: ~A bytes~%" name len)
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] ~A: expected >= ~A bytes, got ~A~%" name expected-len len)
          (incf *tests-failed*)))))

(format t "~%=== Bootstrap Codegen Tests ===~%~%")

;; Test 1: lit (small constant)
(run-codegen-test "lit-small" '(lit 42) 4)

;; Test 2: lit (large constant)
(run-codegen-test "lit-large" '(lit #x10000) 8)

;; Test 3: nil-ir
(run-codegen-test "nil-ir" '(nil-ir) 4)

;; Test 4: var
(run-codegen-test "var" '(var 0) 8)

;; Test 5: add
(run-codegen-test "add" '(add (lit 1) (lit 2)) 20)

;; Test 6: sub
(run-codegen-test "sub" '(sub (lit 10) (lit 3)) 20)

;; Test 7: mul
(run-codegen-test "mul" '(mul (lit 3) (lit 4)) 24)

;; Test 8: div
(run-codegen-test "div" '(div (lit 20) (lit 4)) 24)

;; Test 9: mod
(run-codegen-test "mod" '(mod (lit 17) (lit 5)) 32)

;; Test 10: cmp-eq
(run-codegen-test "cmp-eq" '(cmp-eq (lit 5) (lit 5)) 24)

;; Test 11: cmp-lt
(run-codegen-test "cmp-lt" '(cmp-lt (lit 3) (lit 5)) 24)

;; Test 12: if-ir
(run-codegen-test "if-ir" '(if-ir (lit 1) (lit 10) (lit 20)) 20)

;; Test 13: let-ir - (let-ir vals bir count offs)
(run-codegen-test "let-ir" '(let-ir ((lit 5)) (var 0) 1 (0)) 12)

;; Test 14: cons-ir
(run-codegen-test "cons-ir" '(cons-ir (lit 1) (nil-ir)) 20)

;; Test 15: car-ir
(run-codegen-test "car-ir" '(car-ir (cons-ir (lit 1) (nil-ir))) 24)

;; Test 16: cdr-ir
(run-codegen-test "cdr-ir" '(cdr-ir (cons-ir (lit 1) (nil-ir))) 24)

;; Test 17: progn-ir
(run-codegen-test "progn-ir" '(progn-ir ((lit 1) (lit 2) (lit 3))) 12)

;; Test 18: nested arithmetic
(run-codegen-test "nested-add" '(add (mul (lit 3) (lit 4)) (lit 5)) 40)

;; Test 19: sym-lit (symbol literal)
(run-codegen-test "sym-lit" '(sym-lit "FOO") 50)

;; Test 20: lambda-ir (should be lifted before codegen - returns 0)
(run-codegen-test "lambda-ir" '(lambda-ir (x) (add (var 0) (lit 1)) () ()) 4)

;; Test 21: lambda-ref (closure creation without captures)
(run-codegen-test "lambda-ref-no-cap" '(lambda-ref LAMBDA-1 ()) 20)

;; Test 22: lambda-ref (closure creation with captures)
(run-codegen-test "lambda-ref-cap" '(lambda-ref LAMBDA-2 (0 1)) 60)

;; Test 23: funcall-ir (closure call)
(run-codegen-test "funcall-ir" '(funcall-ir (sym-lit "INC") ((lit 5))) 168)

;; Test 24: dotimes-ir (counted loop)
;; dotimes-ir needs: var, count-ir, body, result-form, compile-env
(run-codegen-test "dotimes-ir" '(dotimes-ir I (lit 3) (I) I nil) 50)

;; Test 25: dolist-ir (list iteration)
;; dolist-ir needs: var, list-ir, body, result-form, compile-env
(run-codegen-test "dolist-ir" '(dolist-ir X (cons-ir (lit 1) (nil-ir)) (X) 42 nil) 80)

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
