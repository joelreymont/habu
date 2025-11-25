#!/usr/bin/env sbcl --script
;;; Tests for symbol-macrolet: local symbol macros

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== symbol-macrolet Tests ===~%~%")

;; Test 1: Basic symbol-macrolet
(run-test "symbol-macrolet-basic"
          '((symbol-macrolet ((x #x5))
              x))
          #x5)

;; Test 2: Multiple symbol macros
(run-test "symbol-macrolet-multiple"
          '((symbol-macrolet ((a #x10)
                              (b #x20))
              (+ a b)))
          #x30)

;; Test 3: Symbol macro with expression
(run-test "symbol-macrolet-expression"
          '((symbol-macrolet ((sum (+ #x3 #x4)))
              sum))
          #x7)

;; Test 4: Symbol macro shadowed by let
(run-test "symbol-macrolet-shadowed-by-let"
          '((symbol-macrolet ((x #x10))
              (let ((x #x5))
                x)))
          #x5)

;; Test 5: Nested symbol-macrolet
(run-test "symbol-macrolet-nested"
          '((symbol-macrolet ((x #x10))
              (symbol-macrolet ((y #x20))
                (+ x y))))
          #x30)

;; Test 6: Symbol macro in function call arg
(run-test "symbol-macrolet-in-call"
          '((defun double (n) (* n #x2))
            (symbol-macrolet ((val #x8))
              (double val)))
          #x10)

;; Test 7: Multiple body forms
(run-test "symbol-macrolet-multiple-body"
          '((symbol-macrolet ((x #x5))
              (+ x #x1)
              (+ x #x2)))
          #x7)

(format t "~%=== All symbol-macrolet Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
