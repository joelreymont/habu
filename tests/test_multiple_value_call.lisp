#!/usr/bin/env sbcl --script
;;; Tests for multiple-value-call: calling functions with multiple values

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== multiple-value-call Tests ===~%~%")

;; Test 1: Basic multiple-value-call with single form returning one value
(run-test "mvc-single-value"
          '((defun my-id (x) x)
            (multiple-value-call #'my-id (values #x42)))
          #x42)

;; Test 2: Multiple-value-call with two values
(run-test "mvc-two-values"
          '((defun my-add (a b) (+ a b))
            (multiple-value-call #'my-add (values #x3 #x5)))
          #x8)

;; Test 3: Multiple-value-call with multiple forms
(run-test "mvc-multiple-forms"
          '((defun sum4 (a b c d) (+ a b c d))
            (multiple-value-call #'sum4 (values #x1 #x2) (values #x3 #x4)))
          #xA)  ; 1 + 2 + 3 + 4 = 10

;; Test 4: values-count after values call
(run-test "values-count-basic"
          '((progn
              (values #x1 #x2 #x3)
              (values-count)))
          #x3)

;; Test 5: values-count after single value
(run-test "values-count-single"
          '((progn
              (values #x5)
              (values-count)))
          #x1)

;; Test 6: Multiple-value-call preserves values correctly
(run-test "mvc-values-preserved"
          '((defun check-args (a b c d)
              (+ (* a #x1000) (* b #x100) (* c #x10) d))
            (multiple-value-call #'check-args (values #x1 #x2) (values #x3 #x4)))
          #x1234)

(format t "~%=== All multiple-value-call Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
