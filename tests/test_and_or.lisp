#!/usr/bin/env sbcl --script
;;; Test and/or short-circuit operators via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: and with all true returns last value
(run-test "and-all-true"
          '((and #x1 #x2 #x3))
          #x3)

;; Test 2: and with nil short-circuits
(run-test "and-short-circuit"
          '((and #x1 #x0 #x99))
          #x0)

;; Test 3: empty and returns t (untagged = 1)
(run-test "and-empty"
          '((and))
          #x1)

;; Test 4: single arg and
(run-test "and-single"
          '((and #x42))
          #x42)

;; Test 5: or with first true returns it
(run-test "or-first-true"
          '((or #x5 #x10))
          #x5)

;; Test 6: or with first nil tries next
(run-test "or-skip-nil"
          '((or #x0 #x7))
          #x7)

;; Test 7: or all nil returns nil
(run-test "or-all-nil"
          '((or #x0 #x0 #x0))
          #x0)

;; Test 8: empty or returns nil
(run-test "or-empty"
          '((or))
          #x0)

;; Test 9: single arg or
(run-test "or-single"
          '((or #x42))
          #x42)

;; Test 10: not true -> nil
(run-test "not-true"
          '((not #x1))
          #x0)

;; Test 11: not nil -> t (untagged = 1)
(run-test "not-nil"
          '((not #x0))
          #x1)

(format t "All and/or/not tests passed~%")
(sb-ext:quit :unix-status 0)
