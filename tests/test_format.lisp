#!/usr/bin/env sbcl --script
;;; Tests for format directive handling

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== Format Directive Tests ===~%~%")

;; Test 1: format with ~A returns nil (0)
(run-test "format-returns-nil"
          '((format t "value: ~A" #x5))
          #x0)

;; Test 2: format evaluates ~A arg
(run-test "format-evaluates-A-arg"
          '((let ((x #x7))
              (format t "x=~A" x)
              x))
          #x7)

;; Test 3: format with ~D evaluates arg
(run-test "format-evaluates-D-arg"
          '((let ((n #xA))
              (format t "n=~D" n)
              n))
          #xA)

;; Test 4: format with ~X evaluates arg
(run-test "format-evaluates-X-arg"
          '((let ((h #xFF))
              (format t "hex: ~X" h)
              h))
          #xFF)

;; Test 5: format with ~B evaluates arg
(run-test "format-evaluates-B-arg"
          '((let ((b #x8))
              (format t "binary: ~B" b)
              b))
          #x8)

;; Test 6: format with multiple args
(run-test "format-multiple-args"
          '((let ((a #x1)
                  (b #x2)
                  (c #x3))
              (format t "~A ~D ~X" a b c)
              (+ a b c)))
          #x6)

;; Test 7: format with ~% (newline) doesn't consume args
(run-test "format-percent-no-consume"
          '((let ((x #x5))
              (format t "line1~%line2: ~A" x)
              x))
          #x5)

;; Test 8: format with no args
(run-test "format-no-args"
          '((format t "hello world"))
          #x0)

;; Test 9: format with ~~ (literal tilde) doesn't consume args
(run-test "format-tilde-no-consume"
          '((let ((x #x3))
              (format t "~~value: ~A" x)
              x))
          #x3)

;; Test 10: format returns 0 (nil) even with side effects
(run-test "format-always-returns-nil"
          '((let ((result (format t "test ~A" #x99)))
              result))
          #x0)

(format t "~%=== All Format Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
