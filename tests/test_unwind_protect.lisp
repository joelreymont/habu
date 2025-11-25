#!/usr/bin/env sbcl --script
;;; Test unwind-protect cleanup execution via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: unwind-protect returns protected form value
(run-test "unwind-protect-value"
          '((unwind-protect #x5 #x99))
          #x5)

;; Test 2: Cleanup forms are evaluated (uses progn since let takes one body form)
(run-test "unwind-protect-cleanup"
          '((let ((x #x1))
              (progn
                (unwind-protect
                    (setq x #x2)
                  (setq x (+ x #x10)))
                x)))
          #x12)

;; Test 3: Multiple cleanup forms
(run-test "unwind-protect-multi-cleanup"
          '((let ((x #x0))
              (progn
                (unwind-protect
                    (setq x #x1)
                  (setq x (+ x #x10))
                  (setq x (+ x #x100)))
                x)))
          #x111)

;; Test 4: Nested unwind-protect
(run-test "unwind-protect-nested"
          '((let ((x #x0))
              (progn
                (unwind-protect
                    (unwind-protect
                        (setq x #x1)
                      (setq x (+ x #x10)))
                  (setq x (+ x #x100)))
                x)))
          #x111)

(format t "All unwind-protect tests passed~%")
(sb-ext:quit :unix-status 0)
