#!/usr/bin/env sbcl --script
;;; Test when/unless guard forms via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: when true executes body
(run-test "when-true"
          '((when (> #x5 #x0) (+ #x1 #x2)))
          #x3)

;; Test 2: when false returns nil
(run-test "when-false"
          '((when (< #x5 #x0) (+ #x1 #x2)))
          #x0)

;; Test 3: when with multiple body forms
(run-test "when-progn"
          '((defun when-multi (x)
              (when (> x #x0)
                (+ x #x1)
                (+ x #x10)))
            (when-multi #x5))
          #x15)

;; Test 4: unless true returns nil
(run-test "unless-true"
          '((unless (> #x5 #x0) (+ #x1 #x2)))
          #x0)

;; Test 5: unless false executes body
(run-test "unless-false"
          '((unless (< #x5 #x0) (+ #x1 #x2)))
          #x3)

;; Test 6: unless with multiple body forms
(run-test "unless-progn"
          '((defun unless-multi (x)
              (unless (< x #x0)
                (+ x #x1)
                (+ x #x10)))
            (unless-multi #x5))
          #x15)

(format t "All when/unless tests passed~%")
(sb-ext:quit :unix-status 0)
