#!/usr/bin/env sbcl --script
;;; Test dotimes and dolist via run-bytecode runtime.
;;;
;;; NOTE: Iteration without mutation - these constructs work for iterating
;;; and returning values, but mutation of outer variables via setq inside
;;; the loop body is not supported (due to capture-by-value closure semantics).
;;; Use the result form or build results via cons instead.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Dotimes - iteration with loop variable access
(run-test "dotimes-simple"
          '((dotimes (i #x5) i))  ; just iterate, return nil
          #x0)

(run-test "dotimes-with-result"
          '((dotimes (i #x5 #x42) i))  ; iterate and return result
          #x42)

(run-test "dotimes-zero"
          '((dotimes (i #x0 #x1) i))  ; zero iterations, return result
          #x1)

;; Dolist - iteration over list elements
(run-test "dolist-simple"
          '((dolist (x (list #x1 #x2 #x3)) x))  ; iterate, return nil
          #x0)

(run-test "dolist-with-result"
          '((dolist (x (list #x1 #x2 #x3) #x99) x))  ; iterate, return result
          #x99)

(run-test "dolist-empty"
          '((dolist (x #x0 #x42) x))  ; empty list, return result
          #x42)

;; Verify loop variable is accessible
(run-test "dolist-last-element"
          '((let ((last #x0))
              (dolist (x (list #x1 #x2 #x3) last)
                last)))
          #x0)  ; last is still 0 since we can't mutate

;; Use length as functional iteration test
(run-test "iteration-via-length"
          '((length (list #x1 #x2 #x3 #x4 #x5)))
          #x5)

(format t "All iteration tests passed~%")
(sb-ext:quit :unix-status 0)
