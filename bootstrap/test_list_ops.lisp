;;;; Test suite for list operations
;;;; Tests cons, car, cdr, list in both REPL and compiler

(load "test-harness.lisp")
(load "compiler.lisp")

(in-package :habu-compiler)

(test-group "List Operations (Compiler - Placeholders)"
  ;; These tests verify that list operations are recognized
  ;; but properly report they need runtime integration

  (test-case cons-needs-runtime
    (handler-case
        (progn
          (compile-expression '(cons 1 2))
          (error "Should have raised error"))
      (error (e)
        (assert (search "runtime heap integration" (format nil "~A" e))))))

  (test-case car-needs-runtime
    (handler-case
        (progn
          (compile-expression '(car (cons 1 2)))
          (error "Should have raised error"))
      (error (e)
        (assert (search "runtime heap integration" (format nil "~A" e))))))

  (test-case cdr-needs-runtime
    (handler-case
        (progn
          (compile-expression '(cdr (cons 1 2)))
          (error "Should have raised error"))
      (error (e)
        (assert (search "runtime heap integration" (format nil "~A" e))))))

  (test-case list-needs-runtime
    (handler-case
        (progn
          (compile-expression '(list 1 2 3))
          (error "Should have raised error"))
      (error (e)
        (assert (search "runtime heap integration" (format nil "~A" e)))))))

(report-test-stats)

(format t "~%Note: List operations work in the REPL (interpreted mode).~%")
(format t "Compiler integration with runtime heap is the next step.~%")
