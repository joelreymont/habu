;;;; Native Reader Tests
;;;; Tests reader conditionals and other native reader features.
;;;; These tests create Stage 1 binaries to test the native reader.

(in-package :habu-test)

(defun run-reader-tests ()
  "Run all native reader tests."
  (define-test-suite "Reader Conditional Tests"
    ;; #-sbcl should include form (sbcl not present in native)
    (test-full "reader-minus-sbcl"
      "#-sbcl (sys-exit 42)" 42)

    ;; #+sbcl should skip form (sbcl not present in native)
    (test-full "reader-plus-sbcl"
      "#+sbcl (sys-exit 99) (sys-exit 42)" 42)

    ;; #+habu should include form (habu is present in native)
    (test-full "reader-plus-habu"
      "#+habu (sys-exit 42)" 42)

    ;; #-habu should skip form (habu is present in native)
    (test-full "reader-minus-habu"
      "#-habu (sys-exit 99) (sys-exit 42)" 42)

    ;; Multiple conditionals in sequence
    (test-full "reader-multiple"
      "#-sbcl (defun foo () 42) #+sbcl (defun foo () 99) (sys-exit (foo))" 42)

    ;; Nested forms with conditionals
    (test-full "reader-nested"
      "(defun bar () #-sbcl 42 #+sbcl 99) (sys-exit (bar))" 42)

    ;; Conditional with complex form
    (test-full "reader-complex-form"
      "#-sbcl (let ((x 20) (y 22)) (sys-exit (+ x y)))" 42)))

;; Auto-run tests when file is loaded
(run-reader-tests)
