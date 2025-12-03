;;;; Package Tests
;;;; Tests package support in the Habu compiler.
;;;; Loaded via ASDF as part of habu/tests system.
;;;;
;;;; NOTE: Cross-package tests only work in native Habu mode.
;;;; In SBCL bootstrap mode, the reader doesn't track packages from
;;;; defpackage/in-package forms, so symbol matching fails.

(in-package :habu-test)

(defun run-package-tests ()
  "Run all package tests."
  (define-test-suite "Package Tests"
    ;; Simple defpackage and in-package (works in both modes)
    (test-full "pkg-simple"
      "(defpackage :test-pkg)
       (in-package :test-pkg)
       (defun foo (x) (+ x 1))
       (sys-exit (foo 41))"
      42)

    ;; Cross-package tests - skip in SBCL bootstrap mode
    (skip-test "pkg-cross-call" "cross-package calls require native reader")
    (skip-test "pkg-multiple" "cross-package calls require native reader")
    (skip-test "pkg-qualified" "qualified symbols require native reader")
    (skip-test "pkg-with-kw" "qualified symbols require native reader")))

;; Auto-run tests when file is loaded
(run-package-tests)
