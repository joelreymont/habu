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

    ;; Cross-package tests - these require native reader to track packages
    ;; SBCL bootstrap mode doesn't track defpackage/in-package forms
    ;; These tests verify that native intern() preserves package prefixes
    (skip-test "pkg-qualified-call" "cross-package calls require native reader")
    (skip-test "pkg-cross-package" "cross-package calls require native reader")
    (skip-test "pkg-nested-qualified" "cross-package calls require native reader"))

  ;; NOTE: The actual cross-package tests are in native-package-tests below
  ;; They will be enabled when Stage 1 can run tests itself
  )

(defun native-package-test-source ()
  "Return source code for native package tests. These test the fix for
   package prefix preservation in intern() - arm64:foo stays ARM64:FOO
   instead of becoming HABU:FOO."
  '(;; Test 1: Package-qualified function call
    ("pkg-qualified-call"
     "(defpackage :mypkg)
      (in-package :mypkg)
      (defun add-ten (x) (+ x 10))
      (in-package :cl-user)
      (sys-exit (mypkg:add-ten 32))"
     42)
    ;; Test 2: Multiple packages with cross-calls
    ("pkg-cross-package"
     "(defpackage :pkg-a)
      (defpackage :pkg-b)
      (in-package :pkg-a)
      (defun double (x) (* x 2))
      (in-package :pkg-b)
      (defun triple (x) (* x 3))
      (in-package :cl-user)
      (sys-exit (+ (pkg-a:double 21) (pkg-b:triple 0)))"
     42)
    ;; Test 3: Nested package-qualified calls
    ("pkg-nested-qualified"
     "(defpackage :math-pkg)
      (in-package :math-pkg)
      (defun inc (x) (+ x 1))
      (in-package :cl-user)
      (sys-exit (math-pkg:inc (math-pkg:inc 40)))"
     42)))

;; Auto-run tests when file is loaded
(run-package-tests)
