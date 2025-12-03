;;;; Habu Test Harness
;;;; Provides utilities for testing the Habu compiler.
;;;; Load via ASDF: (asdf:load-system :habu/tests)

(defpackage :habu-test
  (:use :cl)
  (:export #:*pass-count* #:*fail-count* #:*skip-count*
           #:reset-test-counts
           #:test-compile #:test-compile-full #:skip-test
           #:report-results #:define-test-suite
           #:test #:test-full
           #:run-tests-in-file))

(in-package :habu-test)

;;; ============================================================
;;; Test Counters
;;; ============================================================

(defvar *pass-count* 0)
(defvar *fail-count* 0)
(defvar *skip-count* 0)
(defvar *test-verbose* t)

;; Track totals across all test suites
(defvar *total-pass* 0)
(defvar *total-fail* 0)
(defvar *total-skip* 0)

(defun reset-test-counts ()
  "Reset test counters to zero (for current suite)."
  (setf *pass-count* 0
        *fail-count* 0
        *skip-count* 0))

(defun reset-all-counts ()
  "Reset all counters including totals."
  (setf *pass-count* 0
        *fail-count* 0
        *skip-count* 0
        *total-pass* 0
        *total-fail* 0
        *total-skip* 0))

(defun accumulate-counts ()
  "Add current suite counts to totals."
  (incf *total-pass* *pass-count*)
  (incf *total-fail* *fail-count*)
  (incf *total-skip* *skip-count*))

;;; ============================================================
;;; Core Test Functions
;;; ============================================================

(defun test-compile (name source expected)
  "Compile SOURCE (wrapping in sys-exit) and verify exit code equals EXPECTED."
  (handler-case
    (let* ((output-path (format nil "/tmp/habu_test_~A" name))
           (full-source (format nil "(sys-exit (progn ~A))" source)))
      (habu:deliver full-source output-path)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (prog1
            (if (= result expected)
                (progn
                  (when *test-verbose* (format t "[PASS] ~A = ~A~%" name result))
                  (incf *pass-count*)
                  t)
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                  (incf *fail-count*)
                  nil))
          (ignore-errors (delete-file output-path))
          (ignore-errors (delete-file (format nil "~A.map" output-path))))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*)
      nil)))

(defun test-compile-full (name source expected)
  "Compile SOURCE (which should include sys-exit) and verify exit code equals EXPECTED."
  (handler-case
    (let ((output-path (format nil "/tmp/habu_test_~A" name)))
      (habu:deliver source output-path)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (prog1
            (if (= result expected)
                (progn
                  (when *test-verbose* (format t "[PASS] ~A = ~A~%" name result))
                  (incf *pass-count*)
                  t)
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                  (incf *fail-count*)
                  nil))
          (ignore-errors (delete-file output-path))
          (ignore-errors (delete-file (format nil "~A.map" output-path))))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*)
      nil)))

(defun skip-test (name reason)
  "Mark a test as skipped with REASON."
  (format t "[SKIP] ~A: ~A~%" name reason)
  (incf *skip-count*))

;;; ============================================================
;;; Reporting
;;; ============================================================

(defun report-results (&optional (suite-name "Test Suite"))
  "Print test results summary. Returns T if all tests passed."
  (format t "~%~A Results: ~A passed, ~A failed, ~A skipped~%"
          suite-name *pass-count* *fail-count* *skip-count*)
  (zerop *fail-count*))

;;; ============================================================
;;; Convenience Macros
;;; ============================================================

(defmacro define-test-suite (name &body tests)
  "Define a test suite with automatic setup and reporting."
  `(progn
     (format t "~%=== ~A ===~%~%" ,name)
     (reset-test-counts)
     ,@tests
     (report-results ,name)
     (accumulate-counts)))

(defmacro test (name source expected)
  "Shorthand for test-compile."
  `(test-compile ,name ,source ,expected))

(defmacro test-full (name source expected)
  "Shorthand for test-compile-full."
  `(test-compile-full ,name ,source ,expected))

;;; ============================================================
;;; Test File Runner
;;; ============================================================

(defun run-tests-in-file (path)
  "Load and run tests from a file, returning T if all passed."
  (format t "~%Running tests from: ~A~%" path)
  (reset-test-counts)
  (load path)
  (report-results (pathname-name path)))

;;; ============================================================
;;; ASDF Test Entry Point
;;; ============================================================

(defvar *all-test-results* nil)

(defun run-all-tests ()
  "Run all test suites and report overall results. Returns T if all passed."
  (format t "~%========================================~%")
  (format t "       Habu Compiler Test Suite~%")
  (format t "========================================~%")
  ;; Tests are run when their files are loaded during ASDF load
  ;; This function just reports the final results using totals
  (format t "~%========================================~%")
  (format t "TOTAL: ~A passed, ~A failed, ~A skipped~%"
          *total-pass* *total-fail* *total-skip*)
  (format t "========================================~%")
  (if (zerop *total-fail*)
      (progn (format t "All tests PASSED!~%") t)
      (progn (format t "Some tests FAILED!~%") nil)))
