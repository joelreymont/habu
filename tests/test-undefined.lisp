;;;; Tests for undefined function detection
;;;; Verifies that the compiler catches undefined functions at compile-time

(in-package :habu-test)

(defun run-undefined-function-tests ()
  "Test that undefined functions are properly detected and rejected."
  (reset-test-counts)
  (format t "~%=== Undefined Function Detection Tests ===~%")

  ;; Test 1: Single undefined function is caught
  (test-compile-fails "undefined-single"
                      "(sys-exit (undefined-fn 123))"
                      "undefined functions")

  ;; Test 2: Multiple undefined functions are caught
  (test-compile-fails "undefined-multiple"
                      "(sys-exit (+ (foo 1) (bar 2)))"
                      "undefined functions")

  ;; Test 3: Undefined function in nested expression
  (test-compile-fails "undefined-nested"
                      "(sys-exit (if (> 1 0) (unknown-fn) 0))"
                      "undefined functions")

  ;; Test 4: Undefined function in let body
  (test-compile-fails "undefined-in-let"
                      "(sys-exit (let ((x 1)) (missing-fn x)))"
                      "undefined functions")

  ;; Test 5: Valid code still compiles (sanity check)
  (test "valid-code" "42" 42)

  ;; Test 6: Defined functions work
  (test-full "defined-defun"
             "(defun my-add (a b) (+ a b)) (sys-exit (my-add 20 22))"
             42)

  ;; Test 7: char-at primitive works (was previously undefined)
  (test "char-at-works" "(char-at \"hello\" 0)" 104)  ; 'h' = 104

  ;; Test 8: char-at boundary returns 0
  (test "char-at-boundary" "(char-at \"hi\" 99)" 0)

  ;; Test 9: Recursive function calling undefined
  (test-compile-fails "undefined-in-defun"
                      "(defun f (x) (bogus-call x)) (sys-exit (f 1))"
                      "undefined functions")

  ;; Test 10: Lambda with undefined call
  (test-compile-fails "undefined-in-lambda"
                      "(sys-exit (funcall (lambda (x) (nonexistent x)) 1))"
                      "undefined functions")

  (report-results "Undefined Function Detection"))

;; Auto-run tests when file is loaded
(run-undefined-function-tests)
