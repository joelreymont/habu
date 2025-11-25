#!/usr/bin/env sbcl --script
;;; Tests for the Habu profiler (timing and output verification)

(load "run-habu.lisp")

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (progn
          (format t "[PASS] ~A~%" name)
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] ~A - expected ~A, got ~A~%" name expected result)
          (incf *tests-failed*)))))

(defun run-test-with-output (name forms expected output-pattern)
  "Run test and check both result and that output contains pattern."
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (let ((result-ok (and result (= result expected)))
          (output-ok (search output-pattern output)))
      (if (and result-ok output-ok)
          (progn
            (format t "[PASS] ~A~%" name)
            (incf *tests-passed*))
          (progn
            (format t "[FAIL] ~A~%" name)
            (unless result-ok
              (format t "  Result: expected ~A, got ~A~%" expected result))
            (unless output-ok
              (format t "  Output missing pattern: ~A~%" output-pattern)
              (format t "  Actual output: ~A~%" output))
            (incf *tests-failed*))))))

(format t "~%=== Habu Profiler Tests ===~%~%")

;; Reset profiler state before tests
(setq habu-sbcl-codegen::*profiled-functions* nil)

;; Test 1: get-time-ns returns a positive number
(run-test "get-time-ns-returns-positive"
  '((if (> (get-time-ns) 0) #x1 #x0))
  #x1)

;; Test 2: get-time-ns increases over time
(run-test "get-time-ns-increases"
  '((let ((t1 (get-time-ns)))
      ;; Do some work
      (let ((sum 0))
        (dotimes (i 100)
          (setq sum (+ sum i))))
      (let ((t2 (get-time-ns)))
        (if (> t2 t1) #x1 #x0))))
  #x1)

;; Test 3: Profiled function compiles and runs correctly
(setq habu-sbcl-codegen::*profiled-functions* nil)
(habu-sbcl-codegen::profile-function 'add-one)
(run-test "profiled-function-correct-result"
          '((defun add-one (x) (+ x 1))
            (add-one #x5))
          #x6)
(setq habu-sbcl-codegen::*profiled-functions* nil)

;; Test 4: Profiled function prints profile output
(habu-sbcl-codegen::profile-function 'mul-nums)
(run-test-with-output "profiled-function-output"
                      '((defun mul-nums (x y)
                          (* x y))
                        (mul-nums #x5 #x6))
                      #x1E
                      "PROFILE:")
(setq habu-sbcl-codegen::*profiled-functions* nil)

;; Test 5: Profiled function shows function name in output
(habu-sbcl-codegen::profile-function 'my-func)
(run-test-with-output "profiled-function-shows-name"
                      '((defun my-func (a)
                          (+ a #x1))
                        (my-func #x10))
                      #x11
                      "MY-FUNC")
(setq habu-sbcl-codegen::*profiled-functions* nil)

;; Test 6: Non-profiled function has no profile output
(defun test-no-profile-output ()
  "Test that non-profiled functions don't have PROFILE: in output."
  (multiple-value-bind (result output)
      (habu-sbcl:compile-and-run-forms
        '((defun simple-fn (x) (+ x #x1))
          (simple-fn #x5)))
    (if (and result
             (= result #x6)
             (not (search "PROFILE:" output)))
        (progn
          (format t "[PASS] non-profiled-no-output~%")
          (incf *tests-passed*))
        (progn
          (format t "[FAIL] non-profiled-no-output~%")
          (when (search "PROFILE:" output)
            (format t "  Unexpected PROFILE: in output~%"))
          (incf *tests-failed*)))))
(test-no-profile-output)

;; Test 7: Profiled recursive function prints on each call
(habu-sbcl-codegen::profile-function 'fact)
(run-test-with-output "profiled-recursive-function"
                      '((defun fact (n)
                          (if (= n #x0)
                              #x1
                              (* n (fact (- n #x1)))))
                        (fact #x4))
                      #x18  ; 4! = 24
                      "FACT")
(setq habu-sbcl-codegen::*profiled-functions* nil)

;; Test 8: Multiple profiled functions all print output
(habu-sbcl-codegen::profile-function 'helper)
(habu-sbcl-codegen::profile-function 'caller)
(defun test-multiple-profiled ()
  (multiple-value-bind (result output)
      (habu-sbcl:compile-and-run-forms
        '((defun helper (x) (* x #x2))
          (defun caller (x) (+ (helper x) #x1))
          (caller #x3)))
    (let ((result-ok (and result (= result #x7)))
          (helper-ok (search "HELPER" output))
          (caller-ok (search "CALLER" output)))
      (if (and result-ok helper-ok caller-ok)
          (progn
            (format t "[PASS] multiple-profiled-functions~%")
            (incf *tests-passed*))
          (progn
            (format t "[FAIL] multiple-profiled-functions~%")
            (unless result-ok
              (format t "  Result: expected 7, got ~A~%" result))
            (unless helper-ok
              (format t "  Missing HELPER in output~%"))
            (unless caller-ok
              (format t "  Missing CALLER in output~%"))
            (incf *tests-failed*))))))
(test-multiple-profiled)
(setq habu-sbcl-codegen::*profiled-functions* nil)

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
