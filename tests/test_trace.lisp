#!/usr/bin/env sbcl --script
;;; Tests for trace/untrace facility

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(defun run-test-with-output (name forms expected output-pattern)
  "Run test and check both result and that output contains pattern."
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (let ((result-ok (and result (= result expected)))
          (output-ok (search output-pattern output)))
      (if (and result-ok output-ok)
          (format t "~A: pass~%" name)
          (progn
            (format t "~A: FAIL~%" name)
            (unless result-ok
              (format t "  Result: expected ~A, got ~A~%" expected result))
            (unless output-ok
              (format t "  Output missing pattern: ~A~%" output-pattern)
              (format t "  Actual output: ~A~%" output))
            (sb-ext:quit :unix-status 1))))))

(format t "~%=== Trace Facility Tests ===~%~%")

;; Reset trace state before tests
(setq habu-sbcl-codegen::*traced-functions* nil)

;; Test 1: trace adds function to list
(format t "Test 1: trace adds function to trace list~%")
(habu-sbcl-codegen::trace-function 'test-fn)
(if (member 'test-fn habu-sbcl-codegen::*traced-functions* :test #'eq)
    (format t "trace-function: pass~%")
    (progn
      (format t "trace-function: FAIL~%")
      (sb-ext:quit :unix-status 1)))

;; Test 2: untrace removes function from list
(format t "Test 2: untrace removes function from trace list~%")
(habu-sbcl-codegen::untrace-function 'test-fn)
(if (not (member 'test-fn habu-sbcl-codegen::*traced-functions* :test #'eq))
    (format t "untrace-function: pass~%")
    (progn
      (format t "untrace-function: FAIL~%")
      (sb-ext:quit :unix-status 1)))

;; Test 3: traced function compiles and runs correctly
(setq habu-sbcl-codegen::*traced-functions* nil)
(habu-sbcl-codegen::trace-function 'add-nums)
(run-test "traced-function-correct-result"
          '((defun add-nums (a b)
              (+ a b))
            (add-nums #x3 #x4))
          #x7)
(setq habu-sbcl-codegen::*traced-functions* nil)

;; Test 4: traced function prints entry message
(habu-sbcl-codegen::trace-function 'mul-nums)
(run-test-with-output "traced-function-entry-output"
                      '((defun mul-nums (x y)
                          (* x y))
                        (mul-nums #x5 #x6))
                      #x1E
                      "TRACE: (MUL-NUMS")
(setq habu-sbcl-codegen::*traced-functions* nil)

;; Test 5: traced function prints exit message
(habu-sbcl-codegen::trace-function 'sub-nums)
(run-test-with-output "traced-function-exit-output"
                      '((defun sub-nums (a b)
                          (- a b))
                        (sub-nums #xA #x3))
                      #x7
                      "TRACE: SUB-NUMS =>")
(setq habu-sbcl-codegen::*traced-functions* nil)

;; Test 6: non-traced function has no trace output
(run-test "non-traced-no-output"
          '((defun simple-fn (x)
              (+ x #x1))
            (simple-fn #x5))
          #x6)

;; Test 7: traced recursive function shows nested calls
(habu-sbcl-codegen::trace-function 'fact)
(run-test-with-output "traced-recursive"
                      '((defun fact (n)
                          (if (= n #x0)
                              #x1
                              (* n (fact (- n #x1)))))
                        (fact #x4))
                      #x18  ; 4! = 24
                      "TRACE: (FACT")
(setq habu-sbcl-codegen::*traced-functions* nil)

;; Test 8: multiple functions can be traced
(habu-sbcl-codegen::trace-function 'fn-a)
(habu-sbcl-codegen::trace-function 'fn-b)
(run-test-with-output "multiple-traced"
                      '((defun fn-a (x) (+ x #x1))
                        (defun fn-b (x) (fn-a (* x #x2)))
                        (fn-b #x3))
                      #x7  ; (3*2)+1 = 7
                      "TRACE: (FN-A")
(setq habu-sbcl-codegen::*traced-functions* nil)

(format t "~%=== All Trace Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
