;;;; Test Harness for Habu Compiler
;;;; Provides infrastructure for running and reporting tests

(load "compiler.lisp")
(in-package :habu-compiler)

;;; Test tracking
(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)
(defvar *current-test-name* nil)

;;; Color output (basic ANSI codes)
(defun color-green (text)
  (format nil "~C[32m~A~C[0m" #\Esc text #\Esc))

(defun color-red (text)
  (format nil "~C[31m~A~C[0m" #\Esc text #\Esc))

(defun color-yellow (text)
  (format nil "~C[33m~A~C[0m" #\Esc text #\Esc))

(defun color-blue (text)
  (format nil "~C[34m~A~C[0m" #\Esc text #\Esc))

;;; Test infrastructure
(defun reset-test-stats ()
  "Reset test counters"
  (setf *test-count* 0)
  (setf *test-passed* 0)
  (setf *test-failed* 0))

(defun report-test-stats ()
  "Print final test statistics"
  (format t "~%")
  (format t "=====================================~%")
  (format t "  Test Results~%")
  (format t "=====================================~%")
  (format t "Total:  ~D~%" *test-count*)
  (if (> *test-failed* 0)
      (progn
        (format t "Passed: ~A~%" (color-green (format nil "~D" *test-passed*)))
        (format t "Failed: ~A~%" (color-red (format nil "~D" *test-failed*))))
      (format t "Passed: ~A~%" (color-green (format nil "~D/~D" *test-passed* *test-count*))))
  (format t "~%"))

(defmacro test-group (name &body body)
  "Define a group of related tests"
  `(progn
     (format t "~%~A~%" (color-blue (format nil "~A" ',name)))
     (format t "~A~%" (make-string (length (format nil "~A" ',name)) :initial-element #\=))
     ,@body))

(defmacro test-case (name &body body)
  "Define a single test case"
  `(let ((*current-test-name* ',name))
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (incf *test-passed*)
           (format t "~A ~A~%" (color-green "✓") ',name))
       (error (e)
         (incf *test-failed*)
         (format t "~A ~A~%" (color-red "✗") ',name)
         (format t "  Error: ~A~%" e)))))

(defun assert-compiles (expr arch expected-min-size)
  "Assert that an expression compiles successfully for the given architecture"
  (let ((code (compile-expression expr :arch arch)))
    (unless code
      (error "Compilation returned no code"))
    (when (< (length code) expected-min-size)
      (error "Generated code too small: ~D bytes (expected >= ~D)"
             (length code) expected-min-size))
    code))

(defun assert-compiles-both (expr &optional (min-size 1))
  "Assert that an expression compiles for both x86_64 and ARM64"
  (assert-compiles expr :x86_64 min-size)
  (assert-compiles expr :arm64 min-size))

(defun assert-code-size (expr arch expected-size)
  "Assert that compiled code is exactly the expected size"
  (let ((code (compile-expression expr :arch arch)))
    (unless (= (length code) expected-size)
      (error "Code size mismatch: got ~D bytes, expected ~D"
             (length code) expected-size))
    code))

(defun assert-error (expr)
  "Assert that an expression causes a compilation error"
  (handler-case
      (progn
        (compile-expression expr :arch :x86_64)
        (error "Expected compilation to fail, but it succeeded"))
    (error (e)
      ;; Expected - compilation failed
      t)))

;;; Export test harness API
(export '(test-group test-case assert-compiles assert-compiles-both
          assert-code-size assert-error reset-test-stats report-test-stats))
