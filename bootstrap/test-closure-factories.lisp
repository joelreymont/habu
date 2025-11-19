;;;;Test closure factories - functions that return closures

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Closure Factories~%")
(format t "=========================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%" name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Simple closure factory - defun returns closure
(format t "~%[34m1. Closure Factory via defun[0m~%")
(format t "================================~%")

(handler-case
    (progn
      ;; First compile the factory function
      (compile-expression '(defun make-adder (x)
                            (lambda (y) (+ x y)))
                          :arch :x86_64)
      (test "CLOSURE-FACTORY-COMPILES" t)
      (format t "   Factory function compiled~%"))
  (error (e)
    (test "CLOSURE-FACTORY-COMPILES" nil (format nil "~A" e))))

;; Test 2: Use the factory - call (make-adder 5)
(format t "~%[34m2. Call Closure Factory[0m~%")
(format t "========================~%")

(handler-case
    (let ((code (compile-expression '(make-adder 5) :arch :x86_64)))
      (test "CALL-FACTORY-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "CALL-FACTORY-COMPILES" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: [32m~D/~D[0m~%" *pass-count* *test-count*)
(format t "~%")

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll closure factory tests passed![0m~%")

(sb-ext:quit)
