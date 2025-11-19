;;;; Test error handling (catch/throw)

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Error Handling (Catch/Throw)~%")
(format t "====================================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%"name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Simple catch/throw compilation
(format t "~%[34m1. Test Catch/Throw Compilation[0m~%")
(format t "==================================~%")

(handler-case
    (let ((code (compile-expression '(catch 1 42) :arch :x86_64)))
      (test "CATCH-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "CATCH-COMPILES" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(throw 1 99) :arch :x86_64)))
      (test "THROW-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "THROW-COMPILES" nil (format nil "~A" e))))

;; Test 2: ARM64 catch/throw compilation
(format t "~%[34m2. Test ARM64 Catch/Throw[0m~%")
(format t "=============================~%")

(handler-case
    (let ((code (compile-expression '(catch 1 42) :arch :arm64)))
      (test "ARM64-CATCH-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-CATCH-COMPILES" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(throw 1 99) :arch :arm64)))
      (test "ARM64-THROW-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "ARM64-THROW-COMPILES" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"  *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll error handling compilation tests passed![0m~%")

(sb-ext:quit)
