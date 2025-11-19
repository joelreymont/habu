;;;; Test read/print compilation

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Read/Print Compilation~%")
(format t "===============================~%~%")

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

;; Test 1: Compile print operation
(format t "~%[34m1. Compile Print Operation[0m~%")
(format t "============================~%")

(handler-case
    (let ((code (compile-expression '(print 42) :arch :x86_64)))
      (test "PRINT-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "PRINT-COMPILES" nil (format nil "~A" e))))

;; Test 2: Compile read operation
(format t "~%[34m2. Compile Read Operation[0m~%")
(format t "===========================~%")

(handler-case
    (let ((code (compile-expression '(read "42") :arch :x86_64)))
      (test "READ-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "READ-COMPILES" nil (format nil "~A" e))))

;; Test 3: Print a string
(format t "~%[34m3. Print String[0m~%")
(format t "=================~%")

(handler-case
    (let ((code (compile-expression '(print "hello") :arch :x86_64)))
      (test "PRINT-STRING" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "PRINT-STRING" nil (format nil "~A" e))))

;; Test 4: Read a list
(format t "~%[34m4. Read List[0m~%")
(format t "==============~%")

(handler-case
    (let ((code (compile-expression '(read "(1 2 3)") :arch :x86_64)))
      (test "READ-LIST" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "READ-LIST" nil (format nil "~A" e))))

;; Test 5: Nested - print result of read
(format t "~%[34m5. Nested Read/Print[0m~%")
(format t "======================~%")

(handler-case
    (let ((code (compile-expression '(print (read "(+ 1 2)")) :arch :x86_64)))
      (test "NESTED-READ-PRINT" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "NESTED-READ-PRINT" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll read/print compilation tests passed![0m~%")

(sb-ext:quit)
