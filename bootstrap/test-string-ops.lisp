;;;; Test string operations

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing String Operations~%")
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

;; Test 1: string-length
(format t "~%[34m1. Testing string-length[0m~%")
(format t "============================~%")

(handler-case
    (let ((code (compile-expression '(string-length "hello") :arch :x86_64)))
      (test "STRING-LENGTH-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "STRING-LENGTH-COMPILES" nil (format nil "~A" e))))

;; Test 2: string-concat
(format t "~%[34m2. Testing string-concat[0m~%")
(format t "=============================~%")

(handler-case
    (let ((code (compile-expression '(string-concat "hello" " world") :arch :x86_64)))
      (test "STRING-CONCAT-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "STRING-CONCAT-COMPILES" nil (format nil "~A" e))))

;; Test 3: string-equal
(format t "~%[34m3. Testing string-equal[0m~%")
(format t "============================~%")

(handler-case
    (let ((code (compile-expression '(string-equal "foo" "foo") :arch :x86_64)))
      (test "STRING-EQUAL-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "STRING-EQUAL-COMPILES" nil (format nil "~A" e))))

;; Test 4: string-substring
(format t "~%[34m4. Testing string-substring[0m~%")
(format t "================================~%")

(handler-case
    (let ((code (compile-expression '(string-substring "hello world" 0 5) :arch :x86_64)))
      (test "STRING-SUBSTRING-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "STRING-SUBSTRING-COMPILES" nil (format nil "~A" e))))

;; Test 5: Nested operations
(format t "~%[34m5. Testing Nested String Operations[0m~%")
(format t "=======================================~%")

(handler-case
    (let ((code (compile-expression '(string-length (string-concat "hello" " world")) :arch :x86_64)))
      (test "NESTED-STRING-OPS" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "NESTED-STRING-OPS" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll string operation tests passed![0m~%")

(sb-ext:quit)
