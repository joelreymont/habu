;;;; Test file I/O compilation

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing File I/O Compilation~%")
(format t "=============================~%~%")

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

;; Test 1: Compile read-file
(format t "~%[34m1. Compile read-file[0m~%")
(format t "======================~%")

(handler-case
    (let ((code (compile-expression '(read-file "/tmp/test.txt") :arch :x86_64)))
      (test "READ-FILE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "READ-FILE-COMPILES" nil (format nil "~A" e))))

;; Test 2: Compile write-file
(format t "~%[34m2. Compile write-file[0m~%")
(format t "=======================~%")

(handler-case
    (let ((code (compile-expression '(write-file "/tmp/test.txt" "Hello!") :arch :x86_64)))
      (test "WRITE-FILE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "WRITE-FILE-COMPILES" nil (format nil "~A" e))))

;; Test 3: Compile file-open
(format t "~%[34m3. Compile file-open[0m~%")
(format t "======================~%")

(handler-case
    (let ((code (compile-expression '(file-open "/tmp/test.txt" "r") :arch :x86_64)))
      (test "FILE-OPEN-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "FILE-OPEN-COMPILES" nil (format nil "~A" e))))

;; Test 4: Compile file-read
(format t "~%[34m4. Compile file-read[0m~%")
(format t "======================~%")

(handler-case
    (let ((code (compile-expression '(file-read 10) :arch :x86_64)))
      (test "FILE-READ-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "FILE-READ-COMPILES" nil (format nil "~A" e))))

;; Test 5: Compile file-write
(format t "~%[34m5. Compile file-write[0m~%")
(format t "=======================~%")

(handler-case
    (let ((code (compile-expression '(file-write 10 "data") :arch :x86_64)))
      (test "FILE-WRITE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "FILE-WRITE-COMPILES" nil (format nil "~A" e))))

;; Test 6: Compile file-close
(format t "~%[34m6. Compile file-close[0m~%")
(format t "=======================~%")

(handler-case
    (let ((code (compile-expression '(file-close 10) :arch :x86_64)))
      (test "FILE-CLOSE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (test "FILE-CLOSE-COMPILES" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll file I/O compilation tests passed![0m~%")

(sb-ext:quit)
