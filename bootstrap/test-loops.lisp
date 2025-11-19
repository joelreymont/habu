;;;; Test loop constructs (dotimes, dolist)

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Loop Constructs~%")
(format t "======================~%~%")

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

;; Test 1: dotimes compilation
(format t "~%[34m1. Test dotimes Forms[0m~%")
(format t "===========================~%")

(handler-case
    (let ((code (compile-expression '(dotimes (i 0)) :arch :x86_64)))
      (test "DOTIMES-0-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-0-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dotimes (i 1)
                                        (+ i 1))
                                    :arch :x86_64)))
      (test "DOTIMES-1-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-1-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dotimes (i 5)
                                        (* i i))
                                    :arch :x86_64)))
      (test "DOTIMES-5-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-5-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dotimes (i 3 42))
                                    :arch :x86_64)))
      (test "DOTIMES-RESULT-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-RESULT-X86" nil (format nil "~A" e))))

;; Test 2: ARM64 dotimes
(format t "~%[34m2. Test ARM64 dotimes Forms[0m~%")
(format t "================================~%")

(handler-case
    (let ((code (compile-expression '(dotimes (i 0)) :arch :arm64)))
      (test "DOTIMES-0-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-0-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dotimes (i 3)
                                        (+ i 10))
                                    :arch :arm64)))
      (test "DOTIMES-3-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-3-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dotimes (i 2 99))
                                    :arch :arm64)))
      (test "DOTIMES-RESULT-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-RESULT-ARM64" nil (format nil "~A" e))))

;; Test 3: dolist compilation
(format t "~%[34m3. Test dolist Forms[0m~%")
(format t "========================~%")

(handler-case
    (let ((code (compile-expression '(dolist (x nil))
                                    :arch :x86_64)))
      (test "DOLIST-EMPTY-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-EMPTY-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dolist (x (cons 42 nil))
                                        (+ x 1))
                                    :arch :x86_64)))
      (test "DOLIST-SINGLE-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-SINGLE-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dolist (x (list 1 2 3))
                                        (* x x))
                                    :arch :x86_64)))
      (test "DOLIST-MULTIPLE-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-MULTIPLE-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dolist (x (list 10 20) 99))
                                    :arch :x86_64)))
      (test "DOLIST-RESULT-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-RESULT-X86" nil (format nil "~A" e))))

;; Test 4: ARM64 dolist
(format t "~%[34m4. Test ARM64 dolist Forms[0m~%")
(format t "===============================~%")

(handler-case
    (let ((code (compile-expression '(dolist (x nil))
                                    :arch :arm64)))
      (test "DOLIST-EMPTY-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-EMPTY-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dolist (x (list 5 6 7 8))
                                        (+ x x))
                                    :arch :arm64)))
      (test "DOLIST-MULTIPLE-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-MULTIPLE-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dolist (x (cons 1 nil) 42))
                                    :arch :arm64)))
      (test "DOLIST-RESULT-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-RESULT-ARM64" nil (format nil "~A" e))))

;; Test 5: Nested loops
(format t "~%[34m5. Test Nested Loops[0m~%")
(format t "=======================~%")

(handler-case
    (let ((code (compile-expression '(dotimes (i 3)
                                        (dotimes (j 2)
                                          (+ i j)))
                                    :arch :x86_64)))
      (test "NESTED-DOTIMES-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "NESTED-DOTIMES-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dolist (x (list 1 2))
                                        (dolist (y (list 3 4))
                                          (* x y)))
                                    :arch :x86_64)))
      (test "NESTED-DOLIST-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "NESTED-DOLIST-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(dotimes (i 2)
                                        (dolist (x (list 10 20))
                                          (+ i x)))
                                    :arch :x86_64)))
      (test "NESTED-MIXED-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "NESTED-MIXED-X86" nil (format nil "~A" e))))

;; Test 6: Complex expressions with loops
(format t "~%[34m6. Test Complex Loop Expressions[0m~%")
(format t "=====================================~%")

(handler-case
    (let ((code (compile-expression '(let ((count 5))
                                       (dotimes (i count)
                                         (+ i 1)))
                                    :arch :x86_64)))
      (test "DOTIMES-WITH-LET-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-WITH-LET-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(let ((nums (list 1 2 3)))
                                       (dolist (x nums)
                                         (* x x)))
                                    :arch :x86_64)))
      (test "DOLIST-WITH-LET-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOLIST-WITH-LET-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(if t
                                          (dotimes (i 2)
                                            i)
                                          0)
                                    :arch :x86_64)))
      (test "DOTIMES-IN-IF-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "DOTIMES-IN-IF-X86" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"  *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll loop tests passed![0m~%")

(sb-ext:quit)
