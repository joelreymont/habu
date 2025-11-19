;;;; Test closures as first-class values

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing First-Class Closures~%")
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

;; Test 1: Create a standalone closure
(format t "~%[34m1. Standalone Closure Creation[0m~%")
(format t "================================~%")

(handler-case
    (let ((code (compile-expression '(let ((x 42))
                                       (lambda (y) (+ x y)))
                                    :arch :x86_64)))
      (test "STANDALONE-CLOSURE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes of machine code~%" (length code)))
  (error (e)
    (test "STANDALONE-CLOSURE-COMPILES" nil (format nil "~A" e))))

;; Test 2: Closure with multiple captured variables
(format t "~%[34m2. Closure with Multiple Captured Variables[0m~%")
(format t "=============================================~%")

(handler-case
    (let ((code (compile-expression '(let ((x 10) (y 20))
                                       (lambda (z) (+ x (+ y z))))
                                    :arch :x86_64)))
      (test "MULTI-VAR-CLOSURE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes of machine code~%" (length code)))
  (error (e)
    (test "MULTI-VAR-CLOSURE-COMPILES" nil (format nil "~A" e))))

;; Test 3: Closure with three captured variables
(format t "~%[34m3. Closure with Three Captured Variables[0m~%")
(format t "==========================================~%")

(handler-case
    (let ((code (compile-expression '(let ((a 1) (b 2) (c 3))
                                       (lambda (x) (+ a (+ b (+ c x)))))
                                    :arch :x86_64)))
      (test "THREE-VAR-CLOSURE-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes of machine code~%" (length code)))
  (error (e)
    (test "THREE-VAR-CLOSURE-COMPILES" nil (format nil "~A" e))))

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

(format t "[32mAll first-class closure tests passed![0m~%")

(sb-ext:quit)
