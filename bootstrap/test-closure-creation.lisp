;;;; Test inline closure support (via funcall)

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Inline Closures (via funcall)~%")
(format t "======================================~%~%")

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

;; Test 1: Inline closure with one captured variable
(format t "~%[34m1. Inline Closure with One Captured Variable[0m~%")
(format t "==============================================~%")

(handler-case
    (let ((code (compile-expression '((lambda (y) (+ x y)) 20)
                                    :arch :x86_64)))
      ;; This should error because x is not bound
      (test "INLINE-CLOSURE-DETECTED-UNBOUND" nil "Expected error for unbound variable x"))
  (error (e)
    (test "INLINE-CLOSURE-DETECTED-UNBOUND" t)
    (format t "   Error (expected): ~A~%" e)))

;; Test 2: Inline closure within let binding
(format t "~%[34m2. Inline Closure Within Let[0m~%")
(format t "=================================~%")

(handler-case
    (let ((code (compile-expression '(let ((x 42))
                                       ((lambda (y) (+ x y)) 20))
                                    :arch :x86_64)))
      (test "INLINE-CLOSURE-IN-LET-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes of machine code~%" (length code)))
  (error (e)
    (test "INLINE-CLOSURE-IN-LET-COMPILES" nil (format nil "~A" e))))

;; Test 3: Inline closure with multiple captured variables
(format t "~%[34m3. Inline Closure with Multiple Captured Variables[0m~%")
(format t "====================================================~%")

(handler-case
    (let ((code (compile-expression '(let ((x 10) (y 20))
                                       ((lambda (z) (+ x (+ y z))) 30))
                                    :arch :x86_64)))
      (test "INLINE-CLOSURE-MULTI-VARS-COMPILES" (> (length code) 0))
      (format t "   Generated ~D bytes of machine code~%" (length code)))
  (error (e)
    (test "INLINE-CLOSURE-MULTI-VARS-COMPILES" nil (format nil "~A" e))))

;; Test 4: Nested inline closures
(format t "~%[34m4. Nested Inline Closures[0m~%")
(format t "===========================~%")

(handler-case
    (let ((code (compile-expression '((lambda (x) ((lambda (y) (+ x y)) 20)) 10)
                                    :arch :x86_64)))
      (test "NESTED-INLINE-CLOSURES-COMPILE" (> (length code) 0))
      (format t "   Generated ~D bytes of machine code~%" (length code)))
  (error (e)
    (test "NESTED-INLINE-CLOSURES-COMPILE" nil (format nil "~A" e))))

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

(format t "[32mAll inline closure tests passed![0m~%")

(sb-ext:quit)
