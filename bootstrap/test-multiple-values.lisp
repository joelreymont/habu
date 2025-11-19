;;;; Test multiple return values

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Multiple Return Values~%")
(format t "==============================~%~%")

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

;; Test 1: values compilation
(format t "~%[34m1. Test values Forms[0m~%")
(format t "========================~%")

(handler-case
    (let ((code (compile-expression '(values) :arch :x86_64)))
      (test "VALUES-0-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-0-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(values 42) :arch :x86_64)))
      (test "VALUES-1-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-1-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(values 10 20) :arch :x86_64)))
      (test "VALUES-2-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-2-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(values 1 2 3) :arch :x86_64)))
      (test "VALUES-3-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-3-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(values 1 2 3 4) :arch :x86_64)))
      (test "VALUES-4-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-4-X86" nil (format nil "~A" e))))

;; Test 2: ARM64 values
(format t "~%[34m2. Test ARM64 values Forms[0m~%")
(format t "==============================~%")

(handler-case
    (let ((code (compile-expression '(values) :arch :arm64)))
      (test "VALUES-0-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-0-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(values 42) :arch :arm64)))
      (test "VALUES-1-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-1-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(values 10 20) :arch :arm64)))
      (test "VALUES-2-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-2-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(values 1 2 3 4) :arch :arm64)))
      (test "VALUES-4-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "VALUES-4-ARM64" nil (format nil "~A" e))))

;; Test 3: multiple-value-bind compilation
(format t "~%[34m3. Test multiple-value-bind[0m~%")
(format t "=================================~%")

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (x y)
                                        (values 10 20)
                                      (+ x y))
                                    :arch :x86_64)))
      (test "MVB-MATCHING-ARITY-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MVB-MATCHING-ARITY-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (a b c)
                                        (values 1 2 3)
                                      (list a b c))
                                    :arch :x86_64)))
      (test "MVB-3-VARS-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MVB-3-VARS-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (x)
                                        (values 42)
                                      x)
                                    :arch :x86_64)))
      (test "MVB-1-VAR-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MVB-1-VAR-X86" nil (format nil "~A" e))))

;; Test 4: ARM64 multiple-value-bind
(format t "~%[34m4. Test ARM64 multiple-value-bind[0m~%")
(format t "=====================================~%")

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (x y)
                                        (values 10 20)
                                      (+ x y))
                                    :arch :arm64)))
      (test "MVB-MATCHING-ARITY-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MVB-MATCHING-ARITY-ARM64" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (a b c)
                                        (values 1 2 3)
                                      (list a b c))
                                    :arch :arm64)))
      (test "MVB-3-VARS-ARM64" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MVB-3-VARS-ARM64" nil (format nil "~A" e))))

;; Test 5: Complex expressions
(format t "~%[34m5. Test Complex Multiple Value Expressions[0m~%")
(format t "=============================================~%")

(handler-case
    (let ((code (compile-expression '(let ((result (values 100 200)))
                                       result)
                                    :arch :x86_64)))
      (test "LET-WITH-VALUES-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "LET-WITH-VALUES-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (x y)
                                        (if t
                                            (values 1 2)
                                            (values 3 4))
                                      (+ x y))
                                    :arch :x86_64)))
      (test "MVB-WITH-IF-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MVB-WITH-IF-X86" nil (format nil "~A" e))))

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (a b)
                                        (progn
                                          (+ 1 2)
                                          (values 10 20))
                                      (* a b))
                                    :arch :x86_64)))
      (test "MVB-WITH-PROGN-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "MVB-WITH-PROGN-X86" nil (format nil "~A" e))))

;; Test 6: Nested multiple-value-bind
(format t "~%[34m6. Test Nested Multiple Value Forms[0m~%")
(format t "========================================~%")

(handler-case
    (let ((code (compile-expression '(multiple-value-bind (x y)
                                        (values 1 2)
                                      (multiple-value-bind (a b)
                                          (values x y)
                                        (+ a b)))
                                    :arch :x86_64)))
      (test "NESTED-MVB-X86" (> (length code) 0))
      (format t "   Generated ~D bytes~%~%" (length code)))
  (error (e)
    (test "NESTED-MVB-X86" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"  *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll multiple value tests passed![0m~%")

(sb-ext:quit)
