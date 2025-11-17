;;;; Test lambda expressions

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing lambda expressions~%~%")

;;; Test simple lambda call
(format t "Testing: ((lambda (x) x) 42)~%")
(handler-case
    (let ((code (compile-expression '((lambda (x) x) 42) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '((lambda (x) x) 42) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test lambda with arithmetic
(format t "~%Testing: ((lambda (x) (* x 2)) 21)~%")
(handler-case
    (let ((code (compile-expression '((lambda (x) (* x 2)) 21) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '((lambda (x) (* x 2)) 21) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test lambda with multiple parameters
(format t "~%Testing: ((lambda (x y) (+ x y)) 10 20)~%")
(handler-case
    (let ((code (compile-expression '((lambda (x y) (+ x y)) 10 20) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '((lambda (x y) (+ x y)) 10 20) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test lambda with multiple parameters and complex body
(format t "~%Testing: ((lambda (a b) (+ (* a a) (* b b))) 3 4)~%")
(handler-case
    (let ((code (compile-expression '((lambda (a b) (+ (* a a) (* b b))) 3 4) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '((lambda (a b) (+ (* a a) (* b b))) 3 4) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test lambda with conditional
(format t "~%Testing: ((lambda (x) (if (< x 10) 1 0)) 5)~%")
(handler-case
    (let ((code (compile-expression '((lambda (x) (if (< x 10) 1 0)) 5) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '((lambda (x) (if (< x 10) 1 0)) 5) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test nested lambda calls
(format t "~%Testing: ((lambda (x) ((lambda (y) (+ x y)) 20)) 10)~%")
(handler-case
    (let ((code (compile-expression '((lambda (x) ((lambda (y) (+ x y)) 20)) 10) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '((lambda (x) ((lambda (y) (+ x y)) 20)) 10) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Lambda expression tests complete!~%")
