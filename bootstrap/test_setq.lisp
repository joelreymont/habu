;;;; Test setq - variable mutation

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing setq (variable mutation)~%~%")

;;; Test simple setq in let
(format t "Testing: (let ((x 5)) (setq x 10) x)~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (setq x 10) x) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (setq x 10) x) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test setq with expression
(format t "~%Testing: (let ((x 5)) (setq x (+ x 1)) x)~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (setq x (+ x 1)) x) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (setq x (+ x 1)) x) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test multiple setq
(format t "~%Testing: (let ((x 1) (y 2)) (setq x 10) (setq y 20) (+ x y))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 1) (y 2)) (setq x 10) (setq y 20) (+ x y)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test setq in progn
(format t "~%Testing: (let ((x 5)) (progn (setq x 10) (setq x (* x 2)) x))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (progn (setq x 10) (setq x (* x 2)) x)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test setq in conditional
(format t "~%Testing: (let ((x 5)) (if (< x 10) (setq x 100) (setq x 200)) x)~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (if (< x 10) (setq x 100) (setq x 200)) x) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test setq with nested let
(format t "~%Testing: (let ((x 1)) (let ((y 2)) (setq x (+ x y)) x))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 1)) (let ((y 2)) (setq x (+ x y)) x)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test setq in lambda
(format t "~%Testing: ((lambda (x) (setq x (* x x)) x) 5)~%")
(handler-case
    (let ((code (compile-expression '((lambda (x) (setq x (* x x)) x) 5) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test setq accumulator pattern
(format t "~%Testing: (let ((sum 0)) (setq sum (+ sum 1)) (setq sum (+ sum 2)) (setq sum (+ sum 3)) sum)~%")
(handler-case
    (let ((code (compile-expression '(let ((sum 0)) (setq sum (+ sum 1)) (setq sum (+ sum 2)) (setq sum (+ sum 3)) sum) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test error: setq unbound variable
(format t "~%Testing error: (setq undefined-var 42)~%")
(handler-case
    (let ((code (compile-expression '(setq undefined-var 42) :arch :x86_64)))
      (format t "  x86_64: UNEXPECTED SUCCESS~%"))
  (error (e)
    (format t "  x86_64: Expected error: ~A~%" e)))

(format t "~%Setq tests complete!~%")
