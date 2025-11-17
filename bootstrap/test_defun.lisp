;;;; Test defun - global function definitions

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing defun (global function definitions)~%~%")

;;; Test simple function definition and call
(format t "Testing: (defun square (x) (* x x))~%")
(handler-case
    (let ((code (compile-expression '(defun square (x) (* x x)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes (defun returns 0)~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(defun square (x) (* x x)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes (defun returns 0)~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test calling the defined function
(format t "~%Testing: (square 5)~%")
(handler-case
    (let ((code (compile-expression '(square 5) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(square 5) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test function with multiple parameters
(format t "~%Testing: (defun add (a b) (+ a b))~%")
(handler-case
    (let ((code (compile-expression '(defun add (a b) (+ a b)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(format t "Testing: (add 10 20)~%")
(handler-case
    (let ((code (compile-expression '(add 10 20) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test function with conditionals
(format t "~%Testing: (defun abs-val (n) (if (< n 0) (- 0 n) n))~%")
(handler-case
    (let ((code (compile-expression '(defun abs-val (n) (if (< n 0) (- 0 n) n)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(format t "Testing: (abs-val -10)~%")
(handler-case
    (let ((code (compile-expression '(abs-val -10) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test function calling another function
(format t "~%Testing: (defun double (x) (+ x x))~%")
(handler-case
    (let ((code (compile-expression '(defun double (x) (+ x x)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(format t "Testing: (defun quadruple (x) (double (double x)))~%")
(handler-case
    (let ((code (compile-expression '(defun quadruple (x) (double (double x))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(format t "Testing: (quadruple 3)~%")
(handler-case
    (let ((code (compile-expression '(quadruple 3) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test function with let bindings
(format t "~%Testing: (defun pythag (a b) (let ((a2 (* a a)) (b2 (* b b))) (+ a2 b2)))~%")
(handler-case
    (let ((code (compile-expression '(defun pythag (a b) (let ((a2 (* a a)) (b2 (* b b))) (+ a2 b2))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(format t "Testing: (pythag 3 4)~%")
(handler-case
    (let ((code (compile-expression '(pythag 3 4) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

;;; Test zero-parameter function
(format t "~%Testing: (defun answer () 42)~%")
(handler-case
    (let ((code (compile-expression '(defun answer () 42) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(format t "Testing: (answer)~%")
(handler-case
    (let ((code (compile-expression '(answer) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(format t "~%Defun tests complete!~%")
