;;;; Test let bindings and variables

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing let bindings and variables~%~%")

;;; Test simple let with one binding
(format t "Testing: (let ((x 42)) x)~%")
(handler-case
    (let ((code (compile-expression '(let ((x 42)) x) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 42)) x) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test let with arithmetic
(format t "~%Testing: (let ((x 10)) (+ x 20))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 10)) (+ x 20)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 10)) (+ x 20)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test let with two bindings
(format t "~%Testing: (let ((x 5) (y 10)) (+ x y))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5) (y 10)) (+ x y)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5) (y 10)) (+ x y)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test nested let
(format t "~%Testing: (let ((x 10)) (let ((y 20)) (+ x y)))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 10)) (let ((y 20)) (+ x y))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 10)) (let ((y 20)) (+ x y))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test let with conditionals
(format t "~%Testing: (let ((x 5)) (if (< x 10) 100 200))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (if (< x 10) 100 200)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (if (< x 10) 100 200)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test let with multiple operations
(format t "~%Testing: (let ((a 3) (b 4)) (* (+ a b) 2))~%")
(handler-case
    (let ((code (compile-expression '(let ((a 3) (b 4)) (* (+ a b) 2)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((a 3) (b 4)) (* (+ a b) 2)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Let binding tests complete!~%")
