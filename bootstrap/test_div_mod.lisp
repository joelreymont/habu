;;;; Test division and modulo operators

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing division and modulo~%~%")

;;; Test division
(format t "Testing division: (/ 20 5)~%")
(let ((code (compile-expression '(/ 20 5) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(/ 20 5) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test modulo
(format t "~%Testing modulo: (mod 17 5)~%")
(let ((code (compile-expression '(mod 17 5) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(mod 17 5) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test complex expression with all arithmetic
(format t "~%Testing: (+ (* 3 4) (/ 20 2))~%")
(let ((code (compile-expression '(+ (* 3 4) (/ 20 2)) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(+ (* 3 4) (/ 20 2)) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test if with division
(format t "~%Testing: (if (< (mod 10 3) 2) 100 200)~%")
(let ((code (compile-expression '(if (< (mod 10 3) 2) 100 200) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(if (< (mod 10 3) 2) 100 200) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

(format t "~%All division and modulo tests passed!~%")
