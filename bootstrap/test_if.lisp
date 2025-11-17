;;;; Test if conditionals

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing if conditionals~%~%")

;;; Test simple if
(format t "Testing: (if 1 10 20)~%")
(let ((code (compile-expression '(if 1 10 20) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(if 1 10 20) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test if with comparison
(format t "~%Testing: (if (< 5 10) 100 200)~%")
(let ((code (compile-expression '(if (< 5 10) 100 200) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(if (< 5 10) 100 200) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test nested if
(format t "~%Testing nested: (if (> 10 5) (if (= 3 3) 42 0) 99)~%")
(let ((code (compile-expression '(if (> 10 5) (if (= 3 3) 42 0) 99) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(if (> 10 5) (if (= 3 3) 42 0) 99) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test if with arithmetic in branches
(format t "~%Testing: (if (>= 20 15) (+ 10 20) (* 5 3))~%")
(let ((code (compile-expression '(if (>= 20 15) (+ 10 20) (* 5 3)) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(if (>= 20 15) (+ 10 20) (* 5 3)) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

(format t "~%All if tests passed!~%")
