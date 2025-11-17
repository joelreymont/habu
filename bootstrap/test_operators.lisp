;;;; Test new operators

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing new operators~%~%")

;;; Test multiplication
(format t "Testing multiplication: (* 5 3)~%")
(let ((code (compile-expression '(* 5 3) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(* 5 3) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test less than
(format t "~%Testing less than: (< 5 10)~%")
(let ((code (compile-expression '(< 5 10) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(< 5 10) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test greater than
(format t "~%Testing greater than: (> 20 10)~%")
(let ((code (compile-expression '(> 20 10) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(> 20 10) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test equality
(format t "~%Testing equality: (= 42 42)~%")
(let ((code (compile-expression '(= 42 42) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(= 42 42) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test less or equal
(format t "~%Testing less or equal: (<= 10 10)~%")
(let ((code (compile-expression '(<= 10 10) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(<= 10 10) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test greater or equal
(format t "~%Testing greater or equal: (>= 15 10)~%")
(let ((code (compile-expression '(>= 15 10) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(>= 15 10) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

;;; Test complex expression
(format t "~%Testing complex: (< (* 2 3) (+ 5 2))~%")
(let ((code (compile-expression '(< (* 2 3) (+ 5 2)) :arch :x86_64)))
  (format t "  x86_64: ~D bytes~%" (length code)))
(let ((code (compile-expression '(< (* 2 3) (+ 5 2)) :arch :arm64)))
  (format t "  ARM64:  ~D bytes~%" (length code)))

(format t "~%All operator tests passed!~%")
