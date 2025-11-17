;;;; Test bitwise operators

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing bitwise operators~%~%")

;;; Test logand
(format t "Testing: (logand 15 7) ; should be 7~%")
(handler-case
    (let ((code (compile-expression '(logand 15 7) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(logand 15 7) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test logior
(format t "~%Testing: (logior 8 4) ; should be 12~%")
(handler-case
    (let ((code (compile-expression '(logior 8 4) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(logior 8 4) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test logxor
(format t "~%Testing: (logxor 15 9) ; should be 6~%")
(handler-case
    (let ((code (compile-expression '(logxor 15 9) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(logxor 15 9) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test lognot
(format t "~%Testing: (lognot 0)~%")
(handler-case
    (let ((code (compile-expression '(lognot 0) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(lognot 0) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test ash left shift
(format t "~%Testing: (ash 5 2) ; should be 20~%")
(handler-case
    (let ((code (compile-expression '(ash 5 2) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(ash 5 2) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test ash right shift
(format t "~%Testing: (ash 20 -2) ; should be 5~%")
(handler-case
    (let ((code (compile-expression '(ash 20 -2) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(ash 20 -2) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test nested bitwise operations
(format t "~%Testing: (logand (logior 8 4) 15)~%")
(handler-case
    (let ((code (compile-expression '(logand (logior 8 4) 15) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(logand (logior 8 4) 15) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Bitwise operator tests complete!~%")
