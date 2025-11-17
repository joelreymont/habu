;;;; Test quote for literal data

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing quote expressions~%~%")

;;; Test quoted integers
(format t "Testing: (quote 42) or '42~%")
(handler-case
    (let ((code (compile-expression '(quote 42) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(quote 42) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test quoted zero
(format t "~%Testing: '0~%")
(handler-case
    (let ((code (compile-expression ''0 :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression ''0 :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test quoted nil
(format t "~%Testing: 'nil~%")
(handler-case
    (let ((code (compile-expression ''nil :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression ''nil :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test quoted negative number
(format t "~%Testing: '-100~%")
(handler-case
    (let ((code (compile-expression ''-100 :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression ''-100 :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test quote in expressions
(format t "~%Testing: (+ '10 '20)~%")
(handler-case
    (let ((code (compile-expression '(+ '10 '20) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(+ '10 '20) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test quote in let
(format t "~%Testing: (let ((x '5)) (+ x '10))~%")
(handler-case
    (let ((code (compile-expression '(let ((x '5)) (+ x '10)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x '5)) (+ x '10)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test quote in conditional
(format t "~%Testing: (if (< '5 '10) '100 '200)~%")
(handler-case
    (let ((code (compile-expression '(if (< '5 '10) '100 '200) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(if (< '5 '10) '100 '200) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Quote tests complete!~%")
