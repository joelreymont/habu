;;;; Test when and unless special forms

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing when and unless~%~%")

;;; Test when with true condition
(format t "Testing: (when (< 5 10) 42)~%")
(handler-case
    (let ((code (compile-expression '(when (< 5 10) 42) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(when (< 5 10) 42) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test when with multiple expressions
(format t "~%Testing: (when (< 5 10) (+ 1 2) (* 3 4))~%")
(handler-case
    (let ((code (compile-expression '(when (< 5 10) (+ 1 2) (* 3 4)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(when (< 5 10) (+ 1 2) (* 3 4)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test unless with false condition
(format t "~%Testing: (unless (> 5 10) 42)~%")
(handler-case
    (let ((code (compile-expression '(unless (> 5 10) 42) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(unless (> 5 10) 42) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test unless with multiple expressions
(format t "~%Testing: (unless (> 5 10) (+ 1 2) (* 3 4))~%")
(handler-case
    (let ((code (compile-expression '(unless (> 5 10) (+ 1 2) (* 3 4)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(unless (> 5 10) (+ 1 2) (* 3 4)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test when with variable
(format t "~%Testing: (let ((x 5)) (when (< x 10) (+ x 1)))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (when (< x 10) (+ x 1))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (when (< x 10) (+ x 1))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test unless with variable
(format t "~%Testing: (let ((x 5)) (unless (> x 10) (* x 2)))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (unless (> x 10) (* x 2))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (unless (> x 10) (* x 2))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%When/unless tests complete!~%")
