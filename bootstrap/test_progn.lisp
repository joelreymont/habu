;;;; Test progn (sequential evaluation)

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing progn expressions~%~%")

;;; Test simple progn
(format t "Testing: (progn 1 2 3)~%")
(handler-case
    (let ((code (compile-expression '(progn 1 2 3) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(progn 1 2 3) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test progn with computation
(format t "~%Testing: (progn (+ 1 2) (* 3 4) (- 10 5))~%")
(handler-case
    (let ((code (compile-expression '(progn (+ 1 2) (* 3 4) (- 10 5)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(progn (+ 1 2) (* 3 4) (- 10 5)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test progn in let
(format t "~%Testing: (let ((x 5)) (progn (+ x 1) (* x 2) (- x 1)))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (progn (+ x 1) (* x 2) (- x 1))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (progn (+ x 1) (* x 2) (- x 1))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test progn in lambda
(format t "~%Testing: ((lambda (x) (progn (+ x 1) (* x 2))) 10)~%")
(handler-case
    (let ((code (compile-expression '((lambda (x) (progn (+ x 1) (* x 2))) 10) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '((lambda (x) (progn (+ x 1) (* x 2))) 10) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Progn tests complete!~%")
