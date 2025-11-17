;;;; Test cond (multi-way conditional)

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing cond expressions~%~%")

;;; Test simple cond with two clauses
(format t "Testing: (cond ((< 5 10) 100) (t 200))~%")
(handler-case
    (let ((code (compile-expression '(cond ((< 5 10) 100) (t 200)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(cond ((< 5 10) 100) (t 200)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test cond with multiple clauses
(format t "~%Testing: (cond ((< 5 3) 100) ((< 5 7) 200) (t 300))~%")
(handler-case
    (let ((code (compile-expression '(cond ((< 5 3) 100) ((< 5 7) 200) (t 300)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(cond ((< 5 3) 100) ((< 5 7) 200) (t 300)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test cond without default (last clause is not t)
(format t "~%Testing: (cond ((= 5 5) 42))~%")
(handler-case
    (let ((code (compile-expression '(cond ((= 5 5) 42)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(cond ((= 5 5) 42)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test cond with variable
(format t "~%Testing: (let ((x 5)) (cond ((< x 3) 1) ((< x 7) 2) (t 3)))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (cond ((< x 3) 1) ((< x 7) 2) (t 3))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (cond ((< x 3) 1) ((< x 7) 2) (t 3))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test cond with complex expressions
(format t "~%Testing: (cond ((and (> 5 3) (< 5 10)) (* 2 10)) (t (+ 1 1)))~%")
(handler-case
    (let ((code (compile-expression '(cond ((and (> 5 3) (< 5 10)) (* 2 10)) (t (+ 1 1))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(cond ((and (> 5 3) (< 5 10)) (* 2 10)) (t (+ 1 1))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test nested cond
(format t "~%Testing: (cond ((< 5 3) 1) (t (cond ((< 5 7) 2) (t 3))))~%")
(handler-case
    (let ((code (compile-expression '(cond ((< 5 3) 1) (t (cond ((< 5 7) 2) (t 3)))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(cond ((< 5 3) 1) (t (cond ((< 5 7) 2) (t 3)))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Cond tests complete!~%")
