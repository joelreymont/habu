;;;; Test case pattern matching

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing case expressions~%~%")

;;; Test simple case with single keys
(format t "Testing: (case 2 (1 100) (2 200) (3 300))~%")
(handler-case
    (let ((code (compile-expression '(case 2 (1 100) (2 200) (3 300)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(case 2 (1 100) (2 200) (3 300)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test case with default (t clause)
(format t "~%Testing: (case 5 (1 100) (2 200) (t 999))~%")
(handler-case
    (let ((code (compile-expression '(case 5 (1 100) (2 200) (t 999)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(case 5 (1 100) (2 200) (t 999)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test case with variable
(format t "~%Testing: (let ((x 2)) (case x (1 100) (2 200) (3 300)))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 2)) (case x (1 100) (2 200) (3 300))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 2)) (case x (1 100) (2 200) (3 300))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test case with multiple keys per clause
(format t "~%Testing: (case 3 ((1 2) 100) ((3 4) 200) (t 300))~%")
(handler-case
    (let ((code (compile-expression '(case 3 ((1 2) 100) ((3 4) 200) (t 300)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(case 3 ((1 2) 100) ((3 4) 200) (t 300)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test case with expression as key-form
(format t "~%Testing: (case (+ 1 1) (1 100) (2 200) (t 300))~%")
(handler-case
    (let ((code (compile-expression '(case (+ 1 1) (1 100) (2 200) (t 300)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(case (+ 1 1) (1 100) (2 200) (t 300)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test case with complex result expressions
(format t "~%Testing: (case 2 (1 (+ 10 20)) (2 (* 5 5)) (t (- 100 50)))~%")
(handler-case
    (let ((code (compile-expression '(case 2 (1 (+ 10 20)) (2 (* 5 5)) (t (- 100 50))) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(case 2 (1 (+ 10 20)) (2 (* 5 5)) (t (- 100 50))) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Case tests complete!~%")
