;;;; Test boolean operators (and, or, not)

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing boolean operators~%~%")

;;; Test NOT
(format t "Testing: (not 0) - should be 1 (true)~%")
(handler-case
    (let ((code (compile-expression '(not 0) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(not 0) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (not 1) - should be 0 (false)~%")
(handler-case
    (let ((code (compile-expression '(not 1) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(not 1) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (not (< 5 10)) - should be 0 (false)~%")
(handler-case
    (let ((code (compile-expression '(not (< 5 10)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(not (< 5 10)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test AND
(format t "~%Testing: (and) - empty and is true~%")
(handler-case
    (let ((code (compile-expression '(and) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(and) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (and 1 2 3) - all true, returns last~%")
(handler-case
    (let ((code (compile-expression '(and 1 2 3) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(and 1 2 3) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (and 1 0 3) - returns first false (0)~%")
(handler-case
    (let ((code (compile-expression '(and 1 0 3) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(and 1 0 3) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (and (< 5 10) (> 20 15)) - both true~%")
(handler-case
    (let ((code (compile-expression '(and (< 5 10) (> 20 15)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(and (< 5 10) (> 20 15)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test OR
(format t "~%Testing: (or) - empty or is false~%")
(handler-case
    (let ((code (compile-expression '(or) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(or) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (or 0 0 3) - returns first true (3)~%")
(handler-case
    (let ((code (compile-expression '(or 0 0 3) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(or 0 0 3) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (or 1 2 3) - returns first (1)~%")
(handler-case
    (let ((code (compile-expression '(or 1 2 3) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(or 1 2 3) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Testing: (or (< 10 5) (> 20 15)) - first false, second true~%")
(handler-case
    (let ((code (compile-expression '(or (< 10 5) (> 20 15)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(or (< 10 5) (> 20 15)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test combinations
(format t "~%Testing: (and (or 0 1) (not 0)) - nested boolean ops~%")
(handler-case
    (let ((code (compile-expression '(and (or 0 1) (not 0)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(and (or 0 1) (not 0)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Boolean operator tests complete!~%")
