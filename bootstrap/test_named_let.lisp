;;;; Test named-let for local recursion

(load "compiler.lisp")
(in-package :habu-compiler)

(format t "Testing named-let for recursion~%~%")

;;; Test simple countdown
(format t "Testing: (let loop ((n 5)) (if (= n 0) 100 (loop (- n 1))))~%")
(handler-case
    (let ((code (compile-expression '(let loop ((n 5))
                                       (if (= n 0) 100 (loop (- n 1))))
                                    :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let loop ((n 5))
                                       (if (= n 0) 100 (loop (- n 1))))
                                    :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test accumulator pattern
(format t "~%Testing: (let loop ((n 5) (acc 0)) (if (= n 0) acc (loop (- n 1) (+ acc n))))~%")
(handler-case
    (let ((code (compile-expression '(let loop ((n 5) (acc 0))
                                       (if (= n 0) acc (loop (- n 1) (+ acc n))))
                                    :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let loop ((n 5) (acc 0))
                                       (if (= n 0) acc (loop (- n 1) (+ acc n))))
                                    :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test with multiple recursive calls
(format t "~%Testing: (let loop ((n 3)) (if (< n 2) n (+ (loop (- n 1)) (loop (- n 2)))))~%")
(handler-case
    (let ((code (compile-expression '(let loop ((n 3))
                                       (if (< n 2) n (+ (loop (- n 1)) (loop (- n 2)))))
                                    :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let loop ((n 3))
                                       (if (< n 2) n (+ (loop (- n 1)) (loop (- n 2)))))
                                    :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

;;; Test regular let still works
(format t "~%Testing regular let: (let ((x 5)) (+ x 10))~%")
(handler-case
    (let ((code (compile-expression '(let ((x 5)) (+ x 10)) :arch :x86_64)))
      (format t "  x86_64: ~D bytes~%" (length code)))
  (error (e)
    (format t "  x86_64 ERROR: ~A~%" e)))

(handler-case
    (let ((code (compile-expression '(let ((x 5)) (+ x 10)) :arch :arm64)))
      (format t "  ARM64:  ~D bytes~%" (length code)))
  (error (e)
    (format t "  ARM64 ERROR: ~A~%" e)))

(format t "~%Named-let tests complete!~%")
