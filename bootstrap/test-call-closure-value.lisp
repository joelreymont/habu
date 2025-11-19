;;;; Test calling a closure value via funcall

(load "compiler.lisp")
(in-package :habu-compiler)

(initialize-runtime-integration)

(format t "~%Testing Calling Closure Values~%")
(format t "================================~%~%")

;; Test: Create a closure in let, then call it via funcall
(format t "Test: Create closure then call it~%")

(handler-case
    (let ((code (compile-expression '(let ((x 42))
                                       (let ((f (lambda (y) (+ x y))))
                                         (funcall f 20)))
                                    :arch :x86_64)))
      (format t "[32m✓[0m Test compiled successfully~%")
      (format t "   Generated ~D bytes~%" (length code)))
  (error (e)
    (format t "[31m✗[0m Error: ~A~%" e)
    (sb-ext:quit :unix-status 1)))

(sb-ext:quit)
