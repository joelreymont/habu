;;;; Test lambda and closure functionality

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Lambda and Closures ===~%")

;; Test 1: Simple lambda with funcall
(format t "~%Test 1: Simple lambda - double function~%")
(generate-c-standalone '(let ((double (lambda (x) (* x (quote 2)))))
                          (funcall double (quote 21)))
                      :output-file "/tmp/test_lambda_simple.c")
(compile-and-run-c "/tmp/test_lambda_simple.c")

;; Test 2: Lambda with multiple parameters
(format t "~%Test 2: Lambda with two parameters - add~%")
(generate-c-standalone '(let ((add (lambda (x y) (+ x y))))
                          (funcall add (quote 15) (quote 27)))
                      :output-file "/tmp/test_lambda_add.c")
(compile-and-run-c "/tmp/test_lambda_add.c")

;; Test 3: Passing lambda to a function
(format t "~%Test 3: Apply function that takes a lambda~%")
(generate-c-standalone '(progn
                          (defun apply-twice (f x)
                            (funcall f (funcall f x)))
                          (let ((inc (lambda (n) (+ n (quote 1)))))
                            (apply-twice inc (quote 10))))
                      :output-file "/tmp/test_lambda_apply.c")
(compile-and-run-c "/tmp/test_lambda_apply.c")

;; Test 4: Nested lambdas without closure (simpler test)
(format t "~%Test 4: Nested lambda returning constant~%")
(generate-c-standalone '(let ((make-const (lambda (n) (lambda (x) (* x (quote 2))))))
                          (let ((doubler (funcall make-const (quote 999))))
                            (funcall doubler (quote 10))))
                      :output-file "/tmp/test_lambda_nested.c")
(compile-and-run-c "/tmp/test_lambda_nested.c")

(format t "~%~%=== Lambda and Closure Tests Complete ===~%")
(sb-ext:quit)
