;;;; Test closure variable capture

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Closures with Variable Capture ===~%")

;; Test 1: Simple closure - add n
(format t "~%Test 1: Make-adder closure~%")
(generate-c-standalone '(let ((make-adder (lambda (n) (lambda (x) (+ x n)))))
                          (let ((add5 (funcall make-adder (quote 5))))
                            (funcall add5 (quote 10))))
                      :output-file "/tmp/test_closure_add.c")
(compile-and-run-c "/tmp/test_closure_add.c")

;; Test 2: Multiple captured variables
(format t "~%Test 2: Closure with multiple captured variables~%")
(generate-c-standalone '(let ((a (quote 10))
                              (b (quote 20)))
                          (let ((adder (lambda (x) (+ (+ x a) b))))
                            (funcall adder (quote 5))))
                      :output-file "/tmp/test_closure_multi.c")
(compile-and-run-c "/tmp/test_closure_multi.c")

;; Test 3: Nested closures (closure returning closure)
(format t "~%Test 3: Nested closure - multiplier maker~%")
(generate-c-standalone '(let ((make-multiplier (lambda (n) (lambda (x) (* x n)))))
                          (let ((times3 (funcall make-multiplier (quote 3))))
                            (funcall times3 (quote 7))))
                      :output-file "/tmp/test_closure_nested.c")
(compile-and-run-c "/tmp/test_closure_nested.c")

;; Test 4: Closure with computation
(format t "~%Test 4: Closure computing with captured value~%")
(generate-c-standalone '(let ((base (quote 100)))
                          (let ((calculator (lambda (x) (+ (* x (quote 2)) base))))
                            (funcall calculator (quote 50))))
                      :output-file "/tmp/test_closure_compute.c")
(compile-and-run-c "/tmp/test_closure_compute.c")

(format t "~%~%=== Closure Tests Complete ===~%")
(sb-ext:quit)
