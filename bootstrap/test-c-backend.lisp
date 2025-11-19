;;;; Test C backend with simple expressions

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing C Backend ===~%")

;; Test 1: Simple arithmetic
(format t "~%Test 1: (+ 40 2)~%")
(generate-c-standalone '(+ (quote 40) (quote 2))
                      :output-file "/tmp/test_add.c")
(compile-and-run-c "/tmp/test_add.c")

;; Test 2: Cons cell
(format t "~%~%Test 2: (cons 1 2)~%")
(generate-c-standalone '(cons (quote 1) (quote 2))
                      :output-file "/tmp/test_cons.c")
(compile-and-run-c "/tmp/test_cons.c")

;; Test 3: Car of cons
(format t "~%~%Test 3: (car (cons 42 99))~%")
(generate-c-standalone '(car (cons (quote 42) (quote 99)))
                      :output-file "/tmp/test_car.c")
(compile-and-run-c "/tmp/test_car.c")

(format t "~%~%=== C Backend Tests Complete ===~%")
(sb-ext:quit)
