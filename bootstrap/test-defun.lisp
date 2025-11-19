;;;; Test defun support

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Defun Support ===~%")

;; Test 1: Simple function
(format t "~%Test 1: (defun double (x) (* x 2))~%")
(generate-c-standalone '(progn
                          (defun double (x) (* x (quote 2)))
                          (double (quote 21)))
                      :output-file "/tmp/test_defun1.c")
(compile-and-run-c "/tmp/test_defun1.c")

;; Test 2: Function with multiple parameters
(format t "~%Test 2: (defun add3 (a b c) (+ a (+ b c)))~%")
(generate-c-standalone '(progn
                          (defun add3 (a b c) (+ a (+ b c)))
                          (add3 (quote 10) (quote 20) (quote 30)))
                      :output-file "/tmp/test_defun2.c")
(compile-and-run-c "/tmp/test_defun2.c")

;; Test 3: Recursive function (factorial)
(format t "~%Test 3: Factorial~%")
(generate-c-standalone '(progn
                          (defun fact (n)
                            (if (= n (quote 0))
                                (quote 1)
                                (* n (fact (- n (quote 1))))))
                          (fact (quote 5)))
                      :output-file "/tmp/test_factorial.c")
(compile-and-run-c "/tmp/test_factorial.c")

;; Test 4: Multiple functions
(format t "~%Test 4: Multiple functions~%")
(generate-c-standalone '(progn
                          (defun square (x) (* x x))
                          (defun sum-of-squares (a b)
                            (+ (square a) (square b)))
                          (sum-of-squares (quote 3) (quote 4)))
                      :output-file "/tmp/test_multi.c")
(compile-and-run-c "/tmp/test_multi.c")

(format t "~%~%=== Defun Tests Complete ===~%")
(sb-ext:quit)
