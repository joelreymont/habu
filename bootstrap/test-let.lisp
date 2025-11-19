;;;; Test let bindings

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Let Bindings ===~%")

;; Test 1: Simple let
(format t "~%Test 1: (let ((x 10) (y 20)) (+ x y))~%")
(generate-c-standalone '(let ((x (quote 10)) (y (quote 20))) 
                           (+ x y))
                      :output-file "/tmp/test_let1.c")
(compile-and-run-c "/tmp/test_let1.c")

;; Test 2: Nested let
(format t "~%Test 2: Nested let~%")
(generate-c-standalone '(let ((x (quote 5)))
                          (let ((y (quote 10)))
                            (+ x y)))
                      :output-file "/tmp/test_let2.c")
(compile-and-run-c "/tmp/test_let2.c")

;; Test 3: Let with cons
(format t "~%Test 3: Let with cons~%")
(generate-c-standalone '(let ((pair (cons (quote 1) (quote 2))))
                          (car pair))
                      :output-file "/tmp/test_let3.c")
(compile-and-run-c "/tmp/test_let3.c")

;; Test 4: Progn
(format t "~%Test 4: Progn~%")
(generate-c-standalone '(progn
                          (write-file "/tmp/test.txt" "first")
                          (write-file "/tmp/test.txt" "second")
                          (read-file "/tmp/test.txt"))
                      :output-file "/tmp/test_progn.c")
(compile-and-run-c "/tmp/test_progn.c")

(format t "~%~%=== Let Bindings Tests Complete ===~%")
(sb-ext:quit)
