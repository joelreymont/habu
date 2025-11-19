;;;; Test extended C backend features

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Extended C Backend ===~%")

;; Test 1: If expression
(format t "~%Test 1: (if (= 5 5) 42 99)~%")
(generate-c-standalone '(if (= (quote 5) (quote 5)) (quote 42) (quote 99))
                      :output-file "/tmp/test_if.c")
(compile-and-run-c "/tmp/test_if.c")

;; Test 2: List construction
(format t "~%~%Test 2: (list 1 2 3)~%")
(generate-c-standalone '(list (quote 1) (quote 2) (quote 3))
                      :output-file "/tmp/test_list.c")
(compile-and-run-c "/tmp/test_list.c")

;; Test 3: Nested operations
(format t "~%~%Test 3: (car (cdr (list 10 20 30)))~%")
(generate-c-standalone '(car (cdr (list (quote 10) (quote 20) (quote 30))))
                      :output-file "/tmp/test_nested.c")
(compile-and-run-c "/tmp/test_nested.c")

;; Test 4: Comparison
(format t "~%~%Test 4: (< 10 20)~%")
(generate-c-standalone '(< (quote 10) (quote 20))
                      :output-file "/tmp/test_cmp.c")
(compile-and-run-c "/tmp/test_cmp.c")

(format t "~%~%=== Extended C Backend Tests Complete ===~%")
(sb-ext:quit)
