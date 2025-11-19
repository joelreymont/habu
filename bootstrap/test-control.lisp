;;;; Test setq and while loops

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Control Flow ===~%")

;; Test 1: Setq
(format t "~%Test 1: Setq variable mutation~%")
(generate-c-standalone '(let ((x (quote 10)))
                          (progn
                            (setq x (+ x (quote 5)))
                            x))
                      :output-file "/tmp/test_setq.c")
(compile-and-run-c "/tmp/test_setq.c")

;; Test 2: While loop (count down)
(format t "~%Test 2: While loop countdown~%")
(generate-c-standalone '(let ((n (quote 5)))
                          (progn
                            (while (> n (quote 0))
                              (progn
                                (print n)
                                (setq n (- n (quote 1)))))
                            (quote 0)))
                      :output-file "/tmp/test_while.c")
(compile-and-run-c "/tmp/test_while.c")

;; Test 3: Factorial with while loop
(format t "~%Test 3: Factorial with while~%")
(generate-c-standalone '(let ((n (quote 5))
                              (result (quote 1)))
                          (progn
                            (while (> n (quote 1))
                              (progn
                                (setq result (* result n))
                                (setq n (- n (quote 1)))))
                            result))
                      :output-file "/tmp/test_fact_while.c")
(compile-and-run-c "/tmp/test_fact_while.c")

;; Test 4: Sum with while
(format t "~%Test 4: Sum 1 to 10~%")
(generate-c-standalone '(let ((i (quote 1))
                              (sum (quote 0)))
                          (progn
                            (while (<= i (quote 10))
                              (progn
                                (setq sum (+ sum i))
                                (setq i (+ i (quote 1)))))
                            sum))
                      :output-file "/tmp/test_sum.c")
(compile-and-run-c "/tmp/test_sum.c")

(format t "~%~%=== Control Flow Tests Complete ===~%")
(sb-ext:quit)
