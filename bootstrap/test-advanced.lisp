;;;; Test advanced features

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Advanced Features ===~%")

;; Test 1: Cond
(format t "~%Test 1: Cond expression~%")
(generate-c-standalone '(let ((x (quote 5)))
                          (cond
                            ((< x (quote 0)) (quote -1))
                            ((= x (quote 0)) (quote 0))
                            (t (quote 1))))
                      :output-file "/tmp/test_cond.c")
(compile-and-run-c "/tmp/test_cond.c")

;; Test 2: Vectors
(format t "~%Test 2: Vector creation and access~%")
(generate-c-standalone '(let ((v (make-vector (quote 5))))
                          (progn
                            (vector-set v (quote 0) (quote 10))
                            (vector-set v (quote 1) (quote 20))
                            (vector-set v (quote 2) (quote 30))
                            (vector-ref v (quote 1))))
                      :output-file "/tmp/test_vector.c")
(compile-and-run-c "/tmp/test_vector.c")

;; Test 3: Complex cond with multiple conditions
(format t "~%Test 3: Grade calculator with cond~%")
(generate-c-standalone '(progn
                          (defun grade (score)
                            (cond
                              ((>= score (quote 90)) (quote 65))  ; 'A' = 65
                              ((>= score (quote 80)) (quote 66))  ; 'B' = 66
                              ((>= score (quote 70)) (quote 67))  ; 'C' = 67
                              (t (quote 70))))                    ; 'F' = 70
                          (grade (quote 85)))
                      :output-file "/tmp/test_grade.c")
(compile-and-run-c "/tmp/test_grade.c")

;; Test 4: Fibonacci with vectors (memoization)
(format t "~%Test 4: Build fibonacci sequence in vector~%")
(generate-c-standalone '(let ((v (make-vector (quote 10)))
                              (i (quote 0)))
                          (progn
                            (vector-set v (quote 0) (quote 0))
                            (vector-set v (quote 1) (quote 1))
                            (setq i (quote 2))
                            (while (< i (quote 10))
                              (progn
                                (vector-set v i
                                  (+ (vector-ref v (- i (quote 1)))
                                     (vector-ref v (- i (quote 2)))))
                                (setq i (+ i (quote 1)))))
                            (vector-ref v (quote 9))))
                      :output-file "/tmp/test_fib_vec.c")
(compile-and-run-c "/tmp/test_fib_vec.c")

(format t "~%~%=== Advanced Features Tests Complete ===~%")
(sb-ext:quit)
