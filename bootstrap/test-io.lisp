;;;; Test I/O system

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing I/O System ===~%")

;; Test 1: Write and read a file
(format t "~%Test 1: Write file~%")
(generate-c-standalone '(write-file "/tmp/habu-test.txt" "Hello from Habu!")
                      :output-file "/tmp/test_write.c")
(compile-and-run-c "/tmp/test_write.c")

(format t "~%Test 2: Read file~%")
(generate-c-standalone '(print (read-file "/tmp/habu-test.txt"))
                      :output-file "/tmp/test_read.c")
(compile-and-run-c "/tmp/test_read.c")

;; Test 3: Print values
(format t "~%Test 3: Print value~%")
(generate-c-standalone '(print (+ (quote 10) (quote 32)))
                      :output-file "/tmp/test_print.c")
(compile-and-run-c "/tmp/test_print.c")

(format t "~%~%=== I/O Tests Complete ===~%")
(sb-ext:quit)
