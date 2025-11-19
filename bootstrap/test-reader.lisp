;;;; Test standalone S-expression reader

(load (merge-pathnames "compiler.lisp" *load-truename*))
(load (merge-pathnames "c-backend.lisp" *load-truename*))

(in-package :habu-compiler)

(format t "~%=== Testing Standalone Reader ===~%")

;; Test 1: Read a number
(format t "~%Test 1: Read number~%")
(generate-c-standalone '(let ((input "42"))
                          (read-from-string input))
                      :output-file "/tmp/test_read_num.c")
(compile-and-run-c "/tmp/test_read_num.c")

;; Test 2: Read a list
(format t "~%Test 2: Read list~%")
(generate-c-standalone '(let ((input "(1 2 3)"))
                          (let ((expr (read-from-string input)))
                            (car expr)))
                      :output-file "/tmp/test_read_list.c")
(compile-and-run-c "/tmp/test_read_list.c")

;; Test 3: Read nested list
(format t "~%Test 3: Read nested list~%")
(generate-c-standalone '(let ((input "((10 20) 30)"))
                          (let ((expr (read-from-string input)))
                            (let ((first (car expr)))
                              (car first))))
                      :output-file "/tmp/test_read_nested.c")
(compile-and-run-c "/tmp/test_read_nested.c")

;; Test 4: Read quoted expression
(format t "~%Test 4: Read quoted expression~%")
(generate-c-standalone '(let ((input "'(1 2 3)"))
                          (let ((expr (read-from-string input)))
                            (car expr)))  ; Should be 'quote
                      :output-file "/tmp/test_read_quote.c")
(compile-and-run-c "/tmp/test_read_quote.c")

(format t "~%~%=== Reader Tests Complete ===~%")
(sb-ext:quit)
