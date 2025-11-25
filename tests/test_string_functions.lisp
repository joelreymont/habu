;; Tests for string construction and manipulation functions
(load "run-habu.lisp")

(format t "Test 1 - string-length:~%")
(let* ((forms '((string-length "hello")))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 5)~%~%" result))

(format t "Test 2 - string-ref:~%")
(let* ((forms '((string-ref "hello" #x0)))  ; 'h' = #x68
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 68)~%~%" result))

(format t "Test 3 - string=:~%")
(let* ((forms '((if (string= "abc" "abc") #x1 #x0)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 1)~%~%" result))

(format t "Test 4 - string= (not equal):~%")
(let* ((forms '((if (string= "abc" "abd") #x1 #x0)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 0)~%~%" result))

(format t "Test 5 - write-to-string:~%")
(let* ((forms '((string-length (write-to-string #x7B))))  ; 123 -> "123" -> len 3
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 3)~%~%" result))

(format t "Test 6 - subseq:~%")
(let* ((forms '((string-length (subseq "hello" #x1 #x4))))  ; "ell" -> len 3
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 3)~%~%" result))
