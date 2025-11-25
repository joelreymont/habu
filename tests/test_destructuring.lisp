;; Tests for destructuring-bind
(load "run-habu.lisp")

(format t "Test 1 - simple pattern:~%")
(let* ((forms '((destructuring-bind (a b c) '(#x1 #x2 #x3)
                  (+ a b c))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 6)~%~%" result))

(format t "Test 2 - nested pattern:~%")
(let* ((forms '((destructuring-bind (a (b c) d) '(#x1 (#x2 #x3) #x4)
                  (+ a b c d))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected A = 10)~%~%" result))

(format t "Test 3 - with &rest:~%")
(let* ((forms '((destructuring-bind (a b &rest rest) '(#x1 #x2 #x3 #x4)
                  (+ a b (car rest)))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 6 = 1+2+3)~%~%" result))

(format t "Test 4 - single element:~%")
(let* ((forms '((destructuring-bind (x) '(#xFF)
                  x)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected FF)~%~%" result))
