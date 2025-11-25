;; Tests for set operations: union, intersection, set-difference, subsetp, adjoin
(load "run-habu.lisp")

(format t "Test 1 - union:~%")
(let* ((forms '((length (union '(#x1 #x2 #x3) '(#x2 #x3 #x4)))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 4)~%~%" result))

(format t "Test 2 - intersection:~%")
(let* ((forms '((length (intersection '(#x1 #x2 #x3) '(#x2 #x3 #x4)))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 2)~%~%" result))

(format t "Test 3 - set-difference:~%")
(let* ((forms '((length (set-difference '(#x1 #x2 #x3) '(#x2)))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 2)~%~%" result))

(format t "Test 4 - subsetp true:~%")
(let* ((forms '((if (subsetp '(#x1 #x2) '(#x1 #x2 #x3)) #x1 #x0)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 1)~%~%" result))

(format t "Test 5 - subsetp false:~%")
(let* ((forms '((if (subsetp '(#x1 #x4) '(#x1 #x2 #x3)) #x1 #x0)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 0)~%~%" result))

(format t "Test 6 - adjoin (new element):~%")
(let* ((forms '((length (adjoin #x4 '(#x1 #x2 #x3)))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 4)~%~%" result))

(format t "Test 7 - adjoin (existing element):~%")
(let* ((forms '((length (adjoin #x2 '(#x1 #x2 #x3)))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 3)~%~%" result))
