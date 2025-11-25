;; Tests for list mapping functions: mapcan, maplist, mapcon, mapl, every, some, notevery, notany
(load "run-habu.lisp")

(format t "Test 1 - mapcan:~%")
(let* ((forms '((mapcan (lambda (x) (list x x)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected cons of 1,1,2,2,3,3)~%~%" result))

(format t "Test 2 - maplist (gets cdrs):~%")
(let* ((forms '((maplist (lambda (x) (car x)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected cons of 1,2,3)~%~%" result))

(format t "Test 3 - every true:~%")
(let* ((forms '((every (lambda (x) (> x #x0)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 1)~%~%" result))

(format t "Test 4 - every false:~%")
(let* ((forms '((every (lambda (x) (> x #x1)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 0)~%~%" result))

(format t "Test 5 - some true:~%")
(let* ((forms '((some (lambda (x) (if (= x #x2) #xAA #x0)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected AA)~%~%" result))

(format t "Test 6 - some false:~%")
(let* ((forms '((some (lambda (x) (> x #x5)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 0)~%~%" result))

(format t "Test 7 - notevery true:~%")
(let* ((forms '((notevery (lambda (x) (> x #x1)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 1)~%~%" result))

(format t "Test 8 - notevery false:~%")
(let* ((forms '((notevery (lambda (x) (> x #x0)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 0)~%~%" result))

(format t "Test 9 - notany true:~%")
(let* ((forms '((notany (lambda (x) (> x #x5)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 1)~%~%" result))

(format t "Test 10 - notany false:~%")
(let* ((forms '((notany (lambda (x) (= x #x2)) '(#x1 #x2 #x3))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 0)~%~%" result))
