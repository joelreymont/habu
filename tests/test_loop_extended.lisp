;; Tests for extended loop forms: when/unless, sum, count, maximize/minimize, repeat
(load "run-habu.lisp")

(format t "Test 1 - loop for...in...when...collect:~%")
(let* ((forms '((length (loop for x in '(#x1 #x2 #x3 #x4 #x5) when (> x #x2) collect x))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 3 = count of 3,4,5)~%~%" result))

(format t "Test 2 - loop for...in...unless...collect:~%")
(let* ((forms '((length (loop for x in '(#x1 #x2 #x3 #x4 #x5) unless (> x #x2) collect x))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 2 = count of 1,2)~%~%" result))

(format t "Test 3 - loop sum:~%")
(let* ((forms '((loop for x in '(#x1 #x2 #x3 #x4 #x5) sum x)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected F = 15)~%~%" result))

(format t "Test 4 - loop count:~%")
(let* ((forms '((loop for x in '(#x1 #x2 #x3 #x4 #x5) count (> x #x2))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 3)~%~%" result))

(format t "Test 5 - loop maximize:~%")
(let* ((forms '((loop for x in '(#x3 #x1 #x5 #x2 #x4) maximize x)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 5)~%~%" result))

(format t "Test 6 - loop minimize:~%")
(let* ((forms '((loop for x in '(#x3 #x1 #x5 #x2 #x4) minimize x)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 1)~%~%" result))

(format t "Test 7 - loop repeat collect:~%")
(let* ((forms '((length (loop repeat #x5 collect #x1))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 5)~%~%" result))

(format t "Test 8 - loop for...from...below...do with direct return:~%")
(let* ((forms '((labels ((sum-range (n)
                           (let ((acc #x0))
                             (loop for i from #x0 below n do (setq acc (+ acc i)))
                             acc)))
                  (sum-range #x5))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected A = 0+1+2+3+4)~%~%" result))
