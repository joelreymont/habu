;; Tests for do/do* iteration and other CL spec features
(load "run-habu.lisp")

(format t "Test 1 - do basic iteration:~%")
(let* ((forms '((do ((i #x0 (+ i #x1))
                     (sum #x0 (+ sum i)))
                    ((>= i #x5) sum))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected A = 0+1+2+3+4)~%~%" result))

(format t "Test 2 - do with result form:~%")
(let* ((forms '((do ((x #xA (- x #x1)))
                    ((= x #x0) #xFF))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected FF)~%~%" result))

(format t "Test 3 - do with accumulator in step:~%")
(let* ((forms '((do ((i #x1 (+ i #x1))
                     (sum #x0 (+ sum i)))  ; accumulate in step form
                    ((> i #x5) sum))))  ; 1+2+3+4+5 = 15
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected F = 15)~%~%" result))

(format t "Test 4 - the type declaration (stub):~%")
(let* ((forms '((the fixnum (+ #x3 #x4))))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 7)~%~%" result))

(format t "Test 5 - coerce (stub):~%")
(let* ((forms '((coerce #x42 'character)))
       (result (habu-sbcl:compile-and-run-forms forms)))
  (format t "  Result: ~X (expected 42)~%~%" result))
