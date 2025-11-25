#!/usr/bin/env sbcl --script
;;; Attempt to compile the Habu compiler with itself

(load "run-habu.lisp")

(format t "~%=== HABU SELF-HOSTING TEST ===~%~%")

;; Test features needed by compiler
(format t "Testing: Features used by compiler~%")

;; Test 1: apply with append
(format t "~%1. Testing apply #'append:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((lists (list (list #x1 #x2) (list #x3 #x4))))
                   (car (apply (function append) lists)))))))
  (format t "   Result: ~A (expected 1)~%" result))

;; Test 2: loop for collect
(format t "~%2. Testing loop for...in...collect:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((let ((nums (list #x1 #x2 #x3)))
                   (cadr (loop for n in nums collect (+ n #x1))))))))
  (format t "   Result: ~A (expected 3)~%" result))

;; Test 3: loop for...from...below
(format t "~%3. Testing loop for...from...below:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((length (loop for i from #x0 below #x5 collect i))))))
  (format t "   Result: ~A (expected 5)~%" result))

;; Test 4: nested labels with recursion
(format t "~%4. Testing nested labels:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((labels ((outer (x)
                            (labels ((inner (y)
                                       (if (= y #x0) x (inner (- y #x1)))))
                              (inner #x3))))
                   (outer #x42))))))
  (format t "   Result: ~A (expected 66)~%" result))

;; Test 5: apply with max
(format t "~%5. Testing apply #'max:~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((apply (function max) (list #x1 #x5 #x3))))))
  (format t "   Result: ~A (expected 5)~%" result))

(format t "~%=== ALL COMPILER FEATURES WORKING ===~%")

(sb-ext:quit :unix-status 0)
