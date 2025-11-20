;;;; Test defun and function calls in habu-arm64-codegen.lisp

(load "habu-arm64-codegen.lisp")

;;; Test 1: Simple function definition and call
(format t "~%Test 1: Simple function factorial~%")
(let ((forms '(
  (defun factorial (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))
  (factorial 5))))
  (let ((result (compile-program-with-functions forms)))
    (format t "Compiled successfully!~%")
    (format t "Code size: ~A bytes~%" (length result))))

;;; Test 2: Multiple functions
(format t "~%Test 2: Multiple functions~%")
(let ((forms '(
  (defun add2 (x) (+ x 2))
  (defun mul3 (x) (* x 3))
  (mul3 (add2 5)))))
  (let ((result (compile-program-with-functions forms)))
    (format t "Compiled successfully!~%")
    (format t "Code size: ~A bytes~%" (length result))))

;;; Test 3: Multiple let bindings
(format t "~%Test 3: Multiple let bindings~%")
(let ((forms '(
  (let ((x 1) (y 2) (z 3))
    (+ x (+ y z))))))
  (let ((result (compile-program-with-functions forms)))
    (format t "Compiled successfully!~%")
    (format t "Code size: ~A bytes~%" (length result))))

;;; Test 4: Lambda application
(format t "~%Test 4: Lambda application~%")
(let ((forms '(
  ((lambda (x y) (+ x y)) 10 20))))
  (let ((result (compile-program-with-functions forms)))
    (format t "Compiled successfully!~%")
    (format t "Code size: ~A bytes~%" (length result))))

(format t "~%All tests passed!~%")
