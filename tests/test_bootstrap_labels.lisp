;;; Test labels/flet support in bootstrap compiler
(load "bootstrap/compiler.lisp")
(in-package :habu)

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-eval (name source expected)
  (handler-case
    (let* ((forms (read-all source))
           (result (nc-eval-forms forms)))
      (if (eql result expected)
          (progn
            (format t "[PASS] ~A = ~A~%" name result)
            (incf *tests-passed*))
          (progn
            (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
            (incf *tests-failed*))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *tests-failed*))))

(format t "~%=== Bootstrap Labels Tests ===~%~%")

;; Simple recursion
(test-eval "labels-fact5"
  "(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5))"
  120)

(test-eval "labels-fact6"
  "(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 6))"
  720)

;; Tail-recursive with accumulator
(test-eval "labels-sum"
  "(labels ((sum-to (n acc) (if (= n 0) acc (sum-to (- n 1) (+ acc n))))) (sum-to 10 0))"
  55)

;; Mutual recursion
(test-eval "labels-even"
  "(labels ((is-even (n) (if (= n 0) 1 (is-odd (- n 1)))) (is-odd (n) (if (= n 0) 0 (is-even (- n 1))))) (is-even 10))"
  1)

(test-eval "labels-odd"
  "(labels ((is-even (n) (if (= n 0) 1 (is-odd (- n 1)))) (is-odd (n) (if (= n 0) 0 (is-even (- n 1))))) (is-odd 10))"
  0)

;; Nested labels
(test-eval "labels-nested"
  "(labels ((outer (x) (labels ((inner (y) (* y y))) (+ x (inner x))))) (outer 3))"
  12)

;; Labels with multiple body forms
(test-eval "labels-multi-body"
  "(labels ((foo (x) (+ x 1) (* x 2))) (foo 5))"
  10)

;; Labels in let binding
(test-eval "labels-in-let"
  "(let ((x 10)) (labels ((add-x (y) (+ x y))) (add-x 5)))"
  15)

;; flet - non-recursive local functions
(test-eval "flet-simple"
  "(flet ((double (x) (* x 2))) (double 7))"
  14)

(test-eval "flet-multi"
  "(flet ((add1 (x) (+ x 1)) (mul2 (x) (* x 2))) (mul2 (add1 3)))"
  8)

(format t "~%=== Results: ~A passed, ~A failed ===~%" *tests-passed* *tests-failed*)
(when (> *tests-failed* 0)
  (sb-ext:exit :code 1))
