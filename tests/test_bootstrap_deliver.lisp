;;; Test bootstrap delivery: compile Lisp to standalone executables
(load "bootstrap/deliver.lisp")
(in-package :habu)

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun run-executable (path)
  "Run executable and return the result value"
  (let* ((cmd (format nil "~A 2>&1" path))
         (output (with-output-to-string (s)
                   (sb-ext:run-program "/bin/sh" (list "-c" cmd)
                                       :output s :error :output))))
    ;; Extract numeric result from "Result: N" output
    (let ((pos (search "Result: " output)))
      (when pos
        (parse-integer output :start (+ pos 8) :junk-allowed t)))))

(defun test-deliver (name source expected)
  "Deliver source to executable, run it, check result"
  (let* ((path (format nil "/tmp/test_~A" name)))
    (handler-case
        (progn
          (nc-deliver source path)
          (let ((result (run-executable path)))
            (if (eql result expected)
                (progn
                  (format t "PASS: ~A = ~A~%" name result)
                  (incf *tests-passed*))
                (progn
                  (format t "FAIL: ~A expected ~A got ~A~%" name expected result)
                  (incf *tests-failed*)))
            ;; Clean up
            (delete-file path)))
      (error (e)
        (format t "FAIL: ~A error: ~A~%" name e)
        (incf *tests-failed*)))))

(format t "~%=== Bootstrap Delivery Tests ===~%~%")

;; Test 1: Simple arithmetic
(test-deliver "arith" "(+ 10 20)" 30)

;; Test 2: Nested arithmetic
(test-deliver "nested" "(* (+ 2 3) (- 10 4))" 30)

;; Test 3: Simple function
(test-deliver "square" "(defun square (x) (* x x)) (square 7)" 49)

;; Test 4: Recursive factorial
(test-deliver "fact6" "(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))) (fact 6)" 720)

;; Test 5: Fibonacci
(test-deliver "fib10" "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)" 55)

;; Test 6: Let binding
(test-deliver "let1" "(let ((x 5) (y 7)) (+ (* x x) (* y y)))" 74)

;; Test 7: Let* binding
(test-deliver "letstar" "(let* ((x 3) (y (* x x))) (+ x y))" 12)

;; Test 8: Multiple functions
(test-deliver "multi" "(defun double (x) (* x 2)) (defun quad (x) (double (double x))) (quad 5)" 20)

;; Test 9: Comparison operators
(test-deliver "cmp" "(if (< 3 5) 100 200)" 100)

;; Test 10: Cond expression
(test-deliver "cond1" "(cond ((< 1 0) 1) ((< 0 1) 2) (t 3))" 2)

(format t "~%Results: ~A passed, ~A failed~%" *tests-passed* *tests-failed*)
(when (> *tests-failed* 0)
  (sb-ext:exit :code 1))
