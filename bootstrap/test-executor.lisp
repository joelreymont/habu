;;;; test-executor.lisp - Test code execution

(load "compiler.lisp")
(load "executor.lisp")
(in-package :habu-compiler)

(format t "~%Testing Code Execution~%")
(format t "======================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%"name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Simple arithmetic
(format t "~%[34m1. Test Simple Arithmetic[0m~%")
(format t "============================~%")

(handler-case
    (let ((result (execute-and-untag '(+ 2 3))))
      (test "ADD-2-3" (= result 5) (format nil "Expected 5, got ~D" result)))
  (error (e)
    (test "ADD-2-3" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(* 6 7))))
      (test "MUL-6-7" (= result 42) (format nil "Expected 42, got ~D" result)))
  (error (e)
    (test "MUL-6-7" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(- 10 3))))
      (test "SUB-10-3" (= result 7) (format nil "Expected 7, got ~D" result)))
  (error (e)
    (test "SUB-10-3" nil (format nil "~A" e))))

;; Test 2: Nested expressions
(format t "~%[34m2. Test Nested Expressions[0m~%")
(format t "==============================~%")

(handler-case
    (let ((result (execute-and-untag '(+ (* 2 3) (* 4 5)))))
      (test "NESTED-ARITHMETIC" (= result 26) (format nil "Expected 26, got ~D" result)))
  (error (e)
    (test "NESTED-ARITHMETIC" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(* (+ 1 2) (+ 3 4)))))
      (test "NESTED-PARENS" (= result 21) (format nil "Expected 21, got ~D" result)))
  (error (e)
    (test "NESTED-PARENS" nil (format nil "~A" e))))

;; Test 3: Conditionals
(format t "~%[34m3. Test Conditionals[0m~%")
(format t "=======================~%")

(handler-case
    (let ((result (execute-and-untag '(if t 1 0))))
      (test "IF-TRUE" (= result 1) (format nil "Expected 1, got ~D" result)))
  (error (e)
    (test "IF-TRUE" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(if (< 5 10) 42 99))))
      (test "IF-CONDITION" (= result 42) (format nil "Expected 42, got ~D" result)))
  (error (e)
    (test "IF-CONDITION" nil (format nil "~A" e))))

;; Test 4: Let bindings
(format t "~%[34m4. Test Let Bindings[0m~%")
(format t "=========================~%")

(handler-case
    (let ((result (execute-and-untag '(let ((x 5)) x))))
      (test "LET-SIMPLE" (= result 5) (format nil "Expected 5, got ~D" result)))
  (error (e)
    (test "LET-SIMPLE" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(let ((x 3) (y 4)) (+ x y)))))
      (test "LET-TWO-VARS" (= result 7) (format nil "Expected 7, got ~D" result)))
  (error (e)
    (test "LET-TWO-VARS" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(let ((x 10)) (* x x)))))
      (test "LET-SQUARE" (= result 100) (format nil "Expected 100, got ~D" result)))
  (error (e)
    (test "LET-SQUARE" nil (format nil "~A" e))))

;; Test 5: Lambda expressions
(format t "~%[34m5. Test Lambda Expressions[0m~%")
(format t "=============================~%")

(handler-case
    (let ((result (execute-and-untag '((lambda (x) (* x 2)) 5))))
      (test "LAMBDA-DOUBLE" (= result 10) (format nil "Expected 10, got ~D" result)))
  (error (e)
    (test "LAMBDA-DOUBLE" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '((lambda (x y) (+ x y)) 3 7))))
      (test "LAMBDA-TWO-ARGS" (= result 10) (format nil "Expected 10, got ~D" result)))
  (error (e)
    (test "LAMBDA-TWO-ARGS" nil (format nil "~A" e))))

;; Test 6: Loops
(format t "~%[34m6. Test Loops[0m~%")
(format t "================~%")

(handler-case
    (let ((result (execute-and-untag '(dotimes (i 0) i))))
      (test "DOTIMES-ZERO" (= result 0) (format nil "Expected 0, got ~D" result)))
  (error (e)
    (test "DOTIMES-ZERO" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(dotimes (i 5) i))))
      (test "DOTIMES-FIVE" (= result 0) (format nil "Expected 0 (default result), got ~D" result)))
  (error (e)
    (test "DOTIMES-FIVE" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(dotimes (i 3 42)))))
      (test "DOTIMES-RESULT" (= result 42) (format nil "Expected 42, got ~D" result)))
  (error (e)
    (test "DOTIMES-RESULT" nil (format nil "~A" e))))

;; Test 7: Function calls (using defun)
(format t "~%[34m7. Test Function Calls[0m~%")
(format t "=========================~%")

;; Define a simple function
(defun double (x)
  (* x 2))

(handler-case
    (let ((result (execute-and-untag '((lambda (x) (* x 2)) 21))))
      (test "INLINE-DOUBLE" (= result 42) (format nil "Expected 42, got ~D" result)))
  (error (e)
    (test "INLINE-DOUBLE" nil (format nil "~A" e))))

;; Test 8: Complex expressions
(format t "~%[34m8. Test Complex Expressions[0m~%")
(format t "===============================~%")

(handler-case
    (let ((result (execute-and-untag '(let ((a 2) (b 3))
                                        (let ((c (+ a b)))
                                          (* c c))))))
      (test "NESTED-LET" (= result 25) (format nil "Expected 25, got ~D" result)))
  (error (e)
    (test "NESTED-LET" nil (format nil "~A" e))))

(handler-case
    (let ((result (execute-and-untag '(if (> 10 5)
                                          (+ 20 30)
                                          (- 20 30)))))
      (test "IF-NESTED-EXPR" (= result 50) (format nil "Expected 50, got ~D" result)))
  (error (e)
    (test "IF-NESTED-EXPR" nil (format nil "~A" e))))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%"  *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll execution tests passed![0m~%")

(sb-ext:quit)
