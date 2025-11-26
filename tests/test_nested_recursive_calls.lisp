#!/usr/bin/env sbcl --script
;;; Tests nested recursive calls in argument position
;;; This pattern was broken when arg-spill area was shared

(load "run-habu.lisp")

(defparameter *test-name* "Nested Recursive Calls")
(defparameter *failures* 0)

(defun run-test (name expected forms)
  (format t "~%Test: ~A~%" name)
  (handler-case
      (let ((result (habu-sbcl:compile-and-run-forms forms)))
        (format t "  Result: ~A (expected ~A)~%" result expected)
        (if (= result expected)
            (format t "  PASS~%")
            (progn
              (format t "  *** FAILED~%")
              (incf *failures*))))
    (error (c)
      (format t "  *** ERROR: ~A~%" c)
      (incf *failures*))))

(format t "~%=== ~A ===~%" *test-name*)

;;; Test 1: Simple recursive function
(run-test "Simple recursive (factorial)"
          120
          '((defun fact (n)
              (if (<= n 1)
                  1
                  (* n (fact (- n 1)))))
            (fact 5)))

;;; Test 2: Recursive calls with let* in same function
(run-test "Recursive calls with let* in body"
          17
          '((defun lit-ir (n) (list 'lit n))
            (defun add-ir (a b) (list 'add a b))

            (defun compile-expr (expr)
              (if (numberp expr)
                  (lit-ir expr)
                  (if (consp expr)
                      (if (eq (car expr) '+)
                          (let* ((left (compile-expr (cadr expr)))
                                 (right (compile-expr (caddr expr))))
                            (add-ir left right))
                          (lit-ir 0))
                      (lit-ir 0))))

            (defun eval-ir (ir)
              (let ((tag (car ir)))
                (if (eq tag 'lit)
                    (cadr ir)
                    (if (eq tag 'add)
                        (let* ((left (eval-ir (cadr ir)))
                               (right (eval-ir (caddr ir))))
                          (+ left right))
                        0))))

            (eval-ir (compile-expr '(+ (+ 3 4) (+ 5 5))))))

;;; Test 3: Direct nested recursive calls - key test for the fix
(run-test "Direct nested recursive calls"
          17
          '((defun lit-ir (n) (list 'lit n))
            (defun add-ir (a b) (list 'add a b))
            (defun mul-ir (a b) (list 'mul a b))

            (defun compile-expr (expr)
              (if (numberp expr)
                  (lit-ir expr)
                  (if (consp expr)
                      (let ((op (car expr)))
                        (if (eq op '+)
                            (add-ir (compile-expr (cadr expr))
                                    (compile-expr (caddr expr)))
                            (if (eq op '*)
                                (mul-ir (compile-expr (cadr expr))
                                        (compile-expr (caddr expr)))
                                (lit-ir 0))))
                      (lit-ir 0))))

            (defun eval-ir (ir)
              (let ((tag (car ir)))
                (if (eq tag 'lit)
                    (cadr ir)
                    (if (eq tag 'add)
                        (let* ((left (eval-ir (cadr ir)))
                               (right (eval-ir (caddr ir))))
                          (+ left right))
                        (if (eq tag 'mul)
                            (let* ((left (eval-ir (cadr ir)))
                                   (right (eval-ir (caddr ir))))
                              (* left right))
                            0)))))

            ;; (+ (* 3 4) 5) = 17
            (eval-ir (compile-expr '(+ (* 3 4) 5)))))

;;; Test 4: Triple nesting depth
(run-test "Triple nested recursive calls"
          42
          '((defun lit-ir (n) (list 'lit n))
            (defun add-ir (a b) (list 'add a b))

            (defun compile-expr (expr)
              (if (numberp expr)
                  (lit-ir expr)
                  (if (consp expr)
                      (if (eq (car expr) '+)
                          (add-ir (compile-expr (cadr expr))
                                  (compile-expr (caddr expr)))
                          (lit-ir 0))
                      (lit-ir 0))))

            (defun eval-ir (ir)
              (let ((tag (car ir)))
                (if (eq tag 'lit)
                    (cadr ir)
                    (if (eq tag 'add)
                        (let* ((left (eval-ir (cadr ir)))
                               (right (eval-ir (caddr ir))))
                          (+ left right))
                        0))))

            ;; (+ (+ 10 10) (+ 10 12)) = 20 + 22 = 42
            (eval-ir (compile-expr '(+ (+ 10 10) (+ 10 12))))))

;;; Test 5: Three arguments with nested recursive calls
(run-test "Three args with nested calls"
          14
          '((defun f (a b c)
              (+ a (+ b c)))

            (defun sum-to (n)
              (if (= n 0)
                  0
                  (+ n (sum-to (- n 1)))))

            ;; sum-to(1)=1, sum-to(2)=3, sum-to(4)=10
            ;; f(1, 3, 10) = 1 + (3 + 10) = 14
            (f (sum-to 1) (sum-to 2) (sum-to 4))))

;;; Test 6: Deeply nested recursive function in argument position
(run-test "Deep nesting with fib"
          89
          '((defun fib (n)
              (if (<= n 1)
                  n
                  (+ (fib (- n 1)) (fib (- n 2)))))
            (fib 11)))

;;; Summary
(format t "~%=== Summary ===~%")
(if (= *failures* 0)
    (format t "All tests PASSED~%")
    (progn
      (format t "~A test(s) FAILED~%" *failures*)
      (sb-ext:quit :unix-status 1)))
