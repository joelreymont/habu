#!/usr/bin/env sbcl --script
;;; Debug reduce failure

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Simple recursive labels with funcall of param
(run-test "simple-rec-funcall"
          '((labels ((rec (fn n)
                       (if (= n #x0)
                           #x0
                           (+ (funcall fn n) (rec fn (- n #x1))))))
              (rec (lambda (x) x) #x3)))
          #x6)  ; 3+2+1=6

;; Test 2: Reduce-style iteration with funcall before recursive call
(run-test "reduce-style-iter"
          '((labels ((iter (fn acc lst)
                      (if (null lst)
                          acc
                          (iter fn (funcall fn acc (car lst)) (cdr lst)))))
              (iter (lambda (a b) (+ a b)) #x0 (list #x1 #x2 #x3))))
          #x6)

;; Test 3: Original reduce-sum test
(run-test "reduce-sum"
          '((reduce (lambda (a b) (+ a b)) (list #x1 #x2 #x3 #x4)))
          #xA)  ; 1+2+3+4 = 10

(format t "All reduce debug tests passed~%")
(sb-ext:quit :unix-status 0)
