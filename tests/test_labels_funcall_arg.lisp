#!/usr/bin/env sbcl --script
;;; Test labels with funcall result as argument to recursive call

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Labels without funcall arg - should pass
(run-test "labels-no-funcall-arg"
          '((labels ((rec (n)
                       (if (= n #x0)
                           #x0
                           (+ n (rec (- n #x1))))))
              (rec #x3)))
          #x6)  ; 3+2+1=6

;; Labels with funcall AFTER recursive call result - should pass (from previous tests)
(run-test "labels-funcall-after-rec"
          '((labels ((rec (fn n)
                       (if (= n #x0)
                           #x0
                           (+ (funcall fn n) (rec fn (- n #x1))))))
              (rec (lambda (x) x) #x3)))
          #x6)

;; Labels with funcall AS argument - this might fail
(run-test "labels-funcall-as-arg"
          '((labels ((rec (fn acc n)
                       (if (= n #x0)
                           acc
                           (rec fn (funcall fn acc n) (- n #x1)))))
              (rec (lambda (a b) (+ a b)) #x0 #x3)))
          #x6)  ; 0+3+2+1=6

;; Even simpler: rec with param passed through funcall
(run-test "labels-param-thru-funcall"
          '((labels ((rec (fn n)
                       (if (= n #x0)
                           #x0
                           (rec fn (funcall fn n)))))
              (rec (lambda (x) (- x #x1)) #x3)))
          #x0)  ; countdown 3->2->1->0

;; Test: is it the funcall that fails, or is it accessing params after funcall?
(run-test "labels-access-param-after-inner-funcall"
          '((labels ((rec (fn n)
                       (if (= n #x0)
                           #x0
                           (let ((temp (funcall fn n)))
                             (rec fn (- temp #x1))))))
              (rec (lambda (x) x) #x3)))
          #x0)

(format t "All labels-funcall-arg tests passed~%")
(sb-ext:quit :unix-status 0)
