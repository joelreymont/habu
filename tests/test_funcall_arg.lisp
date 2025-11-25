#!/usr/bin/env sbcl --script
;;; Test funcall result as argument to another funcall

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Simple: funcall result as arg to another funcall
(run-test "funcall-as-arg-simple"
          '((let ((f (lambda (a b) (+ a b))))
              (let ((g (lambda () #x5)))
                (funcall f (funcall g) #x3))))
          #x8)  ; 5+3=8

;; Funcall as arg with multiple args after
(run-test "funcall-as-arg-2"
          '((let ((f (lambda (a b c) (+ a (+ b c)))))
              (let ((g (lambda () #x5)))
                (funcall f (funcall g) #x3 #x2))))
          #xA)  ; 5+3+2=10

;; Funcall as second arg
(run-test "funcall-as-second-arg"
          '((let ((f (lambda (a b) (+ a b))))
              (let ((g (lambda () #x5)))
                (funcall f #x3 (funcall g)))))
          #x8)  ; 3+5=8

;; Two funcalls as args
(run-test "two-funcalls-as-args"
          '((let ((f (lambda (a b) (+ a b))))
              (let ((g (lambda () #x5)))
                (let ((h (lambda () #x3)))
                  (funcall f (funcall g) (funcall h))))))
          #x8)  ; 5+3=8

(format t "All funcall-as-arg tests passed~%")
(sb-ext:quit :unix-status 0)
