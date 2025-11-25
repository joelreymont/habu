#!/usr/bin/env sbcl --script
;;; Reader quasiquote/unquote smoke returning fixnum via car.

(load "run-habu.lisp")

(let* ((forms '((defun qq-test () (car `(,(+ #x1 #x3)))) (qq-test)))
       (expected #x4))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
