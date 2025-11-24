#!/usr/bin/env sbcl --script
;;; Verify quoted list lowering builds runtime cons cells.

(load "run-habu.lisp")

(let* ((forms '((defun head () (car (quote (#x5 #x7))))
                (head)))
       (expected #x5))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
