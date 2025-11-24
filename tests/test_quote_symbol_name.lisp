#!/usr/bin/env sbcl --script
;;; Quoted symbol tag via runtime get-tag.

(load "run-habu.lisp")

(let* ((forms '((defun sym-val () (quote foo))
                (sym-val))))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (/= result 0))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
