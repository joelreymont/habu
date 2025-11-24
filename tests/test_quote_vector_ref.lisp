#!/usr/bin/env sbcl --script
;;; Quoted vector literal returns non-nil pointer.

(load "run-habu.lisp")

(let* ((forms '((defun vec-val () (quote #(#x1 #x4 #x7)))
                (vec-val))))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (/= result 0))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
