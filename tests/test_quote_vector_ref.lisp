#!/usr/bin/env sbcl --script
;;; Quoted vector literal with vector-ref returning fixnum.

(load "run-habu.lisp")

(let* ((forms '((defun vec-second ()
                  (vector-ref (quote #(#x1 #x4 #x7)) #x1))
                (vec-second)))
       (expected #x4))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
