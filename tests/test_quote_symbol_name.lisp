#!/usr/bin/env sbcl --script
;;; Quoted symbol -> symbol-name -> string-length.

(load "run-habu.lisp")

(let* ((forms '((defun sym-len () (string-length (symbol-name (quote foo))))
                (sym-len)))
       (expected #x3))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
