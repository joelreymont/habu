#!/usr/bin/env sbcl --script
;;; Test progn sequencing and quote handling via run-bytecode runtime.

(load "run-habu.lisp")

(let* ((forms '((defun seq (x)
                  (progn
                    (quote #x0)
                    (+ x #x1)
                    (+ x #x2)))
                (seq #x3)))
       (expected #x5))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
