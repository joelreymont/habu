#!/usr/bin/env sbcl --script
;;; Compile forms to ARM64 bytecode and execute via run-bytecode runtime.

(load "run-habu.lisp")

(let* ((forms '((defun make-pair (a b) (cons a b))
                (defun sum-pair (p) (+ (car p) (cdr p)))
                (sum-pair (make-pair #x5 #x7))))
       (expected #xC))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A~%" result)
          (sb-ext:quit :unix-status 1)))))
