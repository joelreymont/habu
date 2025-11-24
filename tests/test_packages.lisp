#!/usr/bin/env sbcl --script
;;; Minimal package smoke using runtime symbols
(load "run-habu.lisp")

(let* ((forms '((defun sym-name () (symbol-name (quote foo))) (sym-name)))
       (tag #x4))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (if (and result (= (logand result #xF) tag))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A Output=~A~%" result output)
          (sb-ext:quit :unix-status 1)))))
