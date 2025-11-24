#!/usr/bin/env sbcl --script
;;; Validate symbol-name returns a string pointer (tagged).

(load "run-habu.lisp")

(let* ((forms '((defun sym-name () (symbol-name (quote foo)))
                (sym-name))))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (if (and result (= (logand result #xF) #x4))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A Output=~A~%" result output)
          (sb-ext:quit :unix-status 1)))))
