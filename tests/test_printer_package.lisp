#!/usr/bin/env sbcl --script
;;; Package-aware printing of symbol (with pkg prefix).

(load "run-habu.lisp")

(let* ((forms '((defun print-foo () (symbol-name (find-symbol (quote foo) (quote pkg-a))))
                (print-foo)))
       (result (multiple-value-bind (res out) (habu-sbcl:compile-and-run-forms forms)
                 (declare (ignore out))
                 res)))
  (if (and result (= (logand result #x4) #x4))
      (sb-ext:quit :unix-status 0)
      (progn
        (format t "Unexpected result ~A~%" result)
        (sb-ext:quit :unix-status 1)))))
