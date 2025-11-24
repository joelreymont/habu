#!/usr/bin/env sbcl --script
;;; find-symbol with package string is accepted (stubbed).

(load "run-habu.lisp")

(let* ((forms '((defun fs () (find-symbol "foo" "pkg")) (symbol-name (fs)))))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (if (and result (= (logand result #xF) #x4))
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A Output=~A~%" result output)
          (sb-ext:quit :unix-status 1)))))
