#!/usr/bin/env sbcl --script
;;; Export/import across packages (runtime tables).

(load "run-habu.lisp")

(let* ((forms '((progn
                  (defpackage (quote "PKG-A"))
                  (defpackage (quote "PKG-B"))
                  (export (quote foo))
                  (use-package (quote "PKG-A"))
                  (find-symbol (quote foo) (quote "PKG-A")))))
       (result (multiple-value-bind (res out) (habu-sbcl:compile-and-run-forms forms)
                 (declare (ignore out))
                 res)))
  (if (and result (= (logand result #xF) #x2))
      (sb-ext:quit :unix-status 0)
      (progn
        (format t "Unexpected result ~A~%" result)
        (sb-ext:quit :unix-status 1)))))
