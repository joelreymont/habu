#!/usr/bin/env sbcl --script
;;; symbols with the same name in different packages should be distinct

(load "run-habu.lisp")

(let* ((forms '((defun compare-symbols ()
                   (let ((a (find-symbol "foo" "PKG-X"))
                         (b (find-symbol "foo" "PKG-Y")))
                     (if (= a b) 0 1)))
                 (compare-symbols))))
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (if (= result 1)
        (sb-ext:quit :unix-status 0)
        (progn
          (format t "Unexpected result ~A Output=~A~%" result output)
          (sb-ext:quit :unix-status 1)))))
