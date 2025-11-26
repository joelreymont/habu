#!/usr/bin/env sbcl --script
(load "sbcl-habu-shim.lisp")
(load "arm64/codegen-sbcl.lisp")

;; Test simple identity function
(let* ((forms '((defun identity (x) x)
                (identity 42)))
       (result (habu-sbcl-codegen:compile-forms forms)))
  (format t "Compile result: ~S~%" result))
