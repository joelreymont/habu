;;; Debug symbol packages in compiler
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader.lisp")
(load "bootstrap/compiler.lisp")

;; Check what compile-expr-full compares against
(format t "~%=== In compiler.lisp, symbols are in HABU package ===~%")
(format t "habu::sys-exit package: ~A~%" (symbol-package 'habu::sys-exit))

;; Parse with read-all
(let* ((forms (habu::read-all "(sys-exit 42)"))
       (f (car forms))
       (op (car f)))
  (format t "~%Parsed (car f): ~S, package: ~A~%" op (symbol-package op))
  (format t "(eq op 'sys-exit): ~A~%" (eq op 'sys-exit))
  (format t "(eq op 'habu::sys-exit): ~A~%" (eq op 'habu::sys-exit))
  (format t "(string= (symbol-name op) \"SYS-EXIT\"): ~A~%" (string= (symbol-name op) "SYS-EXIT")))
