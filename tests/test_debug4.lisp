;;; Debug symbol packages
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader.lisp")
(load "bootstrap/compiler.lisp")
(load "bootstrap/codegen.lisp")
(load "bootstrap/macho-utils.lisp")

(format t "~%=== Debugging symbol packages ===~%")

(let ((forms (habu::read-all "(sys-exit 42)")))
  (let ((f (car forms)))
    (format t "Form: ~S~%" f)
    (format t "(car f): ~S~%" (car f))
    (format t "Package of (car f): ~A~%" (symbol-package (car f)))
    (format t "Package of 'sys-exit: ~A~%" (symbol-package 'sys-exit))))
