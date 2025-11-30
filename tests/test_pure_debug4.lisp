;;; Debug symbol packages
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader-pure.lisp")
(load "bootstrap/compiler-pure.lisp")
(load "bootstrap/codegen-pure.lisp")
(load "bootstrap/macho-pure.lisp")

(format t "~%=== Debugging symbol packages ===~%")

(let ((forms (habu::pure-read-all "(sys-exit 42)")))
  (let ((f (car forms)))
    (format t "Form: ~S~%" f)
    (format t "(car f): ~S~%" (car f))
    (format t "Package of (car f): ~A~%" (symbol-package (car f)))
    (format t "Package of 'sys-exit: ~A~%" (symbol-package 'sys-exit))))
