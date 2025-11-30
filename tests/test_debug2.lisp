;;; Debug pure-find-main-form
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader.lisp")
(load "bootstrap/compiler.lisp")
(load "bootstrap/codegen.lisp")
(load "bootstrap/macho-utils.lisp")

(format t "~%=== Debugging pure-find-main-form ===~%")

(let* ((forms (habu::read-all "(sys-exit (+ 20 22))"))
       (f (car forms)))
  (format t "Forms: ~S~%" forms)
  (format t "First form: ~S~%" f)
  (format t "(car f): ~S~%" (car f))
  (format t "(car f) type: ~A~%" (type-of (car f)))
  (format t "(eq (car f) 'defun): ~A~%" (eq (car f) 'defun))
  (format t "(eq (car f) 'sys-exit): ~A~%" (eq (car f) 'sys-exit))
  (format t "(eq (car f) 'SYS-EXIT): ~A~%" (eq (car f) 'SYS-EXIT))
  ;; Test pure-find-main-form
  (format t "~%pure-find-main-form result: ~S~%" (habu::pure-find-main-form forms nil)))
