;;; Debug pure pipeline
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader-pure.lisp")
(load "bootstrap/compiler-pure.lisp")
(load "bootstrap/codegen-pure.lisp")
(load "bootstrap/macho-pure.lisp")

(format t "~%=== Debugging Pure Pipeline ===~%")

;; Step 1: Read
(format t "~%Step 1: Reading source...~%")
(let ((forms (habu::pure-read-all "(sys-exit (+ 20 22))")))
  (format t "Forms: ~S~%" forms)

  ;; Step 2: Compile forms
  (format t "~%Step 2: Compiling forms...~%")
  (habu::pure-reset-symbol-table)
  (let ((result (habu::pure-compile-forms forms)))
    (format t "Defuns: ~S~%" (car result))
    (format t "Main IR: ~S~%" (cadr result))

    ;; Step 3: Generate code
    (format t "~%Step 3: Generating code...~%")
    (let* ((main-ir (cadr result))
           (code (habu::pure-codegen-main main-ir nil)))
      (format t "Code length: ~A~%" (length code))
      (format t "First 20 bytes: ~S~%" (subseq code 0 (min 20 (length code)))))))
