;;; Debug pure-compile-expr-full
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader-pure.lisp")
(load "bootstrap/compiler-pure.lisp")
(load "bootstrap/codegen-pure.lisp")
(load "bootstrap/macho-pure.lisp")

(format t "~%=== Debugging pure-compile-expr-full ===~%")

;; Test simple literal
(format t "~%Test 1: Literal 42~%")
(format t "Result: ~S~%" (habu::pure-compile-expr-full 42 nil nil))

;; Test addition
(format t "~%Test 2: (+ 20 22)~%")
(format t "Result: ~S~%" (habu::pure-compile-expr-full '(+ 20 22) nil nil))

;; Test sys-exit directly
(format t "~%Test 3: (sys-exit 42)~%")
(format t "Result: ~S~%" (habu::pure-compile-expr-full '(sys-exit 42) nil nil))

;; Test sys-exit with expression
(format t "~%Test 4: (sys-exit (+ 20 22))~%")
(format t "Result: ~S~%" (habu::pure-compile-expr-full '(sys-exit (+ 20 22)) nil nil))
