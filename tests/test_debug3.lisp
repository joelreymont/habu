;;; Debug compile-expr-full
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader.lisp")
(load "bootstrap/compiler.lisp")
(load "bootstrap/codegen.lisp")
(load "bootstrap/macho-utils.lisp")

(format t "~%=== Debugging compile-expr-full ===~%")

;; Test simple literal
(format t "~%Test 1: Literal 42~%")
(format t "Result: ~S~%" (habu::compile-expr-full 42 nil nil))

;; Test addition
(format t "~%Test 2: (+ 20 22)~%")
(format t "Result: ~S~%" (habu::compile-expr-full '(+ 20 22) nil nil))

;; Test sys-exit directly
(format t "~%Test 3: (sys-exit 42)~%")
(format t "Result: ~S~%" (habu::compile-expr-full '(sys-exit 42) nil nil))

;; Test sys-exit with expression
(format t "~%Test 4: (sys-exit (+ 20 22))~%")
(format t "Result: ~S~%" (habu::compile-expr-full '(sys-exit (+ 20 22)) nil nil))
