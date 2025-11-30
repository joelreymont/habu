;;; Simple test of pure pipeline
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader-pure.lisp")
(load "bootstrap/compiler-pure.lisp")
(load "bootstrap/codegen-pure.lisp")
(load "bootstrap/macho-pure.lisp")

(format t "Creating test executable...~%")
(habu:pure-deliver-v3 "(sys-exit (+ 20 22))" "/tmp/test_pure_simple")
(format t "Running /tmp/test_pure_simple...~%")
(sb-ext:run-program "/tmp/test_pure_simple" nil :output t :error t)
