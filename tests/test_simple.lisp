;;; Simple test of pure pipeline
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")
(load "bootstrap/reader.lisp")
(load "bootstrap/compiler.lisp")
(load "bootstrap/codegen.lisp")
(load "bootstrap/macho-utils.lisp")

(format t "Creating test executable...~%")
(habu:deliver-v3 "(sys-exit (+ 20 22))" "/tmp/test_pure_simple")
(format t "Running /tmp/test_pure_simple...~%")
(sb-ext:run-program "/tmp/test_pure_simple" nil :output t :error t)
