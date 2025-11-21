;;;; Pure-Lisp driver (no C backend) to load compiler code
;;;; TODO: wire runtime address table and load pipeline here

(format t "~%[Habu Lisp] Loading SBCL shim for Habu predicates...~%")
(load "sbcl-habu-shim.lisp")
(load "habu-repl.lisp")
(format t "[Habu Lisp] Attempting to load habu-arm64-codegen.lisp (pure Lisp)...~%")
(handler-case
    (progn
      (load "habu-arm64-codegen.lisp")
      (format t "[READY] Compiler definitions loaded in SBCL environment.~%"))
  (error (e)
    (format t "[WARN] Could not load compiler in SBCL: ~A~%" e)
    (format t "[NOTE] Source uses 0x hex and Habu-specific predicates; needs #x rewrite/Lisp shim before loading here.~%")))

(format t "~%[TODO] Wire runtime addresses via make-runtime-addrs and hook load/REPL pipeline.~%")
(format t "[TODO] Add JIT harness invocation and file loading once runtime hooks are plumbed.~%")
