;;;; Pure-Lisp driver (no C backend) to load compiler code
;;;; TODO: wire runtime address table and load pipeline here

(format t "~%[Habu Lisp] Loading SBCL shim for Habu predicates...~%")
(load "sbcl-habu-shim.lisp")

(defpackage :habu-sbcl (:use :cl :habu-shim :habu-sbcl-codegen))

(format t "[Habu Lisp] Attempting to load habu-arm64-codegen.lisp (pure Lisp)...~%")
(handler-case
    (progn
      (let ((*package* (find-package :habu-sbcl)))
        (load "habu-arm64-codegen.lisp")
        (load "habu-arm64-codegen-sbcl.lisp"))
      (format t "[READY] Compiler definitions loaded in SBCL environment.~%")
      (handler-case
          (let* ((*package* (find-package :habu-sbcl)))
            (let ((bytes (compile-to-arm64 42)))
              (format t "[SMOKE] compile-to-arm64 42 produced ~D bytes.~%"
                      (length bytes))))
        (error (e)
          (format t "[WARN] Smoke compile failed: ~A~%" e))))
  (error (e)
    (format t "[WARN] Could not load compiler in SBCL: ~A~%" e)
    (format t "[NOTE] Source expects Habu runtime helpers; add SBCL shims or load runtime first.~%")))

(format t "~%[TODO] Wire runtime addresses via make-runtime-addrs and hook load/REPL pipeline.~%")
(format t "[TODO] Add JIT harness invocation and file loading once runtime hooks are plumbed.~%")
