;;;; Pure-Lisp driver (no C backend) to load compiler code
;;;; TODO: wire runtime address table and load pipeline here

(format t "~%[Habu Lisp] Loading SBCL shim for Habu predicates...~%")
(require :sb-posix)
(load "sbcl-habu-shim.lisp")
(load "habu-arm64-codegen-sbcl.lisp")

(defpackage :habu-sbcl
  (:use :cl :habu-shim :habu-sbcl-codegen)
  (:export :compile-to-arm64 :compile-to-arm64-with-runtime))

(in-package :habu-sbcl)

(defun hexdump-bytes (bytes)
  "Print bytes as hex pairs for quick smoke visibility."
  (loop for b in bytes
        for idx from 0
        do (progn
             (when (and (> idx 0) (zerop (mod idx #x10)))
               (format t "~%"))
             (format t "~2,'0X " b)))
  (when bytes (format t "~%")))

(defparameter +map-jit+ #x800)
(defparameter *icache-invalidate-fn*
  (ignore-errors
    (sb-alien:extern-alien "sys_icache_invalidate"
                           (sb-alien:function sb-alien:void
                                              sb-alien:system-area-pointer
                                              sb-alien:unsigned-long))))
(defparameter *enable-jit-smoke* nil)
(defparameter *jit-lib* nil)
(defparameter *c-jit-exec* nil)
(defparameter *jit-lib-candidates* '("libhabu-jit.dylib" "libhabu-jit.so"))

(defun arm64-host-p ()
  (member :arm64 *features*))

(defun ensure-c-jit ()
  "Attempt to load tiny C JIT helper (libhabu-jit.*); returns alien fn or NIL."
  (or *c-jit-exec*
      (progn
        (dolist (path *jit-lib-candidates*)
          (when (and (not *c-jit-exec*) (probe-file path))
            (ignore-errors
              (sb-alien:load-shared-object path)
              (setf *jit-lib* path
                    *c-jit-exec*
                      (sb-alien:extern-alien "habu_jit_execute"
                                             (sb-alien:function sb-alien:long
                                                                (sb-alien:* sb-alien:unsigned-char)
                                                                sb-alien:size-t)))))))
        *c-jit-exec*)))

(defun align-size (n align)
  (let ((rem (mod n align)))
    (if (zerop rem) n (+ n (- align rem)))))

(defun jit-execute-bytes (bytes)
  "SBCL-only JIT: mmap RWX memory, copy bytes, call, and return int64 result."
  (let* ((byte-vec (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         (size (length byte-vec)))
    (cond
      ((ensure-c-jit)
       (sb-sys:with-pinned-objects (byte-vec)
         (sb-alien:alien-funcall *c-jit-exec* (sb-sys:vector-sap byte-vec) size)))
      (t
       (let* ((page-size (sb-posix:getpagesize))
              (aligned (align-size size page-size))
              (prot (logior sb-posix:prot-read sb-posix:prot-write sb-posix:prot-exec))
              (flags (logior sb-posix:map-private sb-posix:map-anon +map-jit+))
              (sap (sb-posix:mmap nil aligned prot flags -1 0)))
         (when (= (sb-sys:sap-int sap) (lognot 0))
           (error "[JIT] mmap failed"))
         (unwind-protect
              (progn
                (loop for i from 0 below size
                      do (setf (sb-sys:sap-ref-8 sap i) (aref byte-vec i)))
                (when *icache-invalidate-fn*
                  (ignore-errors
                    (sb-alien:alien-funcall *icache-invalidate-fn* sap size)))
                (let ((fn (sb-alien:sap-alien sap
                                              (sb-alien:* (sb-alien:function sb-alien:long)))))
                  (sb-alien:alien-funcall fn)))
           (sb-posix:munmap sap aligned)))))))

(format t "[Habu Lisp] Attempting to load habu-arm64-codegen.lisp (pure Lisp)...~%")
(handler-case
    (progn
      (let ((*package* (find-package :habu-sbcl)))
        ;; For SBCL bring-up, load only the stub file; skip the real codegen.
        (load "habu-arm64-codegen-sbcl.lisp"))
      (format t "[READY] Compiler definitions loaded in SBCL environment.~%")
      (handler-case
          (let* ((*package* (find-package :habu-sbcl)))
            (let* ((runtime-addrs (habu-sbcl-codegen:make-runtime-addrs
                                   #xABCDEF01 #x1234 #x5678))
                   (bytes (habu-sbcl:compile-to-arm64-with-runtime 42 runtime-addrs)))
              (format t "[SMOKE] compile-to-arm64 42 produced ~D bytes.~%"
                      (length bytes))
              (format t "[RUNTIME-ADDRS] ~S~%" runtime-addrs)
              (format t "[HEXDUMP]~%")
              (hexdump-bytes bytes)
              (cond
                ((not (arm64-host-p))
                 (format t "[JIT RUN] skipped (non-ARM64 host).~%"))
                (*enable-jit-smoke*
                 (handler-case
                     (let* ((using-c (ensure-c-jit))
                            (result (jit-execute-bytes bytes)))
                       (format t "[JIT RUN] returned ~D (~:[SBCL mmap~;C helper~])~%"
                               result using-c))
                   (error (je)
                     (format t "[WARN] JIT execution failed: ~A~%" je))))
                (t
                 (format t "[JIT RUN] skipped (set *enable-jit-smoke* to T to run).~%")))))
        (error (e)
          (format t "[WARN] Smoke compile failed: ~A~%" e))))
  (error (e)
    (format t "[WARN] Could not load compiler in SBCL: ~A~%" e)
    (format t "[NOTE] Source expects Habu runtime helpers; add SBCL shims or load runtime first.~%")))

(format t "~%[TODO] Wire runtime addresses via make-runtime-addrs and hook load/REPL pipeline.~%")
(format t "[TODO] Add JIT harness invocation and file loading once runtime hooks are plumbed.~%")
