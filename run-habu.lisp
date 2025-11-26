;;;; Pure-Lisp driver (no C backend) to load compiler code
;;;; TODO: wire runtime address table and load pipeline here

(format t "~%[Habu Lisp] Loading SBCL shim for Habu predicates...~%")
(require :sb-posix)
(load "sbcl-habu-shim.lisp")
(load "habu-arm64-codegen-sbcl.lisp")

(defpackage :habu-sbcl
  (:use :cl :habu-shim :habu-sbcl-codegen)
  (:export :compile-to-arm64 :compile-to-arm64-with-runtime
           :ensure-runtime-addrs :jit-eval :compile-forms-with-runtime
           :compile-and-run-forms :read-forms-from-file :run-bytecode-file))

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

(defun getenv (name)
  #+sbcl (sb-posix:getenv name)
  #-sbcl nil)

(defparameter +map-jit+ #x800)
(defparameter *icache-invalidate-fn*
  (ignore-errors
    (sb-alien:extern-alien "sys_icache_invalidate"
                           (sb-alien:function sb-alien:void
                                              sb-alien:system-area-pointer
                                              sb-alien:unsigned-long))))
(defparameter *enable-jit-smoke* nil)
(defparameter *use-real-codegen*
  (string= (or (getenv "HABU_USE_REAL_CODEGEN") "") "1"))
(defparameter *jit-lib* nil)
(defparameter *c-jit-exec* nil)
(defparameter *jit-lib-candidates* '("libhabu-jit.dylib" "libhabu-jit.so"))
(defparameter *enable-load-smoke*
  (string= (or (getenv "HABU_ENABLE_LOAD_SMOKE") "") "1"))

(defun parse-hex-int (str)
  (let* ((s (string-upcase (string str)))
         (trimmed (if (and (> (length s) 2)
                           (string= (subseq s 0 2) "0X"))
                      (subseq s 2)
                      s)))
    (parse-integer trimmed :radix 16)))

(defun env-addr (name fallback)
  (let ((val (getenv name)))
    (if val
        (ignore-errors (parse-hex-int val))
        fallback)))

(defun parse-runtime-lines (lines)
  (let ((cons-addr nil) (car-addr nil) (cdr-addr nil))
    (dolist (ln lines)
      (when (search "HABU_CONS_ADDR=" ln)
        (setf cons-addr (parse-hex-int (subseq ln (length "HABU_CONS_ADDR=")))))
      (when (search "HABU_CAR_ADDR=" ln)
        (setf car-addr (parse-hex-int (subseq ln (length "HABU_CAR_ADDR=")))))
      (when (search "HABU_CDR_ADDR=" ln)
        (setf cdr-addr (parse-hex-int (subseq ln (length "HABU_CDR_ADDR="))))))
    (when (and cons-addr car-addr cdr-addr)
      (list cons-addr car-addr cdr-addr))))

(defun read-runtime-addrs ()
  "Try env vars, then bin/print-runtime-addrs, else fallback sample values."
  (let ((env-addrs (list (env-addr "HABU_CONS_ADDR" nil)
                         (env-addr "HABU_CAR_ADDR" nil)
                         (env-addr "HABU_CDR_ADDR" nil))))
    (if (every #'identity env-addrs)
        env-addrs
      (let ((helper (merge-pathnames "bin/print-runtime-addrs" (truename "."))))
        (if (probe-file helper)
            (ignore-errors
              (let ((p (sb-ext:run-program (namestring helper) nil
                                           :output :stream
                                           :error :output
                                           :search t)))
                (unwind-protect
                     (let* ((s (sb-ext:process-output p))
                            (lines (loop for line = (read-line s nil nil)
                                         while line collect line)))
                       (or (parse-runtime-lines lines) env-addrs))
                  (sb-ext:process-close p))))
            nil)))))

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
              (setf *jit-lib* path)
              (setf *c-jit-exec*
                    (sb-alien:extern-alien
                     "habu_jit_execute"
                     (sb-alien:function sb-alien:long
                                        (sb-alien:* sb-alien:unsigned-char)
                                        sb-alien:size-t))))))
        *c-jit-exec*)))

(defun parse-hex-int (str)
  (let* ((s (string-upcase (string str)))
         (trimmed (if (and (> (length s) 2)
                           (string= (subseq s 0 2) "0X"))
                      (subseq s 2)
                      s)))
    (parse-integer trimmed :radix 16)))

(defun env-addr (name fallback)
  (let ((val (getenv name)))
    (if val
        (ignore-errors (parse-hex-int val))
        fallback)))

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

(defun ensure-runtime-addrs ()
  "Ensure habu-sbcl-codegen:*runtime-addrs* is populated from env/helper."
  (or habu-sbcl-codegen:*runtime-addrs*
      (let* ((triplet (read-runtime-addrs)))
        (when triplet
          (setf habu-sbcl-codegen:*runtime-addrs*
                (apply #'habu-sbcl-codegen:make-runtime-addrs triplet)))))
  habu-sbcl-codegen:*runtime-addrs*)

(defun write-bytecode-to-file (bytes path)
  "Write bytecode BYTES to PATH as unsigned-byte 8."
  (with-open-file (out path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (dolist (b bytes) (write-byte b out))))

(defun read-forms-from-file (path)
  "Read all Lisp forms from PATH into a list."
  (with-open-file (in path :direction :input)
    (loop for form = (read in nil nil)
          while form collect form)))

(defun parse-run-bytecode-output (output)
  "Extract raw tagged result (preferred) or untagged fixnum from run-bytecode OUTPUT."
  (let* ((lines (loop for i = 0 then (1+ j)
                      as j = (position #\Newline output :start i)
                      collect (subseq output i j)
                      while j))
         (raw-line (find "Raw result:" lines
                         :test (lambda (needle haystack)
                                 (and haystack (search needle haystack)))))
         (fixnum-line (find "Untagged fixnum:" lines
                            :test (lambda (needle haystack)
                                    (and haystack (search needle haystack))))))
    (cond
      (raw-line
       (let ((idx (search "0x" raw-line)))
         (when idx
           (let ((raw (parse-integer raw-line
                                     :start (+ idx 2)
                                     :radix 16
                                     :junk-allowed t)))
             (when raw
               ;; Sign-extend if bit 63 is set (negative in two's complement)
               (when (>= raw (ash 1 63))
                 (setf raw (- raw (ash 1 64))))
               (if (= (logand raw #xF) 0)
                   (ash raw -4)
                   raw))))))
      (fixnum-line
       (parse-integer fixnum-line
                      :start (+ 17 (search "Untagged fixnum:" fixnum-line))
                      :junk-allowed t))
      (t nil))))

(defun compile-forms-with-runtime (forms &key output-path tree-shaking)
  "Compile FORMS (toplevel forms + final expr) to bytecode file.
When TREE-SHAKING is true, eliminate unreachable functions.
Returns two values: byte list and output path."
  (let* ((runtime-addrs (or (ensure-runtime-addrs)
                            (error "No runtime addresses available")))
         (bytes (if tree-shaking
                    (habu-sbcl-codegen:compile-program-with-tree-shaking
                     forms runtime-addrs)
                    (habu-sbcl-codegen:compile-program-with-functions-with-runtime
                     forms runtime-addrs)))
         (tmp-dir (or (getenv "TMPDIR") "/tmp"))
         (path (or output-path
                   (format nil "~A/habu-bytecode-~8,'0X.bin"
                           tmp-dir (random #xFFFFFFFF)))))
    (write-bytecode-to-file bytes path)
    (values bytes path)))

(defun run-bytecode-file (path)
  "Execute PATH via run-bytecode, returning values (result output-string)."
  (let* ((output (with-output-to-string (s)
                   (sb-ext:run-program "./run-bytecode" (list path)
                                       :output s :error :output :search t)))
         (result (parse-run-bytecode-output output)))
    (values result output)))

(defun compile-and-run-forms (forms &key output-path keep-file)
  "Compile FORMS with runtime addresses, execute via run-bytecode.
Returns two values: untagged result (or NIL if parse failed) and output text."
  (multiple-value-bind (bytes path) (compile-forms-with-runtime forms :output-path output-path)
    (declare (ignore bytes))
    (unwind-protect
         (run-bytecode-file path)
      (unless keep-file
        (ignore-errors (delete-file path))))))

(defun jit-eval (expr)
  "Compile EXPR to ARM64 with runtime addresses and execute it (ARM64 only)."
  (ensure-runtime-addrs)
  (if (not *use-real-codegen*)
      (progn
        (format t "[JIT] Stub mode eval (no real codegen), returning host eval.~%")
        (eval expr))
      (progn
        (when (not (arm64-host-p))
          (error "JIT only supported on ARM64 hosts for now."))
        (format t "[JIT] ARM64 eval using runtime addrs: ~S~%" habu-sbcl-codegen:*runtime-addrs*)
        (let* ((runtime-addrs habu-sbcl-codegen:*runtime-addrs*)
               (bytes (habu-sbcl:compile-to-arm64-with-runtime expr runtime-addrs)))
          ;; JIT returns tagged value from generated code; untag fixnums for convenience.
          (let ((raw (jit-execute-bytes bytes)))
            (if (and (integerp raw) (zerop (logand raw #xF)))
                (/ raw 16)
                raw))))))

(format t "[Habu Lisp] Attempting to load habu-arm64-codegen.lisp (pure Lisp)...~%")
(handler-case
    (progn
      (let ((*package* (find-package :habu-sbcl)))
        ;; Default: load stub; if HABU_USE_REAL_CODEGEN=1, load real codegen too.
        (load "habu-arm64-codegen-sbcl.lisp")
        (when (string= (or (getenv "HABU_USE_REAL_CODEGEN") "") "1")
          (load "habu-arm64-codegen.lisp")))
      (format t "[READY] Compiler definitions loaded in SBCL environment.~%")
      (when *enable-load-smoke*
        (handler-case
            (let* ((*package* (find-package :habu-sbcl)))
              (let* ((addr-triplet (or (read-runtime-addrs)
                                       (list #xABCDEF01 #x1234 #x5678)))
                     (cons-addr (first addr-triplet))
                     (car-addr (second addr-triplet))
                     (cdr-addr (third addr-triplet))
                     (runtime-addrs (habu-sbcl-codegen:make-runtime-addrs cons-addr car-addr cdr-addr)))
                (setf habu-sbcl-codegen:*runtime-addrs* runtime-addrs)
                (let ((bytes (habu-sbcl:compile-to-arm64-with-runtime 42 runtime-addrs)))
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
                     (format t "[JIT RUN] skipped (set *enable-jit-smoke* to T to run).~%"))))))
          (error (e)
            (format t "[WARN] Smoke compile failed: ~A~%" e)))))
  (error (e)
    (format t "[WARN] Could not load compiler in SBCL: ~A~%" e)
    (format t "[NOTE] Source expects Habu runtime helpers; add SBCL shims or load runtime first.~%")))

(defun invoked-directly-p ()
  (let* ((argv0 (car sb-ext:*posix-argv*))
         (script (and argv0 (probe-file argv0)))
         (loaded *load-truename*))
    (and script loaded
         (equal (truename script) (truename loaded)))))

(defun handle-cli ()
  "Simple CLI: --run-file <path> | --run-expr \"(expr ...)\"."
  (let* ((args (cdr sb-ext:*posix-argv*))
         (cmd (car args)))
    (cond
      ((and cmd (string= cmd "--run-file"))
       (let* ((path (cadr args))
              (forms (read-forms-from-file path))
              (result (compile-and-run-forms forms :output-path (caddr args))))
         (format t "~A~%" result)
         (sb-ext:quit :unix-status (if result 0 1))))
      ((and cmd (string= cmd "--run-expr"))
       (let* ((expr (read-from-string (cadr args)))
              (result (compile-and-run-forms (list expr) :output-path (caddr args))))
         (format t "~A~%" result)
         (sb-ext:quit :unix-status (if result 0 1))))
      (t
       (format t "[INFO] run-habu.lisp loaded. Use --run-file or --run-expr when invoking directly.~%")))))

(when (invoked-directly-p)
  (handle-cli))
