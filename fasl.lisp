;;; FASL - Fast Load File Format for Habu
;;;
;;; FASL files contain compiled ARM64 machine code that can be loaded
;;; and executed. This is the standard Lisp terminology for compiled code files.
;;;
;;; Format:
;;;   Magic:    4 bytes "HFSL" (Habu FASL)
;;;   Version:  4 bytes (currently 1)
;;;   Flags:    4 bytes (reserved)
;;;   Code-len: 4 bytes (length of code section)
;;;   Code:     N bytes (ARM64 machine code)

(in-package :habu-sbcl)

;;; FASL magic number and version
(defconstant +fasl-magic+ #x4C534648)  ; "HFSL" in little-endian
(defconstant +fasl-version+ 1)

(defun write-u32-le (stream value)
  "Write a 32-bit unsigned integer in little-endian format"
  (write-byte (logand value #xFF) stream)
  (write-byte (logand (ash value -8) #xFF) stream)
  (write-byte (logand (ash value -16) #xFF) stream)
  (write-byte (logand (ash value -24) #xFF) stream))

(defun read-u32-le (stream)
  "Read a 32-bit unsigned integer in little-endian format"
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (logior b0
            (ash b1 8)
            (ash b2 16)
            (ash b3 24))))

(defun write-fasl (code-bytes output-path)
  "Write compiled code to a FASL file"
  (with-open-file (out output-path
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    ;; Header
    (write-u32-le out +fasl-magic+)
    (write-u32-le out +fasl-version+)
    (write-u32-le out 0)  ; flags (reserved)
    (write-u32-le out (length code-bytes))
    ;; Code section
    (dolist (byte code-bytes)
      (write-byte byte out)))
  output-path)

(defun read-fasl (input-path)
  "Read a FASL file and return the code bytes"
  (with-open-file (in input-path
                      :direction :input
                      :element-type '(unsigned-byte 8))
    ;; Verify magic
    (let ((magic (read-u32-le in)))
      (unless (= magic +fasl-magic+)
        (error "Not a valid FASL file: ~A (magic: ~8,'0X)" input-path magic)))
    ;; Read version
    (let ((version (read-u32-le in)))
      (unless (<= version +fasl-version+)
        (error "Unsupported FASL version: ~A" version)))
    ;; Skip flags
    (read-u32-le in)
    ;; Read code length
    (let* ((code-len (read-u32-le in))
           (code (make-array code-len :element-type '(unsigned-byte 8))))
      (read-sequence code in)
      (coerce code 'list))))

(defun compile-file (source-path &key (output-path nil) (tree-shaking t) verbose)
  "Compile a Lisp source file to a FASL file.
   SOURCE-PATH: path to .lisp file
   OUTPUT-PATH: path for .fasl output (defaults to source with .fasl extension)
   TREE-SHAKING: enable dead code elimination (default t)
   VERBOSE: print progress messages"
  (let* ((source-pathname (pathname source-path))
         (fasl-path (or output-path
                        (make-pathname :defaults source-pathname
                                       :type "fasl")))
         (forms (read-forms-from-file source-path))
         (runtime-addrs (ensure-runtime-addrs)))
    (when verbose
      (format t "Compiling ~A...~%" source-path))

    ;; Compile to machine code
    (let ((bytes (if tree-shaking
                     (habu-sbcl-codegen:compile-program-with-tree-shaking
                      forms runtime-addrs)
                     (habu-sbcl-codegen:compile-program-with-functions-with-runtime
                      forms runtime-addrs))))
      (when verbose
        (format t "Generated ~A bytes of ARM64 code~%" (length bytes)))

      ;; Write FASL
      (write-fasl bytes fasl-path)
      (when verbose
        (format t "Wrote ~A~%" fasl-path))

      fasl-path)))

(defun load-fasl (fasl-path &key verbose)
  "Load and execute a FASL file.
   Returns the result of executing the compiled code."
  (when verbose
    (format t "Loading ~A...~%" fasl-path))

  (let* ((code-bytes (read-fasl fasl-path))
         (code-size (length code-bytes)))
    (when verbose
      (format t "Read ~A bytes of ARM64 code~%" code-size))

    ;; Allocate executable memory
    (cffi:with-foreign-pointer (exec-mem code-size)
      ;; Copy code
      (loop for i from 0
            for byte in code-bytes
            do (setf (cffi:mem-ref exec-mem :uint8 i) byte))

      ;; Make executable (platform-specific)
      #+darwin
      (let ((result (sb-posix:mprotect exec-mem code-size
                                        (logior sb-posix:prot-read
                                                sb-posix:prot-exec))))
        (unless (zerop result)
          (error "mprotect failed: ~A" result)))

      ;; Execute
      ;; Note: This requires FFI setup for the runtime table
      ;; For now, we use the external run-bytecode loader
      (error "Direct FASL execution not yet implemented - use run-fasl"))))

(defun fasl-pathname-p (pathname)
  "Check if pathname has .fasl extension"
  (string-equal (pathname-type pathname) "fasl"))

;; Exports
(export '(compile-file load-fasl write-fasl read-fasl fasl-pathname-p
          +fasl-magic+ +fasl-version+)
        :habu-sbcl)
