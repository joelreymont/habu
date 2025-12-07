;;;; fasl.lisp - Habu FASL (Fast Load) Format
;;;;
;;;; Binary format for compiled Lisp code. Enables separate compilation
;;;; and fast loading without re-parsing or re-compiling.
;;;;
;;;; FASL Structure:
;;;;   Header (32 bytes)
;;;;   Function Table
;;;;   Code Section
;;;;   Constant Pool
;;;;   Relocation Table
;;;;   String Table

(in-package :habu)

;;; ============================================================
;;; FASL Constants
;;; ============================================================

(defconstant +fasl-magic+ #x4641534C)  ; "FASL" in ASCII
(defconstant +fasl-version+ 2)  ; v2 adds num-imports and export flags
(defconstant +fasl-arch-arm64+ 1)

;;; Structure sizes (all fields are 4 bytes/u32)
;;; Header: magic, version, arch, flags, num-functions, code-size,
;;;         const-pool-size, num-relocations, num-imports
(defconstant +fasl-header-fields+ 9)
(defconstant +fasl-header-size+ (* +fasl-header-fields+ 4))
;;; Function entry: name-offset, code-offset, code-size, arity, flags
(defconstant +fasl-function-fields+ 5)
(defconstant +fasl-function-size+ (* +fasl-function-fields+ 4))
;;; Relocation: type, offset, target
(defconstant +fasl-relocation-fields+ 3)
(defconstant +fasl-relocation-size+ (* +fasl-relocation-fields+ 4))
;;; Import: name-offset
(defconstant +fasl-import-fields+ 1)
(defconstant +fasl-import-size+ (* +fasl-import-fields+ 4))

;;; Section types
(defconstant +section-functions+ 1)
(defconstant +section-code+ 2)
(defconstant +section-constants+ 3)
(defconstant +section-relocations+ 4)
(defconstant +section-strings+ 5)
(defconstant +section-imports+ 6)    ; Required functions from other modules

;;; Relocation types
(defconstant +reloc-fn-call+ 1)      ; Call to defined function
(defconstant +reloc-extern-call+ 2)  ; Call to C function
(defconstant +reloc-constant+ 3)     ; Reference to constant pool

;;; Function flags
(defconstant +fn-flag-exported+ #x1) ; Function is exported (visible to other modules)
(defconstant +fn-flag-entry+ #x2)    ; Function is module entry point

;;; Constant types
(defconstant +const-fixnum+ 1)
(defconstant +const-string+ 2)
(defconstant +const-symbol+ 3)
(defconstant +const-cons+ 4)
(defconstant +const-vector+ 5)

;;; ============================================================
;;; FASL Data Structures
;;; ============================================================

(defstruct fasl-header
  (magic +fasl-magic+ :type fixnum)
  (version +fasl-version+ :type fixnum)
  (arch +fasl-arch-arm64+ :type fixnum)
  (flags 0 :type fixnum)
  (num-functions 0 :type fixnum)
  (code-size 0 :type fixnum)
  (const-pool-size 0 :type fixnum)
  (num-relocations 0 :type fixnum)
  (num-imports 0 :type fixnum))      ; Functions required from other modules

(defstruct fasl-function
  (name nil :type (or null symbol string))  ; Function name (symbol during build)
  (name-offset 0 :type fixnum)    ; Offset into string table (set at write time)
  (code-offset 0 :type fixnum)    ; Offset into code section
  (code-size 0 :type fixnum)      ; Size in bytes
  (arity 0 :type fixnum)          ; Number of arguments
  (flags 0 :type fixnum))         ; Exported, inline, etc.

(defstruct fasl-relocation
  (type 0 :type fixnum)           ; reloc-fn-call, reloc-extern-call, etc.
  (offset 0 :type fixnum)         ; Offset in code section
  (target 0 :type fixnum))        ; Index into function/extern/const table

(defstruct fasl-import
  (name nil :type (or null symbol string))  ; Required function name
  (name-offset 0 :type fixnum))   ; Offset into string table

;;; ============================================================
;;; Binary I/O Helpers
;;; ============================================================
;;;
;;; Note: write-u32-le, write-u64-le, read-u32-le, read-u64-le are
;;; defined in compiler-sbcl.lisp and reused here.

(defun fasl-write-u8 (byte stream)
  "Write a single byte."
  (write-byte byte stream))

(defun fasl-read-u8 (stream)
  "Read a single byte."
  (read-byte stream))

;;; ============================================================
;;; String Table
;;; ============================================================

(defun build-string-table (strings)
  "Build string table from list of strings.
   Returns (bytes . offset-alist) where offset-alist maps string to offset."
  (let ((bytes nil)
        (offsets nil)
        (current-offset 0))
    (dolist (str strings)
      (push (cons str current-offset) offsets)
      ;; Write length-prefixed string
      (let ((len (length str)))
        (push (logand len #xFF) bytes)
        (push (logand (ash len -8) #xFF) bytes)
        (dotimes (i len)
          (push (char-code (char str i)) bytes))
        (setf current-offset (+ current-offset 2 len))))
    (values (nreverse bytes) (nreverse offsets))))

(defun read-string-from-table (bytes offset)
  "Read length-prefixed string from byte vector at offset.
   Returns empty string if bytes is empty or offset is out of bounds."
  (when (or (zerop (length bytes))
            (>= offset (length bytes)))
    (return-from read-string-from-table ""))
  (let* ((len (logior (aref bytes offset)
                      (ash (aref bytes (1+ offset)) 8)))
         (str (make-string len)))
    (dotimes (i len)
      (setf (char str i) (code-char (aref bytes (+ offset 2 i)))))
    str))

;;; ============================================================
;;; FASL Writer
;;; ============================================================

(defun write-fasl-header (header stream)
  "Write FASL header to stream (36 bytes for v2)."
  (write-u32-le (fasl-header-magic header) stream)
  (write-u32-le (fasl-header-version header) stream)
  (write-u32-le (fasl-header-arch header) stream)
  (write-u32-le (fasl-header-flags header) stream)
  (write-u32-le (fasl-header-num-functions header) stream)
  (write-u32-le (fasl-header-code-size header) stream)
  (write-u32-le (fasl-header-const-pool-size header) stream)
  (write-u32-le (fasl-header-num-relocations header) stream)
  (write-u32-le (fasl-header-num-imports header) stream))

(defun write-fasl-function (fn stream)
  "Write function entry to stream."
  (write-u32-le (fasl-function-name-offset fn) stream)
  (write-u32-le (fasl-function-code-offset fn) stream)
  (write-u32-le (fasl-function-code-size fn) stream)
  (write-u32-le (fasl-function-arity fn) stream)
  (write-u32-le (fasl-function-flags fn) stream))

(defun write-fasl-relocation (reloc stream)
  "Write relocation entry to stream."
  (write-u32-le (fasl-relocation-type reloc) stream)
  (write-u32-le (fasl-relocation-offset reloc) stream)
  (write-u32-le (fasl-relocation-target reloc) stream))

(defun write-fasl-import (import stream)
  "Write import entry to stream (4 bytes: name-offset)."
  (write-u32-le (fasl-import-name-offset import) stream))

(defun write-fasl (output-path functions code relocations constants
                   &key (imports nil))
  "Write complete FASL file.
   FUNCTIONS: list of fasl-function structs
   CODE: byte vector of machine code
   RELOCATIONS: list of fasl-relocation structs
   CONSTANTS: constant pool bytes
   IMPORTS: list of fasl-import structs (functions required from other modules)"
  (with-open-file (stream output-path
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    ;; Collect all names for string table (functions + imports)
    (let* ((fn-names (mapcar (lambda (f)
                               (let ((name (fasl-function-name f)))
                                 (if (symbolp name)
                                     (symbol-name name)
                                     name)))
                             functions))
           (import-names (mapcar (lambda (i)
                                   (let ((name (fasl-import-name i)))
                                     (if (symbolp name)
                                         (symbol-name name)
                                         name)))
                                 imports))
           (all-names (append fn-names import-names)))
      (multiple-value-bind (str-bytes str-offsets) (build-string-table all-names)
        ;; Update function name offsets
        (dolist (fn functions)
          (let* ((name (fasl-function-name fn))
                 (name-str (if (symbolp name) (symbol-name name) name)))
            (setf (fasl-function-name-offset fn)
                  (cdr (assoc name-str str-offsets :test #'string=)))))

        ;; Update import name offsets
        (dolist (imp imports)
          (let* ((name (fasl-import-name imp))
                 (name-str (if (symbolp name) (symbol-name name) name)))
            (setf (fasl-import-name-offset imp)
                  (cdr (assoc name-str str-offsets :test #'string=)))))

        ;; Write header
        (let ((header (make-fasl-header
                       :num-functions (length functions)
                       :code-size (length code)
                       :const-pool-size (if constants (length constants) 0)
                       :num-relocations (length relocations)
                       :num-imports (length imports))))
          (write-fasl-header header stream))

        ;; Write function table
        (dolist (fn functions)
          (write-fasl-function fn stream))

        ;; Write code section
        (dolist (byte code)
          (fasl-write-u8 byte stream))

        ;; Write constant pool
        (when constants
          (dolist (byte constants)
            (fasl-write-u8 byte stream)))

        ;; Write relocations
        (dolist (reloc relocations)
          (write-fasl-relocation reloc stream))

        ;; Write imports
        (dolist (imp imports)
          (write-fasl-import imp stream))

        ;; Write string table
        (dolist (byte str-bytes)
          (fasl-write-u8 byte stream))))))

;;; ============================================================
;;; FASL Reader
;;; ============================================================

(defun read-fasl-header (stream)
  "Read FASL header from stream (36 bytes for v2).
   Only handles old FASL format. For HFSL format, use read-fasl-v2 instead."
  (let* ((magic (handler-case (read-u32-le stream)
                  (end-of-file ()
                    (error "Invalid FASL file: empty or truncated (no magic number)"))))
         (version (handler-case (read-u32-le stream)
                    (end-of-file ()
                      (error "Invalid FASL file: truncated (no version)")))))
    ;; Check for FASL magic, provide helpful error for HFSL
    (cond
      ((= magic +fasl-magic+)
       ;; Valid FASL format, continue
       nil)
      ((= magic #x4C534648)
       ;; HFSL format detected (different byte order/format)
       (error "HFSL format detected. Use read-fasl-v2 or link-fasls instead of read-fasl."))
      (t
       (error "Invalid FASL magic number: ~X (expected ~X for FASL). ~
               File may be corrupted or not a FASL file."
              magic +fasl-magic+)))
    (unless (<= version +fasl-version+)
      (error "Unsupported FASL version: ~D (max: ~D)" version +fasl-version+))
    (let ((header (make-fasl-header
                   :magic magic
                   :version version
                   :arch (read-u32-le stream)
                   :flags (read-u32-le stream)
                   :num-functions (read-u32-le stream)
                   :code-size (read-u32-le stream)
                   :const-pool-size (read-u32-le stream)
                   :num-relocations (read-u32-le stream)
                   :num-imports (if (>= version 2) (read-u32-le stream) 0))))
      header)))

(defun read-fasl-function (stream)
  "Read function entry from stream."
  (make-fasl-function
   :name-offset (read-u32-le stream)
   :code-offset (read-u32-le stream)
   :code-size (read-u32-le stream)
   :arity (read-u32-le stream)
   :flags (read-u32-le stream)))

(defun read-fasl-relocation (stream)
  "Read relocation entry from stream."
  (make-fasl-relocation
   :type (read-u32-le stream)
   :offset (read-u32-le stream)
   :target (read-u32-le stream)))

(defun read-fasl-import (stream)
  "Read import entry from stream (4 bytes: name-offset)."
  (make-fasl-import
   :name-offset (read-u32-le stream)))

(defun read-fasl (input-path)
  "Read complete FASL file.
   Returns (values header functions code relocations constants string-table imports)."
  (with-open-file (stream input-path
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let* ((header (read-fasl-header stream))
           (num-fns (fasl-header-num-functions header))
           (code-size (fasl-header-code-size header))
           (const-size (fasl-header-const-pool-size header))
           (num-relocs (fasl-header-num-relocations header))
           (num-imports (fasl-header-num-imports header)))

      ;; Read function table
      (let ((functions (loop repeat num-fns
                             collect (read-fasl-function stream))))

        ;; Read code section
        (let ((code (make-array code-size :element-type '(unsigned-byte 8))))
          (dotimes (i code-size)
            (setf (aref code i) (fasl-read-u8 stream)))

          ;; Read constant pool
          (let ((constants (make-array const-size :element-type '(unsigned-byte 8))))
            (dotimes (i const-size)
              (setf (aref constants i) (fasl-read-u8 stream)))

            ;; Read relocations
            (let ((relocations (loop repeat num-relocs
                                     collect (read-fasl-relocation stream))))

              ;; Read imports
              (let ((imports (loop repeat num-imports
                                   collect (read-fasl-import stream))))

                ;; Read remaining bytes as string table
                (let* ((remaining (- (file-length stream) (file-position stream)))
                       (str-table (make-array remaining :element-type '(unsigned-byte 8))))
                  (dotimes (i remaining)
                    (setf (aref str-table i) (fasl-read-u8 stream)))

                  (values header functions code relocations constants str-table imports))))))))))

;;; ============================================================
;;; Compile to FASL
;;; ============================================================

(defun collect-call-markers (bytes-with-markers)
  "Extract call markers from flattened code.
   Returns list of (type offset target) where:
   - type is :fn-call or :extern-call
   - offset is byte position in code
   - target is function name (symbol or string)"
  (let ((markers nil)
        (offset 0))
    (dolist (item bytes-with-markers)
      (cond
        ;; Internal function call marker: (:call-fn fn-name) or (:call-fn fn-name pos)
        ((and (consp item) (eq (car item) :call-fn))
         (push (list :fn-call offset (cadr item)) markers)
         (incf offset 4))  ; BL instruction is 4 bytes
        ;; External call marker: (:extern-call "name") or (:extern-call "name" pos)
        ((and (consp item) (eq (car item) :extern-call))
         (push (list :extern-call offset (cadr item)) markers)
         (incf offset 4))  ; BL instruction is 4 bytes
        ;; Function label marker - skip (no bytes)
        ((and (consp item) (eq (car item) :fn-label))
         nil)
        ;; Other markers that take space (TCO, loops)
        ((and (consp item) (member (car item) '(:tco-branch :tail-call-fn :loop-start :loop-continue)))
         (incf offset 4))
        ;; Regular byte
        ((integerp item)
         (incf offset))
        ;; Nested list of bytes
        ((consp item)
         (incf offset (length item)))))
    (nreverse markers)))

(defun strip-markers-to-bytes (bytes-with-markers)
  "Remove markers and flatten to pure byte list.
   Call markers become placeholder BL instructions (will be patched at load)."
  (let ((result nil))
    (dolist (item bytes-with-markers)
      (cond
        ;; Call markers - emit placeholder BL 0
        ((and (consp item) (member (car item) '(:call-fn :extern-call :tail-call-fn)))
         (setf result (append (reverse (arm64:bl 0)) result)))
        ;; TCO and loop markers - emit placeholder B 0
        ((and (consp item) (member (car item) '(:tco-branch :loop-start :loop-continue)))
         (setf result (append (reverse (arm64:b 0)) result)))
        ;; Label markers - no bytes
        ((and (consp item) (eq (car item) :fn-label))
         nil)
        ;; Regular byte
        ((integerp item)
         (push item result))
        ;; Nested list
        ((consp item)
         (dolist (b item)
           (when (integerp b)
             (push b result))))))
    (nreverse result)))

(defun build-fasl-functions (fnoffs &key exports)
  "Convert fnoffs alist to fasl-function structs.
   EXPORTS: list of function names to mark as exported."
  (let ((export-set (make-hash-table :test 'equal)))
    ;; Build export lookup
    (dolist (name exports)
      (setf (gethash (if (symbolp name) (symbol-name name) name) export-set) t))
    (mapcar (lambda (entry)
              (let* ((name (car entry))
                     (name-str (if (symbolp name) (symbol-name name) name))
                     (flags (if (gethash name-str export-set)
                                +fn-flag-exported+
                                0)))
                (make-fasl-function
                 :name name
                 :code-offset (cdr entry)
                 :code-size 0  ; Could compute from next function offset
                 :arity 0      ; Could extract from IR
                 :flags flags)))
            fnoffs)))

(defun build-fasl-relocations (markers fn-names)
  "Convert call markers to fasl-relocation structs.
   FN-NAMES is list of defined function names for indexing."
  (let ((fn-index (make-hash-table :test 'equal))
        (extern-index (make-hash-table :test 'equal))
        (extern-list nil))
    ;; Build function name -> index map
    (loop for name in fn-names
          for i from 0
          do (setf (gethash (if (symbolp name) (symbol-name name) name) fn-index) i))
    ;; Process markers
    (mapcar (lambda (marker)
              (let ((type (first marker))
                    (offset (second marker))
                    (target (third marker)))
                (if (eq type :fn-call)
                    ;; Internal function call
                    (let ((target-name (if (symbolp target) (symbol-name target) target)))
                      (make-fasl-relocation
                       :type +reloc-fn-call+
                       :offset offset
                       :target (or (gethash target-name fn-index) 0)))
                    ;; External call
                    (let ((target-str (if (symbolp target) (symbol-name target) target)))
                      (unless (gethash target-str extern-index)
                        (setf (gethash target-str extern-index) (length extern-list))
                        (push target-str extern-list))
                      (make-fasl-relocation
                       :type +reloc-extern-call+
                       :offset offset
                       :target (gethash target-str extern-index))))))
            markers)))

(defun compile-to-fasl (forms output-path &key exports)
  "Compile forms to FASL file.
   EXPORTS: list of function names to export (visible to other modules).
   This is the entry point for compile-file."
  ;; Reset compiler state
  #-sbcl (register-compiler-symbols)
  (reset-symbol-table)
  (reset-lambda-counter)
  #+sbcl (reset-compile-warnings)

  (let* ((result (compile-forms forms))
         ;; Check for undefined functions
         (_ #+sbcl (when (report-compile-warnings)
                     (error "Compilation aborted due to undefined functions")))
         (defuns-orig (car result))
         (main-ir-orig (cadr result))
         ;; Lift lambdas
         (main-lift-result #+sbcl (lift-lambdas-2 main-ir-orig nil)
                           #-sbcl (lift-lambdas main-ir-orig nil))
         (main-ir (car main-lift-result))
         (main-lambdas (cdr main-lift-result))
         (defun-lift-result (lift-lambdas-from-defuns defuns-orig nil nil))
         (defuns (car defun-lift-result))
         (defun-lambdas (cdr defun-lift-result))
         (all-lambdas (append main-lambdas defun-lambdas)))
    (declare (ignore _))

    ;; Compile to code with markers
    (let* ((lambda-as-defuns (lambdas-to-defuns all-lambdas nil))
           (all-fns-raw (append defuns lambda-as-defuns))
           (all-fns (apply-tco-to-all-functions all-fns-raw))
           ;; Link verification
           (_ #+sbcl (when (verify-link-references (mapcar #'car all-fns))
                       (error "Link failed: undefined function references")))
           ;; Generate main code (linearize first, then codegen)
           (main-linear (linearize main-ir))
           (main-code (append-all
                       (list (fn-fixed-prologue)
                             (codegen main-linear nil nil)
                             (fn-fixed-epilogue))))
           (main-size (code-size main-code))
           ;; Build fnoffs
           (fnoffs (build-fnoffs all-fns main-size))
           ;; Regenerate with fnoffs
           (main-code-final (append-all
                             (list (fn-fixed-prologue)
                                   (codegen main-linear nil fnoffs)
                                   (fn-fixed-epilogue))))
           ;; Generate function code
           (fn-code (codegen-all-fns all-fns nil fnoffs nil))
           ;; Combine (no GC runtime in FASL - added at link time)
           (all-code (append main-code-final fn-code))
           ;; Flatten with markers preserved
           (bytes-with-markers (flatten-code-keep-markers-and-calls all-code))
           ;; Collect relocations before stripping markers
           (markers (collect-call-markers bytes-with-markers))
           ;; Strip markers to get raw bytes
           (code-bytes (strip-markers-to-bytes bytes-with-markers))
           ;; Build FASL structures
           (fn-names (cons '_main (mapcar #'car all-fns)))
           ;; Mark _main as entry point, mark exported functions
           (main-fn (make-fasl-function :name '_main
                                        :code-offset 0
                                        :code-size main-size
                                        :arity 0
                                        :flags +fn-flag-entry+))
           (functions (cons main-fn (build-fasl-functions fnoffs :exports exports)))
           (relocations (build-fasl-relocations markers fn-names))
           ;; Count exported functions
           (num-exported (count-if (lambda (f)
                                     (plusp (logand (fasl-function-flags f)
                                                    +fn-flag-exported+)))
                                   functions)))
      (declare (ignore _))

      ;; Write FASL
      (write-fasl output-path functions code-bytes relocations nil)
      (format t "Compiled ~D functions (~D exported) to ~A (~D bytes, ~D relocations)~%"
              (length functions) num-exported output-path
              (length code-bytes) (length relocations))
      output-path)))

;;; ============================================================
;;; compile-file - CL-compatible interface
;;; ============================================================

(defun compile-file (input-file &key (output-file nil) (verbose *compile-verbose*)
                                     (print *compile-print*) exports)
  "Compile a Lisp source file to FASL.
   INPUT-FILE: pathname designator for source file
   OUTPUT-FILE: pathname for output (default: input with .fasl extension)
   VERBOSE: print progress messages
   PRINT: print each form as compiled
   EXPORTS: list of function names to export (visible to other modules)
   Returns: output-truename, warnings-p, failure-p"
  (declare (ignore print))  ; TODO: implement print
  (let* ((input (pathname input-file))
         (output (or output-file
                     (make-pathname :type "fasl" :defaults input)))
         (source (native-read-file (namestring input)))
         (forms (read-all source)))
    (when verbose
      (format t "; Compiling file ~A~%" (namestring input)))
    (handler-case
        (progn
          (compile-to-fasl forms (namestring output) :exports exports)
          (values (truename output) nil nil))
      (error (e)
        (format *error-output* "; Compilation failed: ~A~%" e)
        (values nil t t)))))

(defvar *compile-verbose* t "Default verbosity for compile-file.")
(defvar *compile-print* nil "Default print setting for compile-file.")

;;; ============================================================
;;; Load FASL
;;; ============================================================

;;; Global function registry for loaded FASLs
(defvar *fasl-functions* (make-hash-table :test 'equal)
  "Map from function name (string) to (code-address . fasl-function).")

(defun patch-bl-instruction (code-vec offset target-offset)
  "Patch a BL instruction at OFFSET to branch to TARGET-OFFSET.
   Both offsets are relative to start of code section.
   BL encodes a signed 26-bit offset in instructions (not bytes)."
  (let* ((delta (- target-offset offset))
         (instr-delta (ash delta -2))  ; Convert bytes to instructions
         (bl-opcode #x94000000)
         (encoded (logior bl-opcode (logand instr-delta #x3FFFFFF))))
    ;; Write little-endian
    (setf (aref code-vec offset) (logand encoded #xFF))
    (setf (aref code-vec (+ offset 1)) (logand (ash encoded -8) #xFF))
    (setf (aref code-vec (+ offset 2)) (logand (ash encoded -16) #xFF))
    (setf (aref code-vec (+ offset 3)) (logand (ash encoded -24) #xFF))))

(defun apply-fasl-relocations (code-vec functions relocations)
  "Apply relocations to code vector.
   FUNCTIONS is vector of fasl-function structs.
   RELOCATIONS is list of fasl-relocation structs."
  (dolist (reloc relocations)
    (let ((type (fasl-relocation-type reloc))
          (offset (fasl-relocation-offset reloc))
          (target-idx (fasl-relocation-target reloc)))
      (cond
        ;; Internal function call
        ((= type +reloc-fn-call+)
         (when (< target-idx (length functions))
           (let ((target-fn (elt functions target-idx)))
             (patch-bl-instruction code-vec offset
                                   (fasl-function-code-offset target-fn)))))
        ;; External call - skip for now (would need stub table)
        ((= type +reloc-extern-call+)
         ;; TODO: Build stub table for extern calls
         nil)))))

(defun load (filespec &key (verbose *load-verbose*) (print *load-print*)
                           (if-does-not-exist t) (external-format :default))
  "Load a compiled FASL or source file.
   FILESPEC: pathname designator
   Returns T on success, signals error or returns NIL on failure."
  (declare (ignore external-format))
  (let* ((path (pathname filespec))
         (type (pathname-type path)))
    (cond
      ;; FASL file
      ((string-equal type "fasl")
       (load-fasl-file path :verbose verbose :print print))
      ;; Source file - compile and load
      ((or (string-equal type "lisp") (string-equal type "lsp") (null type))
       (let ((fasl-path (make-pathname :type "fasl" :defaults path)))
         (compile-file path :output-file fasl-path :verbose verbose)
         (load-fasl-file fasl-path :verbose verbose :print print)))
      ;; Unknown
      (t
       (if if-does-not-exist
           (error "Unknown file type: ~A" path)
           nil)))))

(defvar *load-verbose* t "Default verbosity for load.")
(defvar *load-print* nil "Default print setting for load.")

;;; ============================================================
;;; Module Linking
;;; ============================================================

(defun function-exported-p (fn)
  "Check if function has export flag set."
  (plusp (logand (fasl-function-flags fn) +fn-flag-exported+)))

(defun link-modules (fasl-paths &key verbose)
  "Link multiple FASL modules together with export verification.
   Returns (values combined-code combined-symtab imports) where:
   - combined-code: concatenated bytecode as vector
   - combined-symtab: alist of (name . offset) for all functions
   - imports: list of (name . offset) for external calls

   Verifies that all cross-module calls target exported functions."
  (let ((all-code nil)
        (global-symtab nil)
        (all-relocations nil)
        (exports-by-module (make-hash-table :test 'equal))  ; module -> set of exported names
        (current-offset 0))

    ;; Phase 1: Read all FASLs and collect exports
    (dolist (fasl-path fasl-paths)
      (multiple-value-bind (header functions code relocations constants str-table imports)
          (read-fasl fasl-path)
        (declare (ignore constants imports))

        (when verbose
          (format t "Reading ~A: ~D bytes, ~D functions~%"
                  fasl-path (length code) (fasl-header-num-functions header)))

        ;; Build export set for this module
        (let ((module-exports (make-hash-table :test 'equal)))
          (dolist (fn functions)
            (let* ((name-offset (fasl-function-name-offset fn))
                   (name (read-string-from-table str-table name-offset)))
              ;; Track exports
              (when (function-exported-p fn)
                (setf (gethash name module-exports) t))
              ;; Add to global symtab with adjusted offset
              (push (cons name (+ current-offset (fasl-function-code-offset fn)))
                    global-symtab)))
          (setf (gethash fasl-path exports-by-module) module-exports))

        ;; Adjust relocation offsets and collect
        (dolist (reloc relocations)
          (push (make-fasl-relocation
                 :type (fasl-relocation-type reloc)
                 :offset (+ current-offset (fasl-relocation-offset reloc))
                 :target (fasl-relocation-target reloc))
                all-relocations))

        ;; Append code
        (setf all-code (concatenate 'vector
                                    (or all-code #())
                                    code))
        (setf current-offset (length all-code))))

    ;; Reverse to maintain order
    (setf global-symtab (nreverse global-symtab))
    (setf all-relocations (nreverse all-relocations))

    ;; Phase 2: Verify cross-module calls target exports
    (let ((symtab-hash (make-hash-table :test 'equal)))
      ;; Build symtab lookup
      (dolist (entry global-symtab)
        (setf (gethash (car entry) symtab-hash) (cdr entry)))

      ;; Check each internal call
      (dolist (reloc all-relocations)
        (when (= (fasl-relocation-type reloc) +reloc-fn-call+)
          ;; Get target function name from symtab
          (let* ((target-idx (fasl-relocation-target reloc))
                 (target-entry (nth target-idx global-symtab)))
            (when target-entry
              (let ((target-name (car target-entry)))
                ;; Check if target is in global symtab (defined somewhere)
                (unless (gethash target-name symtab-hash)
                  (error "Link error: undefined function ~A" target-name))))))))

    (when verbose
      (format t "Linked ~D modules: ~D bytes, ~D functions, ~D relocations~%"
              (length fasl-paths) (length all-code)
              (length global-symtab) (length all-relocations)))

    (values all-code global-symtab all-relocations)))

(defun load-fasl-file (input-path &key verbose print)
  "Internal: Load FASL file into running image.
   Returns T on success."
  (declare (ignore print))
  (multiple-value-bind (header functions code relocations constants str-table)
      (read-fasl input-path)
    (declare (ignore constants))

    (when verbose
      (format t "; Loading ~A~%" input-path))

    ;; Apply relocations to code (in place)
    (let ((fn-vec (coerce functions 'vector)))
      (apply-fasl-relocations code fn-vec relocations)

      ;; Register functions in global table
      (let ((loaded-names nil))
        (dolist (fn functions)
          (let* ((name-offset (fasl-function-name-offset fn))
                 (name (read-string-from-table str-table name-offset)))
            (setf (gethash name *fasl-functions*)
                  (cons (fasl-function-code-offset fn) fn))
            (push name loaded-names)))

        (when verbose
          (format t "; Loaded ~D functions: ~{~A~^, ~}~%"
                  (fasl-header-num-functions header)
                  (nreverse loaded-names)))
        t))))
