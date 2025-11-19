;;;; macho-generator.lisp - Generate Mach-O executable binaries

(in-package :habu-compiler)

;;; Mach-O Binary Generation for macOS
;;;
;;; This module generates standalone Mach-O executables that can run
;;; without SBCL. These are real native binaries that macOS can execute.
;;;
;;; References:
;;; - https://github.com/aidansteele/osx-abi-macho-file-format-reference
;;; - /usr/include/mach-o/loader.h

;;; ============================================================
;;; MACH-O CONSTANTS
;;; ============================================================

;; CPU types
(defconstant +CPU_TYPE_X86_64+ #x01000007)
(defconstant +CPU_TYPE_ARM64+  #x0100000C)

;; CPU subtypes
(defconstant +CPU_SUBTYPE_X86_64_ALL+ 3)
(defconstant +CPU_SUBTYPE_ARM64_ALL+  0)

;; File types
(defconstant +MH_EXECUTE+ 2)  ; Executable file

;; Flags
(defconstant +MH_NOUNDEFS+ #x00000001)
(defconstant +MH_DYLDLINK+ #x00000004)
(defconstant +MH_PIE+      #x00200000)  ; Position-independent executable

;; Load commands
(defconstant +LC_SEGMENT_64+ #x19)  ; 64-bit segment
(defconstant +LC_UNIXTHREAD+ #x05)  ; Unix thread state
(defconstant +LC_MAIN+       #x28)  ; Entry point (LC_MAIN, modern)

;; Section flags
(defconstant +S_ATTR_PURE_INSTRUCTIONS+ #x80000000)
(defconstant +S_ATTR_SOME_INSTRUCTIONS+ #x00000400)

;; Protection flags
(defconstant +VM_PROT_READ+    1)
(defconstant +VM_PROT_WRITE+   2)
(defconstant +VM_PROT_EXECUTE+ 4)

;;; ============================================================
;;; MACH-O HEADER
;;; ============================================================

(defstruct mach-header-64
  "Mach-O 64-bit header"
  (magic      #xFEEDFACF)  ; MH_MAGIC_64 (little-endian)
  (cputype    +CPU_TYPE_X86_64+)
  (cpusubtype +CPU_SUBTYPE_X86_64_ALL+)
  (filetype   +MH_EXECUTE+)
  (ncmds      0)            ; Number of load commands
  (sizeofcmds 0)            ; Size of load commands
  (flags      (logior +MH_NOUNDEFS+ +MH_DYLDLINK+ +MH_PIE+))
  (reserved   0))

(defun emit-mach-header (header)
  "Emit Mach-O header as bytes (32 bytes)"
  (coerce
   (append
    (int-to-bytes (mach-header-64-magic header) 4)
    (int-to-bytes (mach-header-64-cputype header) 4)
    (int-to-bytes (mach-header-64-cpusubtype header) 4)
    (int-to-bytes (mach-header-64-filetype header) 4)
    (int-to-bytes (mach-header-64-ncmds header) 4)
    (int-to-bytes (mach-header-64-sizeofcmds header) 4)
    (int-to-bytes (mach-header-64-flags header) 4)
    (int-to-bytes (mach-header-64-reserved header) 4))
   'vector))

;;; ============================================================
;;; SEGMENT COMMAND
;;; ============================================================

(defstruct segment-command-64
  "64-bit segment load command"
  (cmd        +LC_SEGMENT_64+)
  (cmdsize    0)              ; Size including sections
  (segname    "__TEXT")       ; Segment name (16 bytes)
  (vmaddr     #x100000000)    ; Virtual memory address
  (vmsize     0)              ; Virtual memory size
  (fileoff    0)              ; File offset
  (filesize   0)              ; File size
  (maxprot    (logior +VM_PROT_READ+ +VM_PROT_WRITE+ +VM_PROT_EXECUTE+))
  (initprot   (logior +VM_PROT_READ+ +VM_PROT_EXECUTE+))
  (nsects     0)              ; Number of sections
  (flags      0))

(defun pad-string (str len)
  "Pad string to length with nulls"
  (let ((bytes (make-list len :initial-element 0)))
    (loop for char across str
          for i from 0
          do (setf (nth i bytes) (char-code char)))
    bytes))

(defun emit-segment-command (seg)
  "Emit segment command as bytes (72 bytes without sections)"
  (append
   (int-to-bytes (segment-command-64-cmd seg) 4)
   (int-to-bytes (segment-command-64-cmdsize seg) 4)
   (pad-string (segment-command-64-segname seg) 16)
   (int-to-bytes (segment-command-64-vmaddr seg) 8)
   (int-to-bytes (segment-command-64-vmsize seg) 8)
   (int-to-bytes (segment-command-64-fileoff seg) 8)
   (int-to-bytes (segment-command-64-filesize seg) 8)
   (int-to-bytes (segment-command-64-maxprot seg) 4)
   (int-to-bytes (segment-command-64-initprot seg) 4)
   (int-to-bytes (segment-command-64-nsects seg) 4)
   (int-to-bytes (segment-command-64-flags seg) 4)))

;;; ============================================================
;;; SECTION
;;; ============================================================

(defstruct section-64
  "64-bit section"
  (sectname   "__text")       ; Section name (16 bytes)
  (segname    "__TEXT")       ; Segment name (16 bytes)
  (addr       #x100001000)    ; Virtual address
  (size       0)              ; Section size
  (offset     0)              ; File offset
  (align      4)              ; Alignment (2^4 = 16 bytes)
  (reloff     0)              ; File offset of relocation entries
  (nreloc     0)              ; Number of relocation entries
  (flags      (logior +S_ATTR_PURE_INSTRUCTIONS+ +S_ATTR_SOME_INSTRUCTIONS+))
  (reserved1  0)
  (reserved2  0)
  (reserved3  0))

(defun emit-section (sec)
  "Emit section as bytes (80 bytes)"
  (append
   (pad-string (section-64-sectname sec) 16)
   (pad-string (section-64-segname sec) 16)
   (int-to-bytes (section-64-addr sec) 8)
   (int-to-bytes (section-64-size sec) 8)
   (int-to-bytes (section-64-offset sec) 4)
   (int-to-bytes (section-64-align sec) 4)
   (int-to-bytes (section-64-reloff sec) 4)
   (int-to-bytes (section-64-nreloc sec) 4)
   (int-to-bytes (section-64-flags sec) 4)
   (int-to-bytes (section-64-reserved1 sec) 4)
   (int-to-bytes (section-64-reserved2 sec) 4)
   (int-to-bytes (section-64-reserved3 sec) 4)))

;;; ============================================================
;;; ENTRY POINT COMMAND
;;; ============================================================

(defstruct entry-point-command
  "LC_MAIN - Modern entry point"
  (cmd        +LC_MAIN+)
  (cmdsize    24)           ; Fixed size
  (entryoff   0)            ; File offset of entry point
  (stacksize  0))           ; Initial stack size (0 = default)

(defun emit-entry-point-command (ep)
  "Emit LC_MAIN command as bytes (24 bytes)"
  (append
   (int-to-bytes (entry-point-command-cmd ep) 4)
   (int-to-bytes (entry-point-command-cmdsize ep) 4)
   (int-to-bytes (entry-point-command-entryoff ep) 8)
   (int-to-bytes (entry-point-command-stacksize ep) 8)))

;;; ============================================================
;;; MACH-O GENERATOR
;;; ============================================================

(defun generate-macho-executable (code &key (arch :x86_64) (output-file "a.out"))
  "Generate a standalone Mach-O executable from machine code

   ARGS:
   - code: Bytecode vector to execute
   - arch: Target architecture (:x86_64 or :arm64)
   - output-file: Path to output executable

   RETURNS: Path to generated executable"

  (let* ((code-size (length code))
         (header-size 32)
         (segment-cmd-size 72)
         (section-size 80)
         (entry-cmd-size 24)
         (load-cmds-size (+ segment-cmd-size section-size entry-cmd-size))
         (headers-size (+ header-size load-cmds-size))
         (code-offset 4096)  ; Start code at page boundary
         (vm-addr #x100001000)

         ;; Create structures
         (header (make-mach-header-64
                  :cputype (ecase arch
                            (:x86_64 +CPU_TYPE_X86_64+)
                            (:arm64  +CPU_TYPE_ARM64+))
                  :cpusubtype (ecase arch
                               (:x86_64 +CPU_SUBTYPE_X86_64_ALL+)
                               (:arm64  +CPU_SUBTYPE_ARM64_ALL+))
                  :ncmds 2
                  :sizeofcmds load-cmds-size))

         (segment (make-segment-command-64
                   :cmdsize (+ segment-cmd-size section-size)
                   :vmaddr #x100000000
                   :vmsize #x1000
                   :fileoff 0
                   :filesize code-offset
                   :nsects 1))

         (section (make-section-64
                   :addr vm-addr
                   :size code-size
                   :offset code-offset
                   :align 4))

         (entry-point (make-entry-point-command
                       :entryoff code-offset)))

    ;; Write executable
    (with-open-file (out output-file
                        :direction :output
                        :if-exists :supersede
                        :element-type '(unsigned-byte 8))

      ;; Write header
      (write-sequence (emit-mach-header header) out)

      ;; Write segment command
      (write-sequence (emit-segment-command segment) out)

      ;; Write section
      (write-sequence (emit-section section) out)

      ;; Write entry point command
      (write-sequence (emit-entry-point-command entry-point) out)

      ;; Pad to code offset
      (let ((padding (- code-offset (file-position out))))
        (write-sequence (make-array padding
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0)
                       out))

      ;; Write code
      (write-sequence code out))

    ;; Make executable
    #+sbcl
    (sb-ext:run-program "/bin/chmod" (list "+x" output-file))

    (format t "~%Generated Mach-O executable: ~A~%" output-file)
    (format t "  Architecture: ~A~%" arch)
    (format t "  Code size: ~D bytes~%" code-size)
    (format t "  Entry point: 0x~X~%" vm-addr)

    output-file))

;;; ============================================================
;;; HIGH-LEVEL API
;;; ============================================================

(defun add-exit-syscall (code arch)
  "Add exit syscall to code so program terminates cleanly"
  (let ((code-list (coerce code 'list)))  ; Convert vector to list
    (coerce
     (ecase arch
       (:x86_64
        ;; Exit syscall on macOS x86_64:
        ;; mov rdi, rax    ; Exit code from RAX
        ;; mov rax, #x2000001  ; sys_exit on macOS (BSD syscall #1 + #x2000000)
        ;; syscall
        (append code-list
                '(#x48 #x89 #xC7)              ; mov rdi, rax
                '(#x48 #xC7 #xC0 #x01 #x00 #x00 #x02)  ; mov rax, #x2000001
                '(#x0F #x05)))                 ; syscall

       (:arm64
        ;; Exit syscall on macOS ARM64:
        ;; mov x16, #1     ; sys_exit
        ;; mov x0, x0      ; Exit code already in x0
        ;; svc #0x80       ; Syscall
        (append code-list
                '(#x10 #x00 #x80 #xD2)         ; mov x16, #1
                '(#x01 #x10 #x00 #xD4))))      ; svc #0x80
     'vector)))

(defun compile-to-executable (expr &key (arch :x86_64) (output-file "a.out"))
  "Compile expression and generate standalone executable

   USAGE:
   (compile-to-executable '(+ 2 3) :output-file \"hello\")

   This creates a standalone executable that can be run:
   $ ./hello
   $ echo $?  ; Shows exit code (80 = 5 << 4, tagged fixnum)
   "

  (format t "~%Compiling expression: ~S~%" expr)

  ;; Switch to inline allocation mode
  (let ((*allocation-mode* :inline))

    ;; Compile expression
    (let* ((expr-code (compile-expression expr :arch arch))
           ;; Add exit syscall so program terminates
           (full-code (add-exit-syscall expr-code arch)))

      (format t "  Generated ~D bytes of machine code~%" (length full-code))

      ;; Generate executable
      (generate-macho-executable full-code :arch arch :output-file output-file)

      (format t "~%Done! Run with: ./~A~%" output-file)
      (format t "Exit code will be the result (tagged fixnum)~%")
      output-file)))

(export '(generate-macho-executable
          compile-to-executable))
