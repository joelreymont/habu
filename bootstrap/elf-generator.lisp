;;;; elf-generator.lisp - Generate ELF executable binaries for Linux

(in-package :habu-compiler)

;;; ELF Binary Generation for Linux
;;;
;;; ELF is much simpler than Mach-O - no complex load commands needed!
;;; Just: ELF header + Program header + Code = working executable
;;;
;;; References:
;;; - https://en.wikipedia.org/wiki/Executable_and_Linkable_Format
;;; - /usr/include/elf.h

;;; ============================================================
;;; ELF CONSTANTS
;;; ============================================================

;; ELF magic number
(defconstant +ELF-MAGIC+ '(#x7F #x45 #x4C #x46))  ; "\x7FELF"

;; ELF class
(defconstant +ELFCLASS64+ 2)  ; 64-bit

;; Data encoding
(defconstant +ELFDATA2LSB+ 1)  ; Little-endian

;; EI_VERSION
(defconstant +EV_CURRENT+ 1)

;; OS/ABI
(defconstant +ELFOSABI_SYSV+ 0)  ; System V ABI

;; File type
(defconstant +ET_EXEC+ 2)  ; Executable file

;; Machine
(defconstant +EM_X86_64+ 62)   ; AMD x86-64
(defconstant +EM_AARCH64+ 183) ; ARM 64-bit

;; Program header types
(defconstant +PT_LOAD+ 1)  ; Loadable segment

;; Program header flags
(defconstant +PF_X+ 1)  ; Execute
(defconstant +PF_W+ 2)  ; Write
(defconstant +PF_R+ 4)  ; Read

;;; ============================================================
;;; ELF HEADER (64 bytes)
;;; ============================================================

(defun emit-elf-header (entry-point machine phoff phnum)
  "Emit ELF-64 header (64 bytes)

   ARGS:
   - entry-point: Virtual address of entry point
   - machine: EM_X86_64 or EM_AARCH64
   - phoff: Program header offset
   - phnum: Number of program headers"

  (coerce
   (append
    ;; e_ident (16 bytes)
    +ELF-MAGIC+                           ; Magic: 0x7F 'E' 'L' 'F'
    (list +ELFCLASS64+)                   ; Class: 64-bit
    (list +ELFDATA2LSB+)                  ; Data: little-endian
    (list +EV_CURRENT+)                   ; Version: 1
    (list +ELFOSABI_SYSV+)                ; OS/ABI: System V
    (list 0)                              ; ABI version
    (make-list 7 :initial-element 0)     ; Padding

    ;; e_type (2 bytes)
    (int-to-bytes +ET_EXEC+ 2)

    ;; e_machine (2 bytes)
    (int-to-bytes machine 2)

    ;; e_version (4 bytes)
    (int-to-bytes +EV_CURRENT+ 4)

    ;; e_entry (8 bytes)
    (int-to-bytes entry-point 8)

    ;; e_phoff (8 bytes) - program header offset
    (int-to-bytes phoff 8)

    ;; e_shoff (8 bytes) - section header offset (0 = none)
    (int-to-bytes 0 8)

    ;; e_flags (4 bytes)
    (int-to-bytes 0 4)

    ;; e_ehsize (2 bytes) - ELF header size
    (int-to-bytes 64 2)

    ;; e_phentsize (2 bytes) - program header entry size
    (int-to-bytes 56 2)

    ;; e_phnum (2 bytes) - number of program headers
    (int-to-bytes phnum 2)

    ;; e_shentsize (2 bytes) - section header entry size
    (int-to-bytes 0 2)

    ;; e_shnum (2 bytes) - number of section headers
    (int-to-bytes 0 2)

    ;; e_shstrndx (2 bytes) - section header string table index
    (int-to-bytes 0 2))
   'vector))

;;; ============================================================
;;; PROGRAM HEADER (56 bytes)
;;; ============================================================

(defun emit-program-header (vaddr filesz memsz flags)
  "Emit ELF-64 program header (56 bytes)

   ARGS:
   - vaddr: Virtual address to load segment
   - filesz: Size in file
   - memsz: Size in memory
   - flags: PF_R | PF_W | PF_X"

  (coerce
   (append
    ;; p_type (4 bytes)
    (int-to-bytes +PT_LOAD+ 4)

    ;; p_flags (4 bytes) - must come before p_offset in 64-bit
    (int-to-bytes flags 4)

    ;; p_offset (8 bytes) - offset in file
    (int-to-bytes 0 8)  ; Load from start of file

    ;; p_vaddr (8 bytes) - virtual address
    (int-to-bytes vaddr 8)

    ;; p_paddr (8 bytes) - physical address (same as virtual)
    (int-to-bytes vaddr 8)

    ;; p_filesz (8 bytes) - size in file
    (int-to-bytes filesz 8)

    ;; p_memsz (8 bytes) - size in memory
    (int-to-bytes memsz 8)

    ;; p_align (8 bytes) - alignment (4096 = page size)
    (int-to-bytes 4096 8))
   'vector))

;;; ============================================================
;;; ELF GENERATOR
;;; ============================================================

(defun generate-elf-executable (code &key (arch :x86_64) (output-file "a.out"))
  "Generate a standalone ELF executable from machine code

   ARGS:
   - code: Bytecode vector to execute
   - arch: Target architecture (:x86_64 or :arm64)
   - output-file: Path to output executable

   RETURNS: Path to generated executable"

  (let* ((code-size (length code))
         (elf-header-size 64)
         (program-header-size 56)
         (headers-size (+ elf-header-size program-header-size))

         ;; Entry point: right after headers
         (entry-point (+ #x400000 headers-size))  ; Standard load address

         ;; Total file size
         (total-size (+ headers-size code-size))

         ;; Program header info
         (phoff elf-header-size)  ; Program headers start after ELF header
         (phnum 1)                 ; One program header (PT_LOAD)

         ;; Machine type
         (machine (ecase arch
                   (:x86_64 +EM_X86_64+)
                   (:arm64  +EM_AARCH64+)))

         ;; Segment flags (read + execute)
         (flags (logior +PF_R+ +PF_X+)))

    ;; Generate headers
    (let ((elf-header (emit-elf-header entry-point machine phoff phnum))
          (prog-header (emit-program-header #x400000 total-size total-size flags)))

      ;; Write executable
      (with-open-file (out output-file
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))

        ;; Write ELF header
        (write-sequence elf-header out)

        ;; Write program header
        (write-sequence prog-header out)

        ;; Write code
        (write-sequence code out))

      ;; Make executable
      #+sbcl
      (sb-ext:run-program "/bin/chmod" (list "+x" output-file))

      (format t "~%Generated ELF executable: ~A~%" output-file)
      (format t "  Architecture: ~A~%" arch)
      (format t "  Code size: ~D bytes~%" code-size)
      (format t "  Entry point: 0x~X~%" entry-point)
      (format t "  Total file size: ~D bytes~%" total-size)

      output-file)))

;;; ============================================================
;;; EXIT SYSCALL FOR LINUX
;;; ============================================================

(defun add-exit-syscall-linux (code arch)
  "Add exit syscall for Linux (different from macOS)"
  (let ((code-list (coerce code 'list)))
    (coerce
     (ecase arch
       (:x86_64
        ;; Linux x86_64 syscall convention:
        ;; syscall number in RAX, args in RDI, RSI, RDX, R10, R8, R9
        ;; exit = syscall 60
        (append code-list
                '(#x48 #x89 #xC7)              ; mov rdi, rax (exit code)
                '(#x48 #xC7 #xC0 #x3C #x00 #x00 #x00)  ; mov rax, 60 (sys_exit)
                '(#x0F #x05)))                 ; syscall

       (:arm64
        ;; Linux ARM64 syscall convention:
        ;; syscall number in x8, args in x0-x5
        ;; exit = syscall 93
        (append code-list
                '(#x68 #x0B #x80 #xD2)         ; mov x8, #93 (sys_exit)
                '(#x01 #x00 #x00 #xD4))))      ; svc #0
     'vector)))

;;; ============================================================
;;; HIGH-LEVEL API
;;; ============================================================

(defun compile-to-elf (expr &key (arch :x86_64) (output-file "a.out"))
  "Compile expression and generate standalone ELF executable

   USAGE:
   (compile-to-elf '(+ 2 3) :output-file \"hello\")

   This creates a standalone ELF executable for Linux:
   $ ./hello
   $ echo $?  ; Shows exit code (80 = 5 << 4, tagged fixnum)

   To test on macOS, use Docker:
   $ docker run --rm -v $(pwd):/work ubuntu:22.04 /work/hello
   "

  (format t "~%Compiling expression to ELF: ~S~%" expr)

  ;; Switch to inline allocation mode
  (let ((*allocation-mode* :inline))

    ;; Compile expression
    (let* ((expr-code (compile-expression expr :arch arch))
           ;; Add Linux exit syscall
           (full-code (add-exit-syscall-linux expr-code arch)))

      (format t "  Generated ~D bytes of machine code~%" (length full-code))

      ;; Generate ELF executable
      (generate-elf-executable full-code :arch arch :output-file output-file)

      (format t "~%Done! Run with: ./~A~%" output-file)
      (format t "Exit code will be the result (tagged fixnum)~%")
      (format t "~%On macOS, test with Docker:~%")
      (format t "  docker run --rm -v $(pwd):/work ubuntu:22.04 /work/~A~%" output-file)
      output-file)))

(export '(generate-elf-executable
          compile-to-elf
          add-exit-syscall-linux))
