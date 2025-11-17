;;;; ELF binary writer for compiled Habu code

(in-package :habu-compiler)

;;; ELF file format structures
;;; This creates minimal executable ELF binaries

(defun write-elf-header (stream arch)
  "Write ELF header"
  (let ((machine (ecase arch
                   (:x86_64 #x3E) ; EM_X86_64
                   (:arm64 #xB7)))) ; EM_AARCH64

    ;; ELF identification
    (write-sequence #(#x7F #x45 #x4C #x46) stream) ; Magic: 0x7F 'ELF'
    (write-byte #x02 stream)  ; EI_CLASS: 64-bit
    (write-byte #x01 stream)  ; EI_DATA: little-endian
    (write-byte #x01 stream)  ; EI_VERSION: current
    (write-byte #x00 stream)  ; EI_OSABI: System V
    (write-sequence (make-array 8 :initial-element 0) stream) ; padding

    ;; ELF header fields
    (write-u16 #x02 stream)   ; e_type: ET_EXEC
    (write-u16 machine stream) ; e_machine
    (write-u32 #x01 stream)   ; e_version
    (write-u64 #x400000 stream) ; e_entry (entry point address)
    (write-u64 #x40 stream)   ; e_phoff (program header offset)
    (write-u64 #x00 stream)   ; e_shoff (section header offset)
    (write-u32 #x00 stream)   ; e_flags
    (write-u16 #x40 stream)   ; e_ehsize (ELF header size)
    (write-u16 #x38 stream)   ; e_phentsize (program header entry size)
    (write-u16 #x01 stream)   ; e_phnum (number of program headers)
    (write-u16 #x40 stream)   ; e_shentsize (section header entry size)
    (write-u16 #x00 stream)   ; e_shnum (number of section headers)
    (write-u16 #x00 stream))) ; e_shstrndx

(defun write-program-header (stream code-size)
  "Write ELF program header"
  (write-u32 #x01 stream)   ; p_type: PT_LOAD
  (write-u32 #x05 stream)   ; p_flags: PF_R | PF_X (readable, executable)
  (write-u64 #x00 stream)   ; p_offset
  (write-u64 #x400000 stream) ; p_vaddr
  (write-u64 #x400000 stream) ; p_paddr
  (write-u64 code-size stream) ; p_filesz
  (write-u64 code-size stream) ; p_memsz
  (write-u64 #x1000 stream))   ; p_align

(defun write-u16 (value stream)
  "Write 16-bit unsigned integer in little-endian"
  (write-byte (ldb (byte 8 0) value) stream)
  (write-byte (ldb (byte 8 8) value) stream))

(defun write-u32 (value stream)
  "Write 32-bit unsigned integer in little-endian"
  (write-byte (ldb (byte 8 0) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 24) value) stream))

(defun write-u64 (value stream)
  "Write 64-bit unsigned integer in little-endian"
  (loop for i from 0 below 8
        do (write-byte (ldb (byte 8 (* i 8)) value) stream)))

(defun write-elf-binary (code output-file arch)
  "Write machine code as ELF executable"
  (let* ((code-vec (if (vectorp code) code (bytes-to-vector code)))
         (code-size (+ (length code-vec) 1))) ; +1 for ret instruction

    (with-open-file (out output-file
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      ;; Write ELF header
      (write-elf-header out arch)

      ;; Write program header
      (write-program-header out code-size)

      ;; Pad to align code
      (loop repeat (- #x78 (file-position out))
            do (write-byte 0 out))

      ;; Write code
      (write-sequence code-vec out)

      ;; Write return instruction
      (ecase arch
        (:x86_64 (write-byte #xC3 out))  ; ret
        (:arm64 (write-sequence #(#xC0 #x03 #x5F #xD6) out)))) ; ret

    output-file))
