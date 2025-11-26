;;; Mach-O Native Linker for Habu
;;;
;;; Generates standalone ARM64 Mach-O executables without clang.
;;; Creates minimal executables that link against libSystem.B.dylib.
;;;
;;; Required load commands for a working macOS executable:
;;;   1. LC_SEGMENT_64 __PAGEZERO - Null pointer trap zone
;;;   2. LC_SEGMENT_64 __TEXT     - Code and read-only data
;;;   3. LC_SEGMENT_64 __LINKEDIT - Symbol tables and binding info
;;;   4. LC_LOAD_DYLINKER         - Path to dyld
;;;   5. LC_SYMTAB                - Symbol table
;;;   6. LC_DYSYMTAB              - Dynamic symbol table
;;;   7. LC_LOAD_DYLIB            - libSystem.B.dylib
;;;   8. LC_MAIN                  - Entry point

(in-package :cl-user)

;; Load ARM64 assembler
(load (merge-pathnames "arm64/asm.lisp" (or *load-pathname* *default-pathname-defaults*)))

(defpackage :habu-macho
  (:use :cl)
  (:export #:write-macho-executable
           #:write-macho-executable-with-heap
           #:link-with-runtime
           #:deliver-native
           #:deliver-native-with-heap
           #:wrap-bytecode-for-exit
           #:wrap-bytecode-with-heap
           #:test-minimal-macho))

(in-package :habu-macho)

;;; ============================================================
;;; Mach-O Constants
;;; ============================================================

;; Magic numbers
(defconstant +MH-MAGIC-64+ #xFEEDFACF)

;; CPU types
(defconstant +CPU-TYPE-ARM64+ #x0100000C)
(defconstant +CPU-SUBTYPE-ARM64-ALL+ #x00000000)

;; File types
(defconstant +MH-EXECUTE+ 2)

;; Header flags
(defconstant +MH-NOUNDEFS+       #x00000001)
(defconstant +MH-DYLDLINK+       #x00000004)
(defconstant +MH-TWOLEVEL+       #x00000080)
(defconstant +MH-PIE+            #x00200000)

;; Load command types
(defconstant +LC-SEGMENT-64+     #x19)
(defconstant +LC-SYMTAB+         #x02)
(defconstant +LC-DYSYMTAB+       #x0B)
(defconstant +LC-LOAD-DYLINKER+  #x0E)
(defconstant +LC-UUID+           #x1B)
(defconstant +LC-BUILD-VERSION+  #x32)
(defconstant +LC-MAIN+           #x80000028)
(defconstant +LC-LOAD-DYLIB+     #x0C)
(defconstant +LC-FUNCTION-STARTS+ #x26)
(defconstant +LC-CODE-SIGNATURE+ #x1D)
(defconstant +LC-DYLD-CHAINED-FIXUPS+ #x80000034)
(defconstant +LC-DYLD-EXPORTS-TRIE+ #x80000033)

;; VM protection flags
(defconstant +VM-PROT-READ+    #x01)
(defconstant +VM-PROT-WRITE+   #x02)
(defconstant +VM-PROT-EXECUTE+ #x04)

;; Section flags
(defconstant +S-ATTR-PURE-INSTRUCTIONS+ #x80000000)
(defconstant +S-ATTR-SOME-INSTRUCTIONS+ #x00000400)

;; Page size on ARM64 macOS
(defconstant +PAGE-SIZE+ #x4000)  ; 16KB

;; VM base address
(defconstant +VM-BASE+ #x100000000)

;;; ============================================================
;;; Binary Writing Utilities
;;; ============================================================

(defun write-u8 (stream val)
  (write-byte (logand val #xFF) stream))

(defun write-u16-le (stream val)
  (write-u8 stream (logand val #xFF))
  (write-u8 stream (logand (ash val -8) #xFF)))

(defun write-u32-le (stream val)
  (write-u8 stream (logand val #xFF))
  (write-u8 stream (logand (ash val -8) #xFF))
  (write-u8 stream (logand (ash val -16) #xFF))
  (write-u8 stream (logand (ash val -24) #xFF)))

(defun write-u64-le (stream val)
  (write-u32-le stream (logand val #xFFFFFFFF))
  (write-u32-le stream (logand (ash val -32) #xFFFFFFFF)))

(defun write-bytes (stream bytes)
  (dolist (b bytes)
    (write-byte b stream)))

(defun write-string-padded (stream str len)
  "Write string padded with zeros to LEN bytes"
  (let ((slen (length str)))
    (dotimes (i (min slen len))
      (write-byte (char-code (char str i)) stream))
    (dotimes (i (- len slen))
      (write-byte 0 stream))))

(defun write-zeros (stream count)
  (dotimes (i count)
    (write-byte 0 stream)))

(defun align-up (val alignment)
  "Round VAL up to next multiple of ALIGNMENT"
  (let ((rem (mod val alignment)))
    (if (zerop rem)
        val
        (+ val (- alignment rem)))))

;;; ============================================================
;;; Mach-O Header (32 bytes for 64-bit)
;;; ============================================================

(defun write-mach-header-64 (stream ncmds sizeofcmds flags)
  (write-u32-le stream +MH-MAGIC-64+)           ; magic
  (write-u32-le stream +CPU-TYPE-ARM64+)        ; cputype
  (write-u32-le stream +CPU-SUBTYPE-ARM64-ALL+) ; cpusubtype
  (write-u32-le stream +MH-EXECUTE+)            ; filetype
  (write-u32-le stream ncmds)                   ; ncmds
  (write-u32-le stream sizeofcmds)              ; sizeofcmds
  (write-u32-le stream flags)                   ; flags
  (write-u32-le stream 0))                      ; reserved

;;; ============================================================
;;; Load Commands
;;; ============================================================

(defun write-segment-command-64 (stream segname vmaddr vmsize fileoff filesize
                                 maxprot initprot nsects flags)
  (write-u32-le stream +LC-SEGMENT-64+)
  (write-u32-le stream (+ 72 (* nsects 80)))    ; cmdsize
  (write-string-padded stream segname 16)
  (write-u64-le stream vmaddr)
  (write-u64-le stream vmsize)
  (write-u64-le stream fileoff)
  (write-u64-le stream filesize)
  (write-u32-le stream maxprot)
  (write-u32-le stream initprot)
  (write-u32-le stream nsects)
  (write-u32-le stream flags))

(defun write-section-64 (stream sectname segname addr size offset align
                         reloff nreloc flags reserved1 reserved2)
  (write-string-padded stream sectname 16)
  (write-string-padded stream segname 16)
  (write-u64-le stream addr)
  (write-u64-le stream size)
  (write-u32-le stream offset)
  (write-u32-le stream align)
  (write-u32-le stream reloff)
  (write-u32-le stream nreloc)
  (write-u32-le stream flags)
  (write-u32-le stream reserved1)
  (write-u32-le stream reserved2)
  (write-u32-le stream 0))                      ; reserved3

(defun write-dylinker-command (stream path)
  "Write LC_LOAD_DYLINKER command"
  (let* ((path-len (1+ (length path)))
         (cmdsize (align-up (+ 12 path-len) 8)))
    (write-u32-le stream +LC-LOAD-DYLINKER+)
    (write-u32-le stream cmdsize)
    (write-u32-le stream 12)                    ; path.offset
    (write-string-padded stream path (- cmdsize 12))))

(defun write-uuid-command (stream)
  "Write LC_UUID command with random UUID"
  (write-u32-le stream +LC-UUID+)
  (write-u32-le stream 24)
  ;; Generate a simple UUID based on time
  (let ((time (get-universal-time)))
    (write-u32-le stream time)
    (write-u32-le stream (logxor time #xDEADBEEF))
    (write-u32-le stream (logxor time #xCAFEBABE))
    (write-u32-le stream (logxor time #x12345678))))

(defun write-build-version-command (stream)
  "Write LC_BUILD_VERSION for macOS 14.0"
  (write-u32-le stream +LC-BUILD-VERSION+)
  (write-u32-le stream 24)                      ; cmdsize (no tools)
  (write-u32-le stream 1)                       ; platform = macOS
  (write-u32-le stream #x000E0000)              ; minos = 14.0
  (write-u32-le stream #x000E0000)              ; sdk = 14.0
  (write-u32-le stream 0))                      ; ntools

(defun write-main-command (stream entryoff)
  (write-u32-le stream +LC-MAIN+)
  (write-u32-le stream 24)
  (write-u64-le stream entryoff)
  (write-u64-le stream 0))                      ; stacksize = 0 (default)

(defun write-load-dylib-command (stream path)
  (let* ((path-len (1+ (length path)))
         (cmdsize (align-up (+ 24 path-len) 8)))
    (write-u32-le stream +LC-LOAD-DYLIB+)
    (write-u32-le stream cmdsize)
    (write-u32-le stream 24)                    ; name.offset
    (write-u32-le stream 2)                     ; timestamp
    (write-u32-le stream #x054C0000)            ; current_version
    (write-u32-le stream #x00010000)            ; compatibility_version
    (write-string-padded stream path (- cmdsize 24))))

(defun write-symtab-command (stream symoff nsyms stroff strsize)
  (write-u32-le stream +LC-SYMTAB+)
  (write-u32-le stream 24)
  (write-u32-le stream symoff)
  (write-u32-le stream nsyms)
  (write-u32-le stream stroff)
  (write-u32-le stream strsize))

(defun write-dysymtab-command (stream ilocalsym nlocalsym
                               iextdefsym nextdefsym
                               iundefsym nundefsym)
  ;; Total struct size is 80 bytes = 20 u32 fields
  ;; cmd + cmdsize = 8 bytes, remaining 18 fields = 72 bytes
  (write-u32-le stream +LC-DYSYMTAB+)
  (write-u32-le stream 80)
  (write-u32-le stream ilocalsym)
  (write-u32-le stream nlocalsym)
  (write-u32-le stream iextdefsym)
  (write-u32-le stream nextdefsym)
  (write-u32-le stream iundefsym)
  (write-u32-le stream nundefsym)
  ;; 6 params written, 12 more fields to write as zeros = 48 bytes
  (write-zeros stream 48))

(defun write-function-starts-command (stream dataoff datasize)
  (write-u32-le stream +LC-FUNCTION-STARTS+)
  (write-u32-le stream 16)
  (write-u32-le stream dataoff)
  (write-u32-le stream datasize))

(defun write-code-signature-command (stream dataoff datasize)
  "Write LC_CODE_SIGNATURE command - codesign will fill in the actual values"
  (write-u32-le stream +LC-CODE-SIGNATURE+)
  (write-u32-le stream 16)
  (write-u32-le stream dataoff)
  (write-u32-le stream datasize))

;;; ============================================================
;;; nlist_64 Symbol Entry (16 bytes)
;;; ============================================================

(defun write-nlist-64 (stream strx type sect desc value)
  (write-u32-le stream strx)                    ; string table index
  (write-u8 stream type)                        ; n_type
  (write-u8 stream sect)                        ; n_sect
  (write-u16-le stream desc)                    ; n_desc
  (write-u64-le stream value))                  ; n_value

;;; ============================================================
;;; Main Linker Function
;;; ============================================================

(defun write-macho-executable (output-path code-bytes &key verbose)
  "Write a Mach-O executable that runs CODE-BYTES and exits with x0 as status.
   CODE-BYTES should be ARM64 machine code ending in RET."
  (let* ((code-size (length code-bytes))

         ;; Calculate sizes for load commands
         (header-size 32)
         (pagezero-cmd-size 72)
         (text-cmd-size (+ 72 80))              ; segment + 1 section
         (linkedit-cmd-size 72)                 ; segment, no sections
         (dylinker-path "/usr/lib/dyld")
         (dylinker-cmd-size (align-up (+ 12 (length dylinker-path) 1) 8))
         (uuid-cmd-size 24)
         (build-version-cmd-size 24)
         (main-cmd-size 24)
         (libsystem-path "/usr/lib/libSystem.B.dylib")
         (load-dylib-cmd-size (align-up (+ 24 (length libsystem-path) 1) 8))
         (symtab-cmd-size 24)
         (dysymtab-cmd-size 80)

         ;; Number of load commands (no LC_CODE_SIGNATURE - codesign will add it)
         (ncmds 10)
         (sizeofcmds (+ pagezero-cmd-size
                       text-cmd-size
                       linkedit-cmd-size
                       dylinker-cmd-size
                       uuid-cmd-size
                       build-version-cmd-size
                       main-cmd-size
                       load-dylib-cmd-size
                       symtab-cmd-size
                       dysymtab-cmd-size))

         ;; Reserve space for codesign to add LC_CODE_SIGNATURE (16 bytes)
         ;; Round up to page boundary minus a bit for good measure
         ;; Code placement - leave room for codesign load command (align to 64)
         (code-offset (align-up (+ header-size sizeofcmds 64) 64))

         ;; __TEXT segment covers first page
         (text-segment-size +PAGE-SIZE+)

         ;; __LINKEDIT segment starts at second page
         (linkedit-fileoff +PAGE-SIZE+)
         (linkedit-vmaddr (+ +VM-BASE+ +PAGE-SIZE+))

         ;; String table for symbols (just null byte and _main)
         (string-table '(0                      ; first byte is null
                         95 109 97 105 110 0))  ; "_main\0"
         (string-table-size (length string-table))

         ;; Symbol table offset (at start of LINKEDIT)
         (symtab-offset linkedit-fileoff)
         (nsyms 1)
         (nlist-size (* nsyms 16))

         ;; String table offset (after symbols)
         (strtab-offset (+ symtab-offset nlist-size))

         ;; LINKEDIT total size
         (linkedit-size (align-up (+ nlist-size string-table-size) 8))

         ;; Entry point offset from start of file (LC_MAIN.entryoff)
         (entry-offset code-offset))

    (when verbose
      (format t "Code offset: ~D (~X)~%" code-offset code-offset)
      (format t "Code size: ~D~%" code-size)
      (format t "LINKEDIT offset: ~D~%" linkedit-fileoff)
      (format t "Entry offset: ~D~%" entry-offset))

    (with-open-file (out output-path
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))

      ;; === Mach-O Header ===
      (write-mach-header-64 out ncmds sizeofcmds
                            (logior +MH-NOUNDEFS+ +MH-DYLDLINK+
                                    +MH-TWOLEVEL+ +MH-PIE+))

      ;; === Load Commands ===

      ;; 1. __PAGEZERO (4GB null zone)
      (write-segment-command-64 out "__PAGEZERO"
                                0 +VM-BASE+     ; vmaddr=0, vmsize=4GB
                                0 0             ; fileoff=0, filesize=0
                                0 0             ; no protection
                                0 0)            ; no sections

      ;; 2. __TEXT segment
      (write-segment-command-64 out "__TEXT"
                                +VM-BASE+ text-segment-size
                                0 text-segment-size
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                1 0)
      ;; __text section
      (write-section-64 out "__text" "__TEXT"
                        (+ +VM-BASE+ code-offset) code-size
                        code-offset
                        2                       ; align 2^2 = 4
                        0 0                     ; no relocations
                        (logior +S-ATTR-PURE-INSTRUCTIONS+ +S-ATTR-SOME-INSTRUCTIONS+)
                        0 0)

      ;; 3. __LINKEDIT segment
      (write-segment-command-64 out "__LINKEDIT"
                                linkedit-vmaddr +PAGE-SIZE+
                                linkedit-fileoff linkedit-size
                                +VM-PROT-READ+
                                +VM-PROT-READ+
                                0 0)            ; no sections

      ;; 4. LC_LOAD_DYLINKER
      (write-dylinker-command out dylinker-path)

      ;; 5. LC_UUID
      (write-uuid-command out)

      ;; 6. LC_BUILD_VERSION
      (write-build-version-command out)

      ;; 7. LC_MAIN
      (write-main-command out entry-offset)

      ;; 8. LC_LOAD_DYLIB for libSystem
      (write-load-dylib-command out libsystem-path)

      ;; 9. LC_SYMTAB
      (write-symtab-command out symtab-offset nsyms strtab-offset string-table-size)

      ;; 10. LC_DYSYMTAB
      (write-dysymtab-command out
                              0 0               ; no locals
                              0 1               ; 1 external symbol at index 0
                              1 0)              ; no undefined

      ;; === Padding to code offset ===
      (let ((current (file-position out)))
        (when verbose
          (format t "After load commands, position: ~D (~X), need: ~D (~X)~%"
                  current current code-offset code-offset))
        (when (< current code-offset)
          (write-zeros out (- code-offset current))))

      ;; === Code Section ===
      (dolist (b code-bytes)
        (write-byte b out))

      ;; === Padding to LINKEDIT ===
      (let ((current (file-position out)))
        (write-zeros out (- linkedit-fileoff current)))

      ;; === LINKEDIT Section ===

      ;; Symbol table (nlist_64)
      ;; _main symbol: external defined in section 1
      (write-nlist-64 out
                      1                         ; strx = offset of "_main" in strtab
                      #x0F                      ; N_SECT | N_EXT
                      1                         ; section 1 (__text)
                      #x0010                    ; REFERENCED_DYNAMICALLY
                      (+ +VM-BASE+ code-offset)) ; address

      ;; String table
      (dolist (b string-table)
        (write-byte b out))

      ;; Pad to alignment
      (let ((current (file-position out)))
        (write-zeros out (- (+ linkedit-fileoff linkedit-size) current))))

    ;; Make executable
    (sb-ext:run-program "/bin/chmod" (list "+x" output-path))

    output-path))

;;; ============================================================
;;; Code Transformation for Standalone Execution
;;; ============================================================

(defun wrap-bytecode-for-exit (code-bytes)
  "Wrap compiled bytecode to untag the result for use as exit code.
   The bootstrap compiler returns tagged fixnums (value << 4).

   Solution: Prepend a wrapper stub that:
   1. Saves LR (x30) on stack
   2. Calls the original main via BL
   3. Untags the result (lsr x0, x0, #4)
   4. Restores LR
   5. Returns to OS

   Stub (28 bytes = 7 instructions):
     sub sp, sp, #16      ; allocate stack space
     str x30, [sp]        ; save LR
     bl +5                ; call original main at offset 28
     lsr x0, x0, #4       ; untag result
     ldr x30, [sp]        ; restore LR
     add sp, sp, #16      ; clean up stack
     ret                  ; return to OS
     <original code>      ; starts at offset 28"
  (let ((stub (append
               (arm64:sub arm64:+sp+ arm64:+sp+ #x10 :imm t)  ; sub sp, sp, #16
               (arm64:str arm64:+lr+ arm64:+sp+)              ; str x30, [sp]
               (arm64:bl 5)                                    ; bl +5
               (arm64:lsr 0 0 4 :imm t)                        ; lsr x0, x0, #4
               (arm64:ldr arm64:+lr+ arm64:+sp+)              ; ldr x30, [sp]
               (arm64:add arm64:+sp+ arm64:+sp+ #x10 :imm t)  ; add sp, sp, #16
               (arm64:ret))))                                  ; ret
    (append stub code-bytes)))

(defun wrap-bytecode-with-heap (code-bytes heap-page-offset)
  "Wrap bytecode with heap initialization, code base, and untagging.

   HEAP-PAGE-OFFSET is the page offset from the ADRP instruction to __DATA.
   For standard layout (code at page 0, heap at page 1), this is 1.
   x28 is used as the heap bump pointer.
   x26 is used as the code base pointer (for closure funcall).

   Uses PC-relative addressing (ADRP + ADD) so PIE/ASLR works correctly.

   Stub (52 bytes = 13 instructions):
     0: sub sp, sp, #32      ; allocate stack space
     1: str x30, [sp]        ; save LR
     2: str x28, [sp, #8]    ; save x28
     3: str x26, [sp, #16]   ; save x26
     4: adrp x28, #page_off  ; load heap page address (PC-relative)
     5: adr x26, +32         ; x26 = address of main code (8 instrs ahead = 32 bytes)
     6: bl +7                ; call original main (skip 6 instrs to reach instr 13)
     7: lsr x0, x0, #4       ; untag result
     8: ldr x26, [sp, #16]   ; restore x26
     9: ldr x28, [sp, #8]    ; restore x28
    10: ldr x30, [sp]        ; restore LR
    11: add sp, sp, #32      ; clean up stack
    12: ret                  ; return to OS
    13: <original code>      ; starts at offset 52"
  (let* ((stub (append
                (arm64:sub arm64:+sp+ arm64:+sp+ #x20 :imm t)  ; sub sp, sp, #32
                (arm64:str arm64:+lr+ arm64:+sp+)              ; str x30, [sp]
                (arm64:str 28 arm64:+sp+ :offset 8)            ; str x28, [sp, #8]
                (arm64:str 26 arm64:+sp+ :offset 16)           ; str x26, [sp, #16]
                (arm64:adrp 28 heap-page-offset)               ; adrp x28, heap (PC-relative)
                ;; ADR x26, +32 puts address of main code (instr 13) into x26
                ;; ADR offset = (target_offset - current_offset) = 52 - 20 = 32
                (arm64:adr 26 32)                              ; adr x26, +32 (code base)
                (arm64:bl 7)                                   ; bl +7 (skip 6 instrs to main)
                (arm64:lsr 0 0 4 :imm t)                       ; lsr x0, x0, #4
                (arm64:ldr 26 arm64:+sp+ :offset 16)           ; ldr x26, [sp, #16]
                (arm64:ldr 28 arm64:+sp+ :offset 8)            ; ldr x28, [sp, #8]
                (arm64:ldr arm64:+lr+ arm64:+sp+)              ; ldr x30, [sp]
                (arm64:add arm64:+sp+ arm64:+sp+ #x20 :imm t)  ; add sp, sp, #32
                (arm64:ret))))                                 ; ret
    (append stub code-bytes)))

;;; ============================================================
;;; Mach-O with Heap (__DATA segment)
;;; ============================================================

(defun write-macho-executable-with-heap (output-path code-bytes heap-size &key verbose)
  "Write a Mach-O executable with a __DATA segment for heap allocation.
   CODE-BYTES should be ARM64 machine code.
   HEAP-SIZE is the size in bytes for the heap (typically 1MB = #x100000).
   Returns the virtual address of the heap."
  (let* ((code-size (length code-bytes))

         ;; Calculate sizes for load commands
         (header-size 32)
         (pagezero-cmd-size 72)
         (text-cmd-size (+ 72 80))              ; segment + 1 section
         (data-cmd-size (+ 72 80))              ; segment + 1 section for heap
         (linkedit-cmd-size 72)                 ; segment, no sections
         (dylinker-path "/usr/lib/dyld")
         (dylinker-cmd-size (align-up (+ 12 (length dylinker-path) 1) 8))
         (uuid-cmd-size 24)
         (build-version-cmd-size 24)
         (main-cmd-size 24)
         (libsystem-path "/usr/lib/libSystem.B.dylib")
         (load-dylib-cmd-size (align-up (+ 24 (length libsystem-path) 1) 8))
         (symtab-cmd-size 24)
         (dysymtab-cmd-size 80)

         ;; Number of load commands
         (ncmds 11)                             ; Added __DATA segment
         (sizeofcmds (+ pagezero-cmd-size
                       text-cmd-size
                       data-cmd-size
                       linkedit-cmd-size
                       dylinker-cmd-size
                       uuid-cmd-size
                       build-version-cmd-size
                       main-cmd-size
                       load-dylib-cmd-size
                       symtab-cmd-size
                       dysymtab-cmd-size))

         ;; Code placement - leave room for codesign load command
         (code-offset (align-up (+ header-size sizeofcmds 64) 64))

         ;; __TEXT segment covers first page
         (text-segment-size +PAGE-SIZE+)

         ;; __DATA segment starts at second page
         (data-fileoff +PAGE-SIZE+)
         (data-vmaddr (+ +VM-BASE+ +PAGE-SIZE+))
         (data-vmsize (align-up heap-size +PAGE-SIZE+))
         (data-filesize data-vmsize)            ; Initialize with zeros

         ;; __LINKEDIT segment starts after __DATA
         (linkedit-fileoff (+ data-fileoff data-filesize))
         (linkedit-vmaddr (+ data-vmaddr data-vmsize))

         ;; String table for symbols (just null byte and _main)
         (string-table '(0                      ; first byte is null
                         95 109 97 105 110 0))  ; "_main\0"
         (string-table-size (length string-table))

         ;; Symbol table offset (at start of LINKEDIT)
         (symtab-offset linkedit-fileoff)
         (nsyms 1)
         (nlist-size (* nsyms 16))

         ;; String table offset (after symbols)
         (strtab-offset (+ symtab-offset nlist-size))

         ;; LINKEDIT total size
         (linkedit-size (align-up (+ nlist-size string-table-size) 8))

         ;; Entry point offset from start of file (LC_MAIN.entryoff)
         (entry-offset code-offset))

    (when verbose
      (format t "Code offset: ~D (~X)~%" code-offset code-offset)
      (format t "Code size: ~D~%" code-size)
      (format t "DATA offset: ~D (~X)~%" data-fileoff data-fileoff)
      (format t "DATA vmaddr: ~X~%" data-vmaddr)
      (format t "DATA size: ~D (~X)~%" data-vmsize data-vmsize)
      (format t "LINKEDIT offset: ~D~%" linkedit-fileoff)
      (format t "Entry offset: ~D~%" entry-offset))

    (with-open-file (out output-path
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))

      ;; === Mach-O Header ===
      ;; PIE enabled - heap address is PC-relative via ADRP so ASLR works
      (write-mach-header-64 out ncmds sizeofcmds
                            (logior +MH-NOUNDEFS+ +MH-DYLDLINK+
                                    +MH-TWOLEVEL+ +MH-PIE+))

      ;; === Load Commands ===

      ;; 1. __PAGEZERO (4GB null zone)
      (write-segment-command-64 out "__PAGEZERO"
                                0 +VM-BASE+     ; vmaddr=0, vmsize=4GB
                                0 0             ; fileoff=0, filesize=0
                                0 0             ; no protection
                                0 0)            ; no sections

      ;; 2. __TEXT segment
      (write-segment-command-64 out "__TEXT"
                                +VM-BASE+ text-segment-size
                                0 text-segment-size
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                1 0)
      ;; __text section
      (write-section-64 out "__text" "__TEXT"
                        (+ +VM-BASE+ code-offset) code-size
                        code-offset
                        2                       ; align 2^2 = 4
                        0 0                     ; no relocations
                        (logior +S-ATTR-PURE-INSTRUCTIONS+ +S-ATTR-SOME-INSTRUCTIONS+)
                        0 0)

      ;; 3. __DATA segment for heap
      (write-segment-command-64 out "__DATA"
                                data-vmaddr data-vmsize
                                data-fileoff data-filesize
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                1 0)
      ;; __heap section
      (write-section-64 out "__heap" "__DATA"
                        data-vmaddr data-vmsize
                        data-fileoff
                        3                       ; align 2^3 = 8
                        0 0                     ; no relocations
                        0                       ; regular section
                        0 0)

      ;; 4. __LINKEDIT segment
      (write-segment-command-64 out "__LINKEDIT"
                                linkedit-vmaddr +PAGE-SIZE+
                                linkedit-fileoff linkedit-size
                                +VM-PROT-READ+
                                +VM-PROT-READ+
                                0 0)            ; no sections

      ;; 5. LC_LOAD_DYLINKER
      (write-dylinker-command out dylinker-path)

      ;; 6. LC_UUID
      (write-uuid-command out)

      ;; 7. LC_BUILD_VERSION
      (write-build-version-command out)

      ;; 8. LC_MAIN
      (write-main-command out entry-offset)

      ;; 9. LC_LOAD_DYLIB for libSystem
      (write-load-dylib-command out libsystem-path)

      ;; 10. LC_SYMTAB
      (write-symtab-command out symtab-offset nsyms strtab-offset string-table-size)

      ;; 11. LC_DYSYMTAB
      (write-dysymtab-command out
                              0 0               ; no locals
                              0 1               ; 1 external symbol at index 0
                              1 0)              ; no undefined

      ;; === Padding to code offset ===
      (let ((current (file-position out)))
        (when verbose
          (format t "After load commands, position: ~D (~X), need: ~D (~X)~%"
                  current current code-offset code-offset))
        (when (< current code-offset)
          (write-zeros out (- code-offset current))))

      ;; === Code Section ===
      (dolist (b code-bytes)
        (write-byte b out))

      ;; === Padding to DATA ===
      (let ((current (file-position out)))
        (write-zeros out (- data-fileoff current)))

      ;; === DATA Section (heap initialized to zeros) ===
      (write-zeros out data-vmsize)

      ;; === LINKEDIT Section ===

      ;; Symbol table (nlist_64)
      ;; _main symbol: external defined in section 1
      (write-nlist-64 out
                      1                         ; strx = offset of "_main" in strtab
                      #x0F                      ; N_SECT | N_EXT
                      1                         ; section 1 (__text)
                      #x0010                    ; REFERENCED_DYNAMICALLY
                      (+ +VM-BASE+ code-offset)) ; address

      ;; String table
      (dolist (b string-table)
        (write-byte b out))

      ;; Pad to alignment
      (let ((current (file-position out)))
        (write-zeros out (- (+ linkedit-fileoff linkedit-size) current))))

    ;; Make executable
    (sb-ext:run-program "/bin/chmod" (list "+x" output-path))

    ;; Return the heap virtual address
    data-vmaddr))

(defun deliver-native-with-heap (output-path code-bytes &key (heap-size #x100000) verbose)
  "Create a standalone native executable from compiled bytecode with heap support.
   Uses x28 as the heap bump pointer, initialized to the __DATA segment.
   HEAP-SIZE defaults to 1MB."
  ;; ADRP uses 4KB page units. __DATA is at +PAGE-SIZE+ (16KB = 0x4000).
  ;; Page offset = 0x4000 / 0x1000 = 4 (in ADRP's 4KB page units)
  ;; This works because ADRP is PC-relative and both code and heap
  ;; shift together with ASLR/PIE, maintaining the same page offset.
  (let* ((heap-page-offset (/ +PAGE-SIZE+ #x1000))  ; 16KB / 4KB = 4
         (wrapped (wrap-bytecode-with-heap code-bytes heap-page-offset)))
    (write-macho-executable-with-heap output-path wrapped heap-size :verbose verbose)))

;;; ============================================================
;;; Link with Runtime
;;; ============================================================

(defun link-with-runtime (output-path code-bytes runtime-path &key verbose)
  "Link compiled Lisp code with the Habu runtime into a standalone executable.
   For now, just create a simple executable from code-bytes."
  (declare (ignore runtime-path))
  (write-macho-executable output-path code-bytes :verbose verbose))

(defun deliver-native (output-path code-bytes &key verbose)
  "Create a standalone native executable from compiled bytecode.
   Untags the result so it can be used as an exit code."
  (let ((wrapped (wrap-bytecode-for-exit code-bytes)))
    (write-macho-executable output-path wrapped :verbose verbose)))

;;; ============================================================
;;; Test Function
;;; ============================================================

(defun test-minimal-macho ()
  "Create a minimal Mach-O that returns 42"
  ;; ARM64 code: MOV x0, #42; RET
  ;; movz x0, #42 = 0xD2800540 (little-endian: 40 05 80 D2)
  ;; ret         = 0xD65F03C0 (little-endian: C0 03 5F D6)
  (let ((code (list #x40 #x05 #x80 #xD2         ; movz x0, #42
                    #xC0 #x03 #x5F #xD6)))      ; ret
    (write-macho-executable "/tmp/test_macho" code :verbose t)
    (format t "~%Created /tmp/test_macho~%")
    (format t "Test with: codesign -s - /tmp/test_macho && /tmp/test_macho ; echo $?~%")))
