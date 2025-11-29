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
           #:write-macho-executable-with-imports
           #:write-macho-executable-with-imports-and-heap
           #:link-with-runtime
           #:deliver-native
           #:deliver-native-with-heap
           #:deliver-native-with-imports
           #:deliver-native-with-imports-and-heap
           #:wrap-bytecode-for-exit
           #:wrap-bytecode-with-heap
           #:wrap-bytecode-with-heap-for-imports
           #:test-minimal-macho
           #:test-import-macho
           #:test-syscall-macho))

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

;; Section types (low 8 bits of flags)
(defconstant +S-NON-LAZY-SYMBOL-POINTERS+ #x06)  ; __got section type
(defconstant +S-SYMBOL-STUBS+             #x08)  ; __stubs section type

;; Chained fixups constants
(defconstant +DYLD-CHAINED-PTR-64-OFFSET+ 6)     ; pointer format for arm64

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
  "Wrap bytecode with heap initialization, code base, symbol table, and untagging.

   HEAP-PAGE-OFFSET is the page offset from the ADRP instruction to __DATA.
   For standard layout (code at page 0, heap at page 1), this is 1.
   x28 is used as the heap bump pointer.
   x26 is used as the code base pointer (for closure funcall).
   x27 is used as heap base pointer (for symbol table access).

   Heap layout (first 16 bytes reserved for symbol table):
     heap[0]  = next symbol ID (8 bytes, starts at 0)
     heap[8]  = symbol table pointer (8 bytes, starts at nil = 0)
     heap[16] = start of bump allocation area

   Uses PC-relative addressing (ADRP + ADD) so PIE/ASLR works correctly.

   Stub (68 bytes = 17 instructions):
     0: sub sp, sp, #48      ; allocate stack space
     1: str x30, [sp]        ; save LR
     2: str x28, [sp, #8]    ; save x28
     3: str x26, [sp, #16]   ; save x26
     4: str x27, [sp, #24]   ; save x27
     5: adrp x28, #page_off  ; load heap page address (PC-relative)
     6: mov x27, x28         ; save heap base for symbol table
     7: add x28, x28, #16    ; skip reserved 16 bytes for symbol table
     8: adr x26, +36         ; x26 = address of main code (9 instrs ahead = 36 bytes)
     9: bl +8                ; call original main (jump to instr 17)
    10: lsr x0, x0, #4       ; untag result
    11: ldr x27, [sp, #24]   ; restore x27
    12: ldr x26, [sp, #16]   ; restore x26
    13: ldr x28, [sp, #8]    ; restore x28
    14: ldr x30, [sp]        ; restore LR
    15: add sp, sp, #48      ; clean up stack
    16: ret                  ; return to OS
    17: <original code>      ; starts at offset 68"
  (let* ((stub (append
                (arm64:sub arm64:+sp+ arm64:+sp+ #x30 :imm t)  ; sub sp, sp, #48
                (arm64:str arm64:+lr+ arm64:+sp+)              ; str x30, [sp]
                (arm64:str 28 arm64:+sp+ :offset 8)            ; str x28, [sp, #8]
                (arm64:str 26 arm64:+sp+ :offset 16)           ; str x26, [sp, #16]
                (arm64:str 27 arm64:+sp+ :offset 24)           ; str x27, [sp, #24]
                (arm64:adrp 28 heap-page-offset)               ; adrp x28, heap (PC-relative)
                (arm64:mov 27 28)                              ; mov x27, x28 (heap base)
                (arm64:add 28 28 #x10 :imm t)                  ; add x28, x28, #16 (skip reserved)
                (arm64:adr 26 36)                              ; adr x26, +36 (code base = 9 instrs)
                (arm64:bl 8)                                   ; bl +8 (jump to instr 17)
                (arm64:lsr 0 0 4 :imm t)                       ; lsr x0, x0, #4
                (arm64:ldr 27 arm64:+sp+ :offset 24)           ; ldr x27, [sp, #24]
                (arm64:ldr 26 arm64:+sp+ :offset 16)           ; ldr x26, [sp, #16]
                (arm64:ldr 28 arm64:+sp+ :offset 8)            ; ldr x28, [sp, #8]
                (arm64:ldr arm64:+lr+ arm64:+sp+)              ; ldr x30, [sp]
                (arm64:add arm64:+sp+ arm64:+sp+ #x30 :imm t)  ; add sp, sp, #48
                (arm64:ret))))                                 ; ret
    (append stub code-bytes)))

(defun wrap-bytecode-with-heap-for-imports (code-bytes heap-page-offset)
  "Wrap bytecode with heap initialization for executables with imports.
   Similar to wrap-bytecode-with-heap but the heap is further away (after __DATA_CONST).

   HEAP-PAGE-OFFSET is the page offset from the ADRP instruction to __DATA.
   For imports layout (code at page 0, GOT at page 1, heap at page 2), this is 2.
   x28 is used as the heap bump pointer.
   x26 is used as the code base pointer (for closure funcall).
   x27 is used as heap base pointer (for symbol table access).

   Heap layout (first 16 bytes reserved for symbol table):
     heap[0]  = next symbol ID (8 bytes, starts at 0)
     heap[8]  = symbol table pointer (8 bytes, starts at nil = 0)
     heap[16] = start of bump allocation area

   Stub (68 bytes = 17 instructions):
     0: sub sp, sp, #48      ; allocate stack space
     1: str x30, [sp]        ; save LR
     2: str x28, [sp, #8]    ; save x28
     3: str x26, [sp, #16]   ; save x26
     4: str x27, [sp, #24]   ; save x27
     5: adrp x28, #page_off  ; load heap page address (PC-relative)
     6: mov x27, x28         ; save heap base for symbol table
     7: add x28, x28, #16    ; skip reserved 16 bytes for symbol table
     8: adr x26, +36         ; x26 = address of main code (9 instrs ahead = 36 bytes)
     9: bl +8                ; call original main (jump to instr 17)
    10: lsr x0, x0, #4       ; untag result
    11: ldr x27, [sp, #24]   ; restore x27
    12: ldr x26, [sp, #16]   ; restore x26
    13: ldr x28, [sp, #8]    ; restore x28
    14: ldr x30, [sp]        ; restore LR
    15: add sp, sp, #48      ; clean up stack
    16: ret                  ; return to OS
    17: <original code>      ; starts at offset 68"
  (let* ((stub (append
                (arm64:sub arm64:+sp+ arm64:+sp+ #x30 :imm t)  ; sub sp, sp, #48
                (arm64:str arm64:+lr+ arm64:+sp+)              ; str x30, [sp]
                (arm64:str 28 arm64:+sp+ :offset 8)            ; str x28, [sp, #8]
                (arm64:str 26 arm64:+sp+ :offset 16)           ; str x26, [sp, #16]
                (arm64:str 27 arm64:+sp+ :offset 24)           ; str x27, [sp, #24]
                (arm64:adrp 28 heap-page-offset)               ; adrp x28, heap (PC-relative)
                (arm64:mov 27 28)                              ; mov x27, x28 (heap base)
                (arm64:add 28 28 #x10 :imm t)                  ; add x28, x28, #16 (skip reserved)
                (arm64:adr 26 36)                              ; adr x26, +36 (code base = 9 instrs)
                (arm64:bl 8)                                   ; bl +8 (jump to instr 17)
                (arm64:lsr 0 0 4 :imm t)                       ; lsr x0, x0, #4
                (arm64:ldr 27 arm64:+sp+ :offset 24)           ; ldr x27, [sp, #24]
                (arm64:ldr 26 arm64:+sp+ :offset 16)           ; ldr x26, [sp, #16]
                (arm64:ldr 28 arm64:+sp+ :offset 8)            ; ldr x28, [sp, #8]
                (arm64:ldr arm64:+lr+ arm64:+sp+)              ; ldr x30, [sp]
                (arm64:add arm64:+sp+ arm64:+sp+ #x30 :imm t)  ; add sp, sp, #48
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
;;; Mach-O with External Imports (Chained Fixups)
;;; ============================================================

;;; Modern macOS uses chained fixups for dynamic symbol binding.
;;; This implementation generates executables that can call libSystem functions.

(defun write-dysymtab-command-full (stream ilocalsym nlocalsym
                                    iextdefsym nextdefsym
                                    iundefsym nundefsym
                                    indirectsymoff nindirectsyms)
  "Write LC_DYSYMTAB with indirect symbol table info"
  (write-u32-le stream +LC-DYSYMTAB+)
  (write-u32-le stream 80)
  (write-u32-le stream ilocalsym)
  (write-u32-le stream nlocalsym)
  (write-u32-le stream iextdefsym)
  (write-u32-le stream nextdefsym)
  (write-u32-le stream iundefsym)
  (write-u32-le stream nundefsym)
  (write-u32-le stream 0)                       ; tocoff
  (write-u32-le stream 0)                       ; ntoc
  (write-u32-le stream 0)                       ; modtaboff
  (write-u32-le stream 0)                       ; nmodtab
  (write-u32-le stream 0)                       ; extrefsymoff
  (write-u32-le stream 0)                       ; nextrefsyms
  (write-u32-le stream indirectsymoff)
  (write-u32-le stream nindirectsyms)
  (write-u32-le stream 0)                       ; extreloff
  (write-u32-le stream 0)                       ; nextrel
  (write-u32-le stream 0)                       ; locreloff
  (write-u32-le stream 0))                      ; nlocrel

(defun write-chained-fixups-command (stream dataoff datasize)
  "Write LC_DYLD_CHAINED_FIXUPS command"
  (write-u32-le stream +LC-DYLD-CHAINED-FIXUPS+)
  (write-u32-le stream 16)
  (write-u32-le stream dataoff)
  (write-u32-le stream datasize))

(defun write-exports-trie-command (stream dataoff datasize)
  "Write LC_DYLD_EXPORTS_TRIE command"
  (write-u32-le stream +LC-DYLD-EXPORTS-TRIE+)
  (write-u32-le stream 16)
  (write-u32-le stream dataoff)
  (write-u32-le stream datasize))

(defun generate-stub-code (got-page-offset got-slot-offset)
  "Generate ARM64 stub code that loads from GOT and branches.
   GOT-PAGE-OFFSET: signed page offset for ADRP (in 4KB pages)
   GOT-SLOT-OFFSET: byte offset within page for LDR"
  (append
   (arm64:adrp 16 got-page-offset)              ; adrp x16, got_page
   (arm64:ldr 16 16 :offset got-slot-offset)    ; ldr x16, [x16, #offset]
   (arm64:br 16)))                              ; br x16

(defun build-chained-fixups-data (imports num-segments got-segment-index got-vm-offset)
  "Build chained fixups data for binding external symbols.
   IMPORTS: list of symbol names (strings like \"_write\")
   NUM-SEGMENTS: total number of segments
   GOT-SEGMENT-INDEX: 0-based index of segment containing GOT
   GOT-VM-OFFSET: VM offset from binary base to first fixup (e.g. 0x4000 for __DATA_CONST)

   Returns a byte list."
  (let* ((num-imports (length imports))
         ;; Build symbols string: NUL-separated, starts with NUL
         (symbols-list (cons 0  ; leading NUL
                            (loop for name in imports
                                  append (append (map 'list #'char-code name)
                                                '(0)))))
         ;; Calculate offsets within the data block
         ;; Header: 32 bytes (dyld_chained_fixups_header)
         ;; Starts: 4 + 4*num-segments bytes (dyld_chained_starts_in_image)
         ;; Seg info: 22 + 2 = 24 bytes (dyld_chained_starts_in_segment with 1 page)
         ;; Imports: 4 * num-imports bytes
         ;; Symbols: (length symbols-list) bytes
         (header-size 32)
         (starts-header-size (+ 4 (* 4 num-segments)))
         (seg-info-size 24)  ; size=24, page_size, pointer_format, segment_offset, max_valid_pointer, page_count=1, page_start[0]
         (imports-entry-size 4)

         (starts-offset header-size)
         ;; seg_info must be 8-byte aligned within starts_in_image
         (seg-info-rel-offset (align-up starts-header-size 8))
         ;; imports come after seg_info (which is at starts-offset + seg-info-rel-offset)
         (imports-offset (+ starts-offset seg-info-rel-offset seg-info-size))
         (symbols-offset (+ imports-offset (* num-imports imports-entry-size)))
         (total-size (align-up (+ symbols-offset (length symbols-list)) 8))

         (data (make-array total-size :element-type '(unsigned-byte 8) :initial-element 0)))

    ;; === dyld_chained_fixups_header (32 bytes) ===
    ;; fixups_version = 0 (at offset 0)
    ;; starts_offset (at offset 4)
    (setf (aref data 4) (logand starts-offset #xFF))
    (setf (aref data 5) (logand (ash starts-offset -8) #xFF))
    ;; imports_offset (at offset 8)
    (setf (aref data 8) (logand imports-offset #xFF))
    (setf (aref data 9) (logand (ash imports-offset -8) #xFF))
    ;; symbols_offset (at offset 12)
    (setf (aref data 12) (logand symbols-offset #xFF))
    (setf (aref data 13) (logand (ash symbols-offset -8) #xFF))
    ;; imports_count (at offset 16)
    (setf (aref data 16) (logand num-imports #xFF))
    (setf (aref data 17) (logand (ash num-imports -8) #xFF))
    ;; imports_format = 1 (DYLD_CHAINED_IMPORT) (at offset 20)
    (setf (aref data 20) 1)
    ;; symbols_format = 0 (uncompressed) (at offset 24)
    ;; padding (at offset 28-31)

    ;; === dyld_chained_starts_in_image ===
    (let ((base starts-offset))
      ;; seg_count
      (setf (aref data base) (logand num-segments #xFF))
      ;; seg_info_offset[i] - only GOT segment has non-zero offset
      (loop for i from 0 below num-segments
            do (let ((off-pos (+ base 4 (* i 4))))
                 (when (= i got-segment-index)
                   (setf (aref data off-pos) (logand seg-info-rel-offset #xFF))
                   (setf (aref data (+ off-pos 1)) (logand (ash seg-info-rel-offset -8) #xFF))))))

    ;; === dyld_chained_starts_in_segment (at starts-offset + seg-info-rel-offset) ===
    (let ((base (+ starts-offset seg-info-rel-offset)))
      ;; size = 24 (at offset 0, 4 bytes)
      (setf (aref data base) 24)
      ;; page_size = 0x4000 (at offset 4, 2 bytes)
      (setf (aref data (+ base 4)) #x00)
      (setf (aref data (+ base 5)) #x40)
      ;; pointer_format = 6 (DYLD_CHAINED_PTR_64_OFFSET) (at offset 6, 2 bytes)
      (setf (aref data (+ base 6)) +DYLD-CHAINED-PTR-64-OFFSET+)
      ;; segment_offset = got-vm-offset (at offset 8, 8 bytes)
      (setf (aref data (+ base 8)) (logand got-vm-offset #xFF))
      (setf (aref data (+ base 9)) (logand (ash got-vm-offset -8) #xFF))
      (setf (aref data (+ base 10)) (logand (ash got-vm-offset -16) #xFF))
      (setf (aref data (+ base 11)) (logand (ash got-vm-offset -24) #xFF))
      ;; max_valid_pointer = 0 (at offset 16, 4 bytes)
      ;; page_count = 1 (at offset 20, 2 bytes)
      (setf (aref data (+ base 20)) 1)
      ;; page_start[0] = 0 (at offset 22, 2 bytes) - first fixup at start of page
      )

    ;; === Import entries (DYLD_CHAINED_IMPORT format) ===
    ;; Each entry: lib_ordinal (8 bits) | weak (1 bit) | name_offset (23 bits)
    (let ((name-offset 0))  ; starts after leading NUL
      (loop for i from 0 below num-imports
            for name in imports
            do (let* ((entry-off (+ imports-offset (* i 4)))
                      ;; lib_ordinal = 1 (first LC_LOAD_DYLIB = libSystem)
                      ;; weak = 0
                      ;; name_offset = position in symbols string
                      (entry (logior 1  ; lib_ordinal in bits 0-7
                                    (ash (1+ name-offset) 9))))  ; name_offset in bits 9-31
                 (setf (aref data entry-off) (logand entry #xFF))
                 (setf (aref data (+ entry-off 1)) (logand (ash entry -8) #xFF))
                 (setf (aref data (+ entry-off 2)) (logand (ash entry -16) #xFF))
                 (setf (aref data (+ entry-off 3)) (logand (ash entry -24) #xFF))
                 (incf name-offset (1+ (length name))))))

    ;; === Symbol strings ===
    (loop for i from 0 below (length symbols-list)
          do (setf (aref data (+ symbols-offset i)) (nth i symbols-list)))

    (coerce data 'list)))

(defun write-macho-executable-with-imports (output-path code-bytes imports
                                            &key (heap-size #x100000) verbose)
  "Write a Mach-O executable that can call external functions via stubs.
   CODE-BYTES: ARM64 machine code (should call stubs via BL)
   IMPORTS: list of symbol names to import (e.g. '(\"_write\" \"_exit\"))
   HEAP-SIZE: size of heap segment (default 1MB)

   Returns: (values output-path code-offset stubs-offset stub-size heap-vmaddr)"
  (let* ((num-imports (length imports))
         (stub-size 12)                          ; 3 instructions per stub
         (stubs-total-size (* num-imports stub-size))
         (got-entry-size 8)
         (got-total-size (max 8 (* num-imports got-entry-size)))  ; at least 8 bytes

         ;; Segment layout (4 segments):
         ;; 0: __PAGEZERO (no file content)
         ;; 1: __TEXT with __text and __stubs sections
         ;; 2: __DATA_CONST with __got section
         ;; 3: __LINKEDIT
         (num-segments 4)
         (got-segment-index 2)

         ;; Heap size
         (heap-vmsize (align-up heap-size +PAGE-SIZE+))

         ;; Header and load command sizes
         (header-size 32)
         (pagezero-cmd-size 72)
         (text-cmd-size (+ 72 (* 2 80)))         ; segment + 2 sections
         (data-const-cmd-size (+ 72 80))         ; segment + 1 section
         (linkedit-cmd-size 72)                  ; segment only
         (dylinker-path "/usr/lib/dyld")
         (dylinker-cmd-size (align-up (+ 12 (length dylinker-path) 1) 8))
         (uuid-cmd-size 24)
         (build-version-cmd-size 24)
         (main-cmd-size 24)
         (libsystem-path "/usr/lib/libSystem.B.dylib")
         (load-dylib-cmd-size (align-up (+ 24 (length libsystem-path) 1) 8))
         (chained-fixups-cmd-size 16)
         (exports-trie-cmd-size 16)
         (symtab-cmd-size 24)
         (dysymtab-cmd-size 80)

         (ncmds 13)                           ; 4 segments + 9 other commands
         (sizeofcmds (+ pagezero-cmd-size
                       text-cmd-size
                       data-const-cmd-size
                       linkedit-cmd-size
                       dylinker-cmd-size
                       uuid-cmd-size
                       build-version-cmd-size
                       main-cmd-size
                       load-dylib-cmd-size
                       chained-fixups-cmd-size
                       exports-trie-cmd-size
                       symtab-cmd-size
                       dysymtab-cmd-size))

         ;; Code placement - leave room for codesign to add LC_CODE_SIGNATURE (16 bytes)
         (code-offset (align-up (+ header-size sizeofcmds 16) 64))
         (code-size (length code-bytes))

         ;; Stubs follow code
         (stubs-offset (align-up (+ code-offset code-size) 4))

         ;; __TEXT segment spans first page
         (text-vmsize +PAGE-SIZE+)
         (text-filesize +PAGE-SIZE+)

         ;; __DATA_CONST segment at second page
         (data-const-fileoff +PAGE-SIZE+)
         (data-const-vmaddr (+ +VM-BASE+ +PAGE-SIZE+))
         (data-const-vmsize +PAGE-SIZE+)
         (data-const-filesize +PAGE-SIZE+)

         ;; __LINKEDIT segment after __DATA_CONST
         (linkedit-fileoff (+ data-const-fileoff +PAGE-SIZE+))
         (linkedit-vmaddr (+ data-const-vmaddr +PAGE-SIZE+))

         ;; Build symbols and strings
         ;; Symbols: _main (external defined), then each import (external undefined)
         (nsyms (1+ num-imports))
         ;; String table: NUL + "_main" + NUL + each import name + NUL
         (string-table-entries (cons "_main" imports))
         (string-table (cons 0  ; leading NUL
                            (loop for name in string-table-entries
                                  append (append (map 'list #'char-code name) '(0)))))
         (string-table-size (length string-table))

         ;; LINKEDIT contents layout
         (symtab-offset linkedit-fileoff)
         (nlist-size (* nsyms 16))               ; 16 bytes per nlist_64
         (strtab-offset (+ symtab-offset nlist-size))
         (indirect-offset (align-up (+ strtab-offset string-table-size) 4))
         (num-indirect-syms (+ num-imports num-imports))  ; stubs + got
         (indirect-size (* num-indirect-syms 4))
         (fixups-offset (align-up (+ indirect-offset indirect-size) 8))
         ;; GOT VM offset from binary base = data-const-vmaddr - +VM-BASE+ = +PAGE-SIZE+
         (got-vm-offset +PAGE-SIZE+)
         (fixups-data (build-chained-fixups-data imports num-segments got-segment-index got-vm-offset))
         (fixups-size (length fixups-data))
         (exports-offset (align-up (+ fixups-offset fixups-size) 8))
         ;; Minimal exports trie: empty root node (no exports needed for simple executable)
         ;; Format: terminal size = 0, no children
         (exports-data '(#x00 #x00))  ; terminal size = 0, child count = 0
         (exports-size (length exports-data))

         ;; LINKEDIT filesize is the actual data size, vmsize is page-aligned
         (linkedit-filesize (- (+ exports-offset exports-size) linkedit-fileoff))
         (linkedit-vmsize (align-up linkedit-filesize +PAGE-SIZE+))

         ;; GOT page calculations for stubs
         ;; Stub is in __TEXT at stubs-offset, GOT is at data-const-vmaddr
         (stub-vmaddr (+ +VM-BASE+ stubs-offset))
         (stub-page (ash stub-vmaddr -12))
         (got-page (ash data-const-vmaddr -12))
         (got-page-diff (- got-page stub-page))

         ;; Entry point
         (entry-offset code-offset))

    (when verbose
      (format t "Code at ~X (size ~D), stubs at ~X~%" code-offset code-size stubs-offset)
      (format t "GOT at VM ~X, page diff from stubs: ~D~%" data-const-vmaddr got-page-diff)
      (format t "LINKEDIT at ~X, filesize ~X vmsize ~X~%" linkedit-fileoff linkedit-filesize linkedit-vmsize)
      (format t "Imports: ~{~A~^, ~}~%" imports))

    (with-open-file (out output-path
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))

      ;; === Mach-O Header ===
      (write-mach-header-64 out ncmds sizeofcmds
                            (logior +MH-NOUNDEFS+ +MH-DYLDLINK+ +MH-TWOLEVEL+ +MH-PIE+))

      ;; === Load Commands ===

      ;; 1. __PAGEZERO
      (write-segment-command-64 out "__PAGEZERO"
                                0 +VM-BASE+ 0 0 0 0 0 0)

      ;; 2. __TEXT with __text and __stubs
      (write-segment-command-64 out "__TEXT"
                                +VM-BASE+ text-vmsize
                                0 text-filesize
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                2 0)
      ;; __text section
      (write-section-64 out "__text" "__TEXT"
                        (+ +VM-BASE+ code-offset) code-size
                        code-offset 2 0 0
                        (logior +S-ATTR-PURE-INSTRUCTIONS+ +S-ATTR-SOME-INSTRUCTIONS+)
                        0 0)
      ;; __stubs section
      (write-section-64 out "__stubs" "__TEXT"
                        (+ +VM-BASE+ stubs-offset) stubs-total-size
                        stubs-offset 2 0 0
                        (logior +S-SYMBOL-STUBS+ +S-ATTR-PURE-INSTRUCTIONS+)
                        0                         ; reserved1 = index into indirect sym table
                        stub-size)                ; reserved2 = stub size

      ;; 3. __DATA_CONST with __got
      ;; initprot must include WRITE so dyld can apply fixups
      (write-segment-command-64 out "__DATA_CONST"
                                data-const-vmaddr data-const-vmsize
                                data-const-fileoff data-const-filesize
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                1 #x10)           ; flags = SG_READ_ONLY
      ;; __got section
      (write-section-64 out "__got" "__DATA_CONST"
                        data-const-vmaddr got-total-size
                        data-const-fileoff 3 0 0
                        +S-NON-LAZY-SYMBOL-POINTERS+
                        num-imports               ; reserved1 = index into indirect sym table
                        0)

      ;; 4. __LINKEDIT
      (write-segment-command-64 out "__LINKEDIT"
                                linkedit-vmaddr linkedit-vmsize
                                linkedit-fileoff linkedit-filesize
                                +VM-PROT-READ+
                                +VM-PROT-READ+
                                0 0)

      ;; Load commands in clang order (critical for dyld compatibility)
      ;; 5. LC_DYLD_CHAINED_FIXUPS (must come early)
      (write-chained-fixups-command out fixups-offset fixups-size)

      ;; 6. LC_DYLD_EXPORTS_TRIE
      (write-exports-trie-command out exports-offset exports-size)

      ;; 7. LC_SYMTAB
      (write-symtab-command out symtab-offset nsyms strtab-offset string-table-size)

      ;; 8. LC_DYSYMTAB
      (write-dysymtab-command-full out
                                   0 0             ; no locals
                                   0 1             ; 1 extdef (_main)
                                   1 num-imports   ; undefs start at sym 1
                                   indirect-offset num-indirect-syms)

      ;; 9. LC_LOAD_DYLINKER
      (write-dylinker-command out dylinker-path)

      ;; 10. LC_UUID
      (write-uuid-command out)

      ;; 11. LC_BUILD_VERSION
      (write-build-version-command out)

      ;; 12. LC_MAIN
      (write-main-command out entry-offset)

      ;; 13. LC_LOAD_DYLIB for libSystem
      (write-load-dylib-command out libsystem-path)

      ;; === Padding to code ===
      (let ((pos (file-position out)))
        (when (< pos code-offset)
          (write-zeros out (- code-offset pos))))

      ;; === Code ===
      (dolist (b code-bytes)
        (write-byte b out))

      ;; === Padding to stubs ===
      (let ((pos (file-position out)))
        (when (< pos stubs-offset)
          (write-zeros out (- stubs-offset pos))))

      ;; === Stubs ===
      (loop for i from 0 below num-imports
            do (let* ((got-slot-offset (* i got-entry-size))
                      (stub (generate-stub-code got-page-diff got-slot-offset)))
                 (dolist (b stub)
                   (write-byte b out))))

      ;; === Padding to __DATA_CONST ===
      (let ((pos (file-position out)))
        (when (< pos data-const-fileoff)
          (write-zeros out (- data-const-fileoff pos))))

      ;; === GOT entries (chained bind pointers) ===
      ;; For DYLD_CHAINED_PTR_64_OFFSET bind format:
      ;; bit 63 = 1 (bind), bits 51-62 = next, bits 0-23 = ordinal
      (loop for i from 0 below num-imports
            for is-last = (= i (1- num-imports))
            do (let* ((ordinal i)
                      (next (if is-last 0 2))    ; stride = 2 (8 bytes, since each unit is 4 bytes)
                      (entry (logior #x8000000000000000  ; bind bit
                                    ordinal
                                    (ash next 51))))
                 (write-u64-le out entry)))
      ;; Pad rest of GOT if needed
      (let ((pos (file-position out)))
        (when (< (- pos data-const-fileoff) data-const-filesize)
          (write-zeros out (- (+ data-const-fileoff data-const-filesize) pos))))

      ;; === LINKEDIT ===

      ;; Symbol table entries (nlist_64, 16 bytes each)
      ;; First: _main (external defined in section 1)
      (write-nlist-64 out
                      1                           ; strx = offset of "_main"
                      #x0F                        ; N_SECT | N_EXT
                      1                           ; section 1 (__text)
                      #x0010                      ; REFERENCED_DYNAMICALLY
                      (+ +VM-BASE+ code-offset))
      ;; Then: each import (external undefined)
      (let ((strx (+ 1 (length "_main") 1)))      ; skip NUL + "_main" + NUL
        (loop for name in imports
              do (write-nlist-64 out
                                strx              ; string offset
                                #x01              ; N_EXT (external undefined)
                                0                 ; no section
                                #x0100            ; N_SYMBOL_RESOLVER flag... or just 0
                                0)                ; value = 0 for undefined
                 (incf strx (1+ (length name)))))

      ;; String table
      (dolist (b string-table)
        (write-byte b out))

      ;; Padding to indirect symbol table
      (let ((pos (file-position out)))
        (when (< pos indirect-offset)
          (write-zeros out (- indirect-offset pos))))

      ;; Indirect symbol table
      ;; First: indices for __stubs (one per import)
      (loop for i from 0 below num-imports
            do (write-u32-le out (1+ i)))         ; symbol index (imports start at 1)
      ;; Then: indices for __got (one per import)
      (loop for i from 0 below num-imports
            do (write-u32-le out (1+ i)))

      ;; Padding to chained fixups
      (let ((pos (file-position out)))
        (when (< pos fixups-offset)
          (write-zeros out (- fixups-offset pos))))

      ;; Chained fixups data
      (dolist (b fixups-data)
        (write-byte b out))

      ;; Padding to exports trie
      (let ((pos (file-position out)))
        (when (< pos exports-offset)
          (write-zeros out (- exports-offset pos))))

      ;; Exports trie
      (dolist (b exports-data)
        (write-byte b out))

      ;; Padding to end of LINKEDIT (use filesize, not vmsize)
      (let ((pos (file-position out)))
        (when (< pos (+ linkedit-fileoff linkedit-filesize))
          (write-zeros out (- (+ linkedit-fileoff linkedit-filesize) pos)))))

    ;; Make executable
    (sb-ext:run-program "/bin/chmod" (list "+x" output-path) :wait t)

    ;; Ad-hoc codesign for macOS
    (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" "-f" output-path)
                        :output nil :error nil :wait t)

    ;; Return values for caller
    (values output-path
            code-offset
            stubs-offset
            stub-size
            (+ +VM-BASE+ data-const-vmaddr heap-vmsize))))  ; heap would be after DATA_CONST

(defun deliver-native-with-imports (output-path code-bytes imports
                                    &key (heap-size #x100000) verbose)
  "Create a standalone executable that can call external functions.
   CODE-BYTES should contain BL instructions to stub addresses.
   IMPORTS is a list of external function names (e.g. (\"_write\" \"_exit\"))

   Returns: (values output-path code-offset stubs-offset stub-size heap-addr)"
  (write-macho-executable-with-imports output-path code-bytes imports
                                       :heap-size heap-size :verbose verbose))

;;; ============================================================
;;; Mach-O with Imports AND Heap
;;; ============================================================

(defun write-macho-executable-with-imports-and-heap (output-path code-bytes imports
                                                     &key (heap-size #x100000) verbose)
  "Write a Mach-O executable that can call external functions AND has a heap segment.
   CODE-BYTES: ARM64 machine code (should call stubs via BL)
   IMPORTS: list of symbol names to import (e.g. '(\"_write\" \"_exit\"))
   HEAP-SIZE: size of heap segment (default 1MB)

   Segment layout:
   0: __PAGEZERO (4GB null zone)
   1: __TEXT with __text and __stubs sections
   2: __DATA_CONST with __got section
   3: __DATA with __heap section (NEW)
   4: __LINKEDIT

   Returns: (values output-path code-offset stubs-offset stub-size heap-vmaddr heap-page-offset)"
  (let* ((num-imports (length imports))
         (stub-size 12)                          ; 3 instructions per stub
         (stubs-total-size (* num-imports stub-size))
         (got-entry-size 8)
         (got-total-size (max 8 (* num-imports got-entry-size)))

         ;; 5 segments now
         (num-segments 5)
         (got-segment-index 2)

         ;; Header and load command sizes
         (header-size 32)
         (pagezero-cmd-size 72)
         (text-cmd-size (+ 72 (* 2 80)))         ; segment + 2 sections
         (data-const-cmd-size (+ 72 80))         ; segment + 1 section (__got)
         (data-cmd-size (+ 72 80))               ; segment + 1 section (__heap) - NEW
         (linkedit-cmd-size 72)                  ; segment only
         (dylinker-path "/usr/lib/dyld")
         (dylinker-cmd-size (align-up (+ 12 (length dylinker-path) 1) 8))
         (uuid-cmd-size 24)
         (build-version-cmd-size 24)
         (main-cmd-size 24)
         (libsystem-path "/usr/lib/libSystem.B.dylib")
         (load-dylib-cmd-size (align-up (+ 24 (length libsystem-path) 1) 8))
         (chained-fixups-cmd-size 16)
         (exports-trie-cmd-size 16)
         (symtab-cmd-size 24)
         (dysymtab-cmd-size 80)

         (ncmds 14)                              ; 5 segments + 9 other commands
         (sizeofcmds (+ pagezero-cmd-size
                       text-cmd-size
                       data-const-cmd-size
                       data-cmd-size             ; NEW
                       linkedit-cmd-size
                       dylinker-cmd-size
                       uuid-cmd-size
                       build-version-cmd-size
                       main-cmd-size
                       load-dylib-cmd-size
                       chained-fixups-cmd-size
                       exports-trie-cmd-size
                       symtab-cmd-size
                       dysymtab-cmd-size))

         ;; Code placement
         (code-offset (align-up (+ header-size sizeofcmds 16) 64))
         (code-size (length code-bytes))

         ;; Stubs follow code
         (stubs-offset (align-up (+ code-offset code-size) 4))
         (stubs-end (+ stubs-offset stubs-total-size))

         ;; __TEXT segment must be large enough for code + stubs
         (text-end-needed (align-up stubs-end +PAGE-SIZE+))
         (text-vmsize text-end-needed)
         (text-filesize text-end-needed)

         ;; __DATA_CONST segment follows __TEXT
         (data-const-fileoff text-filesize)
         (data-const-vmaddr (+ +VM-BASE+ text-vmsize))
         (data-const-vmsize +PAGE-SIZE+)
         (data-const-filesize +PAGE-SIZE+)

         ;; __DATA segment at third page (for heap) - NEW
         (data-fileoff (+ data-const-fileoff +PAGE-SIZE+))
         (data-vmaddr (+ data-const-vmaddr +PAGE-SIZE+))
         (heap-vmsize (align-up heap-size +PAGE-SIZE+))
         (data-filesize heap-vmsize)

         ;; __LINKEDIT segment after __DATA
         (linkedit-fileoff (+ data-fileoff data-filesize))
         (linkedit-vmaddr (+ data-vmaddr heap-vmsize))

         ;; Build symbols and strings
         (nsyms (1+ num-imports))
         (string-table-entries (cons "_main" imports))
         (string-table (cons 0
                            (loop for name in string-table-entries
                                  append (append (map 'list #'char-code name) '(0)))))
         (string-table-size (length string-table))

         ;; LINKEDIT contents layout
         (symtab-offset linkedit-fileoff)
         (nlist-size (* nsyms 16))
         (strtab-offset (+ symtab-offset nlist-size))
         (indirect-offset (align-up (+ strtab-offset string-table-size) 4))
         (num-indirect-syms (+ num-imports num-imports))
         (indirect-size (* num-indirect-syms 4))
         (fixups-offset (align-up (+ indirect-offset indirect-size) 8))

         ;; GOT VM offset from binary base = data-const-vmaddr - +VM-BASE+ = text-vmsize
         (got-vm-offset text-vmsize)

         ;; Build chained fixups data using existing helper
         (fixups-data (build-chained-fixups-data imports num-segments got-segment-index got-vm-offset))
         (fixups-size (length fixups-data))
         (aligned-fixups-size (align-up fixups-size 8))

         ;; Exports trie (minimal - empty root node with no exports)
         (exports-offset (align-up (+ fixups-offset fixups-size) 8))
         (exports-trie-data '(#x00 #x00))  ; terminal size = 0, child count = 0
         (exports-trie-size (length exports-trie-data))
         (aligned-exports-size (align-up exports-trie-size 8))

         ;; Linkedit total
         (linkedit-size (align-up (+ nlist-size string-table-size
                                    (- indirect-offset strtab-offset string-table-size)
                                    indirect-size
                                    (- fixups-offset indirect-offset indirect-size)
                                    aligned-fixups-size
                                    aligned-exports-size)
                                 +PAGE-SIZE+))

         ;; Entry point
         (entry-offset code-offset))

    (when verbose
      (format t "Code at ~X (size ~D), stubs at ~X~%" code-offset code-size stubs-offset)
      (format t "DATA_CONST at VM ~X (GOT)~%" data-const-vmaddr)
      (format t "DATA at VM ~X (heap, size ~X)~%" data-vmaddr heap-vmsize)
      (format t "LINKEDIT at ~X, filesize ~X vmsize ~X~%"
              linkedit-fileoff linkedit-size +PAGE-SIZE+)
      (format t "Imports: ~{~A~^ ~}~%" imports))

    (with-open-file (out output-path
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))

      ;; === Mach-O Header ===
      (write-mach-header-64 out ncmds sizeofcmds
                            (logior +MH-NOUNDEFS+ +MH-DYLDLINK+
                                    +MH-TWOLEVEL+ +MH-PIE+))

      ;; === Load Commands ===

      ;; 1. __PAGEZERO
      (write-segment-command-64 out "__PAGEZERO"
                                0 +VM-BASE+
                                0 0
                                0 0
                                0 0)

      ;; 2. __TEXT segment with code and stubs
      (write-segment-command-64 out "__TEXT"
                                +VM-BASE+ text-vmsize
                                0 text-filesize
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                (logior +VM-PROT-READ+ +VM-PROT-EXECUTE+)
                                2 0)
      ;; __text section
      (write-section-64 out "__text" "__TEXT"
                        (+ +VM-BASE+ code-offset) code-size
                        code-offset
                        2
                        0 0
                        (logior +S-ATTR-PURE-INSTRUCTIONS+ +S-ATTR-SOME-INSTRUCTIONS+)
                        0 0)
      ;; __stubs section
      (write-section-64 out "__stubs" "__TEXT"
                        (+ +VM-BASE+ stubs-offset) stubs-total-size
                        stubs-offset 2 0 0
                        (logior +S-SYMBOL-STUBS+ +S-ATTR-PURE-INSTRUCTIONS+)
                        0                         ; reserved1 = index into indirect sym table
                        stub-size)                ; reserved2 = stub size

      ;; 3. __DATA_CONST segment with GOT
      (write-segment-command-64 out "__DATA_CONST"
                                data-const-vmaddr data-const-vmsize
                                data-const-fileoff data-const-filesize
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                1 0)
      ;; __got section (at offset 0 within __DATA_CONST)
      (write-section-64 out "__got" "__DATA_CONST"
                        data-const-vmaddr got-total-size
                        data-const-fileoff 3 0 0
                        +S-NON-LAZY-SYMBOL-POINTERS+
                        num-imports               ; reserved1 = index into indirect sym table
                        0)

      ;; 4. __DATA segment with heap - NEW
      (write-segment-command-64 out "__DATA"
                                data-vmaddr heap-vmsize
                                data-fileoff data-filesize
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                (logior +VM-PROT-READ+ +VM-PROT-WRITE+)
                                1 0)
      ;; __heap section
      (write-section-64 out "__heap" "__DATA"
                        data-vmaddr heap-vmsize
                        data-fileoff
                        3
                        0 0
                        0
                        0 0)

      ;; 5. __LINKEDIT segment
      (write-segment-command-64 out "__LINKEDIT"
                                linkedit-vmaddr +PAGE-SIZE+
                                linkedit-fileoff linkedit-size
                                +VM-PROT-READ+
                                +VM-PROT-READ+
                                0 0)

      ;; 6. LC_LOAD_DYLINKER
      (write-dylinker-command out dylinker-path)

      ;; 7. LC_UUID
      (write-uuid-command out)

      ;; 8. LC_BUILD_VERSION
      (write-build-version-command out)

      ;; 9. LC_MAIN
      (write-main-command out entry-offset)

      ;; 10. LC_LOAD_DYLIB
      (write-load-dylib-command out libsystem-path)

      ;; 11. LC_DYLD_CHAINED_FIXUPS
      (write-u32-le out +LC-DYLD-CHAINED-FIXUPS+)
      (write-u32-le out chained-fixups-cmd-size)
      (write-u32-le out fixups-offset)
      (write-u32-le out aligned-fixups-size)

      ;; 12. LC_DYLD_EXPORTS_TRIE
      (write-u32-le out +LC-DYLD-EXPORTS-TRIE+)
      (write-u32-le out exports-trie-cmd-size)
      (write-u32-le out exports-offset)
      (write-u32-le out aligned-exports-size)

      ;; 13. LC_SYMTAB
      (write-symtab-command out symtab-offset nsyms strtab-offset string-table-size)

      ;; 14. LC_DYSYMTAB
      (write-dysymtab-command-full out
                                   0 0             ; no locals
                                   0 1             ; 1 extdef (_main)
                                   1 num-imports   ; undefs start at sym 1
                                   indirect-offset num-indirect-syms)

      ;; === Padding to code ===
      (let ((current (file-position out)))
        (when (< current code-offset)
          (write-zeros out (- code-offset current))))

      ;; === Code Section ===
      (dolist (b code-bytes)
        (write-byte b out))

      ;; === Stubs ===
      (let ((current (file-position out)))
        (when (< current stubs-offset)
          (write-zeros out (- stubs-offset current))))

      ;; Generate stubs: ADRP + LDR + BR for each import
      ;; GOT is at data-const-vmaddr, stubs call to data_const_vmaddr + (i * 8)
      ;; ADRP page diff = (target_page - adrp_page), where pages are 4KB aligned
      (let* ((stub-vmaddr (+ +VM-BASE+ stubs-offset))
             (stub-page (ash stub-vmaddr -12))
             (got-page (ash data-const-vmaddr -12))
             (got-page-diff (- got-page stub-page)))
        (when verbose
          (format t "GOT at VM ~X, page diff from stubs: ~D~%" data-const-vmaddr got-page-diff))
        (dotimes (i num-imports)
          (let* ((got-entry-vmaddr (+ data-const-vmaddr (* i 8)))
                 (got-page-off (logand got-entry-vmaddr #xFFF)))
            (dolist (b (arm64:adrp 16 got-page-diff))
              (write-byte b out))
            (dolist (b (arm64:ldr 16 16 :offset got-page-off))
              (write-byte b out))
            (dolist (b (arm64:br 16))
              (write-byte b out)))))

      ;; === Pad to DATA_CONST ===
      (let ((current (file-position out)))
        (when (< current data-const-fileoff)
          (write-zeros out (- data-const-fileoff current))))

      ;; === GOT Section ===
      ;; Write bind entries for chained fixups (DYLD_CHAINED_PTR_64_OFFSET format)
      ;; bit 63 = 1 (bind), bits 51-62 = next, bits 0-23 = ordinal
      (loop for i from 0 below num-imports
            for is-last = (= i (1- num-imports))
            do (let* ((ordinal i)
                      (next (if is-last 0 2))    ; stride = 2 (8 bytes, since each unit is 4 bytes)
                      (entry (logior #x8000000000000000  ; bind bit
                                    ordinal
                                    (ash next 51))))
                 (write-u64-le out entry)))

      ;; Pad rest of DATA_CONST
      (let ((current (file-position out)))
        (when (< current data-fileoff)
          (write-zeros out (- data-fileoff current))))

      ;; === DATA Section (heap) === - NEW
      (write-zeros out data-filesize)

      ;; === LINKEDIT Section ===

      ;; Symbol table
      ;; _main symbol
      (write-nlist-64 out
                      1
                      #x0F
                      1
                      #x0010
                      (+ +VM-BASE+ code-offset))
      ;; Import symbols
      (let ((strx (+ 1 6)))
        (dolist (name imports)
          (write-nlist-64 out strx #x01 0 #x0100 0)
          (incf strx (1+ (length name)))))

      ;; String table
      (dolist (b string-table)
        (write-byte b out))

      ;; Pad to indirect symbols
      (let ((current (file-position out)))
        (when (< current indirect-offset)
          (write-zeros out (- indirect-offset current))))

      ;; Indirect symbol table
      ;; First: stubs (symbol indices 1, 2, ...)
      (dotimes (i num-imports)
        (write-u32-le out (1+ i)))
      ;; Second: GOT (same symbol indices)
      (dotimes (i num-imports)
        (write-u32-le out (1+ i)))

      ;; Pad to fixups
      (let ((current (file-position out)))
        (when (< current fixups-offset)
          (write-zeros out (- fixups-offset current))))

      ;; === Chained Fixups (pre-built blob) ===
      (dolist (b fixups-data)
        (write-byte b out))

      ;; Pad fixups to aligned size
      (let ((current (file-position out))
            (target (+ fixups-offset aligned-fixups-size)))
        (when (< current target)
          (write-zeros out (- target current))))

      ;; === Exports Trie ===
      (dolist (b exports-trie-data)
        (write-byte b out))

      ;; Pad exports
      (let ((current (file-position out))
            (target (+ exports-offset aligned-exports-size)))
        (when (< current target)
          (write-zeros out (- target current))))

      ;; Pad LINKEDIT to declared size
      (let ((current (file-position out)))
        (write-zeros out (- (+ linkedit-fileoff linkedit-size) current))))

    ;; Make executable
    (sb-ext:run-program "/bin/chmod" (list "+x" output-path) :wait t)

    ;; Ad-hoc codesign for macOS
    (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" "-f" output-path)
                        :output nil :error nil :wait t)

    ;; Return values for caller
    ;; heap-page-offset: ADRP uses 4KB pages
    ;; __TEXT at VM 0x100000000, __DATA at 0x100008000 = 8 pages difference
    (values output-path
            code-offset
            stubs-offset
            stub-size
            data-vmaddr                           ; heap VM address
            8)))                                  ; heap page offset for ADRP (4KB pages)

(defun deliver-native-with-imports-and-heap (output-path code-bytes imports
                                             &key (heap-size #x100000) verbose)
  "Create a standalone executable with external imports AND heap support.
   CODE-BYTES should be raw bytecode (will be wrapped with heap setup).
   IMPORTS is a list of external function names (e.g. (\"_write\" \"_exit\"))

   Returns: (values output-path code-offset stubs-offset stub-size heap-vmaddr)"
  ;; Calculate heap page offset dynamically based on code size
  ;; The wrapper stub adds 68 bytes, and we need to account for that
  ;; when calculating where segments will land.
  (let* ((wrapper-stub-size 68)
         (num-imports (length imports))
         (stub-size 12)
         (stubs-total-size (* num-imports stub-size))
         ;; Approximate code-offset (after header + commands + padding)
         ;; This is an approximation; the actual value is calculated in write-macho...
         ;; We're slightly over-estimating to be safe (header ~32 + cmds ~700 + padding)
         (approx-code-offset #x400)
         ;; Total code after wrapping
         (total-code-size (+ (length code-bytes) wrapper-stub-size))
         ;; Stubs follow code
         (stubs-offset (align-up (+ approx-code-offset total-code-size) 4))
         (stubs-end (+ stubs-offset stubs-total-size))
         ;; __TEXT segment size (page-aligned)
         (text-vmsize (align-up stubs-end +PAGE-SIZE+))
         ;; __DATA_CONST follows __TEXT
         ;; __DATA (heap) follows __DATA_CONST
         ;; heap page offset = (text-vmsize + PAGE-SIZE) / PAGE-SIZE
         ;; = text-vmsize/PAGE-SIZE + 1 pages from the ADRP instruction (which is at page 0)
         (heap-page-offset (+ (floor text-vmsize +PAGE-SIZE+) 1)))
    (when verbose
      (format t "Calculated heap-page-offset: ~D (text-vmsize=~X)~%" heap-page-offset text-vmsize))
    (let ((wrapped-code (wrap-bytecode-with-heap-for-imports code-bytes heap-page-offset)))
      (write-macho-executable-with-imports-and-heap output-path wrapped-code imports
                                                    :heap-size heap-size :verbose verbose))))

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

(defun test-import-macho ()
  "Create a Mach-O that calls write() via dynamic linking and returns 42"
  ;; This tests the full chained fixups implementation:
  ;; 1. Generate code that calls a stub
  ;; 2. Generate the executable with imports
  ;; 3. Patch the BL instruction to point to the correct stub
  ;; 4. Run and verify output
  (let* ((imports '("_write"))
         ;; Code layout:
         ;; 0: sub sp, sp, #16    (prologue)
         ;; 1: str x30, [sp]
         ;; 2: mov x0, #1         (fd = stdout)
         ;; 3: adr x1, +24        (string addr, 6 instrs ahead)
         ;; 4: mov x2, #3         (length)
         ;; 5: bl stub            (placeholder, patched later)
         ;; 6: mov x0, #42        (return value)
         ;; 7: ldr x30, [sp]      (epilogue)
         ;; 8: add sp, sp, #16
         ;; 9: ret
         ;; 10: "Hi\n\0"
         (code (append
                (arm64:sub arm64:+sp+ arm64:+sp+ #x10 :imm t)
                (arm64:str arm64:+lr+ arm64:+sp+)
                (arm64:movz 0 1)
                (arm64:adr 1 28)                  ; 7 instrs * 4 bytes = 28 to string
                (arm64:movz 2 3)
                '(0 0 0 #x94)                     ; bl placeholder
                (arm64:movz 0 42)
                (arm64:ldr arm64:+lr+ arm64:+sp+)
                (arm64:add arm64:+sp+ arm64:+sp+ #x10 :imm t)
                (arm64:ret)
                '(#x48 #x69 #x0A #x00))))         ; "Hi\n\0"

    ;; Generate executable
    (multiple-value-bind (path code-offset stubs-offset stub-size heap-addr)
        (write-macho-executable-with-imports "/tmp/test_import" code imports
                                             :heap-size #x10000 :verbose t)
      (declare (ignore stub-size heap-addr))

      ;; Patch BL instruction
      ;; BL is at code-offset + 20 (5 instructions * 4 bytes)
      ;; Target is stubs-offset
      (let* ((bl-file-offset (+ code-offset 20))
             (bl-instr-offset (- stubs-offset bl-file-offset))
             (bl-imm26 (ash bl-instr-offset -2))
             (bl-instr (logior #x94000000 (logand bl-imm26 #x03FFFFFF))))
        (format t "Patching BL at ~X to stub at ~X (offset ~D instructions)~%"
                bl-file-offset stubs-offset (ash bl-instr-offset -2))
        (with-open-file (f path :direction :io
                               :element-type '(unsigned-byte 8)
                               :if-exists :overwrite)
          (file-position f bl-file-offset)
          (write-u32-le f bl-instr)))

      (format t "~%Created ~A~%" path)
      (format t "Test with: codesign -s - ~A && ~A ; echo $?~%" path path))))

(defun test-syscall-macho ()
  "Create a Mach-O that uses direct syscalls - write and exit.
   This bypasses dynamic linking entirely using SVC instructions."
  (let ((code (append
               ;; write(1, \"OK\\n\", 3): x16=0x2000004, x0=1, x1=addr, x2=3
               (arm64:movz 0 1)              ; x0 = stdout
               (arm64:adr 1 36)              ; x1 = string addr (9 instrs * 4 = 36)
               (arm64:movz 2 3)              ; x2 = length
               (arm64:movz 16 4)             ; x16 = SYS_write (4)
               (arm64:movk 16 #x200 :lsl 16) ; x16 |= 0x2000000 (BSD syscall)
               (arm64:svc 0)                 ; syscall
               ;; exit(42): x16=0x2000001, x0=42
               (arm64:movz 0 42)             ; x0 = exit code
               (arm64:movz 16 1)             ; x16 = SYS_exit (1)
               (arm64:movk 16 #x200 :lsl 16) ; x16 |= 0x2000000
               (arm64:svc 0)                 ; syscall
               ;; Data: "OK\n"
               '(#x4F #x4B #x0A #x00))))
    (write-macho-executable "/tmp/test_syscall" code :verbose nil)
    (format t "Created /tmp/test_syscall~%")
    (format t "Test: codesign -s - /tmp/test_syscall && /tmp/test_syscall ; echo $?~%")))
