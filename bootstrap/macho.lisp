;;; Mach-O Linker for Habu Self-Hosting
;;;
;;; Buffer-based Mach-O generation - no stream I/O
;;; Pure Habu - uses only features available in bootstrap compiler

(in-package :habu)

;;; ============================================================
;;; Mach-O Constants
;;; ============================================================

(defconstant +MH-MAGIC-64+ #xFEEDFACF)
(defconstant +CPU-TYPE-ARM64+ #x0100000C)
(defconstant +CPU-SUBTYPE-ARM64-ALL+ #x00000000)
(defconstant +MH-EXECUTE+ 2)

(defconstant +MH-NOUNDEFS+ #x00000001)
(defconstant +MH-DYLDLINK+ #x00000004)
(defconstant +MH-TWOLEVEL+ #x00000080)
(defconstant +MH-PIE+ #x00200000)

(defconstant +LC-SEGMENT-64+ #x19)
(defconstant +LC-SYMTAB+ #x02)
(defconstant +LC-DYSYMTAB+ #x0B)
(defconstant +LC-LOAD-DYLINKER+ #x0E)
(defconstant +LC-UUID+ #x1B)
(defconstant +LC-BUILD-VERSION+ #x32)
(defconstant +LC-MAIN+ #x80000028)
(defconstant +LC-LOAD-DYLIB+ #x0C)
(defconstant +LC-DYLD-CHAINED-FIXUPS+ #x80000034)

(defconstant +VM-PROT-READ+ #x01)
(defconstant +VM-PROT-WRITE+ #x02)
(defconstant +VM-PROT-EXECUTE+ #x04)

(defconstant +S-ATTR-PURE-INSTRUCTIONS+ #x80000000)
(defconstant +S-ATTR-SOME-INSTRUCTIONS+ #x00000400)
(defconstant +S-NON-LAZY-SYMBOL-POINTERS+ #x06)
(defconstant +S-SYMBOL-STUBS+ #x08)

(defconstant +DYLD-CHAINED-PTR-64-OFFSET+ 6)
(defconstant +PAGE-SIZE+ #x4000)
(defconstant +VM-BASE+ #x100000000)

;;; ============================================================
;;; Buffer Building Helpers
;;; ============================================================

(defun buf-u8 (val)
  (cons (logand val #xFF) nil))

(defun buf-u16-le (val)
  (cons (logand val #xFF)
        (cons (logand (ash val -8) #xFF)
              nil)))

(defun buf-u32-le (val)
  (cons (logand val #xFF)
        (cons (logand (ash val -8) #xFF)
              (cons (logand (ash val -16) #xFF)
                    (cons (logand (ash val -24) #xFF)
                          nil)))))

(defun buf-u64-le (val)
  (append (buf-u32-le (logand val #xFFFFFFFF))
          (buf-u32-le (logand (ash val -32) #xFFFFFFFF))))

(defun buf-string-padded (str len)
  (let* ((slen (string-length str)))
    (labels ((collect-chars (idx)
               (if (>= idx (if (< slen len) slen len))
                   nil
                   (cons (string-ref str idx)
                         (collect-chars (+ idx 1)))))
             (collect-zeros (count)
               (if (<= count 0)
                   nil
                   (cons 0 (collect-zeros (- count 1))))))
      (append (collect-chars 0)
              (collect-zeros (- len slen))))))

(defun buf-zeros (count)
  (if (<= count 0)
      nil
      (cons 0 (buf-zeros (- count 1)))))

(defun buf-append-all (bufs)
  (if (null bufs)
      nil
      (append (car bufs) (buf-append-all (cdr bufs)))))

(defun buf-to-string (buf)
  (let* ((len (length buf))
         (vec (make-vector len)))
    (labels ((fill-vec (remaining idx)
               (if (null remaining)
                   vec
                   (progn
                     (vector-set vec idx (car remaining))
                     (fill-vec (cdr remaining) (+ idx 1))))))
      (let ((filled (fill-vec buf 0)))
        (make-string-from-vector filled)))))

(defun align-up (val alignment)
  (let ((rem (mod val alignment)))
    (if (= rem 0)
        val
        (+ val (- alignment rem)))))

;;; ============================================================
;;; Mach-O Header (32 bytes)
;;; ============================================================

(defun buf-mach-header-64 (ncmds sizeofcmds flags)
  "Generate Mach-O header as buffer"
  (buf-append-all
   (cons (buf-u32-le +MH-MAGIC-64+)
         (cons (buf-u32-le +CPU-TYPE-ARM64+)
               (cons (buf-u32-le +CPU-SUBTYPE-ARM64-ALL+)
                     (cons (buf-u32-le +MH-EXECUTE+)
                           (cons (buf-u32-le ncmds)
                                 (cons (buf-u32-le sizeofcmds)
                                       (cons (buf-u32-le flags)
                                             (cons (buf-u32-le 0)  ; reserved
                                                   nil))))))))))

;;; ============================================================
;;; Load Commands
;;; ============================================================

(defun buf-segment-command-64 (segname vmaddr vmsize fileoff filesize
                                maxprot initprot nsects flags)
  "Generate LC_SEGMENT_64 load command as buffer"
  (buf-append-all
   (cons (buf-u32-le +LC-SEGMENT-64+)
         (cons (buf-u32-le (+ 72 (* nsects 80)))
               (cons (buf-string-padded segname 16)
                     (cons (buf-u64-le vmaddr)
                           (cons (buf-u64-le vmsize)
                                 (cons (buf-u64-le fileoff)
                                       (cons (buf-u64-le filesize)
                                             (cons (buf-u32-le maxprot)
                                                   (cons (buf-u32-le initprot)
                                                         (cons (buf-u32-le nsects)
                                                               (cons (buf-u32-le flags)
                                                                     nil)))))))))))))

(defun buf-section-64 (sectname segname addr size offset align
                       reloff nreloc flags reserved1 reserved2)
  "Generate section_64 structure as buffer"
  (buf-append-all
   (cons (buf-string-padded sectname 16)
         (cons (buf-string-padded segname 16)
               (cons (buf-u64-le addr)
                     (cons (buf-u64-le size)
                           (cons (buf-u32-le offset)
                                 (cons (buf-u32-le align)
                                       (cons (buf-u32-le reloff)
                                             (cons (buf-u32-le nreloc)
                                                   (cons (buf-u32-le flags)
                                                         (cons (buf-u32-le reserved1)
                                                               (cons (buf-u32-le reserved2)
                                                                     (cons (buf-u32-le 0)  ; reserved3
                                                                           nil))))))))))))))

(defun buf-load-dylinker-command (path)
  "Generate LC_LOAD_DYLINKER command as buffer"
  (let* ((path-len (+ 1 (string-length path)))
         (cmdsize (align-up (+ 12 path-len) 8)))
    (buf-append-all
     (cons (buf-u32-le +LC-LOAD-DYLINKER+)
           (cons (buf-u32-le cmdsize)
                 (cons (buf-u32-le 12)
                       (cons (buf-string-padded path (- cmdsize 12))
                             nil)))))))

(defun buf-uuid-command (uuid-val)
  "Generate LC_UUID command as buffer (uuid-val = list of 4 u32 values)"
  (let* ((uuid0 (car uuid-val))
         (uuid1 (car (cdr uuid-val)))
         (uuid2 (car (cdr (cdr uuid-val))))
         (uuid3 (car (cdr (cdr (cdr uuid-val))))))
    (buf-append-all
     (cons (buf-u32-le +LC-UUID+)
           (cons (buf-u32-le 24)
                 (cons (buf-u32-le uuid0)
                       (cons (buf-u32-le uuid1)
                             (cons (buf-u32-le uuid2)
                                   (cons (buf-u32-le uuid3)
                                         nil)))))))))

(defun buf-build-version-command ()
  "Generate LC_BUILD_VERSION for macOS 14.0 as buffer"
  (buf-append-all
   (cons (buf-u32-le +LC-BUILD-VERSION+)
         (cons (buf-u32-le 24)
               (cons (buf-u32-le 1)           ; platform = macOS
                     (cons (buf-u32-le #x000E0000)  ; minos = 14.0
                           (cons (buf-u32-le #x000E0000)  ; sdk = 14.0
                                 (cons (buf-u32-le 0)  ; ntools
                                       nil))))))))

(defun buf-main-command (entryoff)
  "Generate LC_MAIN command as buffer"
  (buf-append-all
   (cons (buf-u32-le +LC-MAIN+)
         (cons (buf-u32-le 24)
               (cons (buf-u64-le entryoff)
                     (cons (buf-u64-le 0)  ; stacksize
                           nil))))))

(defun buf-load-dylib-command (path)
  "Generate LC_LOAD_DYLIB command as buffer"
  (let* ((path-len (+ 1 (string-length path)))
         (cmdsize (align-up (+ 24 path-len) 8)))
    (buf-append-all
     (cons (buf-u32-le +LC-LOAD-DYLIB+)
           (cons (buf-u32-le cmdsize)
                 (cons (buf-u32-le 24)        ; name.offset
                       (cons (buf-u32-le 2)   ; timestamp
                             (cons (buf-u32-le #x054C0000)  ; current_version
                                   (cons (buf-u32-le #x00010000)  ; compat_version
                                         (cons (buf-string-padded path (- cmdsize 24))
                                               nil))))))))))

(defun buf-symtab-command (symoff nsyms stroff strsize)
  "Generate LC_SYMTAB command as buffer"
  (buf-append-all
   (cons (buf-u32-le +LC-SYMTAB+)
         (cons (buf-u32-le 24)
               (cons (buf-u32-le symoff)
                     (cons (buf-u32-le nsyms)
                           (cons (buf-u32-le stroff)
                                 (cons (buf-u32-le strsize)
                                       nil))))))))

(defun buf-dysymtab-command (ilocalsym nlocalsym iextdefsym nextdefsym
                              iundefsym nundefsym)
  "Generate LC_DYSYMTAB command as buffer"
  (buf-append-all
   (cons (buf-u32-le +LC-DYSYMTAB+)
         (cons (buf-u32-le 80)
               (cons (buf-u32-le ilocalsym)
                     (cons (buf-u32-le nlocalsym)
                           (cons (buf-u32-le iextdefsym)
                                 (cons (buf-u32-le nextdefsym)
                                       (cons (buf-u32-le iundefsym)
                                             (cons (buf-u32-le nundefsym)
                                                   (cons (buf-zeros 48)  ; remaining fields
                                                         nil)))))))))))

;;; ============================================================
;;; Chained Fixups for Dynamic Linking
;;; ============================================================

(defun buf-chained-fixups-command (dataoff datasize)
  "Generate LC_DYLD_CHAINED_FIXUPS command as buffer"
  (buf-append-all
   (cons (buf-u32-le +LC-DYLD-CHAINED-FIXUPS+)
         (cons (buf-u32-le 16)
               (cons (buf-u32-le dataoff)
                     (cons (buf-u32-le datasize)
                           nil))))))

(defun string-to-bytes (str)
  "Convert string to list of character codes (bytes)"
  (labels ((collect (idx)
             (if (>= idx (string-length str))
                 nil
                 (cons (string-ref str idx)
                       (collect (+ idx 1))))))
    (collect 0)))

(defun build-symbols-list (imports)
  "Build symbols string: NUL-separated, starts with NUL"
  (labels ((append-name (name rest)
             (append (string-to-bytes name) (cons 0 rest)))
           (build-all (remaining)
             (if (null remaining)
                 nil
                 (append-name (car remaining) (build-all (cdr remaining))))))
    (cons 0 (build-all imports))))

(defun set-u32-le-at (buf offset val)
  "Set 32-bit little-endian value at offset in buffer (returns modified buffer)"
  (let* ((b0 (logand val #xFF))
         (b1 (logand (ash val -8) #xFF))
         (b2 (logand (ash val -16) #xFF))
         (b3 (logand (ash val -24) #xFF)))
    (labels ((set-bytes (lst idx)
               (cond
                 ((null lst) nil)
                 ((= idx offset) (cons b0 (set-bytes (cdr lst) (+ idx 1))))
                 ((= idx (+ offset 1)) (cons b1 (set-bytes (cdr lst) (+ idx 1))))
                 ((= idx (+ offset 2)) (cons b2 (set-bytes (cdr lst) (+ idx 1))))
                 ((= idx (+ offset 3)) (cons b3 (set-bytes (cdr lst) (+ idx 1))))
                 (t (cons (car lst) (set-bytes (cdr lst) (+ idx 1)))))))
      (set-bytes buf 0))))

(defun set-u16-le-at (buf offset val)
  "Set 16-bit little-endian value at offset in buffer"
  (let* ((b0 (logand val #xFF))
         (b1 (logand (ash val -8) #xFF)))
    (labels ((set-bytes (lst idx)
               (cond
                 ((null lst) nil)
                 ((= idx offset) (cons b0 (set-bytes (cdr lst) (+ idx 1))))
                 ((= idx (+ offset 1)) (cons b1 (set-bytes (cdr lst) (+ idx 1))))
                 (t (cons (car lst) (set-bytes (cdr lst) (+ idx 1)))))))
      (set-bytes buf 0))))

(defun set-u64-le-at (buf offset val)
  "Set 64-bit little-endian value at offset in buffer"
  (let* ((lo (logand val #xFFFFFFFF))
         (hi (logand (ash val -32) #xFFFFFFFF)))
    (set-u32-le-at (set-u32-le-at buf offset lo) (+ offset 4) hi)))

(defun set-byte-at (buf offset val)
  "Set single byte at offset in buffer"
  (labels ((set-bytes (lst idx)
             (cond
               ((null lst) nil)
               ((= idx offset) (cons val (set-bytes (cdr lst) (+ idx 1))))
               (t (cons (car lst) (set-bytes (cdr lst) (+ idx 1)))))))
    (set-bytes buf 0)))

(defun build-chained-fixups-data (imports num-segments got-segment-index got-vm-offset)
  "Build chained fixups data for binding external symbols.
   IMPORTS: list of symbol names (strings like \"_write\")
   NUM-SEGMENTS: total number of segments
   GOT-SEGMENT-INDEX: 0-based index of segment containing GOT
   GOT-VM-OFFSET: VM offset from binary base to first fixup
   
   Returns a byte list."
  (let* ((num-imports (length imports))
         (symbols-list (build-symbols-list imports))
         ;; Calculate offsets
         (header-size 32)
         (starts-header-size (+ 4 (* 4 num-segments)))
         (seg-info-size 24)
         (imports-entry-size 4)
         (starts-offset header-size)
         (seg-info-rel-offset (align-up starts-header-size 8))
         (imports-offset (+ starts-offset seg-info-rel-offset seg-info-size))
         (symbols-offset (+ imports-offset (* num-imports imports-entry-size)))
         (total-size (align-up (+ symbols-offset (length symbols-list)) 8)))
    
    ;; Create zero-filled buffer
    (let* ((data (buf-zeros total-size)))
      
      ;; Set dyld_chained_fixups_header fields
      (let* ((d1 (set-u32-le-at data 4 starts-offset))
             (d2 (set-u32-le-at d1 8 imports-offset))
             (d3 (set-u32-le-at d2 12 symbols-offset))
             (d4 (set-u32-le-at d3 16 num-imports))
             (d5 (set-byte-at d4 20 1)))  ; imports_format = 1
        
        ;; Set dyld_chained_starts_in_image
        (let* ((d6 (set-u32-le-at d5 starts-offset num-segments)))
          
          ;; Set seg_info_offset for GOT segment
          (let* ((seg-info-off-pos (+ starts-offset 4 (* got-segment-index 4)))
                 (d7 (set-u32-le-at d6 seg-info-off-pos seg-info-rel-offset)))
            
            ;; Set dyld_chained_starts_in_segment
            (let* ((seg-base (+ starts-offset seg-info-rel-offset))
                   (d8 (set-u32-le-at d7 seg-base 24))  ; size
                   (d9 (set-u16-le-at d8 (+ seg-base 4) #x4000))  ; page_size
                   (d10 (set-u16-le-at d9 (+ seg-base 6) +DYLD-CHAINED-PTR-64-OFFSET+))
                   (d11 (set-u64-le-at d10 (+ seg-base 8) got-vm-offset))
                   (d12 (set-u16-le-at d11 (+ seg-base 20) 1)))  ; page_count
              
              ;; Set import entries
              (labels ((set-imports (remaining idx name-offset buf)
                         (if (null remaining)
                             buf
                             (let* ((name (car remaining))
                                    (entry-off (+ imports-offset (* idx 4)))
                                    (entry (logior 1 (ash (+ 1 name-offset) 9)))
                                    (new-buf (set-u32-le-at buf entry-off entry))
                                    (new-name-offset (+ name-offset 1 (string-length name))))
                               (set-imports (cdr remaining) (+ idx 1) new-name-offset new-buf)))))
                
                (let* ((d13 (set-imports imports 0 0 d12)))
                  
                  ;; Copy symbol strings to buffer
                  (labels ((copy-symbols (src-list dst-idx buf)
                             (if (null src-list)
                                 buf
                                 (copy-symbols (cdr src-list)
                                              (+ dst-idx 1)
                                              (set-byte-at buf (+ symbols-offset dst-idx) (car src-list))))))
                    (copy-symbols symbols-list 0 d13))))))))))
