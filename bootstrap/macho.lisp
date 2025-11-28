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
