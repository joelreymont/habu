;;; DWARF5 debug info generation for Habu
;;;
;;; Generates minimal DWARF debug info for lldb:
;;; - Function names and addresses (DW_TAG_subprogram)
;;; - Line number tables (.debug_line)
;;;
;;; Reference: DWARF Version 5 Standard (dwarfstd.org)

(in-package :habu)

;;; DWARF5 constants

;; Tags (DW_TAG_*)
(defconstant +dw-tag-compile-unit+ #x11)
(defconstant +dw-tag-subprogram+ #x2e)
(defconstant +dw-tag-base-type+ #x24)

;; Attributes (DW_AT_*)
(defconstant +dw-at-name+ #x03)
(defconstant +dw-at-producer+ #x25)
(defconstant +dw-at-language+ #x13)
(defconstant +dw-at-low-pc+ #x11)
(defconstant +dw-at-high-pc+ #x12)
(defconstant +dw-at-stmt-list+ #x10)
(defconstant +dw-at-comp-dir+ #x1b)
(defconstant +dw-at-external+ #x3f)
(defconstant +dw-at-decl-file+ #x3a)
(defconstant +dw-at-decl-line+ #x3b)

;; Forms (DW_FORM_*)
(defconstant +dw-form-addr+ #x01)
(defconstant +dw-form-data1+ #x0b)
(defconstant +dw-form-data2+ #x05)
(defconstant +dw-form-data4+ #x06)
(defconstant +dw-form-string+ #x08)
(defconstant +dw-form-flag-present+ #x19)
(defconstant +dw-form-sec-offset+ #x17)

;; Language codes (DW_LANG_*)
(defconstant +dw-lang-lisp+ #x0008)  ; Common Lisp

;; Children flag
(defconstant +dw-children-yes+ 1)
(defconstant +dw-children-no+ 0)

;;; Line number opcodes (DW_LNS_*)
(defconstant +dw-lns-copy+ #x01)
(defconstant +dw-lns-advance-pc+ #x02)
(defconstant +dw-lns-advance-line+ #x03)
(defconstant +dw-lns-set-file+ #x04)
(defconstant +dw-lns-set-column+ #x05)
(defconstant +dw-lns-negate-stmt+ #x06)
(defconstant +dw-lns-set-basic-block+ #x07)
(defconstant +dw-lns-const-add-pc+ #x08)
(defconstant +dw-lns-fixed-advance-pc+ #x09)
(defconstant +dw-lns-set-prologue-end+ #x0a)
(defconstant +dw-lns-set-epilogue-begin+ #x0b)

;; Extended opcodes (DW_LNE_*)
(defconstant +dw-lne-end-sequence+ #x01)
(defconstant +dw-lne-set-address+ #x02)

;;; ULEB128 encoding

(defun encode-uleb128 (value)
  "Encode VALUE as ULEB128, returning list of bytes"
  (let ((result nil))
    (loop
      (let ((byte (logand value #x7f)))
        (setq value (ash value -7))
        (if (zerop value)
            (progn
              (push byte result)
              (return (nreverse result)))
            (push (logior byte #x80) result))))))

#+sbcl
(defun encode-sleb128 (value)
  "Encode VALUE as SLEB128, returning list of bytes"
  (let ((result nil)
        (more t)
        (negative (< value 0)))
    (loop while more do
      (let ((byte (logand value #x7f)))
        (setq value (ash value -7))
        (if negative
            (when (and (= value -1) (not (zerop (logand byte #x40))))
              (setq more nil))
            (when (and (zerop value) (zerop (logand byte #x40)))
              (setq more nil)))
        (if more
            (push (logior byte #x80) result)
            (push byte result))))
    (nreverse result)))

;;; String table builder

(defun make-string-table ()
  "Create a new string table (list of (string . offset) pairs)"
  (list nil 0))  ; (strings . next-offset)

(defun string-table-add (table string)
  "Add STRING to TABLE, returning its offset"
  (let* ((strings (car table))
         (existing (assoc string strings :test #'equal)))
    (if existing
        (cdr existing)
        (let ((offset (cadr table)))
          (setf (car table) (cons (cons string offset) strings))
          (setf (cadr table) (+ offset (length string) 1))  ; +1 for null
          offset))))

(defun string-table-bytes (table)
  "Return the string table as a byte list"
  (let ((result nil))
    (dolist (entry (reverse (car table)))
      (let ((string (car entry)))
        (dotimes (i (length string))
          (push (char-code (char string i)) result))
        (push 0 result)))  ; null terminator
    (nreverse result)))

;;; Abbreviation table generation

(defun make-abbrev-table ()
  "Create abbreviation table for our DIEs.
   Returns bytes for .debug_abbrev section.

   Abbrev 1: compile_unit (has children)
     - DW_AT_name (string)
     - DW_AT_producer (string)
     - DW_AT_language (data2)
     - DW_AT_low_pc (addr)
     - DW_AT_high_pc (data4 - offset from low_pc)
     - DW_AT_stmt_list (sec_offset)
     - DW_AT_comp_dir (string)

   Abbrev 2: subprogram (no children)
     - DW_AT_name (string)
     - DW_AT_low_pc (addr)
     - DW_AT_high_pc (data4)
     - DW_AT_external (flag_present)
     - DW_AT_decl_file (data1)
     - DW_AT_decl_line (data1)"
  (append
   ;; Abbrev 1: compile_unit
   (list 1)  ; abbrev code
   (list +dw-tag-compile-unit+)  ; tag
   (list +dw-children-yes+)      ; has children
   (list +dw-at-name+ +dw-form-string+)
   (list +dw-at-producer+ +dw-form-string+)
   (list +dw-at-language+ +dw-form-data2+)
   (list +dw-at-low-pc+ +dw-form-addr+)
   (list +dw-at-high-pc+ +dw-form-data4+)
   (list +dw-at-stmt-list+ +dw-form-sec-offset+)
   (list +dw-at-comp-dir+ +dw-form-string+)
   (list 0 0)  ; end of attributes

   ;; Abbrev 2: subprogram
   (list 2)  ; abbrev code
   (list +dw-tag-subprogram+)   ; tag
   (list +dw-children-no+)      ; no children
   (list +dw-at-name+ +dw-form-string+)
   (list +dw-at-low-pc+ +dw-form-addr+)
   (list +dw-at-high-pc+ +dw-form-data4+)
   (list +dw-at-external+ +dw-form-flag-present+)
   (list +dw-at-decl-file+ +dw-form-data1+)
   (list +dw-at-decl-line+ +dw-form-data1+)
   (list 0 0)  ; end of attributes

   ;; End of abbreviation table
   (list 0)))

;;; Debug info generation

(defun emit-u8 (byte)
  "Return single byte as list"
  (list (logand byte #xff)))

(defun emit-u16-le (value)
  "Return 16-bit value as little-endian byte list"
  (list (logand value #xff)
        (logand (ash value -8) #xff)))

(defun emit-u32-le (value)
  "Return 32-bit value as little-endian byte list"
  (list (logand value #xff)
        (logand (ash value -8) #xff)
        (logand (ash value -16) #xff)
        (logand (ash value -24) #xff)))

(defun emit-u64-le (value)
  "Return 64-bit value as little-endian byte list"
  (list (logand value #xff)
        (logand (ash value -8) #xff)
        (logand (ash value -16) #xff)
        (logand (ash value -24) #xff)
        (logand (ash value -32) #xff)
        (logand (ash value -40) #xff)
        (logand (ash value -48) #xff)
        (logand (ash value -56) #xff)))

(defun emit-string (str)
  "Return null-terminated string as byte list"
  (let ((result nil))
    (dotimes (i (length str))
      (push (char-code (char str i)) result))
    (push 0 result)
    (nreverse result)))

(defun make-debug-info (functions source-name comp-dir code-start code-size line-offset)
  "Generate .debug_info section.
   FUNCTIONS is a list of (name offset size line-num) tuples.
   Returns bytes for the section."
  (let* ((producer "Habu Lisp Compiler 0.1")
         ;; Build DIE tree
         (dies nil))

    ;; Emit compile unit DIE
    (setq dies (append dies
                       (list 1)  ; abbrev code 1 = compile_unit
                       (emit-string source-name)
                       (emit-string producer)
                       (emit-u16-le +dw-lang-lisp+)
                       (emit-u64-le code-start)   ; low_pc
                       (emit-u32-le code-size)    ; high_pc (offset)
                       (emit-u32-le line-offset)  ; stmt_list
                       (emit-string comp-dir)))

    ;; Emit subprogram DIEs for each function
    (dolist (fn functions)
      (let ((name (first fn))
            (offset (second fn))
            (size (third fn))
            (line (fourth fn)))
        (setq dies (append dies
                           (list 2)  ; abbrev code 2 = subprogram
                           (emit-string name)
                           (emit-u64-le (+ code-start offset))  ; low_pc
                           (emit-u32-le size)                   ; high_pc (offset)
                           ;; DW_AT_external is flag_present, no data
                           (emit-u8 1)   ; decl_file
                           (emit-u8 (or line 1))))))  ; decl_line

    ;; Null DIE to end children
    (setq dies (append dies (list 0)))

    ;; Build complete .debug_info with header
    (let* ((dies-flat (apply #'append (mapcar (lambda (x) (if (listp x) x (list x))) dies)))
           ;; DWARF5 header after unit_length: version(2) + unit_type(1) + address_size(1) + abbrev_offset(4) = 8
           (unit-length (+ 8 (length dies-flat))))
      (append
       ;; Unit header (DWARF32 format)
       (emit-u32-le unit-length)  ; unit_length (excluding this field)
       (emit-u16-le 5)            ; version = 5
       (emit-u8 1)                ; unit_type = DW_UT_compile
       (emit-u8 8)                ; address_size = 8
       (emit-u32-le 0)            ; debug_abbrev_offset = 0
       ;; DIEs
       dies-flat))))

;;; Line number program generation

(defun make-debug-line (functions source-name source-dir code-start)
  "Generate .debug_line section.
   FUNCTIONS is a list of (name offset size line-num) tuples.
   Returns bytes for the section."
  (let* ((min-inst-length 4)    ; ARM64 instructions are 4 bytes
         (max-ops 1)
         (default-is-stmt 1)
         (line-base -5)
         (line-range 14)
         (opcode-base 13)
         ;; Build file/directory tables
         (dir-table (list source-dir))
         (file-table (list (list source-name 0)))  ; (name dir-index)
         ;; Build line number program
         (program nil))

    ;; Generate line program entries for each function
    (let ((current-addr 0)
          (current-line 1))
      (dolist (fn functions)
        (let* ((name (first fn))
               (offset (second fn))
               (size (third fn))
               (line (or (fourth fn) 1))
               (addr (+ code-start offset)))
          ;; Set address
          (setq program (append program
                                (list 0)  ; extended opcode
                                (encode-uleb128 9)  ; length (1 + 8)
                                (list +dw-lne-set-address+)
                                (emit-u64-le addr)))
          ;; Advance line if needed
          (when (/= line current-line)
            (setq program (append program
                                  (list +dw-lns-advance-line+)
                                  (encode-sleb128 (- line current-line))))
            (setq current-line line))
          ;; Set prologue end and copy
          (setq program (append program
                                (list +dw-lns-set-prologue-end+)
                                (list +dw-lns-copy+)))
          (setq current-addr addr))))

    ;; End sequence
    (setq program (append program
                          (list 0)  ; extended opcode
                          (encode-uleb128 1)  ; length
                          (list +dw-lne-end-sequence+)))

    ;; Build header
    (let* ((program-flat (apply #'append (mapcar (lambda (x) (if (listp x) x (list x))) program)))
           ;; Directory entry format (DWARF5)
           (dir-format (list 1    ; entry_format_count
                            1     ; DW_LNCT_path
                            +dw-form-string+))  ; DW_FORM_string
           ;; File entry format (DWARF5)
           (file-format (list 2   ; entry_format_count
                             1    ; DW_LNCT_path
                             +dw-form-string+
                             2    ; DW_LNCT_directory_index
                             +dw-form-data1+))
           ;; Directory entries
           (dir-entries (append
                         (encode-uleb128 (length dir-table))
                         (apply #'append (mapcar #'emit-string dir-table))))
           ;; File entries
           (file-entries (append
                          (encode-uleb128 (length file-table))
                          (apply #'append
                                 (mapcar (lambda (f)
                                           (append (emit-string (first f))
                                                   (list (second f))))
                                         file-table))))
           ;; Standard opcode lengths
           (std-opcode-lengths (list 0 1 1 1 1 0 0 0 1 0 0 1))  ; 12 entries
           ;; Header fields
           (header-fields (append
                          (list min-inst-length)
                          (list max-ops)
                          (list default-is-stmt)
                          (list (logand line-base #xff))  ; signed
                          (list line-range)
                          (list opcode-base)
                          std-opcode-lengths
                          dir-format
                          dir-entries
                          file-format
                          file-entries))
           (header-length (length header-fields))
           (total-length (+ 8 header-length (length program-flat))))  ; 8 = fields before prologue_length

      ;; Assemble complete section
      (append
       (emit-u32-le total-length)    ; unit_length
       (emit-u16-le 5)               ; version = 5
       (emit-u8 8)                   ; address_size
       (emit-u8 0)                   ; segment_selector_size
       (emit-u32-le header-length)   ; header_length
       header-fields
       program-flat))))

;;; Main interface

(defun generate-dwarf (functions source-name comp-dir code-start code-size)
  "Generate DWARF5 debug info sections.
   FUNCTIONS: list of (name offset size line-num)
   Returns: (debug-abbrev debug-info debug-line)"
  (let* ((abbrev-bytes (make-abbrev-table))
         (line-bytes (make-debug-line functions source-name comp-dir code-start))
         (info-bytes (make-debug-info functions source-name comp-dir
                                      code-start code-size 0)))
    (values (coerce abbrev-bytes 'vector)
            (coerce info-bytes 'vector)
            (coerce line-bytes 'vector))))

;;; Export for macho.lisp integration

(defun dwarf-sections-for-macho (functions source-file code-base code-size)
  "Generate DWARF sections ready for Mach-O embedding.
   Returns alist: ((\"__debug_abbrev\" . bytes) (\"__debug_info\" . bytes) (\"__debug_line\" . bytes))"
  (let* ((source-name (or source-file "program.lisp"))
         (comp-dir "/tmp"))
    (multiple-value-bind (abbrev info line)
        (generate-dwarf functions source-name comp-dir code-base code-size)
      (list (cons "__debug_abbrev" abbrev)
            (cons "__debug_info" info)
            (cons "__debug_line" line)))))

;;; ============================================================
;;; Integration with Source Locations
;;; ============================================================

#+sbcl
(defun build-dwarf-function-list (fnoffs fn-locations)
  "Build function list for DWARF from fnoffs and source locations.
   FNOFFS: alist of (symbol . byte-offset) from codegen
   FN-LOCATIONS: alist of (symbol . srcloc) from source-locations
   Returns: list of (name offset size line-num) for DWARF generation"
  (let ((result nil)
        (sorted-fnoffs (sort (copy-list fnoffs) #'< :key #'cdr)))
    ;; Calculate sizes from consecutive offsets
    ;; fnoffs names are now strings (normalized in codegen.lisp)
    (loop for (fn-entry . rest) on sorted-fnoffs do
      (let* ((name-str (car fn-entry))  ; Already a string
             (offset (cdr fn-entry))
             (next-offset (if rest (cdar rest) (+ offset 100))) ; default size
             (size (- next-offset offset))
             ;; Look up source location - fn-locations uses symbol keys
             ;; so we need to find by string match
             (loc-entry (find-if (lambda (e)
                                   (and (symbolp (car e))
                                        (string= name-str (symbol-name (car e)))))
                                 fn-locations))
             (line (if (and loc-entry (cdr loc-entry))
                       (srcloc-line (cdr loc-entry))
                       1)))
        (push (list name-str offset size line) result)))
    (nreverse result)))

(defun generate-dwarf-from-locations (fnoffs code-size &optional source-file)
  "Generate DWARF sections using source location info.
   Uses *function-locations* if available for line numbers."
  (let* ((fn-locations (if (boundp '*function-locations*)
                           (symbol-value '*function-locations*)
                           nil))
         (source-name (or source-file
                          (if (boundp '*current-source-file*)
                              (symbol-value '*current-source-file*)
                              nil)
                          "program.lisp"))
         (functions (build-dwarf-function-list fnoffs fn-locations))
         (code-base #x100000468)) ; VM_BASE + code_offset + wrapper
    (dwarf-sections-for-macho functions source-name code-base code-size)))

;;; ============================================================
;;; dSYM Bundle Generation
;;; ============================================================

;;; Mach-O dSYM file type
(defconstant +mh-dsym+ 10)
(defconstant +mh-magic-64+ #xFEEDFACF)
(defconstant +cpu-type-arm64+ #x0100000C)
(defconstant +cpu-subtype-arm64-all+ #x00000000)
(defconstant +lc-segment-64+ #x19)
(defconstant +lc-uuid+ #x1B)
(defconstant +lc-symtab+ #x02)
(defconstant +s-attr-debug+ #x02000000)

(defun dsym-emit-u32-le (value)
  "Return 32-bit value as little-endian byte list"
  (list (logand value #xff)
        (logand (ash value -8) #xff)
        (logand (ash value -16) #xff)
        (logand (ash value -24) #xff)))

(defun dsym-emit-u64-le (value)
  "Return 64-bit value as little-endian byte list"
  (list (logand value #xff)
        (logand (ash value -8) #xff)
        (logand (ash value -16) #xff)
        (logand (ash value -24) #xff)
        (logand (ash value -32) #xff)
        (logand (ash value -40) #xff)
        (logand (ash value -48) #xff)
        (logand (ash value -56) #xff)))

(defun dsym-emit-string-padded (str len)
  "Emit string padded to LEN bytes with nulls"
  (let ((result nil))
    (dotimes (i (min (length str) len))
      (push (char-code (char str i)) result))
    (dotimes (i (- len (min (length str) len)))
      (push 0 result))
    (nreverse result)))

(defun dsym-emit-zeros (n)
  "Emit N zero bytes"
  (make-list n :initial-element 0))

(defun dsym-align-up (val alignment)
  "Align VAL up to ALIGNMENT"
  (let ((rem (mod val alignment)))
    (if (= rem 0)
        val
        (+ val (- alignment rem)))))

(defun build-dsym-macho (dwarf-sections uuid-bytes)
  "Build a Mach-O dSYM file containing DWARF sections.
   DWARF-SECTIONS: alist of (name . byte-vector)
   UUID-BYTES: 16-byte UUID to match the main binary
   Returns byte vector of the complete dSYM Mach-O file."
  (let* ((header-size 32)
         ;; Calculate section sizes
         (section-count (length dwarf-sections))
         (segment-cmd-size (+ 72 (* section-count 80)))
         (uuid-cmd-size 24)
         (symtab-cmd-size 24)
         (ncmds 3)  ; __DWARF segment, UUID, SYMTAB
         (sizeofcmds (+ segment-cmd-size uuid-cmd-size symtab-cmd-size))
         ;; Data starts after header + load commands, aligned to 8
         (data-start (dsym-align-up (+ header-size sizeofcmds) 8))
         ;; Calculate section file offsets
         (section-infos nil)
         (current-offset data-start))

    ;; Build section info list: (name offset size vmaddr)
    (dolist (sec dwarf-sections)
      (let* ((name (car sec))
             (bytes (cdr sec))
             (size (length bytes)))
        (push (list name current-offset size 0) section-infos)
        (setq current-offset (dsym-align-up (+ current-offset size) 4))))
    (setq section-infos (nreverse section-infos))

    (let* ((total-section-size (- current-offset data-start))
           (file-size current-offset)
           ;; Mach-O header
           (header (append
                    (dsym-emit-u32-le +mh-magic-64+)
                    (dsym-emit-u32-le +cpu-type-arm64+)
                    (dsym-emit-u32-le +cpu-subtype-arm64-all+)
                    (dsym-emit-u32-le +mh-dsym+)
                    (dsym-emit-u32-le ncmds)
                    (dsym-emit-u32-le sizeofcmds)
                    (dsym-emit-u32-le 0)  ; flags
                    (dsym-emit-u32-le 0))) ; reserved
           ;; __DWARF segment command
           (segment-cmd (append
                         (dsym-emit-u32-le +lc-segment-64+)
                         (dsym-emit-u32-le segment-cmd-size)
                         (dsym-emit-string-padded "__DWARF" 16)
                         (dsym-emit-u64-le 0)  ; vmaddr
                         (dsym-emit-u64-le total-section-size)  ; vmsize
                         (dsym-emit-u64-le data-start)  ; fileoff
                         (dsym-emit-u64-le total-section-size)  ; filesize
                         (dsym-emit-u32-le 0)  ; maxprot (not executable)
                         (dsym-emit-u32-le 0)  ; initprot
                         (dsym-emit-u32-le section-count)
                         (dsym-emit-u32-le 0))) ; flags
           ;; Section headers
           (section-hdrs nil))

      ;; Build section headers
      (dolist (info section-infos)
        (let ((name (first info))
              (offset (second info))
              (size (third info)))
          (setq section-hdrs
                (append section-hdrs
                        (dsym-emit-string-padded name 16)
                        (dsym-emit-string-padded "__DWARF" 16)
                        (dsym-emit-u64-le 0)  ; addr
                        (dsym-emit-u64-le size)  ; size
                        (dsym-emit-u32-le offset)  ; offset
                        (dsym-emit-u32-le 0)  ; align (2^0 = 1)
                        (dsym-emit-u32-le 0)  ; reloff
                        (dsym-emit-u32-le 0)  ; nreloc
                        (dsym-emit-u32-le +s-attr-debug+)  ; flags
                        (dsym-emit-u32-le 0)  ; reserved1
                        (dsym-emit-u32-le 0)  ; reserved2
                        (dsym-emit-u32-le 0))))) ; reserved3

      ;; UUID command
      (let ((uuid-cmd (append
                       (dsym-emit-u32-le +lc-uuid+)
                       (dsym-emit-u32-le uuid-cmd-size)
                       uuid-bytes)))

        ;; SYMTAB command (empty but required)
        (let ((symtab-cmd (append
                           (dsym-emit-u32-le +lc-symtab+)
                           (dsym-emit-u32-le symtab-cmd-size)
                           (dsym-emit-u32-le 0)  ; symoff
                           (dsym-emit-u32-le 0)  ; nsyms
                           (dsym-emit-u32-le 0)  ; stroff
                           (dsym-emit-u32-le 0)))) ; strsize

          ;; Assemble all load commands
          (let* ((load-commands (append segment-cmd section-hdrs uuid-cmd symtab-cmd))
                 ;; Padding between commands and data
                 (padding-size (- data-start (+ header-size (length load-commands))))
                 (padding (dsym-emit-zeros padding-size))
                 ;; Section data
                 (section-data nil))

            ;; Collect section data with padding
            (let ((data-offset data-start))
              (dolist (sec dwarf-sections)
                (let* ((bytes (cdr sec))
                       (size (length bytes))
                       (next-offset (dsym-align-up (+ data-offset size) 4))
                       (pad-size (- next-offset (+ data-offset size))))
                  (setq section-data (append section-data (coerce bytes 'list)))
                  (when (> pad-size 0)
                    (setq section-data (append section-data (dsym-emit-zeros pad-size))))
                  (setq data-offset next-offset))))

            ;; Return complete file as byte vector
            (coerce (append header load-commands padding section-data)
                    'vector)))))))

(defun write-dsym-info-plist (path binary-name)
  "Write Info.plist for dSYM bundle"
  (with-open-file (out path :direction :output
                            :if-exists :supersede)
    (format out "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format out "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">~%")
    (format out "<plist version=\"1.0\">~%")
    (format out "<dict>~%")
    (format out "  <key>CFBundleDevelopmentRegion</key>~%")
    (format out "  <string>English</string>~%")
    (format out "  <key>CFBundleIdentifier</key>~%")
    (format out "  <string>com.habu.~A.dSYM</string>~%" binary-name)
    (format out "  <key>CFBundleInfoDictionaryVersion</key>~%")
    (format out "  <string>6.0</string>~%")
    (format out "  <key>CFBundlePackageType</key>~%")
    (format out "  <string>dSYM</string>~%")
    (format out "  <key>CFBundleSignature</key>~%")
    (format out "  <string>????</string>~%")
    (format out "  <key>CFBundleVersion</key>~%")
    (format out "  <string>1</string>~%")
    (format out "</dict>~%")
    (format out "</plist>~%")))

(defun generate-uuid-bytes ()
  "Generate a random 16-byte UUID for dSYM"
  (let ((uuid nil))
    (dotimes (i 16)
      (push (random 256) uuid))
    ;; Set version (4) and variant (2) bits
    (setf (nth 6 uuid) (logior #x40 (logand (nth 6 uuid) #x0f)))
    (setf (nth 8 uuid) (logior #x80 (logand (nth 8 uuid) #x3f)))
    (nreverse uuid)))

(defun write-dsym-bundle (output-path fnoffs code-size &optional source-file)
  "Write a dSYM bundle for the compiled binary.
   The bundle is placed at OUTPUT-PATH.dSYM/
   lldb automatically finds dSYM bundles next to executables."
  (let* ((dsym-path (format nil "~A.dSYM" output-path))
         (contents-path (format nil "~A/Contents" dsym-path))
         (dwarf-path (format nil "~A/Resources/DWARF" contents-path))
         (binary-name (file-namestring output-path))
         (dwarf-file (format nil "~A/~A" dwarf-path binary-name)))

    ;; Create directory structure
    (ensure-directories-exist (format nil "~A/" dwarf-path))

    ;; Generate DWARF sections
    (let ((sections (generate-dwarf-from-locations fnoffs code-size source-file))
          (uuid-bytes (generate-uuid-bytes)))

      ;; Write Info.plist
      (write-dsym-info-plist (format nil "~A/Info.plist" contents-path) binary-name)

      ;; Build and write dSYM Mach-O file
      (let ((dsym-macho (build-dsym-macho sections uuid-bytes)))
        (with-open-file (out dwarf-file
                             :direction :output
                             :if-exists :supersede
                             :element-type '(unsigned-byte 8))
          (write-sequence dsym-macho out)))

      (format t "dSYM bundle written to ~A~%" dsym-path)
      (format t "  Sections: ~{~A~^, ~}~%" (mapcar #'car sections))
      dsym-path)))
