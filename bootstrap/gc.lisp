;;; Native ARM64 Garbage Collector for Habu
;;;
;;; Cheney's copying collector with two semispaces.
;;; Written in ARM64 assembly via arm64/asm.lisp intrinsics.
;;;
;;; This code generates ARM64 machine code bytes that implement GC.
;;; The GC runs when x28 (bump pointer) exceeds from_end threshold.
;;;
;;; Heap Globals Layout (at x27):
;;;   [x27+0]:   intern_table      (tagged pointer)
;;;   [x27+8]:   lambda_counter    (untagged integer)
;;;   [x27+16]:  from_end          (GC trigger address)
;;;   [x27+24]:  half_heap_size    (constant)
;;;   [x27+32]:  space_flag        (0 or half_heap_size)
;;;   [x27+40]:  gc_state          (0=idle, for future incremental)
;;;   [x27+48]:  symbol_counter    (codegen symbol interning)
;;;   [x27+56]:  symbol_table      (codegen symbol interning)
;;;   [x27+64]:  heap data starts
;;;
;;; Semispace 0: [x27+64 .. x27+64+half)
;;; Semispace 1: [x27+64+half .. x27+64+2*half)
;;;
;;; Tags: 0=fixnum, 1=cons, 2=symbol, 3=vector, 4=string, 5=closure, 6=nil, 7=forward
;;;
;;; Register usage during GC:
;;;   x16 = to_scan (Cheney scan pointer)
;;;   x17 = to_free (allocation pointer in to-space)
;;;   x18 = from_start
;;;   x19 = from_end
;;;   x20-x23 = scratch during copy
;;;   x0-x15, x24 = saved as roots

(in-package :habu)

;;; ============================================================
;;; GC Constants
;;; ============================================================

(defconstant +gc-intern-table-offset+ 0)
(defconstant +gc-lambda-counter-offset+ 8)
(defconstant +gc-from-end-offset+ 16)
(defconstant +gc-half-heap-offset+ 24)
(defconstant +gc-space-flag-offset+ 32)
(defconstant +gc-state-offset+ 40)
(defconstant +gc-symbol-counter-offset+ 48)  ;; For codegen symbol table
(defconstant +gc-symbol-table-offset+ 56)    ;; For codegen symbol table
(defconstant +gc-heap-data-offset+ 64)       ;; Heap data starts after globals

(defconstant +gc-tag-mask+ #xF)
(defconstant +gc-tag-forward+ 7)

;;; ============================================================
;;; GC Trigger Check (inline after allocations)
;;; ============================================================

(defun gc-trigger-check ()
  "Generate inline GC trigger check. Call after bumping x28.
   Uses x9 as scratch. Calls gc_collect if x28 >= from_end."
  (append
   (arm64:ldr 9 27 :offset +gc-from-end-offset+)  ; x9 = from_end
   (arm64:cmp 28 9)                                ; compare x28, from_end
   (arm64:b.lo 2)                                  ; skip if x28 < from_end
   (list '(:call-fn GC-COLLECT))))                 ; bl gc_collect

;;; ============================================================
;;; Object Size Calculation
;;; ============================================================

(defun gc-object-size-asm ()
  "Generate code to calculate object size from tagged pointer in x0.
   Result in x1 (size in bytes). Uses x2 as scratch.
   Assumes tag is NOT 0 (fixnum), 6 (nil), or 7 (forward)."
  (append
   ;; Extract tag
   (arm64:and* 2 0 +gc-tag-mask+ :imm t)  ; x2 = tag

   ;; Check for cons (tag 1) -> 16 bytes
   (arm64:cmp 2 1 :imm t)
   (arm64:b.ne 3)
   (arm64:movz 1 16)
   (arm64:b 24)  ; jump to end

   ;; Check for symbol (tag 2) -> 8 bytes
   (arm64:cmp 2 2 :imm t)
   (arm64:b.ne 3)
   (arm64:movz 1 8)
   (arm64:b 20)

   ;; Check for closure (tag 5) -> 16 bytes
   (arm64:cmp 2 5 :imm t)
   (arm64:b.ne 3)
   (arm64:movz 1 16)
   (arm64:b 16)

   ;; Check for vector (tag 3) -> 8 + length*8
   (arm64:cmp 2 3 :imm t)
   (arm64:b.ne 7)
   (arm64:and* 2 0 -16 :imm t)        ; x2 = base address (clear low 4 bits)
   (arm64:ldr 1 2 :offset 0)          ; x1 = length (untagged)
   (arm64:lsl 1 1 3 :imm t)           ; x1 = length * 8
   (arm64:add 1 1 8 :imm t)           ; x1 = 8 + length*8
   (arm64:b 7)

   ;; String (tag 4) -> align16(8 + length)
   (arm64:and* 2 0 -16 :imm t)        ; x2 = base address (clear low 4 bits)
   (arm64:ldr 1 2 :offset 0)          ; x1 = length (untagged)
   (arm64:add 1 1 23 :imm t)          ; x1 = length + 8 + 15
   (arm64:and* 1 1 -16 :imm t)))      ; x1 = align to 16

;;; ============================================================
;;; gc_copy: Copy one object to to-space
;;; ============================================================

(defun gc-copy-asm ()
  "Generate gc_copy function.
   Input: x0 = tagged pointer
   Output: x0 = new tagged pointer (or unchanged if not from-space)
   Uses: x1-x5 as scratch
   Assumes: x17 = to_free, x18 = from_start, x19 = from_end"
  (append
   ;; Function entry
   (list '(:fn-label GC-COPY))

   ;; Check if immediate (fixnum tag 0 or nil tag 6)
   (arm64:and* 1 0 +gc-tag-mask+ :imm t)  ; x1 = tag
   (arm64:cbz 1 14)                    ; if tag=0 (fixnum), return unchanged (skip to ret)
   (arm64:cmp 1 6 :imm t)
   (arm64:b.eq 12)                     ; if tag=6 (nil), return unchanged (skip to ret)

   ;; Get base address (clear low 4 bits)
   (arm64:and* 2 0 -16 :imm t)        ; x2 = base (ptr & ~0xF)

   ;; Check if in from-space: from_start <= base < from_end
   (arm64:cmp 2 18)                    ; compare base, from_start
   (arm64:b.lo 9)                      ; if base < from_start, return unchanged (skip to ret)
   (arm64:cmp 2 19)                    ; compare base, from_end
   (arm64:b.hs 7)                      ; if base >= from_end, return unchanged (skip to ret)

   ;; Check if already forwarded
   (arm64:ldr 3 2 :offset 0)          ; x3 = first word at base
   (arm64:and* 4 3 +gc-tag-mask+ :imm t)  ; x4 = tag of first word
   (arm64:cmp 4 +gc-tag-forward+ :imm t)
   (arm64:b.ne 4)                      ; if not forwarded, go copy

   ;; Already forwarded: return forward_addr | original_tag
   (arm64:and* 0 3 -16 :imm t)        ; x0 = forward address (clear tag)
   (arm64:orr 0 0 1)                   ; x0 = forward_addr | original_tag
   (arm64:ret)

   ;; Not forwarded: copy object
   ;; x0 = original tagged ptr, x1 = original tag, x2 = base

   ;; Save original tag and base
   (arm64:mov 5 1)                     ; x5 = original tag (save)
   (arm64:mov 4 2)                     ; x4 = original base (save)

   ;; Calculate object size -> x1
   ;; (inline size calculation based on tag in x5)
   (arm64:cmp 5 1 :imm t)             ; cons?
   (arm64:b.ne 3)
   (arm64:movz 1 16)
   (arm64:b 20)

   (arm64:cmp 5 2 :imm t)             ; symbol?
   (arm64:b.ne 3)
   (arm64:movz 1 8)
   (arm64:b 16)

   (arm64:cmp 5 5 :imm t)             ; closure?
   (arm64:b.ne 3)
   (arm64:movz 1 16)
   (arm64:b 12)

   (arm64:cmp 5 3 :imm t)             ; vector?
   (arm64:b.ne 6)
   (arm64:ldr 1 4 :offset 0)          ; x1 = length
   (arm64:lsl 1 1 3 :imm t)           ; * 8
   (arm64:add 1 1 8 :imm t)           ; + 8
   (arm64:b 5)

   ;; string (tag 4)
   (arm64:ldr 1 4 :offset 0)          ; x1 = length
   (arm64:add 1 1 23 :imm t)          ; + 8 + 15
   (arm64:and* 1 1 -16 :imm t)        ; align to 16

   ;; x1 = size, x4 = from_base, x5 = original tag, x17 = to_free
   ;; Copy bytes: from x4 to x17, size x1
   (arm64:mov 2 17)                    ; x2 = to_free (new location)
   (arm64:mov 3 1)                     ; x3 = remaining bytes

   ;; Copy loop (8 bytes at a time)
   ;; copy_loop:
   (arm64:cbz 3 5)                     ; if remaining=0, done
   (arm64:ldr 0 4 :offset 0)          ; x0 = load from source
   (arm64:str 0 17 :offset 0)         ; store to dest
   (arm64:add 4 4 8 :imm t)           ; source += 8
   (arm64:add 17 17 8 :imm t)         ; to_free += 8
   (arm64:sub 3 3 8 :imm t)           ; remaining -= 8
   (arm64:b -6)                        ; loop back

   ;; Install forwarding pointer at original location
   ;; x2 = new base, x4 = (advanced past object), x5 = original tag
   ;; Need original base - recalculate from x4 and x1
   (arm64:sub 4 4 1)                   ; x4 = original base again
   ;; Load forward tag (7) into x6, then OR with x2
   (arm64:movz 6 +gc-tag-forward+)
   (arm64:orr 0 2 6)                   ; x0 = new_base | 7
   (arm64:str 0 4 :offset 0)          ; store forwarding pointer

   ;; Return new address with original tag
   (arm64:orr 0 2 5)                   ; x0 = new_base | original_tag
   (arm64:ret)))

;;; ============================================================
;;; gc_collect: Main collection routine
;;; ============================================================

(defun gc-collect-asm ()
  "Generate gc_collect function.
   Called when x28 >= from_end.
   Saves roots, copies live objects, flips spaces, updates x28."
  (append
   (list '(:fn-label GC-COLLECT))

   ;; ===== Prologue: save all potential roots =====
   ;; Stack frame: 176 bytes for x0-x15, x24, x30, padding
   (arm64:sub 31 31 176 :imm t)
   (arm64:stp 30 29 31 :offset 0)      ; lr, fp
   (arm64:stp 0 1 31 :offset 16)
   (arm64:stp 2 3 31 :offset 32)
   (arm64:stp 4 5 31 :offset 48)
   (arm64:stp 6 7 31 :offset 64)
   (arm64:stp 8 9 31 :offset 80)
   (arm64:stp 10 11 31 :offset 96)
   (arm64:stp 12 13 31 :offset 112)
   (arm64:stp 14 15 31 :offset 128)
   (arm64:stp 24 25 31 :offset 144)
   (arm64:str 26 31 :offset 160)

   ;; ===== Setup GC pointers =====
   ;; x18 = from_start = x27 + 48 + space_flag
   (arm64:ldr 18 27 :offset +gc-space-flag-offset+)
   (arm64:add 18 18 27)
   (arm64:add 18 18 +gc-heap-data-offset+ :imm t)

   ;; x19 = from_end = from_start + half_heap
   (arm64:ldr 9 27 :offset +gc-half-heap-offset+)
   (arm64:add 19 18 9)

   ;; to_start: if space_flag=0, to=x27+48+half; else to=x27+48
   ;; x17 = to_free = to_start
   (arm64:ldr 10 27 :offset +gc-space-flag-offset+)
   (arm64:cbnz 10 4)
   ;; space_flag=0: to_start = x27 + 48 + half
   (arm64:add 17 27 +gc-heap-data-offset+ :imm t)
   (arm64:add 17 17 9)
   (arm64:b 2)
   ;; space_flag!=0: to_start = x27 + 48
   (arm64:add 17 27 +gc-heap-data-offset+ :imm t)

   ;; x16 = to_scan = to_free (initially)
   (arm64:mov 16 17)

   ;; ===== Copy roots =====

   ;; Copy intern_table at [x27+0]
   (arm64:ldr 0 27 :offset +gc-intern-table-offset+)
   (list '(:call-fn GC-COPY))
   (arm64:str 0 27 :offset +gc-intern-table-offset+)

   ;; Copy saved x0-x7 (potential roots on stack)
   ;; x0 at [sp+16]
   (arm64:ldr 0 31 :offset 16)
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 16)

   (arm64:ldr 0 31 :offset 24)  ; x1
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 24)

   (arm64:ldr 0 31 :offset 32)  ; x2
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 32)

   (arm64:ldr 0 31 :offset 40)  ; x3
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 40)

   (arm64:ldr 0 31 :offset 48)  ; x4
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 48)

   (arm64:ldr 0 31 :offset 56)  ; x5
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 56)

   (arm64:ldr 0 31 :offset 64)  ; x6
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 64)

   (arm64:ldr 0 31 :offset 72)  ; x7
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 72)

   ;; Copy x24 (closure environment)
   (arm64:ldr 0 31 :offset 144)
   (list '(:call-fn GC-COPY))
   (arm64:str 0 31 :offset 144)

   ;; ===== Cheney scan loop =====
   ;; while (to_scan < to_free) { scan object at to_scan }
   ;; scan_loop:
   (list '(:label GC-SCAN-LOOP))
   (arm64:cmp 16 17)
   (arm64:b.hs 20)                     ; if to_scan >= to_free, done

   ;; Load word at to_scan, check if it's a heap pointer to copy
   (arm64:ldr 0 16 :offset 0)
   (arm64:and* 1 0 +gc-tag-mask+ :imm t)  ; x1 = tag
   (arm64:cbz 1 6)                     ; skip if fixnum (tag 0)
   (arm64:cmp 1 6 :imm t)
   (arm64:b.eq 4)                      ; skip if nil (tag 6)
   (arm64:cmp 1 7 :imm t)
   (arm64:b.eq 2)                      ; skip if forward (tag 7)

   ;; It's a potential heap pointer - copy it
   (list '(:call-fn GC-COPY))
   (arm64:str 0 16 :offset 0)         ; store updated pointer

   ;; Advance to_scan by 8 bytes
   (arm64:add 16 16 8 :imm t)
   (arm64:b -14)                       ; back to scan_loop

   ;; ===== Flip spaces =====
   (list '(:label GC-SCAN-DONE))

   ;; new_flag = half - old_flag
   (arm64:ldr 9 27 :offset +gc-half-heap-offset+)
   (arm64:ldr 10 27 :offset +gc-space-flag-offset+)
   (arm64:sub 10 9 10)                 ; new_flag = half - old_flag
   (arm64:str 10 27 :offset +gc-space-flag-offset+)

   ;; new from_end = x27 + 48 + new_flag + half
   (arm64:add 11 27 +gc-heap-data-offset+ :imm t)
   (arm64:add 11 11 10)                ; + new_flag
   (arm64:add 11 11 9)                 ; + half
   (arm64:str 11 27 :offset +gc-from-end-offset+)

   ;; x28 = to_free (new allocation pointer)
   (arm64:mov 28 17)

   ;; ===== Epilogue: restore registers =====
   (arm64:ldp 30 29 31 :offset 0)
   (arm64:ldp 0 1 31 :offset 16)
   (arm64:ldp 2 3 31 :offset 32)
   (arm64:ldp 4 5 31 :offset 48)
   (arm64:ldp 6 7 31 :offset 64)
   (arm64:ldp 8 9 31 :offset 80)
   (arm64:ldp 10 11 31 :offset 96)
   (arm64:ldp 12 13 31 :offset 112)
   (arm64:ldp 14 15 31 :offset 128)
   (arm64:ldp 24 25 31 :offset 144)
   (arm64:ldr 26 31 :offset 160)
   (arm64:add 31 31 176 :imm t)

   (arm64:ret)))

;;; ============================================================
;;; Complete GC runtime code
;;; ============================================================

(defun gc-runtime-code ()
  "Generate complete GC runtime (gc_copy + gc_collect).
   Returns list of ARM64 instruction bytes with function markers."
  (append (gc-copy-asm) (gc-collect-asm)))

;;; ============================================================
;;; GC-enabled heap wrapper
;;; ============================================================

(defun gc-heap-init-code (heap-page-offset half-heap-size)
  "Generate heap initialization code for GC-enabled runtime.
   HEAP-PAGE-OFFSET: pages from ADRP to __DATA segment
   HALF-HEAP-SIZE: size of each semispace in bytes

   Initializes:
     [x27+0]:  intern_table = nil (0x06)
     [x27+8]:  lambda_counter = 0
     [x27+16]: from_end = x27 + 64 + half_heap_size
     [x27+24]: half_heap_size
     [x27+32]: space_flag = 0
     [x27+40]: gc_state = 0
     [x27+48]: symbol_counter (codegen)
     [x27+56]: symbol_table (codegen)
     [x27+64]: heap data starts, x28 points here"
  (let* ((half-high (ash half-heap-size -16))
         (half-low (logand half-heap-size #xFFFF)))
    (append
     ;; Setup heap base via ADRP
     (arm64:adrp 27 heap-page-offset)

     ;; Store nil (0x06) at intern_table
     (arm64:movz 9 6)
     (arm64:str 9 27 :offset +gc-intern-table-offset+)

     ;; Store 0 at lambda_counter
     (arm64:movz 9 0)
     (arm64:str 9 27 :offset +gc-lambda-counter-offset+)

     ;; Load half_heap_size into x10
     (arm64:movz 10 half-low)
     (if (> half-high 0)
         (arm64:movk 10 half-high :lsl 16)
         (list (arm64:nop)))

     ;; Store half_heap_size
     (arm64:str 10 27 :offset +gc-half-heap-offset+)

     ;; Store space_flag = 0
     (arm64:str 9 27 :offset +gc-space-flag-offset+)  ; x9 still 0

     ;; Store gc_state = 0
     (arm64:str 9 27 :offset +gc-state-offset+)

     ;; Compute from_end = x27 + 64 + half_heap_size
     (arm64:add 11 27 +gc-heap-data-offset+ :imm t)
     (arm64:add 11 11 10)
     (arm64:str 11 27 :offset +gc-from-end-offset+)

     ;; Set x28 = x27 + 64 (allocation pointer)
     (arm64:add 28 27 +gc-heap-data-offset+ :imm t))))
