;;; Generational Garbage Collector for Habu
;;;
;;; A generational (two-generation) collector with:
;;; - Nursery: Small, fast bump-pointer region for new allocations
;;; - Old space: Larger region for long-lived objects (uses copying GC)
;;;
;;; Design goals:
;;; 1. Most allocations never trigger GC (nursery is fast)
;;; 2. Minor GC only scans nursery + remembered set
;;; 3. Major GC is rare (only when old space fills up)
;;; 4. Compatible with existing tagged value representation
;;;
;;; Memory Layout:
;;;   [x27+0..80]:    GC globals (same as current)
;;;   [x27+80]:       nursery-start
;;;   [x27+80+N]:     nursery-end, old-space-start
;;;   [x27+80+N+H]:   old-space-midpoint (semispace boundary)
;;;   [x27+80+N+2H]:  old-space-end
;;;
;;; Where N = nursery size (e.g., 1MB), H = old space half (e.g., 16MB)
;;;
;;; Register usage:
;;;   x28 = allocation pointer (within nursery during normal operation)
;;;   x27 = globals base
;;;
;;; New GC globals (added to existing layout):
;;;   [x27+80]:  nursery-start
;;;   [x27+88]:  nursery-end (also old-space-start)
;;;   [x27+96]:  card-table-start (for remembered set)
;;;   [x27+104]: old-space-half-size
;;;   [x27+112]: old-space-flag (0 or half, same as current space-flag)
;;;   [x27+120]: old-space-alloc (allocation pointer in old space)
;;;
;;; The heap data area now starts at [x27+128] instead of [x27+96].
;;;
;;; Card Table:
;;; - Divides old space into 512-byte cards
;;; - Each card has a 1-byte dirty flag
;;; - Write barrier sets flag when writing pointer to old object
;;; - Minor GC scans dirty cards as additional roots
;;;
;;; Write Barrier:
;;; - Generated after every pointer store
;;; - Checks if target is in old space
;;; - If so, marks the card as dirty
;;; - Fast path: most stores are to nursery objects (no barrier needed)

(in-package :habu)

;;; ============================================================
;;; Configuration Constants
;;; ============================================================

(defconstant +gen-nursery-size+ (* 1 1024 1024))      ; 1MB nursery
(defconstant +gen-old-space-half+ (* 16 1024 1024))   ; 16MB per old semispace
(defconstant +gen-card-size+ 512)                     ; 512 bytes per card
(defconstant +gen-card-shift+ 9)                      ; log2(512)

;;; New GC globals offsets (extend existing layout)
(defconstant +gen-nursery-start-offset+ 80)
(defconstant +gen-nursery-end-offset+ 88)
(defconstant +gen-card-table-offset+ 96)
(defconstant +gen-old-half-size-offset+ 104)
(defconstant +gen-old-space-flag-offset+ 112)
(defconstant +gen-old-alloc-offset+ 120)      ; old-space-alloc pointer
(defconstant +gen-heap-data-offset+ 128)

;;; Card table sizing
;;; With 32MB old space (2 x 16MB) and 512-byte cards:
;;; card_table_size = 32MB / 512 = 65536 bytes = 64KB
(defconstant +gen-card-table-size+ (/ (* 2 +gen-old-space-half+) +gen-card-size+))

;;; ============================================================
;;; Write Barrier Generation
;;; ============================================================

(defun gen-write-barrier (target-reg)
  "Generate write barrier for stores to heap objects.
   TARGET-REG is the register containing the target object address.
   This should be called after every heap store that may create
   an old->young pointer.

   The barrier:
   1. Checks if target is in old space (address >= nursery_end)
   2. If so, computes card index and marks card dirty

   Uses x9, x10 as scratch registers."
  (append
   ;; Get base address (clear tag bits)
   (arm64:and* :x9 target-reg -16 :imm t)  ; x9 = base address

   ;; Load nursery_end (old space starts here)
   (arm64:ldr :x10 :gc :offset +gen-nursery-end-offset+)  ; x10 = nursery_end

   ;; Check if target < nursery_end (in nursery, no barrier needed)
   (arm64:cmp :x9 :x10)
   (arm64:b.lo 7)  ; skip barrier if in nursery

   ;; Target is in old space - mark card dirty
   ;; card_index = (addr - old_space_start) >> 9
   (arm64:sub :x9 :x9 :x10)                   ; x9 = addr - old_space_start
   (arm64:lsr :x9 :x9 +gen-card-shift+ :imm t)  ; x9 = card index

   ;; card_addr = card_table + card_index
   (arm64:ldr :x10 :gc :offset +gen-card-table-offset+)  ; x10 = card_table
   (arm64:add :x9 :x9 :x10)                   ; x9 = card address

   ;; Mark card dirty (store 1)
   (arm64:movz :x10 1)
   (arm64:strb :x10 :x9 0)))                ; card[index] = 1

;;; ============================================================
;;; Allocation Fast Path
;;; ============================================================

(defun gen-alloc-check ()
  "Generate nursery overflow check. Call after bumping x28.
   If x28 >= nursery_end, triggers minor GC.
   Uses x8 as scratch (reserved for runtime, not allocatable)."
  (append
   (arm64:ldr :x8 :gc :offset +gen-nursery-end-offset+)  ; x8 = nursery_end
   (arm64:cmp :heap :x8)                                   ; compare x28, nursery_end
   (arm64:b.lo 2)                                     ; skip if x8 < nursery_end
   (list '(:call-fn GEN-MINOR-GC))))                  ; bl minor_gc

;;; ============================================================
;;; Minor GC (Nursery Collection)
;;; ============================================================

(defun gen-minor-gc-asm ()
  "Generate minor GC function.
   Copies live nursery objects to old space.
   Roots: registers + dirty cards in old space.

   Algorithm:
   1. Save register roots
   2. Scan register roots, copy reachable nursery objects to old space
   3. Scan dirty cards, copy reachable nursery objects
   4. Cheney scan the promoted objects
   5. Reset nursery (x28 = nursery_start)
   6. Clear card table
   7. Restore registers"
  (append
   (list '(:fn-label GEN-MINOR-GC))

   ;; Prologue: save registers
   (arm64:sub :sp :sp 176 :imm t)
   (arm64:stp :lr :fp :sp :offset 0)
   (arm64:stp :x0 :x1 :sp :offset 16)
   (arm64:stp :x2 :x3 :sp :offset 32)
   (arm64:stp :x4 :x5 :sp :offset 48)
   (arm64:stp :x6 :x7 :sp :offset 64)
   (arm64:stp :x8 :x9 :sp :offset 80)
   (arm64:stp :x10 :x11 :sp :offset 96)
   (arm64:stp :x12 :x13 :sp :offset 112)
   (arm64:stp :x14 :x15 :sp :offset 128)
   (arm64:stp :closure :x25 :sp :offset 144)
   (arm64:str :code-base :sp :offset 160)

   ;; Setup:
   ;; x18 = nursery-start
   ;; x19 = nursery-end
   ;; x16 = old-to-scan (where to scan promoted objects)
   ;; x17 = old-to-free (where to allocate in old space)

   (arm64:ldr :x18 :gc :offset +gen-nursery-start-offset+)
   (arm64:ldr :x19 :gc :offset +gen-nursery-end-offset+)

   ;; Load old space allocation pointer from global
   (arm64:ldr :x17 :gc :offset +gen-old-alloc-offset+)
   (arm64:mov :x16 :x17)                               ; to-scan = to-free

   ;; Process register roots (x0-x7, x24)
   ;; For each: if it points to nursery, copy to old space

   ;; x0
   (arm64:ldr :x0 :sp :offset 16)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 16)

   ;; x1
   (arm64:ldr :x0 :sp :offset 24)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 24)

   ;; x2
   (arm64:ldr :x0 :sp :offset 32)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 32)

   ;; x3
   (arm64:ldr :x0 :sp :offset 40)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 40)

   ;; x4
   (arm64:ldr :x0 :sp :offset 48)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 48)

   ;; x5
   (arm64:ldr :x0 :sp :offset 56)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 56)

   ;; x6
   (arm64:ldr :x0 :sp :offset 64)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 64)

   ;; x7
   (arm64:ldr :x0 :sp :offset 72)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 72)

   ;; x24
   (arm64:ldr :x0 :sp :offset 144)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :sp :offset 144)

   ;; ===== Scan dirty cards =====
   ;; x20 = card table pointer (iterate through)
   ;; x21 = card table end
   ;; x22 = old-space-start (for computing card addresses)
   ;; x23 = scratch for card scanning
   (arm64:ldr :env :gc :offset +gen-card-table-offset+)   ; x20 = card-table-start
   (arm64:ldr :x22 :gc :offset +gen-nursery-end-offset+)  ; x22 = old-space-start
   ;; card-table-end = card-table-start + card-table-size
   ;; card-table-size is constant: +gen-card-table-size+
   (arm64:movz :x21 (logand +gen-card-table-size+ #xFFFF))
   (arm64:add :x21 :env :x21)                                 ; x21 = card-table-end

   ;; Card scan loop
   (list '(:label GEN-CARD-SCAN))
   (arm64:cmp :env :x21)
   (arm64:b.hs 22)                          ; done with cards

   ;; Load card byte
   (arm64:ldrb :x23 :env 0)                     ; x23 = card[i]
   (arm64:cbz :x23 18)                        ; if clean (0), skip to next card

   ;; Card is dirty - scan all words in this card's range
   ;; card-addr = old-space-start + (card-index * 512)
   ;; card-index = current-card-ptr - card-table-start
   (arm64:sub :x8 :env :gc)                      ; temp calculation
   (arm64:ldr :x8 :gc :offset +gen-card-table-offset+)
   (arm64:sub :x8 :env :x8)                       ; x8 = card-index
   (arm64:lsl :x8 :x8 +gen-card-shift+ :imm t)  ; x8 = card-index * 512
   (arm64:add :x8 :x22 :x8)                       ; x8 = start of card region
   (arm64:add :x9 :x8 +gen-card-size+ :imm t)   ; x9 = end of card region

   ;; Scan words in card
   (list '(:label GEN-CARD-WORD-SCAN))
   (arm64:cmp :x8 :x9)
   (arm64:b.hs 6)                           ; done with this card

   (arm64:ldr :x0 :x8 :offset 0)                ; x0 = word at current position
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :x8 :offset 0)                ; store updated pointer
   (arm64:add :x8 :x8 8 :imm t)                 ; next word
   (arm64:b -6)                             ; back to word scan

   ;; Next card
   (arm64:add :env :env 1 :imm t)               ; next card
   (arm64:b -23)                            ; back to card scan

   ;; ===== Cheney scan loop for promoted objects =====
   (list '(:label GEN-MINOR-SCAN))
   (arm64:cmp :x16 :x17)
   (arm64:b.hs 8)                           ; if to-scan >= to-free, done

   (arm64:ldr :x0 :x16 :offset 0)
   (list '(:call-fn GEN-COPY-IF-NURSERY))
   (arm64:str :x0 :x16 :offset 0)
   (arm64:add :x16 :x16 8 :imm t)
   (arm64:b -7)                             ; back to scan loop

   ;; ===== Save old-space-alloc =====
   (arm64:str :x17 :gc :offset +gen-old-alloc-offset+)

   ;; ===== Reset nursery =====
   (arm64:ldr :heap :gc :offset +gen-nursery-start-offset+)

   ;; ===== Clear card table =====
   ;; Zero all bytes from card-table-start to card-table-end
   (arm64:ldr :env :gc :offset +gen-card-table-offset+)
   (arm64:movz :x21 (logand +gen-card-table-size+ #xFFFF))
   (arm64:add :x21 :env :x21)                     ; x21 = end
   (arm64:movz :x22 0)                        ; x22 = 0 (clear value)
   (list '(:label GEN-CLEAR-CARDS))
   (arm64:cmp :env :x21)
   (arm64:b.hs 4)
   (arm64:strb :x22 :env 0)                     ; card[i] = 0
   (arm64:add :env :env 1 :imm t)
   (arm64:b -4)

   ;; ===== Check if old space needs major GC =====
   ;; If old-space-alloc > old-space-threshold, trigger major GC
   ;; threshold = old-space-start + old-half-size (80% would be better but keep simple)
   (arm64:ldr :x8 :gc :offset +gen-nursery-end-offset+)   ; old-space-start
   (arm64:ldr :x9 :gc :offset +gen-old-half-size-offset+) ; half-size
   (arm64:add :x8 :x8 :x9)                         ; threshold = start + half
   (arm64:ldr :x10 :gc :offset +gen-old-alloc-offset+)
   (arm64:cmp :x10 :x8)
   (arm64:b.lo 2)                            ; skip if alloc < threshold
   (list '(:call-fn GEN-MAJOR-GC))

   ;; Epilogue: restore registers
   (arm64:ldp :lr :fp :sp :offset 0)
   (arm64:ldp :x0 :x1 :sp :offset 16)
   (arm64:ldp :x2 :x3 :sp :offset 32)
   (arm64:ldp :x4 :x5 :sp :offset 48)
   (arm64:ldp :x6 :x7 :sp :offset 64)
   (arm64:ldp :x8 :x9 :sp :offset 80)
   (arm64:ldp :x10 :x11 :sp :offset 96)
   (arm64:ldp :x12 :x13 :sp :offset 112)
   (arm64:ldp :x14 :x15 :sp :offset 128)
   (arm64:ldp :closure :x25 :sp :offset 144)
   (arm64:ldr :code-base :sp :offset 160)
   (arm64:add :sp :sp 176 :imm t)

   (arm64:ret)))

;;; ============================================================
;;; Copy Object if in Nursery
;;; ============================================================

(defun gen-copy-if-nursery-asm ()
  "Generate gen-copy-if-nursery function.
   Input: x0 = tagged pointer
   Output: x0 = new tagged pointer (or unchanged if not in nursery)
   Uses: x1-x5
   Assumes: x17 = to-free in old space, x18 = nursery-start, x19 = nursery-end"
  (append
   (list '(:fn-label GEN-COPY-IF-NURSERY))

   ;; Check if immediate (fixnum tag 0 or nil tag 6)
   (arm64:and* 1 0 +gc-tag-mask+ :imm t)
   (arm64:cbz :x1 14)                     ; if tag=0, return unchanged
   (arm64:cmp :x1 6 :imm t)
   (arm64:b.eq 12)                      ; if tag=6 (nil), return unchanged

   ;; Get base address
   (arm64:and* 2 0 -16 :imm t)         ; x2 = base

   ;; Check if in nursery: nursery_start <= base < nursery_end
   (arm64:cmp :x2 :x18)
   (arm64:b.lo 9)                       ; if base < nursery_start, return unchanged
   (arm64:cmp :x2 :x19)
   (arm64:b.hs 7)                       ; if base >= nursery_end, return unchanged

   ;; Check if already forwarded
   (arm64:ldr :x3 :x2 :offset 0)           ; x3 = first word
   (arm64:and* 4 3 +gc-tag-mask+ :imm t)
   (arm64:cmp :x4 +gc-tag-forward+ :imm t)
   (arm64:b.ne 4)                       ; if not forwarded, copy

   ;; Already forwarded
   (arm64:and* 0 3 -16 :imm t)
   (arm64:orr :x0 :x0 :x1)
   (arm64:ret)

   ;; Copy object (same logic as gc_copy but to old space)
   ;; Save tag and base
   (arm64:mov :x5 :x1)                      ; x5 = original tag
   (arm64:mov :x4 :x2)                      ; x4 = original base

   ;; Calculate size based on tag in x5
   (arm64:cmp :x5 1 :imm t)              ; cons?
   (arm64:b.ne 3)
   (arm64:movz :x1 16)
   (arm64:b 20)

   (arm64:cmp :x5 2 :imm t)              ; symbol?
   (arm64:b.ne 3)
   (arm64:movz :x1 8)
   (arm64:b 16)

   (arm64:cmp :x5 5 :imm t)              ; closure?
   (arm64:b.ne 3)
   (arm64:movz :x1 16)
   (arm64:b 12)

   (arm64:cmp :x5 3 :imm t)              ; vector?
   (arm64:b.ne 6)
   (arm64:ldr :x1 :x4 :offset 0)
   (arm64:lsl :x1 :x1 3 :imm t)
   (arm64:add :x1 :x1 8 :imm t)
   (arm64:b 5)

   ;; string
   (arm64:ldr :x1 :x4 :offset 0)
   (arm64:add :x1 :x1 23 :imm t)
   (arm64:and* 1 1 -16 :imm t)

   ;; Copy bytes from nursery to old space
   (arm64:mov :x2 :x17)                     ; x2 = to_free (new location in old space)
   (arm64:mov :x3 :x1)                      ; x3 = remaining bytes

   ;; Copy loop
   (arm64:cbz :x3 5)
   (arm64:ldr :x0 :x4 :offset 0)
   (arm64:str :x0 :x17 :offset 0)
   (arm64:add :x4 :x4 8 :imm t)
   (arm64:add :x17 :x17 8 :imm t)
   (arm64:sub :x3 :x3 8 :imm t)
   (arm64:b -6)

   ;; Install forwarding pointer
   (arm64:sub :x4 :x4 :x1)
   (arm64:movz :x6 +gc-tag-forward+)
   (arm64:orr :x0 :x2 :x6)
   (arm64:str :x0 :x4 :offset 0)

   ;; Return new address with original tag
   (arm64:orr :x0 :x2 :x5)
   (arm64:ret)))

;;; ============================================================
;;; Major GC (Full Collection)
;;; ============================================================

(defun gen-major-gc-asm ()
  "Generate major GC function.
   Collects both nursery and old space using copying GC.
   Called when old space fills up during minor GC.

   Algorithm:
   1. Flip old space semispaces
   2. Copy all live objects from both nursery and old from-space to old to-space
   3. Reset nursery
   4. Clear card table

   This treats nursery and old from-space as one logical from-space."
  (append
   (list '(:fn-label GEN-MAJOR-GC))

   ;; Prologue: save registers (if not already saved by minor GC caller)
   ;; Note: Major GC is called from minor GC which already saved registers
   ;; But we need to save x30 for our own call returns
   (arm64:sub :sp :sp 16 :imm t)
   (arm64:str :lr :sp :offset 0)

   ;; ===== Flip old space semispaces =====
   ;; x18 = old from-space start (current old-space-start + flag)
   ;; x19 = old from-space end
   ;; x16 = to-scan pointer
   ;; x17 = to-free pointer (in new to-space)

   ;; Load old-space-flag and old-half-size
   (arm64:ldr :x8 :gc :offset +gen-old-space-flag-offset+)   ; old flag
   (arm64:ldr :x9 :gc :offset +gen-old-half-size-offset+)    ; half size

   ;; Compute new flag = half - old_flag (flip between 0 and half)
   (arm64:sub :x10 :x9 :x8)                       ; new flag
   (arm64:str :x10 :gc :offset +gen-old-space-flag-offset+)

   ;; Compute addresses:
   ;; old-space-base = nursery-end + card-table-size
   (arm64:ldr :x11 :gc :offset +gen-nursery-end-offset+)
   (arm64:movz :x12 (logand +gen-card-table-size+ #xFFFF))
   (arm64:add :x11 :x11 :x12)                     ; x11 = old-space-base

   ;; from-space-start = old-space-base + old-flag
   (arm64:add :x18 :x11 :x8)                      ; x18 = from-start
   ;; from-space-end = from-start + half
   (arm64:add :x19 :x18 :x9)                      ; x19 = from-end

   ;; to-space-start = old-space-base + new-flag
   (arm64:add :x17 :x11 :x10)                     ; x17 = to-free
   (arm64:mov :x16 :x17)                        ; x16 = to-scan

   ;; Also need nursery bounds for copying nursery objects
   (arm64:ldr :env :gc :offset +gen-nursery-start-offset+)  ; nursery-start
   (arm64:ldr :x21 :gc :offset +gen-nursery-end-offset+)    ; nursery-end

   ;; ===== Copy roots =====
   ;; The register roots were already processed by minor GC
   ;; But we need to re-process them with the full copying function

   ;; Copy intern_table
   (arm64:ldr :x0 :gc :offset +gc-intern-table-offset+)
   (list '(:call-fn GEN-COPY-FULL))
   (arm64:str :x0 :gc :offset +gc-intern-table-offset+)

   ;; ===== Cheney scan loop =====
   (list '(:label GEN-MAJOR-SCAN))
   (arm64:cmp :x16 :x17)
   (arm64:b.hs 8)                           ; if to-scan >= to-free, done

   (arm64:ldr :x0 :x16 :offset 0)
   (list '(:call-fn GEN-COPY-FULL))
   (arm64:str :x0 :x16 :offset 0)
   (arm64:add :x16 :x16 8 :imm t)
   (arm64:b -7)

   ;; ===== Update old-space-alloc =====
   (arm64:str :x17 :gc :offset +gen-old-alloc-offset+)

   ;; ===== Reset nursery =====
   (arm64:ldr :heap :gc :offset +gen-nursery-start-offset+)

   ;; ===== Clear card table =====
   (arm64:ldr :x22 :gc :offset +gen-card-table-offset+)
   (arm64:movz :x23 (logand +gen-card-table-size+ #xFFFF))
   (arm64:add :x23 :x22 :x23)
   (arm64:movz :x24 0)
   (list '(:label GEN-MAJOR-CLEAR-CARDS))
   (arm64:cmp :x22 :x23)
   (arm64:b.hs 4)
   (arm64:strb :x24 :x22 0)
   (arm64:add :x22 :x22 1 :imm t)
   (arm64:b -4)

   ;; Epilogue
   (arm64:ldr :lr :sp :offset 0)
   (arm64:add :sp :sp 16 :imm t)
   (arm64:ret)))

;;; ============================================================
;;; Copy Object from Nursery or Old From-Space
;;; ============================================================

(defun gen-copy-full-asm ()
  "Generate gen-copy-full function for major GC.
   Copies objects from either nursery or old from-space to old to-space.
   Input: x0 = tagged pointer
   Output: x0 = new tagged pointer
   Uses: x1-x7
   Assumes: x17 = to-free, x18 = old from-start, x19 = old from-end,
            x20 = nursery-start, x21 = nursery-end"
  (append
   (list '(:fn-label GEN-COPY-FULL))

   ;; Check if immediate
   (arm64:and* 1 0 +gc-tag-mask+ :imm t)
   (arm64:cbz :x1 24)                         ; fixnum -> return
   (arm64:cmp :x1 6 :imm t)
   (arm64:b.eq 22)                          ; nil -> return

   ;; Get base address
   (arm64:and* 2 0 -16 :imm t)             ; x2 = base

   ;; Check if in nursery
   (arm64:cmp :x2 :env)
   (arm64:b.lo 6)                           ; below nursery -> check old space
   (arm64:cmp :x2 :x21)
   (arm64:b.hs 4)                           ; above nursery -> check old space
   (arm64:b 8)                              ; in nursery -> copy

   ;; Check if in old from-space
   (arm64:cmp :x2 :x18)
   (arm64:b.lo 14)                          ; below from-space -> return unchanged
   (arm64:cmp :x2 :x19)
   (arm64:b.hs 12)                          ; above from-space -> return unchanged

   ;; Object is in nursery or old from-space -> copy it
   ;; Check if already forwarded
   (arm64:ldr :x3 :x2 :offset 0)
   (arm64:and* 4 3 +gc-tag-mask+ :imm t)
   (arm64:cmp :x4 +gc-tag-forward+ :imm t)
   (arm64:b.ne 4)

   ;; Already forwarded
   (arm64:and* 0 3 -16 :imm t)
   (arm64:orr :x0 :x0 :x1)
   (arm64:ret)

   ;; Copy object - same logic as gen-copy-if-nursery but to old to-space
   (arm64:mov :x5 :x1)                          ; save tag
   (arm64:mov :x4 :x2)                          ; save base

   ;; Calculate size
   (arm64:cmp :x5 1 :imm t)
   (arm64:b.ne 3)
   (arm64:movz :x1 16)
   (arm64:b 20)

   (arm64:cmp :x5 2 :imm t)
   (arm64:b.ne 3)
   (arm64:movz :x1 8)
   (arm64:b 16)

   (arm64:cmp :x5 5 :imm t)
   (arm64:b.ne 3)
   (arm64:movz :x1 16)
   (arm64:b 12)

   (arm64:cmp :x5 3 :imm t)
   (arm64:b.ne 6)
   (arm64:ldr :x1 :x4 :offset 0)
   (arm64:lsl :x1 :x1 3 :imm t)
   (arm64:add :x1 :x1 8 :imm t)
   (arm64:b 5)

   (arm64:ldr :x1 :x4 :offset 0)
   (arm64:add :x1 :x1 23 :imm t)
   (arm64:and* 1 1 -16 :imm t)

   ;; Copy bytes
   (arm64:mov :x2 :x17)
   (arm64:mov :x3 :x1)

   (arm64:cbz :x3 5)
   (arm64:ldr :x6 :x4 :offset 0)
   (arm64:str :x6 :x17 :offset 0)
   (arm64:add :x4 :x4 8 :imm t)
   (arm64:add :x17 :x17 8 :imm t)
   (arm64:sub :x3 :x3 8 :imm t)
   (arm64:b -6)

   ;; Install forwarding pointer
   (arm64:sub :x4 :x4 :x1)
   (arm64:movz :x6 +gc-tag-forward+)
   (arm64:orr :x7 :x2 :x6)
   (arm64:str :x7 :x4 :offset 0)

   ;; Return new address with original tag
   (arm64:orr :x0 :x2 :x5)
   (arm64:ret)))

;;; ============================================================
;;; Heap Initialization for Generational GC
;;; ============================================================

(defun gen-heap-init-code (heap-page-offset)
  "Generate heap initialization code for generational GC.
   Sets up nursery, old space, and card table."
  (let* ((nursery-high (ash +gen-nursery-size+ -16))
         (nursery-low (logand +gen-nursery-size+ #xFFFF))
         (old-half-high (ash +gen-old-space-half+ -16))
         (old-half-low (logand +gen-old-space-half+ #xFFFF)))
    (append
     ;; Setup heap base via ADRP
     (arm64:adrp :gc heap-page-offset)

     ;; Initialize existing GC globals (intern_table, lambda_counter, etc.)
     (arm64:movz :x9 6)                   ; nil
     (arm64:str :x9 :gc :offset +gc-intern-table-offset+)
     (arm64:movz :x9 0)
     (arm64:str :x9 :gc :offset +gc-lambda-counter-offset+)

     ;; Compute nursery_start = x27 + 128
     (arm64:add :x10 :gc +gen-heap-data-offset+ :imm t)
     (arm64:str :x10 :gc :offset +gen-nursery-start-offset+)

     ;; Compute nursery_end = nursery_start + nursery_size
     (arm64:movz :x11 nursery-low)
     (if (> nursery-high 0)
         (arm64:movk :x11 nursery-high :lsl 16)
         (list (arm64:nop)))
     (arm64:add :x12 :x10 :x11)               ; x12 = nursery_end
     (arm64:str :x12 :gc :offset +gen-nursery-end-offset+)

     ;; Card table starts after nursery_end
     ;; Card table size = old_space_size / 512 = 2 * old_half / 512
     (arm64:str :x12 :gc :offset +gen-card-table-offset+)

     ;; Store old space half size
     (arm64:movz :x13 old-half-low)
     (if (> old-half-high 0)
         (arm64:movk :x13 old-half-high :lsl 16)
         (list (arm64:nop)))
     (arm64:str :x13 :gc :offset +gen-old-half-size-offset+)

     ;; old-space-flag = 0
     (arm64:str :x9 :gc :offset +gen-old-space-flag-offset+)  ; x9 still 0

     ;; Compute old-space-start and initialize old-space-alloc
     ;; old-space-start = nursery-end + card-table-size
     ;; Card table is placed between nursery and old space
     (arm64:movz :x14 (logand +gen-card-table-size+ #xFFFF))
     (arm64:add :x14 :x12 :x14)                   ; x14 = old-space-start
     (arm64:str :x14 :gc :offset +gen-old-alloc-offset+)

     ;; Set x28 = nursery-start (allocation pointer)
     (arm64:mov :heap :x10))))

;;; ============================================================
;;; Runtime Code Assembly
;;; ============================================================

(defun gen-gc-runtime-code ()
  "Generate complete generational GC runtime code.
   Returns list of ARM64 instruction bytes with function markers."
  (append
   (gen-copy-if-nursery-asm)
   (gen-copy-full-asm)
   (gen-minor-gc-asm)
   (gen-major-gc-asm)))

;;; ============================================================
;;; Integration Notes
;;; ============================================================

;;; To integrate generational GC:
;;;
;;; 1. Replace gc-trigger-check with gen-alloc-check in all allocation sites
;;;
;;; 2. Add write barrier after pointer stores:
;;;    - cons car/cdr updates
;;;    - vector element updates
;;;    - closure environment updates
;;;    - setq to heap locations
;;;
;;; 3. Update macho.lisp to:
;;;    - Use gen-heap-init-code instead of gc-heap-init-code
;;;    - Include gen-gc-runtime-code in the binary
;;;    - Allocate space for card table
;;;
;;; 4. Modify codegen to:
;;;    - Use gen-alloc-check after allocations
;;;    - Insert write barriers after pointer stores
;;;
;;; The write barrier is critical for correctness. Without it, nursery objects
;;; that are only referenced from old space would be collected prematurely.
;;;
;;; Performance considerations:
;;; - Nursery size affects minor GC frequency vs. promotion rate
;;; - Card table granularity affects barrier overhead vs. false sharing
;;; - Current choice: 1MB nursery, 512-byte cards

