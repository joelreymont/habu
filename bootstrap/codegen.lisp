;;; Pure ARM64 Codegen - Uses ONLY Habu primitives (no SBCL dependencies)
;;; No defparameter, no dotimes, no dolist, no loop
;;; This can be compiled to native and run without SBCL

#+sbcl (in-package :habu)

;;; SBCL-compatible setcar (native Habu has setcar as a primitive)
#+sbcl
(defun setcar (cell value)
  "Set the CAR of CELL to VALUE, return VALUE"
  (setf (car cell) value))

;;; ============================================================
;;; State Management using Cons Cells (not defparameter)
;;; ============================================================

;; Symbol table state for codegen symbol interning.
;; SBCL mode: uses *symbol-state* defvar as ((counter . table) . nil)
;; Native mode: uses get-symbol-counter/set-symbol-counter at [x27+48]
;;              and get-symbol-table-sym/set-symbol-table-sym at [x27+56]

#+sbcl (defvar *symbol-state* (cons (cons 1 nil) nil))

(defun reset-symbol-table ()
  "Reset symbol table state.
   In SBCL: resets *symbol-state* cons cell.
   In native: resets [x27+48] = 16 (tagged 1), [x27+56] = nil."
  #+sbcl (progn
           (setf (car (car *symbol-state*)) 1)
           (setf (cdr (car *symbol-state*)) nil))
  #-sbcl (progn
           (set-symbol-counter 1)  ; Start at 1
           (set-symbol-table-sym nil)))

#-sbcl
(defun intern-symbol (name)
  "Get or create a symbol ID for NAME.
   Native mode: uses [x27+48] for counter, [x27+56] for table."
  (let* ((counter (get-symbol-counter))
         (table (get-symbol-table-sym)))
    (labels ((find-in-table (lst)
               (if (null lst)
                   nil
                   (if (string-equal name (car (car lst)))
                       (cdr (car lst))
                       (find-in-table (cdr lst))))))
      (let ((existing (find-in-table table)))
        (if existing
            existing
            (let ((id counter))
              (set-symbol-counter (+ counter 1))
              (set-symbol-table-sym (cons (cons name id) table))
              id))))))

;;; ============================================================
;;; Lambda Counter State (for lambda lifting)
;;; ============================================================

#+sbcl (defvar *lambda-state* (cons 0 nil))

(defun reset-lambda-counter ()
  "Reset lambda counter.
   In SBCL: uses *lambda-state* cons cell.
   In native: uses get-lambda-counter/set-lambda-counter primitives (stores at [x27+8])."
  #+sbcl (setf (car *lambda-state*) 0)
  #-sbcl (set-lambda-counter 0))

#-sbcl
(defun gensym-lambda ()
  "Generate unique lambda name as an interned symbol like LAMBDA-1, LAMBDA-2, etc."
  #+sbcl
  (let* ((state *lambda-state*)
         (counter (car state))
         (new-count (+ counter 1)))
    (setf (car state) new-count)
    (intern (format nil "LAMBDA-~D" new-count) :habu))
  #-sbcl
  (let* ((counter (get-lambda-counter))
         (new-count (+ counter 1)))
    (set-lambda-counter new-count)
    (labels ((digits (n acc)
               (if (= n 0)
                   (if (null acc) (cons 48 nil) acc)
                   (digits (/ n 10)
                           (cons (+ 48 (mod n 10)) acc))))
             (chars-to-vec (cs)
               (let ((len (length cs)))
                 (labels ((build (i cs vec)
                            (if (null cs)
                                vec
                                (progn
                                  (vector-set vec i (car cs))
                                  (build (+ i 1) (cdr cs) vec)))))
                   (build 0 cs (make-vector len))))))
      (let* ((num-chars (digits new-count nil))
             (prefix (list 76 65 77 66 68 65 45))
             (all-chars (append prefix num-chars)))
        (make-string-from-vector (chars-to-vec all-chars))))))

;;; ============================================================
;;; Lambda Lifting (extract lambdas, replace with references)
;;; ============================================================

;; SBCL wrappers: adapt 1-arg lift-lambdas to 2-arg interface used by deliver
#+sbcl
(defun lift-lambdas-2 (ir lambdas)
  "2-arg wrapper for SBCL lift-lambdas (which is 1-arg).
   Calls SBCL lift-lambdas and prepends existing lambdas to result."
  (let* ((result (lift-lambdas ir))
         (new-ir (car result))
         (new-lambdas (cdr result)))
    (cons new-ir (append new-lambdas lambdas))))

#+sbcl
(defun lift-lambdas-from-defuns (defuns acc-defuns acc-lambdas)
  "Lift lambdas from defun bodies (SBCL version).
   Defun format from compile-forms: (name params body param-base).
   Uses SBCL lift-lambdas (1-arg) internally."
  (if (null defuns)
      (cons (reverse acc-defuns) acc-lambdas)
      (let* ((defun (car defuns))
             (name (car defun))
             (params (cadr defun))
             (body (caddr defun))
             (param-base (cadddr defun))
             (body-result (lift-lambdas body))
             (new-body (car body-result))
             (new-lambdas (cdr body-result))
             (new-defun (list name params new-body param-base)))
        ;; Use nconc instead of append - O(1) amortized instead of O(n)
        (lift-lambdas-from-defuns (cdr defuns)
                                  (cons new-defun acc-defuns)
                                  (nconc acc-lambdas new-lambdas)))))

#+sbcl
(defun lambdas-to-defuns (lambdas acc)
  "Convert lifted lambda entries to defun format (SBCL version).
   CONTRACT: Lambda entry is ALWAYS (name params body free-vars free-offsets).
   The (name . lambda-ir) format was a bug - lift-lambdas must always produce
   the canonical 5-element list format.
   Defun format: (name params body param-base)"
  (if (null lambdas)
      (reverse acc)
      (let* ((entry (car lambdas))
             (name (car entry))
             (params (cadr entry))
             (body (caddr entry))
             (free-vars (cadddr entry))
             ;; Validate format - crash on unexpected input
             (_ (unless (and (consp entry)
                             (= (length entry) 5))
                  (error "lambdas-to-defuns: invalid lambda-entry ~S~%Expected (name params body free-vars free-offsets)" entry)))
             (param-base (length free-vars))
             (defun-entry (list name params body param-base)))
        (lambdas-to-defuns (cdr lambdas) (cons defun-entry acc)))))

#-sbcl
(defun lift-lambdas (ir lambdas)
  "Extract lambda-ir nodes from IR, replacing with lambda-ref.
   Returns (cons transformed-ir lambdas) where lambdas is alist of (name params body free-vars free-offsets)"
  (cond
    ((null ir) (cons ir lambdas))
    ((not (consp ir)) (cons ir lambdas))

    ;; Found a lambda - extract it
    ((has-tag ir 'lambda-ir)
     (let* ((name (gensym-lambda))
            (params (cadr ir))
            (body (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth 4 ir)))
       ;; Recursively lift from body
       (let* ((body-result (lift-lambdas body lambdas))
              (new-body (car body-result))
              (more-lambdas (cdr body-result))
              (lambda-entry (list name params new-body free-vars free-offsets)))
         (cons (list 'lambda-ref name free-offsets)
               (cons lambda-entry more-lambdas)))))

    ;; let-ir: (let-ir vals body count offs)
    ((has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (body (caddr ir))
            (count (cadddr ir))
            (offs (nth 4 ir))
            (vals-result (lift-list vals lambdas))
            (new-vals (car vals-result))
            (l1 (cdr vals-result))
            (body-result (lift-lambdas body l1))
            (new-body (car body-result))
            (l2 (cdr body-result)))
       (cons (list 'let-ir new-vals new-body count offs) l2)))

    ;; if-ir
    ((has-tag ir 'if-ir)
     (let* ((test (cadr ir))
            (then (caddr ir))
            (else (cadddr ir))
            (test-result (lift-lambdas test lambdas))
            (new-test (car test-result))
            (l1 (cdr test-result))
            (then-result (lift-lambdas then l1))
            (new-then (car then-result))
            (l2 (cdr then-result))
            (else-result (lift-lambdas else l2))
            (new-else (car else-result))
            (l3 (cdr else-result)))
       (cons (list 'if-ir new-test new-then new-else) l3)))

    ;; while-ir
    ((has-tag ir 'while-ir)
     (let* ((test (cadr ir))
            (body (caddr ir))
            (test-result (lift-lambdas test lambdas))
            (new-test (car test-result))
            (l1 (cdr test-result))
            (body-result (lift-lambdas body l1))
            (new-body (car body-result))
            (l2 (cdr body-result)))
       (cons (list 'while-ir new-test new-body) l2)))

    ;; dolist-ir: (dolist-ir var list body)
    ((has-tag ir 'dolist-ir)
     (let* ((var (cadr ir))
            (list-expr (caddr ir))
            (body (cadddr ir))
            (list-result (lift-lambdas list-expr lambdas))
            (new-list (car list-result))
            (l1 (cdr list-result))
            (body-result (lift-lambdas body l1))
            (new-body (car body-result))
            (l2 (cdr body-result)))
       (cons (list 'dolist-ir var new-list new-body) l2)))

    ;; dotimes-ir: (dotimes-ir var count body)
    ((has-tag ir 'dotimes-ir)
     (let* ((var (cadr ir))
            (count-expr (caddr ir))
            (body (cadddr ir))
            (count-result (lift-lambdas count-expr lambdas))
            (new-count (car count-result))
            (l1 (cdr count-result))
            (body-result (lift-lambdas body l1))
            (new-body (car body-result))
            (l2 (cdr body-result)))
       (cons (list 'dotimes-ir var new-count new-body) l2)))

    ;; progn-ir
    ((has-tag ir 'progn-ir)
     (let* ((forms (cadr ir))
            (forms-result (lift-list forms lambdas))
            (new-forms (car forms-result))
            (new-lambdas (cdr forms-result)))
       (cons (list 'progn-ir new-forms) new-lambdas)))

    ;; funcall-ir
    ((has-tag ir 'funcall-ir)
     (let* ((fn-ir (cadr ir))
            (args (caddr ir))
            (fn-result (lift-lambdas fn-ir lambdas))
            (new-fn (car fn-result))
            (l1 (cdr fn-result))
            (args-result (lift-list args l1))
            (new-args (car args-result))
            (l2 (cdr args-result)))
       (cons (list 'funcall-ir new-fn new-args) l2)))

    ;; call-fn
    ((has-tag ir 'call-fn)
     (let* ((name (cadr ir))
            (args (caddr ir))
            (args-result (lift-list args lambdas))
            (new-args (car args-result))
            (new-lambdas (cdr args-result)))
       (cons (list 'call-fn name new-args) new-lambdas)))

    ;; Binary ops
    ((or (has-tag ir 'add) (has-tag ir 'sub)
         (has-tag ir 'mul) (has-tag ir 'div)
         (has-tag ir 'mod) (has-tag ir 'cmp-eq)
         (has-tag ir 'cmp-lt) (has-tag ir 'cmp-gt)
         (has-tag ir 'cons-ir)
         (has-tag ir 'setcar-ir) (has-tag ir 'setcdr-ir)
         (has-tag ir 'string-ref-ir) (has-tag ir 'string-concat-ir)
         (has-tag ir 'string-equal-ir) (has-tag ir 'vector-ref-ir)
         (has-tag ir 'buffer-byte-ref-ir)
         (has-tag ir 'set-tag)
         (has-tag ir 'nthcdr-ir))
     (let* ((left (cadr ir))
            (right (caddr ir))
            (left-result (lift-lambdas left lambdas))
            (new-left (car left-result))
            (l1 (cdr left-result))
            (right-result (lift-lambdas right l1))
            (new-right (car right-result))
            (l2 (cdr right-result)))
       (cons (list (car ir) new-left new-right) l2)))

    ;; Ternary ops (vector-set-ir, buffer-byte-set-ir, substring-ir)
    ((or (has-tag ir 'vector-set-ir)
         (has-tag ir 'buffer-byte-set-ir)
         (has-tag ir 'substring-ir))
     (let* ((arg1 (cadr ir))
            (arg2 (caddr ir))
            (arg3 (cadddr ir))
            (r1 (lift-lambdas arg1 lambdas))
            (r2 (lift-lambdas arg2 (cdr r1)))
            (r3 (lift-lambdas arg3 (cdr r2))))
       (cons (list (car ir) (car r1) (car r2) (car r3)) (cdr r3))))

    ;; Unary ops
    ((or (has-tag ir 'car-ir) (has-tag ir 'cdr-ir) (has-tag ir 'get-tag)
         (has-tag ir 'symbol-name-ir) (has-tag ir 'make-symbol-ir)
         (has-tag ir 'string-length-ir)
         (has-tag ir 'make-vector-ir) (has-tag ir 'vector-length-ir)
         (has-tag ir 'make-string-from-vector-ir)
         (has-tag ir 'set-global-vars-ir)
         (has-tag ir 'set-intern-table-ir)
         (has-tag ir 'set-keyword-table-ir)
         (has-tag ir 'consp-ir) (has-tag ir 'numberp-ir)
         (has-tag ir 'stringp-ir) (has-tag ir 'symbolp-ir)
         (has-tag ir 'vectorp-ir) (has-tag ir 'null-ir)
         (has-tag ir 'println-ir) (has-tag ir 'system-ir))
     (let* ((arg (cadr ir))
            (arg-result (lift-lambdas arg lambdas))
            (new-arg (car arg-result))
            (new-lambdas (cdr arg-result)))
       (cons (list (car ir) new-arg) new-lambdas)))

    ;; sys-exit-ir
    ((has-tag ir 'sys-exit-ir)
     (let* ((arg (cadr ir))
            (arg-result (lift-lambdas arg lambdas))
            (new-arg (car arg-result))
            (new-lambdas (cdr arg-result)))
       (cons (list 'sys-exit-ir new-arg) new-lambdas)))

    ;; setq-ir: (setq-ir offset val-ir)
    ((has-tag ir 'setq-ir)
     (let* ((off (cadr ir))
            (val-ir (caddr ir))
            (val-result (lift-lambdas val-ir lambdas))
            (new-val (car val-result))
            (new-lambdas (cdr val-result)))
       (cons (list 'setq-ir off new-val) new-lambdas)))

    ;; Default - crash on unknown IR tag
    (t (error "lift-lambdas: unhandled IR tag ~S in ~S" (car ir) ir))))

#-sbcl
(defun lift-list (lst lambdas)
  "Lift lambdas from a list of IR nodes"
  (if (null lst)
      (cons nil lambdas)
      (let* ((first-result (lift-lambdas (car lst) lambdas))
             (new-first (car first-result))
             (l1 (cdr first-result))
             (rest-result (lift-list (cdr lst) l1))
             (new-rest (car rest-result))
             (l2 (cdr rest-result)))
        (cons (cons new-first new-rest) l2))))

#-sbcl
(defun lift-lambdas-from-defuns (defuns acc-defuns acc-lambdas)
  "Lift lambdas from all defun bodies.
   Defun format: (name params body param-base)
   Must preserve param-base after lifting."
  (if (null defuns)
      (cons (reverse acc-defuns) acc-lambdas)
      (let* ((defun (car defuns))
             (name (car defun))
             (params (cadr defun))
             (body (caddr defun))
             (param-base (cadddr defun))  ;; Preserve param-base!
             (body-result (lift-lambdas body acc-lambdas))
             (new-body (car body-result))
             (more-lambdas (cdr body-result))
             (new-defun (list name params new-body param-base)))  ;; Keep 4 elements
        (lift-lambdas-from-defuns (cdr defuns)
                                        (cons new-defun acc-defuns)
                                        more-lambdas))))

#-sbcl
(defun lambdas-to-defuns (lambdas acc)
  "Convert lifted lambda entries to defun format.
   Lambda entry: (name params body free-vars free-offsets)
   Defun format: (name params body param-base)
   The param-base for lambdas is the number of captured variables,
   since params are stored after captured vars in the environment."
  (if (null lambdas)
      (reverse acc)
      (let* ((lambda-entry (car lambdas))
             (name (car lambda-entry))
             (params (cadr lambda-entry))
             (body (caddr lambda-entry))
             (free-vars (cadddr lambda-entry))
             ;; param-base = number of free vars (params come after captures)
             (param-base (length free-vars))
             (defun-entry (list name params body param-base)))
        (lambdas-to-defuns (cdr lambdas) (cons defun-entry acc)))))

;;; ============================================================
;;; ARM64 Instruction API
;;; All ARM64 encoding lives in arm64/asm.lisp. Use arm64:* directly.
;;;
;;; Examples:
;;;   (arm64:movz rd imm)             - Move with zero
;;;   (arm64:movk rd imm :lsl 16)     - Move with keep, shift by 16
;;;   (arm64:add rd rn imm :imm t)    - ADD immediate
;;;   (arm64:ldr rt rn :offset off)   - LDR with offset
;;;   (arm64:cmp rn imm :imm t)       - CMP immediate
;;;   (arm64:b.eq offset)             - Branch if equal (instruction count)
;;;   (arm64:strb rt rn offset)       - Store byte with immediate offset
;;;   (arm64:strb rt rn rm :reg t)    - Store byte with register offset
;;;   arm64:+cc-eq+                   - Condition code constants
;;; ============================================================


;;; ============================================================
;;; GC Trigger Code
;;; ============================================================

;;; GC globals offsets (must match gc.lisp)
(defconstant +gc-from-end-offset+ 16)

;;; Generational GC offsets (must match gen-gc.lisp)
(defconstant +gen-nursery-end-offset+ 88)
(defconstant +gen-card-table-offset+ 96)
(defconstant +gen-card-shift+ 9)  ; log2(512) for card size

;;; Toggle for generational GC mode
;;; When t, uses nursery allocation with write barriers
#+sbcl (defvar *use-generational-gc* nil)

;;; Register allocation is ALWAYS used - no fallback to accumulator codegen
;;; The old accumulator-based codegen had register clobbering bugs and was removed


(defun gc-trigger-code ()
  "Generate inline GC trigger check. Insert after allocations.
   Uses x8 as scratch (reserved for runtime, not allocatable).
   Emits :call-fn marker if GC needed.
   In generational mode: checks nursery-end, calls GEN-MINOR-GC.
   In simple mode: checks from-end, calls GC-COLLECT."
  #+sbcl
  (if *use-generational-gc*
      ;; Generational GC: compare against nursery-end
      (append-all
       (list (arm64:ldr :x8 :gc :offset +gen-nursery-end-offset+)  ; x8 = nursery_end
             (arm64:cmp :heap :x8)                                   ; compare x28, nursery_end
             (arm64:b.lo 2)                                     ; skip if x28 < nursery_end
             (list (list :call-fn 'GEN-MINOR-GC))))
      ;; Simple GC: compare against from-end
      (append-all
       (list (arm64:ldr :x8 :gc :offset +gc-from-end-offset+)  ; x8 = from_end
             (arm64:cmp :heap :x8)                               ; compare x28, from_end
             (arm64:b.lo 2)
             (list (list :call-fn 'GC-COLLECT)))))
  #-sbcl
  ;; Native mode: always use simple GC for now
  (append-all
   (list (arm64:ldr :x8 :gc :offset +gc-from-end-offset+)
         (arm64:cmp :heap :x8)
         (arm64:b.lo 2)
         (list (list :call-fn 'GC-COLLECT)))))

(defun gen-write-barrier-code (target-reg)
  "Generate write barrier for stores to heap objects.
   TARGET-REG is the register containing the target object address.
   Call after every heap store that may create an old->young pointer.

   The barrier:
   1. Checks if target is in old space (address >= nursery_end)
   2. If so, computes card index and marks card dirty

   Uses x16, x17 (IP0/IP1) as scratch. Only generated in generational GC mode."
  #+sbcl
  (if *use-generational-gc*
      (append-all
       (list
        ;; Clear tag bits to get base address
        (arm64:and* :x16 target-reg -16 :imm t)     ; x9 = base address
        ;; Load nursery_end (old space starts here)
        (arm64:ldr :x17 :gc :offset +gen-nursery-end-offset+)  ; x10 = nursery_end
        ;; Check if target < nursery_end (in nursery, no barrier needed)
        (arm64:cmp :x16 :x17)
        (arm64:b.lo 7)                           ; skip barrier if in nursery (7 instrs)
        ;; Target is in old space - mark card dirty
        ;; card_index = (addr - old_space_start) >> 9
        (arm64:sub :x16 :x16 :x17)                       ; x9 = addr - old_space_start
        (arm64:lsr :x16 :x16 +gen-card-shift+ :imm t)  ; x9 = card index
        ;; card_addr = card_table + card_index
        (arm64:ldr :x17 :gc :offset +gen-card-table-offset+)   ; x10 = card_table
        (arm64:add :x16 :x16 :x17)                       ; x9 = card address
        ;; Mark card dirty (store 1)
        (arm64:movz :x17 1)
        (arm64:strb :x17 :x16 0)))                    ; card[index] = 1
      ;; No barrier in simple GC mode
      nil)
  #-sbcl
  ;; Native mode: no barrier for now
  nil)

;;; ============================================================
;;; Helper Functions
;;; ============================================================

#-sbcl
(defun reverse-helper (lst acc)
  "Iterative reverse helper using while loop"
  (let ((remaining lst)
        (result acc))
    (while (not (null remaining))
      (setq result (cons (car remaining) result))
      (setq remaining (cdr remaining)))
    result))

#-sbcl
(defun reverse (lst)
  "Reverse a list using iterative while loop"
  (let ((remaining lst)
        (result nil))
    (while (not (null remaining))
      (setq result (cons (car remaining) result))
      (setq remaining (cdr remaining)))
    result))

#-sbcl
(defun append (lst1 lst2)
  "Append two lists using iterative while loop to avoid stack overflow"
  (let ((reversed nil)
        (remaining lst1)
        (result lst2))
    ;; First reverse lst1
    (while (not (null remaining))
      (setq reversed (cons (car remaining) reversed))
      (setq remaining (cdr remaining)))
    ;; Then prepend reversed elements to lst2
    (setq remaining reversed)
    (while (not (null remaining))
      (setq result (cons (car remaining) result))
      (setq remaining (cdr remaining)))
    result))

#-sbcl
(defun length (lst)
  "List length using iterative while loop"
  (let ((remaining lst)
        (n 0))
    (while (not (null remaining))
      (setq n (+ n 1))
      (setq remaining (cdr remaining)))
    n))

#-sbcl
(defun append-all (lists)
  "Append all lists using iterative while loop"
  (if (null lists)
      nil
      (let ((remaining lists)
            (result nil))
        ;; First reverse the list of lists
        (let ((reversed nil))
          (while (not (null remaining))
            (setq reversed (cons (car remaining) reversed))
            (setq remaining (cdr remaining)))
          ;; Then append each list from right to left
          (setq remaining reversed)
          (while (not (null remaining))
            (setq result (append (car remaining) result))
            (setq remaining (cdr remaining))))
        result)))

#-sbcl
(defun temp-slot (td)
  "Calculate temp slot offset for depth TD.
   Temp slots occupy 0x40-0x100 (24 slots, 192 bytes)."
  (if (>= td 24)
      (progn
        ;; Error: too many temp slots - but we can't use format in pure code
        ;; Just return a safe value within spill area
        #x100)
      (+ #x40 (* td 8))))

;; load-addr - SBCL version in compiler-sbcl.lisp uses arm64:* directly
#-sbcl
(defun load-addr (rd addr)
  "Load large address into register (up to 64 bits)"
  (if (< addr #x10000)
      (arm64:movz rd addr)
      (if (< addr #x100000000)
          (append (arm64:movz rd (logand addr #xFFFF))
                  (arm64:movk rd (ash addr -16) :lsl 16))
          (if (< addr #x1000000000000)
              ;; 48-bit address
              (append-all (list (arm64:movz rd (logand addr #xFFFF))
                                (arm64:movk rd (logand (ash addr -16) #xFFFF) :lsl 16)
                                (arm64:movk rd (logand (ash addr -32) #xFFFF) :lsl 32)))
              ;; 64-bit address (for packed string data)
              (append-all (list (arm64:movz rd (logand addr #xFFFF))
                                (arm64:movk rd (logand (ash addr -16) #xFFFF) :lsl 16)
                                (arm64:movk rd (logand (ash addr -32) #xFFFF) :lsl 32)
                                (arm64:movk rd (logand (ash addr -48) #xFFFF) :lsl 48)))))))

(defun load-addr-8 (rd addr)
  "Load address into register, always producing 8 bytes (2 instructions).
   Used for lambda/function references where consistent code size is needed."
  (append (arm64:movz rd (logand addr #xFFFF))
          (arm64:movk rd (ash addr -16) :lsl 16)))

(defun gen-string-lit (str len total-size)
  "Generate code to allocate string literal on heap.
   String layout: [length:8][data:N]
   Returns tagged string pointer in x0, bumps x28.
   IMPORTANT: GC trigger checked BEFORE allocation to prevent writing to unmapped memory."
  (labels
      ;; Store up to 8 bytes at a time using MOVZ/MOVK + STR
      ((gen-store-bytes (offset bytes acc)
         (if (null bytes)
             acc
             (let* ((chunk (take-bytes bytes 8))
                    (val (bytes-to-u64 chunk))
                    (rest (drop-bytes bytes 8)))
               (gen-store-bytes
                (+ offset 8)
                rest
                (append-all
                 (list acc
                       (load-addr :x16 val)
                       (arm64:str :x16 :heap :offset offset)))))))
       ;; Convert string to list of bytes
       (str-to-bytes (s i acc)
         (if (>= i (string-length s))
             (reverse acc)
             (str-to-bytes s (+ i 1) (cons (string-ref s i) acc)))))
    (let* ((bytes (str-to-bytes str 0 nil))
           ;; Add null terminator for C string compatibility
           (bytes-with-nul (append bytes (list 0)))
           ;; Check GC BEFORE allocation to ensure heap is valid
           (pre-check (gc-trigger-code))
           ;; Store length first, then data starting at offset 8
           (len-code (append-all
                      (list (load-addr :x16 len)
                            (arm64:str :x16 :heap :offset 0))))
           (data-code (gen-store-bytes 8 bytes-with-nul nil))
           ;; Return tagged pointer and bump heap
           ;; Note: NO post-allocation GC check - x0 is unrooted until caller saves it
           (result-code (append-all
                         (list (arm64:mov :x0 :heap)
                               (arm64:add :x0 :x0 #.+tag-string+ :imm t)
                               (arm64:add :heap :heap total-size :imm t)))))
      (append-all (list pre-check len-code data-code result-code)))))

(defun gen-symbol-lit (str len total-size)
  "Generate code to allocate symbol literal on heap.
   Symbol layout: same as string [length:8][name:N]
   Returns tagged symbol pointer (tag 2) in x0, bumps x28."
  (labels
      ;; Store up to 8 bytes at a time using MOVZ/MOVK + STR
      ((gen-store-bytes (offset bytes acc)
         (if (null bytes)
             acc
             (let* ((chunk (take-bytes bytes 8))
                    (val (bytes-to-u64 chunk))
                    (rest (drop-bytes bytes 8)))
               (gen-store-bytes
                (+ offset 8)
                rest
                (append-all
                 (list acc
                       (load-addr :x16 val)
                       (arm64:str :x16 :heap :offset offset)))))))
       ;; Convert string to list of bytes
       (str-to-bytes (s i acc)
         (if (>= i (string-length s))
             (reverse acc)
             (str-to-bytes s (+ i 1) (cons (string-ref s i) acc)))))
    (let* ((bytes (str-to-bytes str 0 nil))
           ;; Add null terminator for C string compatibility
           (bytes-with-nul (append bytes (list 0)))
           ;; Check GC BEFORE allocation to ensure heap is valid
           (pre-check (gc-trigger-code))
           ;; Store length first, then data starting at offset 8
           (len-code (append-all
                      (list (load-addr :x16 len)
                            (arm64:str :x16 :heap :offset 0))))
           (data-code (gen-store-bytes 8 bytes-with-nul nil))
           ;; Return tagged pointer with symbol tag and bump heap
           ;; Note: NO post-allocation GC check - x0 is unrooted until caller saves it
           (result-code (append-all
                         (list (arm64:mov :x0 :heap)
                               (arm64:add :x0 :x0 #.+tag-symbol+ :imm t)
                               (arm64:add :heap :heap total-size :imm t)))))
      (append-all (list pre-check len-code data-code result-code)))))

(defun take-bytes (bytes n)
  "Take up to N bytes from list"
  (if (or (null bytes) (<= n 0))
      nil
      (cons (car bytes) (take-bytes (cdr bytes) (- n 1)))))

(defun drop-bytes (bytes n)
  "Drop N bytes from list"
  (if (or (null bytes) (<= n 0))
      bytes
      (drop-bytes (cdr bytes) (- n 1))))

(defun bytes-to-u64 (bytes)
  "Convert list of up to 8 bytes to u64 (little-endian)"
  (labels ((to-u64 (bs shift acc)
             (if (null bs)
                 acc
                 (to-u64 (cdr bs) (+ shift 8)
                         (logior acc (ash (car bs) shift))))))
    (to-u64 bytes 0 0)))

;; save-temp/load-temp - SBCL versions in compiler-sbcl.lisp use temp registers
#-sbcl
(defun save-temp (td)
  (arm64:str :x0 :sp :offset (temp-slot td)))

#-sbcl
(defun load-temp (rd td)
  (arm64:ldr rd :sp :offset (temp-slot td)))


(defun gen-memcpy-inline (count-reg)
  "Generate inline memcpy loop.
   x1 = src, x3 = dst, count-reg = count (modified).
   x4 = temp for byte. Increments x1, x3."
  ;; Generate a simple loop:
  ;; loop: cbz count, done (+20)
  ;;       ldrb w4, [x1]
  ;;       strb w4, [x3]
  ;;       add x1, x1, #1
  ;;       add x3, x3, #1
  ;;       sub count, count, #1
  ;;       b loop (-24)
  (let* ((skip-if-zero (arm64:cbz count-reg 7))  ; skip 7 instructions if zero
         (load-byte (arm64:ldrb :x4 :x1 0))
         (store-byte (arm64:strb :x4 :x3 0))
         (inc-src (arm64:add :x1 :x1 1 :imm t))
         (inc-dst (arm64:add :x3 :x3 1 :imm t))
         (dec-count (arm64:sub count-reg count-reg 1 :imm t))
         (loop-back (arm64:b -6)))  ; back 6 instructions
    (append-all (list skip-if-zero load-byte store-byte
                           inc-src inc-dst dec-count loop-back))))

;;; ============================================================
;;; IR Tag Predicates
;;; ============================================================

#-sbcl
(defun has-tag (ir tag)
  "Check if IR has the given tag"
  (and (consp ir) (eq (car ir) tag)))

(defun ir-may-call (ir)
  "Check if IR may involve a function call"
  (cond
    ((null ir) nil)
    ((not (consp ir)) nil)
    ((has-tag ir 'lit) nil)
    ((has-tag ir 'var) nil)
    ((has-tag ir 'sym-lit) nil)
    ((has-tag ir 'call-fn) t)
    ((has-tag ir 'funcall-ir) t)
    ((has-tag ir 'sys-exit-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'get-cmdline-args-ir) nil)  ; no subexpressions
    ((has-tag ir 'add) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'sub) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'mul) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'mod) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'cons-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'car-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'cdr-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'get-tag) (ir-may-call (cadr ir)))
    ((has-tag ir 'set-tag) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'setq-ir) (ir-may-call (caddr ir)))
    ((has-tag ir 'setcar-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'setcdr-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'symbol-name-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'make-symbol-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'string-length-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'string-ref-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'string-concat-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'string-equal-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'substring-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir)) (ir-may-call (cadddr ir))))
    ((has-tag ir 'nthcdr-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'println-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'system-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'make-vector-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'vector-ref-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'vector-set-ir) (or (ir-may-call (cadr ir))
                                      (ir-may-call (caddr ir))
                                      (ir-may-call (cadddr ir))))
    ((has-tag ir 'vector-length-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'buffer-byte-ref-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'buffer-byte-set-ir) (or (ir-may-call (cadr ir))
                                           (ir-may-call (caddr ir))
                                           (ir-may-call (cadddr ir))))
    ((has-tag ir 'make-string-from-vector-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'get-global-vars-ir) nil)
    ((has-tag ir 'get-intern-table-ir) nil)
    ((has-tag ir 'get-keyword-table-ir) nil)
    ((has-tag ir 'set-global-vars-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'set-intern-table-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'set-keyword-table-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'consp-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'numberp-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'stringp-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'symbolp-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'vectorp-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'null-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'str-lit) nil)
    ((has-tag ir 'if-ir) t)
    ((has-tag ir 'while-ir) t)
    ((has-tag ir 'dolist-ir) t)
    ((has-tag ir 'dotimes-ir) t)
    ((has-tag ir 'let-ir) t)
    ((has-tag ir 'let*-ir) t)
    ((has-tag ir 'progn-ir) t)
    ;; Syscalls act like function calls (clobber registers)
    ((has-tag ir 'sys-open-ir) t)
    ((has-tag ir 'sys-write-ir) t)
    ((has-tag ir 'sys-write-char-ir) t)
    ((has-tag ir 'sys-read-byte-ir) t)
    ((has-tag ir 'sys-read-ir) t)
    ((has-tag ir 'sys-close-ir) t)
    ;; JIT primitives (extern calls)
    ((has-tag ir 'mmap-jit-ir) t)
    ((has-tag ir 'pthread-jit-write-protect-np-ir) t)
    ((has-tag ir 'sys-dcache-flush-ir) t)
    ((has-tag ir 'sys-icache-invalidate-ir) t)
    ((has-tag ir 'funcall-ptr-ir) t)
    ;; Memory access (may have subexpressions that call)
    ((has-tag ir 'mem-set-byte-ir) (or (ir-may-call (cadr ir))
                                        (ir-may-call (caddr ir))
                                        (ir-may-call (cadddr ir))))
    ((has-tag ir 'mem-load-64-ir) (or (ir-may-call (cadr ir))
                                       (ir-may-call (caddr ir))))
    (t (error "ir-may-call: unknown IR type ~S" (if (consp ir) (car ir) ir)))))

;;; ============================================================
;;; Name Normalization - Phase Boundary Conversion
;;; ============================================================
;;;
;;; Function names transition from symbols (reader/compiler) to strings
;;; (codegen/linker). Normalize at the boundary, then use strings throughout.

(defun normalize-fn-name (name)
  "Normalize a function name to string at phase boundary.
   CONTRACT: After normalization, all fnoffs entries are strings."
  (cond
    ((stringp name) name)
    ((symbolp name) (symbol-name name))
    (t (error "normalize-fn-name: expected string or symbol, got ~S" name))))

;;; ============================================================
;;; String Lookup in Fnoffs (all names are strings after normalization)
;;; ============================================================

(defun lookup-string (name fnoffs)
  "Look up a name in fnoffs alist.
   fnoffs entries are normalized to strings at creation.
   NAME can be symbol or string (will be normalized for lookup)."
  (let ((name-str (normalize-fn-name name)))
    (labels ((search-list (lst)
               (if (null lst)
                   nil
                   (let ((entry (car lst)))
                     (if (string-equal name-str (car entry))
                         entry
                         (search-list (cdr lst)))))))
      (search-list fnoffs))))

;;; ============================================================
;;; Build Captures for Closure Creation
;;; ============================================================

(defun build-captures (free-offsets)
  "Generate code to build a cons list of captured values.
   free-offsets = list of stack offsets where captured values live.
   Result in x0 is a tagged cons list."
  (if (null free-offsets)
      (arm64:movz :x0 0)  ;; nil
      ;; Build cons list iteratively using push/nreverse for O(n) performance
      ;; Start with nil, then cons each value
      ;; NOTE: No GC trigger here - the variable-length cons chain causes
      ;; issues with function offset calculations. Closures with captures
      ;; are rare enough that skipping GC check here is acceptable.
      (let ((code-parts nil))
        ;; Start with movz :x0 0 (nil)
        (push (arm64:movz :x0 0) code-parts)
        ;; For each offset, generate code to cons it onto the list
        (dolist (off free-offsets)
          (let ((off8 (* off 8)))
            ;; Save cdr in heap
            (push (arm64:str :x0 :heap :offset 8) code-parts)
            ;; Load current value
            (push (arm64:sub :x1 :env off8 :imm t) code-parts)
            (push (arm64:ldr :x0 :x1 :offset 0) code-parts)
            ;; Store as car
            (push (arm64:str :x0 :heap :offset 0) code-parts)
            ;; Make cons pointer
            (push (arm64:mov :x0 :heap) code-parts)
            (push (arm64:add :x0 :x0 #.+tag-cons+ :imm t) code-parts)
            (push (arm64:add :heap :heap 16 :imm t) code-parts)))
        ;; Flatten and return
        (append-all (nreverse code-parts)))))

;;; ============================================================
;;; Linearization Pass (Tree IR → Linear IR)
;;; ============================================================
;;; Converts tree-structured IR to flat Three-Address Code style.
;;; This enables iterative codegen and opens optimization opportunities.
;;;
;;; Linear IR instruction format: (op dst src1 src2 ...)
;;;   dst = temp slot number (t0, t1, t2, ...)
;;;   src = either temp slot or inline value
;;;
;;; Uses iterative post-order traversal to avoid deep recursion.

;; Linear IR temp counter state (shared SBCL/native)
#+sbcl (defvar *linear-temp-counter* 0)
#+sbcl (defvar *linear-output* nil)
#+sbcl (defvar *linear-temp-map* nil)  ; maps IR node identity to temp slot
#+sbcl (defvar *linear-label-counter* 0)

;;; Old linearize-based codegen (SBCL only - habu0 uses reg-alloc codegen)
#+sbcl
(defun reset-linear-state ()
  "Reset linearization state"
  #+sbcl (progn
           (setf *linear-temp-counter* 0)
           (setf *linear-output* nil)
           (setf *linear-temp-map* nil)
           (setf *linear-label-counter* 0))
  #-sbcl nil)  ; Native uses stack-allocated state

#+sbcl
(defun fresh-temp ()
  "Allocate a fresh temp slot"
  #+sbcl (let ((n *linear-temp-counter*))
           (setf *linear-temp-counter* (+ n 1))
           n)
  #-sbcl (error "fresh-temp: native mode not yet implemented"))

#+sbcl
(defun fresh-label ()
  "Allocate a fresh label name"
  #+sbcl (let ((n *linear-label-counter*))
           (setf *linear-label-counter* (+ n 1))
           (intern (format nil "L~D" n) :habu))
  #-sbcl (error "fresh-label: native mode not yet implemented"))

#+sbcl
(defun emit-linear (instr)
  "Emit a linear IR instruction"
  #+sbcl (push instr *linear-output*)
  #-sbcl (error "emit-linear: native mode not yet implemented"))

#+sbcl
(defun linear-leaf-p (ir)
  "Check if IR is a leaf node (no sub-expressions to linearize)"
  (or (not (consp ir))
      (has-tag ir 'lit)
      (has-tag ir 'nil-ir)
      (has-tag ir 'var)
      (has-tag ir 'sym-lit)
      (has-tag ir 'str-lit)
      (has-tag ir 'kw-lit)
      (has-tag ir 'lambda-ref)
      (has-tag ir 'get-global-vars-ir)
      (has-tag ir 'get-cmdline-args-ir)
      (has-tag ir 'get-intern-table-ir)
      (has-tag ir 'get-keyword-table-ir)))

#+sbcl
(defun linearize-leaf (ir)
  "Linearize a leaf IR node, returns temp holding result"
  (let ((dst (fresh-temp)))
    (cond
      ((has-tag ir 'lit)
       (emit-linear (list 'load-lit dst (cadr ir))))
      ((has-tag ir 'nil-ir)
       (emit-linear (list 'load-nil dst)))
      ((has-tag ir 'var)
       (emit-linear (list 'load-var dst (cadr ir))))
      ((has-tag ir 'sym-lit)
       (emit-linear (list 'load-sym dst (cadr ir))))
      ((has-tag ir 'str-lit)
       (emit-linear (list 'load-str dst (cadr ir))))
      ((has-tag ir 'kw-lit)
       (emit-linear (list 'load-kw dst (cadr ir))))
      ((has-tag ir 'lambda-ref)
       (emit-linear (list 'load-lambda dst (cadr ir) (caddr ir))))
      ((has-tag ir 'get-global-vars-ir)
       (emit-linear (list 'get-global-vars dst)))
      ((has-tag ir 'get-cmdline-args-ir)
       (emit-linear (list 'get-cmdline-args dst)))
      ;; get-intern-table: load from [x27 + 0]
      ((has-tag ir 'get-intern-table-ir)
       (emit-linear (list 'get-intern-table dst)))
      ;; get-keyword-table: load from [x27 + 128]
      ((has-tag ir 'get-keyword-table-ir)
       (emit-linear (list 'get-keyword-table dst)))
      ((numberp ir)  ; bare number
       (emit-linear (list 'load-lit dst ir)))
      (t (error "linearize-leaf: unknown leaf type ~S" ir)))
    dst))

#+sbcl
(defun linearize-binary (tag ir)
  "Linearize a binary operation, returns temp holding result"
  (let* ((left-temp (linearize-expr (cadr ir)))
         (right-temp (linearize-expr (caddr ir)))
         (dst (fresh-temp)))
    (emit-linear (list tag dst left-temp right-temp))
    dst))

#+sbcl
(defun linearize-unary (tag ir)
  "Linearize a unary operation, returns temp holding result"
  (let* ((arg-temp (linearize-expr (cadr ir)))
         (dst (fresh-temp)))
    (emit-linear (list tag dst arg-temp))
    dst))

#+sbcl
(defun linearize-if (ir)
  "Linearize if expression with explicit jumps"
  (let* ((else-label (fresh-label))
         (end-label (fresh-label))
         (test-temp (linearize-expr (cadr ir)))
         (dst (fresh-temp)))
    ;; Jump to else if test is nil
    (emit-linear (list 'jump-if-nil test-temp else-label))
    ;; Then branch
    (let ((then-temp (linearize-expr (caddr ir))))
      (emit-linear (list 'move dst then-temp)))
    (emit-linear (list 'jump end-label))
    ;; Else branch
    (emit-linear (list 'label else-label))
    (let ((else-temp (if (cadddr ir)
                         (linearize-expr (cadddr ir))
                         (let ((nil-dst (fresh-temp)))
                           (emit-linear (list 'load-nil nil-dst))
                           nil-dst))))
      (emit-linear (list 'move dst else-temp)))
    (emit-linear (list 'label end-label))
    dst))

#+sbcl
(defun linearize-progn (ir)
  "Linearize progn, returns temp of last expression"
  (let ((forms (cadr ir))
        (last-temp nil))
    (labels ((do-forms (fs)
               (if (null fs)
                   (if last-temp last-temp
                       (let ((nil-dst (fresh-temp)))
                         (emit-linear (list 'load-nil nil-dst))
                         nil-dst))
                   (progn
                     (setf last-temp (linearize-expr (car fs)))
                     (do-forms (cdr fs))))))
      (do-forms forms))))

#+sbcl
(defun linearize-let (ir)
  "Linearize let binding"
  (let* ((vals (cadr ir))
         (body (caddr ir))
         (count (cadddr ir))
         (offs-raw (nth 4 ir))
         ;; Handle both formats: (0 1 2) or just 0 (for backward compat with tests)
         (offs (if (listp offs-raw) offs-raw (list offs-raw))))
    ;; Emit bind instruction
    (emit-linear (list 'bind count offs))
    ;; Linearize each binding value and store using actual offsets
    (labels ((do-bindings (vs offsets idx)
               (when vs
                 (let* ((val-temp (linearize-expr (car vs)))
                        ;; Use offset from list if available, else fall back to idx
                        (offset (if offsets (car offsets) idx)))
                   (emit-linear (list 'store-binding offset val-temp)))
                 (do-bindings (cdr vs) (if offsets (cdr offsets) nil) (1+ idx)))))
      (do-bindings vals offs 0))
    ;; Linearize body
    (let ((body-temp (linearize-expr body)))
      ;; Emit unbind
      (emit-linear (list 'unbind count))
      body-temp)))

#+sbcl
(defun linearize-call (ir)
  "Linearize function call"
  (let* ((name (cadr ir))
         (args (caddr ir))
         (arg-temps (mapcar #'linearize-expr args))
         (dst (fresh-temp)))
    (emit-linear (cons 'call (cons dst (cons name arg-temps))))
    dst))

#+sbcl
(defun linearize-funcall (ir)
  "Linearize funcall (indirect call)"
  (let* ((fn-ir (cadr ir))
         (args (caddr ir))
         (fn-temp (linearize-expr fn-ir))
         (arg-temps (mapcar #'linearize-expr args))
         (dst (fresh-temp)))
    (emit-linear (cons 'funcall (cons dst (cons fn-temp arg-temps))))
    dst))

#+sbcl
(defun linearize-setq (ir)
  "Linearize variable assignment"
  (let* ((off (cadr ir))
         (val-ir (caddr ir))
         (val-temp (linearize-expr val-ir)))
    (emit-linear (list 'setq off val-temp))
    val-temp))  ; setq returns the value

#+sbcl
(defun linearize-cons (ir)
  "Linearize cons cell creation"
  (let* ((car-temp (linearize-expr (cadr ir)))
         (cdr-temp (linearize-expr (caddr ir)))
         (dst (fresh-temp)))
    (emit-linear (list 'cons dst car-temp cdr-temp))
    dst))

#+sbcl
(defun linearize-while (ir)
  "Linearize while loop"
  (let* ((loop-label (fresh-label))
         (end-label (fresh-label))
         (dst (fresh-temp)))
    ;; Result starts as nil
    (emit-linear (list 'load-nil dst))
    (emit-linear (list 'label loop-label))
    ;; Test
    (let ((test-temp (linearize-expr (cadr ir))))
      (emit-linear (list 'jump-if-nil test-temp end-label)))
    ;; Body
    (let ((body-temp (linearize-expr (caddr ir))))
      (emit-linear (list 'move dst body-temp)))
    (emit-linear (list 'jump loop-label))
    (emit-linear (list 'label end-label))
    dst))

#+sbcl
(defun linearize-dolist (ir)
  "Linearize dolist loop: (dolist-ir var list body)
   Expands to a while loop that iterates over list"
  (let* ((var-offset (cadr ir))
         (list-expr (caddr ir))
         (body (cadddr ir))
         (loop-label (fresh-label))
         (end-label (fresh-label))
         (list-temp (linearize-expr list-expr))
         (dst (fresh-temp))
         (current-temp (fresh-temp)))
    ;; Initialize result to nil
    (emit-linear (list 'load-nil dst))
    ;; Initialize current to the list
    (emit-linear (list 'move current-temp list-temp))
    ;; Loop label
    (emit-linear (list 'label loop-label))
    ;; Test if current is nil (end of list)
    (emit-linear (list 'jump-if-nil current-temp end-label))
    ;; Set var to (car current)
    (let ((car-temp (fresh-temp)))
      (emit-linear (list 'car car-temp current-temp))
      (emit-linear (list 'setq var-offset car-temp)))
    ;; Execute body
    (let ((body-temp (linearize-expr body)))
      (emit-linear (list 'move dst body-temp)))
    ;; Advance current to (cdr current)
    (let ((cdr-temp (fresh-temp)))
      (emit-linear (list 'cdr cdr-temp current-temp))
      (emit-linear (list 'move current-temp cdr-temp)))
    ;; Jump back to loop start
    (emit-linear (list 'jump loop-label))
    ;; End label
    (emit-linear (list 'label end-label))
    dst))

#+sbcl
(defun linearize-dotimes (ir)
  "Linearize dotimes loop: (dotimes-ir var count body)
   Expands to a while loop that counts from 0 to count-1"
  (let* ((var-offset (cadr ir))
         (count-expr (caddr ir))
         (body (cadddr ir))
         (loop-label (fresh-label))
         (end-label (fresh-label))
         (count-temp (linearize-expr count-expr))
         (dst (fresh-temp))
         (idx-temp (fresh-temp))
         (zero-temp (fresh-temp)))
    ;; Initialize result to nil
    (emit-linear (list 'load-nil dst))
    ;; Initialize index to 0
    (emit-linear (list 'load-lit zero-temp 0))
    (emit-linear (list 'move idx-temp zero-temp))
    ;; Set var to initial index
    (emit-linear (list 'setq var-offset idx-temp))
    ;; Loop label
    (emit-linear (list 'label loop-label))
    ;; Test if idx >= count
    (let ((test-temp (fresh-temp)))
      (emit-linear (list 'cmp-lt test-temp idx-temp count-temp))
      (emit-linear (list 'jump-if-nil test-temp end-label)))
    ;; Execute body
    (let ((body-temp (linearize-expr body)))
      (emit-linear (list 'move dst body-temp)))
    ;; Increment index
    (let ((one-temp (fresh-temp))
          (new-idx-temp (fresh-temp)))
      (emit-linear (list 'load-lit one-temp 1))
      (emit-linear (list 'add new-idx-temp idx-temp one-temp))
      (emit-linear (list 'move idx-temp new-idx-temp))
      ;; Update var
      (emit-linear (list 'setq var-offset idx-temp)))
    ;; Jump back to loop start
    (emit-linear (list 'jump loop-label))
    ;; End label
    (emit-linear (list 'label end-label))
    dst))

#+sbcl
(defun linearize-expr (ir)
  "Linearize any IR expression, returns temp holding result"
  (cond
    ;; Leaf nodes
    ((linear-leaf-p ir)
     (linearize-leaf ir))

    ;; Binary arithmetic (both 'add and 'add-ir variants)
    ((or (has-tag ir 'add) (has-tag ir 'add-ir)) (linearize-binary 'add ir))
    ((or (has-tag ir 'sub) (has-tag ir 'sub-ir)) (linearize-binary 'sub ir))
    ((or (has-tag ir 'mul) (has-tag ir 'mul-ir)) (linearize-binary 'mul ir))
    ((or (has-tag ir 'div) (has-tag ir 'div-ir)) (linearize-binary 'div ir))
    ((or (has-tag ir 'mod) (has-tag ir 'mod-ir)) (linearize-binary 'mod ir))

    ;; Comparisons
    ((has-tag ir 'cmp-eq) (linearize-binary 'cmp-eq ir))
    ((has-tag ir 'cmp-lt) (linearize-binary 'cmp-lt ir))
    ((has-tag ir 'cmp-gt) (linearize-binary 'cmp-gt ir))
    ((has-tag ir 'cmp-le) (linearize-binary 'cmp-le ir))
    ((has-tag ir 'cmp-ge) (linearize-binary 'cmp-ge ir))

    ;; Bitwise operations
    ((has-tag ir 'band) (linearize-binary 'band ir))
    ((has-tag ir 'bor) (linearize-binary 'bor ir))
    ((has-tag ir 'bxor) (linearize-binary 'bxor ir))
    ((has-tag ir 'bsh) (linearize-binary 'bsh ir))
    ((has-tag ir 'bnot)
     (let* ((arg-temp (linearize-expr (cadr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'bnot dst arg-temp))
       dst))
    ((has-tag ir 'mvn-ir)
     (let* ((arg-temp (linearize-expr (cadr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'bnot dst arg-temp))  ; mvn-ir uses same codegen as bnot
       dst))

    ;; List operations
    ((has-tag ir 'cons-ir) (linearize-cons ir))
    ((has-tag ir 'car-ir) (linearize-unary 'car ir))
    ((has-tag ir 'cdr-ir) (linearize-unary 'cdr ir))
    ((has-tag ir 'setcar-ir) (linearize-binary 'setcar ir))
    ((has-tag ir 'setcdr-ir) (linearize-binary 'setcdr ir))
    ((has-tag ir 'nthcdr-ir) (linearize-binary 'nthcdr ir))

    ;; String operations
    ((has-tag ir 'string-length-ir) (linearize-unary 'string-length ir))
    ((has-tag ir 'string-ref-ir) (linearize-binary 'string-ref ir))
    ((has-tag ir 'string-concat-ir) (linearize-binary 'string-concat ir))
    ((has-tag ir 'string-equal-ir) (linearize-binary 'string-equal ir))
    ((has-tag ir 'make-string-from-vector-ir) (linearize-unary 'make-string-from-vector ir))
    ((has-tag ir 'substring-ir)
     (let* ((str-temp (linearize-expr (cadr ir)))
            (start-temp (linearize-expr (caddr ir)))
            (end-temp (linearize-expr (cadddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'substring dst str-temp start-temp end-temp))
       dst))
    ((has-tag ir 'println-ir) (linearize-unary 'println ir))
    ((has-tag ir 'system-ir) (linearize-unary 'system ir))

    ;; Symbol operations
    ((has-tag ir 'symbol-name-ir) (linearize-unary 'symbol-name ir))
    ((has-tag ir 'make-symbol-ir) (linearize-unary 'make-symbol ir))
    ((has-tag ir 'make-symbol-from-string-ir) (linearize-unary 'make-symbol ir))

    ;; Vector operations
    ((has-tag ir 'make-vector-ir) (linearize-unary 'make-vector ir))
    ((has-tag ir 'vector-length-ir) (linearize-unary 'vector-length ir))
    ((has-tag ir 'vector-ref-ir) (linearize-binary 'vector-ref ir))
    ((has-tag ir 'vector-set-ir)
     (let* ((vec-temp (linearize-expr (cadr ir)))
            (idx-temp (linearize-expr (caddr ir)))
            (val-temp (linearize-expr (cadddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'vector-set dst vec-temp idx-temp val-temp))
       dst))

    ;; Buffer operations
    ((has-tag ir 'buffer-byte-ref-ir) (linearize-binary 'buffer-byte-ref ir))
    ((has-tag ir 'buffer-byte-set-ir)
     (let* ((buf-temp (linearize-expr (cadr ir)))
            (idx-temp (linearize-expr (caddr ir)))
            (val-temp (linearize-expr (cadddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'buffer-byte-set dst buf-temp idx-temp val-temp))
       dst))

    ;; Tag operations
    ((has-tag ir 'get-tag) (linearize-unary 'get-tag ir))
    ((has-tag ir 'set-tag) (linearize-binary 'set-tag ir))

    ;; Type predicates
    ((has-tag ir 'consp-ir) (linearize-unary 'consp ir))
    ((has-tag ir 'numberp-ir) (linearize-unary 'numberp ir))
    ((has-tag ir 'stringp-ir) (linearize-unary 'stringp ir))
    ((has-tag ir 'symbolp-ir) (linearize-unary 'symbolp ir))
    ((has-tag ir 'vectorp-ir) (linearize-unary 'vectorp ir))
    ((has-tag ir 'null-ir) (linearize-unary 'null-check ir))

    ;; Control flow
    ((has-tag ir 'if-ir) (linearize-if ir))
    ((has-tag ir 'while-ir) (linearize-while ir))
    ((has-tag ir 'dolist-ir) (linearize-dolist ir))
    ((has-tag ir 'dotimes-ir) (linearize-dotimes ir))
    ((has-tag ir 'progn-ir) (linearize-progn ir))

    ;; Bindings
    ((has-tag ir 'let-ir) (linearize-let ir))
    ((has-tag ir 'let*-ir) (linearize-let ir))  ; same as let for our purposes
    ((has-tag ir 'setq-ir) (linearize-setq ir))

    ;; Function calls
    ((has-tag ir 'call-fn) (linearize-call ir))
    ((has-tag ir 'funcall-ir) (linearize-funcall ir))

    ;; System calls - linearize args then emit
    ((has-tag ir 'sys-exit-ir)
     (let ((arg-temp (linearize-expr (cadr ir))))
       (emit-linear (list 'sys-exit arg-temp))
       arg-temp))

    ((has-tag ir 'sys-open-ir)
     (let* ((path-temp (linearize-expr (cadr ir)))
            (flags-temp (linearize-expr (caddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'sys-open dst path-temp flags-temp))
       dst))

    ((has-tag ir 'sys-read-ir)
     (let* ((fd-temp (linearize-expr (cadr ir)))
            (buf-temp (linearize-expr (caddr ir)))
            (len-temp (linearize-expr (cadddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'sys-read dst fd-temp buf-temp len-temp))
       dst))

    ((has-tag ir 'sys-write-ir)
     (let* ((fd-temp (linearize-expr (cadr ir)))
            (buf-temp (linearize-expr (caddr ir)))
            (len-temp (linearize-expr (cadddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'sys-write dst fd-temp buf-temp len-temp))
       dst))

    ((has-tag ir 'sys-close-ir)
     (let* ((fd-temp (linearize-expr (cadr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'sys-close dst fd-temp))
       dst))

    ;; Global vars
    ((has-tag ir 'set-global-vars-ir)
     (let ((val-temp (linearize-expr (cadr ir))))
       (emit-linear (list 'set-global-vars val-temp))
       val-temp))

    ;; Intern table
    ((has-tag ir 'set-intern-table-ir)
     (let ((val-temp (linearize-expr (cadr ir))))
       (emit-linear (list 'set-intern-table val-temp))
       val-temp))

    ;; Keyword table (separate from intern table)
    ((has-tag ir 'set-keyword-table-ir)
     (let ((val-temp (linearize-expr (cadr ir))))
       (emit-linear (list 'set-keyword-table val-temp))
       val-temp))

    ;; Memory operations
    ((has-tag ir 'mem-set-byte-ir)
     (let* ((ptr-temp (linearize-expr (cadr ir)))
            (off-temp (linearize-expr (caddr ir)))
            (val-temp (linearize-expr (cadddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'mem-set-byte dst ptr-temp off-temp val-temp))
       dst))

    ((has-tag ir 'mem-load-64-ir)
     (let* ((ptr-temp (linearize-expr (cadr ir)))
            (off-temp (linearize-expr (caddr ir)))
            (dst (fresh-temp)))
       (emit-linear (list 'mem-load-64 dst ptr-temp off-temp))
       dst))

    ;; Block/return-from: non-local exit with markers
    ((has-tag ir 'block-ir)
     (let* ((block-id (cadr ir))
            (body-ir (caddr ir))
            (end-label (gensym "BLOCK-END-"))
            (dst (fresh-temp)))
       ;; Emit block start marker, body, then end marker
       (emit-linear (list 'block-start block-id end-label dst))
       (let ((body-temp (linearize-expr body-ir)))
         ;; Move body result to dst
         (emit-linear (list 'move dst body-temp)))
       (emit-linear (list 'label end-label))
       dst))

    ((has-tag ir 'return-from-ir)
     (let* ((block-id (cadr ir))
            (value-ir (caddr ir)))
       ;; Evaluate value, emit return-from marker
       (let ((val-temp (linearize-expr value-ir)))
         (emit-linear (list 'return-from block-id val-temp))
         ;; Return nil since we're jumping away
         (let ((dst (fresh-temp)))
           (emit-linear (list 'load-nil dst))
           dst))))

    ;; Loop-ir: TCO loop wrapper
    ((has-tag ir 'loop-ir)
     (let* ((body-ir (cadr ir))
            (loop-label (gensym "LOOP-"))
            (dst (fresh-temp)))
       ;; Emit loop start marker
       (emit-linear (list 'loop-start loop-label))
       (emit-linear (list 'label loop-label))
       ;; Linearize body
       (let ((body-temp (linearize-expr body-ir)))
         (emit-linear (list 'move dst body-temp)))
       dst))

    ;; Continue-ir: jump back to loop start
    ((has-tag ir 'continue-ir)
     (let* ((args (cadr ir))
            (dst (fresh-temp)))
       ;; Linearize args and emit stores to param slots (at sp+0x40+)
       (let ((arg-temps nil))
         ;; First, linearize all args to get their temp slots (use push, then reverse)
         (dolist (arg-ir args)
           (let ((arg-temp (linearize-expr arg-ir)))
             (push arg-temp arg-temps)))
         (setq arg-temps (nreverse arg-temps))
         ;; Now emit copies from arg temps to param slots
         ;; Param slots start at sp+0x380 (environment base)
         (let ((idx 0))
           (dolist (arg-temp arg-temps)
             (emit-linear (list 'store-param arg-temp idx))
             (setq idx (+ idx 1))))
         ;; Finally emit the continue marker
         ;; Note: continue never returns, so we don't emit load-nil or return a value
         ;; However, the linearize framework expects us to return a temp, so we return dst
         ;; but the code after continue should be unreachable
         (emit-linear (list 'continue))
         dst)))

    ;; buffer-to-string-ir: convert raw byte buffer to string
    ((has-tag ir 'buffer-to-string-ir)
     (let* ((buf-ir (cadr ir))
            (len-ir (caddr ir))
            (buf-temp (linearize-expr buf-ir))
            (len-temp (linearize-expr len-ir))
            (dst (fresh-temp)))
       (emit-linear (list 'buffer-to-string dst buf-temp len-temp))
       dst))

    ;; get-symtab-offset-ir: load symbol table offset from GC globals
    ((has-tag ir 'get-symtab-offset-ir)
     (let ((dst (fresh-temp)))
       (emit-linear (list 'get-symtab-offset dst))
       dst))

    ;; Default: unknown IR
    (t (error "linearize-expr: unknown IR type ~S" (if (consp ir) (car ir) ir)))))

#+sbcl
(defun linearize (ir)
  "Convert tree IR to linear IR.
   Returns a list of linear instructions in execution order."
  (reset-linear-state)
  (let ((result-temp (linearize-expr ir)))
    ;; Add final instruction to mark result
    (emit-linear (list 'result result-temp))
    ;; Return in execution order
    (reverse *linear-output*)))

;;; ============================================================
;;; Linear IR Pretty Printer (for debugging)
;;; ============================================================

#+sbcl
(defun print-linear-ir (linear-ir &optional (stream t))
  "Pretty print linear IR for debugging"
  (dolist (instr linear-ir)
    (format stream "~&  ~S~%" instr)))

;;; ============================================================
;;; Linear IR Codegen (iterative, no recursion)
;;; ============================================================
;;; Generates ARM64 code from linear IR by simple iteration.
;;; Each temp slot maps to a stack location.
;;; This replaces the recursive tree-walking codegen for self-hosting.

#+sbcl
(defvar *linear-labels* nil "Maps label symbols to byte offsets")
#+sbcl
(defvar *linear-fixups* nil "List of (offset . label) for forward jumps")
#+sbcl
(defvar *linear-block-info* nil "Maps block-id to (end-label . dst-temp)")
#+sbcl
(defvar *linear-loop-stack* nil "Stack of loop labels for TCO continue")

#+sbcl
(defun linear-temp-slot (temp)
  "Calculate stack offset for linear temp slot.
   Uses temp area 0x40-0x3840 = 1792 slots (frame is 16KB)."
  (if (>= temp 1792)
      (error "codegen-linear: temp ~D exceeds 1792 slot limit" temp)
      (+ #x40 (* temp 8))))

#+sbcl
(defun linear-load-temp (rd temp)
  "Load temp slot into register"
  (arm64:ldr rd :sp :offset (linear-temp-slot temp)))

#+sbcl
(defun linear-save-temp (temp)
  "Save x0 to temp slot"
  (arm64:str :x0 :sp :offset (linear-temp-slot temp)))

#+sbcl
(defun linear-load-lit (val)
  "Generate code to load tagged fixnum literal into x0"
  (let ((tagged (logior (ash val 1) +fixnum-bit+)))
    (cond
      ;; Small positive: single movz
      ((and (>= tagged 0) (< tagged #x10000))
       (arm64:movz :x0 tagged))
      ;; Small negative: use movn (move wide with NOT)
      ;; movn x0, #imm loads ~imm into x0
      ((and (< tagged 0) (>= tagged (- #x10000)))
       ;; For small negatives: movn x0, #(~tagged & 0xFFFF)
       (arm64:movn :x0 (logand (lognot tagged) #xFFFF)))
      ;; Large positive: movz + movk
      ((>= tagged 0)
       (append (arm64:movz :x0 (logand tagged #xFFFF))
               (arm64:movk :x0 (logand (ash tagged -16) #xFFFF) :lsl 16)))
      ;; Large negative: movn + movk for upper bits
      (t
       ;; Use movn for the low 16 bits, then movk for upper halfwords
       (let ((inv (lognot tagged)))
         (append (arm64:movn :x0 (logand inv #xFFFF))
                 (arm64:movk :x0 (logand (ash tagged -16) #xFFFF) :lsl 16)
                 (arm64:movk :x0 (logand (ash tagged -32) #xFFFF) :lsl 32)
                 (arm64:movk :x0 (logand (ash tagged -48) #xFFFF) :lsl 48)))))))

#+sbcl
(defun gen-bool-to-tagged ()
  "Convert boolean 0/1 in x0 to tagged nil/t.
   Uses: x0 = 1 (true) or 0 (false)
   Result: x0 = +t-value+ (3) or +nil-value+ (0)"
  ;; x0 = 1 or 0
  ;; neg x0, x0      => x0 = -1 or 0 (negate: 0-x0)
  ;; and x0, x0, #3  => x0 = 3 or 0
  (append (arm64:neg :x0 :x0)
          (arm64:and* :x0 :x0 #.+t-value+ :imm t)))

#+sbcl
(defun codegen-linear-instr (instr rtaddrs fnoffs)
  "Generate ARM64 code for a single linear IR instruction.
   Returns list of ARM64 instruction bytes."
  (let ((op (car instr)))
    (case op
      ;; Load instructions
      (load-lit
       (let ((dst (cadr instr))
             (val (caddr instr)))
         (append (linear-load-lit val)
                 (linear-save-temp dst))))

      (load-nil
       (let ((dst (cadr instr)))
         (append (arm64:movz :x0 #.+nil-value+)
                 (linear-save-temp dst))))

      (load-var
       (let ((dst (cadr instr))
             (offset (caddr instr)))
         (append (arm64:sub :x1 :env (* offset 8) :imm t)
                 (arm64:ldr :x0 :x1 :offset 0)
                 (linear-save-temp dst))))

      (load-sym
       ;; Call intern at runtime to get the interned symbol
       ;; CONTRACT: name is always a STRING (produced by compiler's sym-lit)
       (let* ((dst (cadr instr))
              (name (caddr instr))
              (len (length name))
              (total-size (logand (+ len 8 15) (lognot 15))))
         (unless (stringp name)
           (error "load-sym: expected string name, got ~S" name))
         (append (gen-string-lit name len total-size)
                 ;; String is now in x0, call intern with it
                 (list (list :call-fn 'INTERN))
                 ;; Result (interned symbol) is in x0
                 (linear-save-temp dst))))

      (load-str
       (let* ((dst (cadr instr))
              (str (caddr instr))
              (len (length str))
              (total-size (logand (+ len 8 15) (lognot 15))))
         (append (gen-string-lit str len total-size)
                 (linear-save-temp dst))))

      (load-kw
       ;; Keyword literal: allocate string, call intern-keyword
       ;; Name is uppercase without colon (e.g., "TEST" for :test)
       (let* ((dst (cadr instr))
              (name (caddr instr))
              (len (length name))
              (total-size (logand (+ len 8 15) (lognot 15))))
         (append (gen-string-lit name len total-size)
                 ;; String is now in x0, call intern-keyword to get keyword with tag 7
                 (list (list :call-fn 'INTERN-KEYWORD))
                 ;; Result (interned keyword) is in x0
                 (linear-save-temp dst))))

      ;; Binary arithmetic
      ;; With hybrid 1-bit tagging, fixnums are (val << 1) | 1
      ;; add: (a<<1|1) + (b<<1|1) = (a+b)<<1 + 2, need to subtract 1
      (add
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:add :x0 :x0 :x1)
                 (arm64:sub :x0 :x0 #.+fixnum-bit+ :imm t)  ; fix tag: subtract 1
                 (linear-save-temp dst))))

      ;; sub: (a<<1|1) - (b<<1|1) = (a-b)<<1, need to set low bit
      (sub
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:sub :x0 :x0 :x1)
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; fix tag: set low bit
                 (linear-save-temp dst))))

      ;; mul: untag both, multiply, retag
      (mul
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; untag x0
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag x1
                 (arm64:mul :x0 :x0 :x1)
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      ;; div: untag both, divide, retag
      (div
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; untag x0
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag x1
                 (arm64:sdiv :x0 :x0 :x1)
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      (mod
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         ;; Mod: a mod b = a - (a/b)*b
         ;; Need to untag, compute, retag
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:asr :x0 :x0 1 :imm t)   ; untag a
                 (arm64:asr :x1 :x1 1 :imm t)   ; untag b
                 (arm64:sdiv :x2 :x0 :x1)       ; x2 = a/b
                 (arm64:mul :x2 :x2 :x1)        ; x2 = (a/b)*b
                 (arm64:sub :x0 :x0 :x2)        ; x0 = a - (a/b)*b
                 (arm64:lsl :x0 :x0 1 :imm t)   ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      ;; Comparisons - return tagged t (3) or nil (0)
      (cmp-eq
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:cmp :x0 :x1)
                 (arm64:cset :x0 arm64:+eq+)
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (cmp-lt
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:cmp :x0 :x1)
                 (arm64:cset :x0 arm64:+lt+)
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (cmp-gt
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:cmp :x0 :x1)
                 (arm64:cset :x0 arm64:+gt+)
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (cmp-le
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:cmp :x0 :x1)
                 (arm64:cset :x0 arm64:+le+)
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (cmp-ge
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:cmp :x0 :x1)
                 (arm64:cset :x0 arm64:+ge+)
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      ;; Bitwise operations: must untag, operate, retag (like bsh and bnot)
      ;; Operands are tagged fixnums, so we shift right 4 to get values,
      ;; perform the operation, then shift left 4 to retag.
      (band
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; untag src1
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag src2
                 (arm64:and* :x0 :x0 :x1)      ; bitwise AND
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      (bor
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; untag src1
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag src2
                 (arm64:orr :x0 :x0 :x1)       ; bitwise OR
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      (bxor
       (let ((dst (cadr instr))
             (src1 (caddr instr))
             (src2 (cadddr instr)))
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; untag src1
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag src2
                 (arm64:eor :x0 :x0 :x1)       ; bitwise XOR
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      (bsh
       ;; Bitwise shift: (bsh val amount) - positive = left, negative = right
       ;; Both args are tagged. Untag both, shift, retag.
       ;; ARM64 LSLV/ASRV only use low 6 bits, so we must branch on sign.
       (let ((dst (cadr instr))
             (src1 (caddr instr))  ; value
             (src2 (cadddr instr))) ; shift amount
         (append (linear-load-temp :x0 src1)
                 (linear-load-temp :x1 src2)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)   ; untag value
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)   ; untag amount
                 ;; Check if shift amount is negative
                 (arm64:cmp :x1 0 :imm t)       ; compare amount to 0
                 (arm64:b.lt 3)                 ; if negative, jump to right shift
                 ;; Positive (left shift)
                 (arm64:lsl :x0 :x0 :x1)        ; LSLV - variable left shift
                 (arm64:b 4)                    ; skip right shift (now 4 instructions)
                 ;; Negative (right shift): negate amount first
                 (arm64:neg :x1 :x1)            ; x1 = -x1 (make positive)
                 (arm64:asr :x0 :x0 :x1)        ; ASRV - variable arithmetic right shift
                 ;; Retag result
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)   ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)   ; set fixnum bit
                 (linear-save-temp dst))))

      (bnot
       ;; Bitwise NOT on tagged fixnum:
       ;; 1. Untag (arithmetic shift right 1)
       ;; 2. MVN (bitwise NOT)
       ;; 3. Retag (shift left 1, then OR with fixnum bit)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)   ; untag
                 (arm64:mvn :x0 :x0)            ; bitwise NOT
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)   ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)   ; set fixnum bit
                 (linear-save-temp dst))))

      ;; System calls
      (sys-exit
       (let ((src (cadr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; untag exit code
                 (arm64:movz :x16 1)           ; syscall number for exit
                 (arm64:svc 0))))              ; supervisor call

      ;; List operations
      (cons
       (let ((dst (cadr instr))
             (car-src (caddr instr))
             (cdr-src (cadddr instr)))
         ;; Allocate cons cell on heap
         (append (linear-load-temp :x0 car-src)
                 (linear-load-temp :x1 cdr-src)
                 ;; Store car at heap, cdr at heap+8
                 (arm64:str :x0 :heap :offset 0)
                 (arm64:str :x1 :heap :offset 8)
                 ;; Make tagged cons pointer (cons tag = 0)
                 (arm64:mov :x0 :heap)
                 ;; cons tag is 0, so no add needed
                 ;; Bump heap
                 (arm64:add :heap :heap 16 :imm t)
                 (linear-save-temp dst))))

      (car
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 ;; cons tag is 0, so no need to untag
                 (arm64:ldr :x0 :x0 :offset 0)
                 (linear-save-temp dst))))

      (cdr
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 ;; cons tag is 0, so no need to untag
                 (arm64:ldr :x0 :x0 :offset 8)
                 (linear-save-temp dst))))

      ;; Get tag (returns tag bits 0-3 as tagged fixnum)
      (get-tag
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:and* :x0 :x0 #xF :imm t)  ; extract tag bits
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      ;; Set tag: (set-tag dst value new-tag)
      ;; Changes the low 4 bits of value to (untag new-tag)
      (set-tag
       (let ((dst (cadr instr))
             (val (caddr instr))
             (new-tag (cadddr instr)))
         (append (linear-load-temp :x0 val)       ; load value
                 (linear-load-temp :x1 new-tag)   ; load new tag (tagged)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)     ; untag new-tag
                 (arm64:movz :x2 15 :lsl 0)       ; x2 = 0xF mask
                 (arm64:bic :x0 :x0 :x2)          ; clear low 4 bits of value
                 (arm64:orr :x0 :x0 :x1)          ; apply new tag bits
                 (linear-save-temp dst))))

      ;; String operations

      ;; Type predicates
      (consp
       ;; Type contract: (tagged-value) -> tagged-boolean
       ;; Input: any tagged Habu value
       ;; Output: nil (0) if not cons, t (3) if cons
       ;;
       ;; Test if value is cons: tag == 0 AND value != 0 (not nil)
       ;; Hybrid scheme: cons tag is 0, same as nil's tag bits, so must check non-zero
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 ;; Assume false first
                 (arm64:movz :x9 0)
                 ;; Check nil - if nil, skip to convert (x9=0)
                 (arm64:cmp :x0 #.+nil-value+ :imm t)
                 (arm64:b.eq 4)                    ; if nil, skip to convert (+4 instrs to neg)
                 ;; Not nil, check if low nibble == 0 (cons tag)
                 (arm64:and* :x10 :x0 #.+tag-mask+ :imm t)
                 (arm64:cbnz :x10 2)               ; if tag != 0, skip to convert (+2 instrs to neg)
                 ;; It's a cons! Set x9 to 1
                 (arm64:movz :x9 1)
                 ;; Convert x9 (0/1) to nil(0)/t(3)
                 (arm64:neg :x0 :x9)
                 (arm64:and* :x0 :x0 #.+t-value+ :imm t)
                 (linear-save-temp dst))))

      (numberp
       ;; Test if value is fixnum: bit 0 == 1 (hybrid scheme)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:and* :x0 :x0 #.+fixnum-bit+ :imm t) ; extract bit 0
                 ;; x0 is now 1 if fixnum, 0 if pointer
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (stringp
       ;; Test if value is string (tag == 6)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:and* :x0 :x0 #.+tag-mask+ :imm t)
                 (arm64:cmp :x0 #.+tag-string+ :imm t)
                 (arm64:cset :x0 arm64:+eq+)
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (symbolp
       ;; Test if value is symbol (tag == 2) OR keyword (tag == 10)
       ;; In CL, keywords are symbols - (symbolp :foo) => t
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:and* :x1 :x0 #.+tag-mask+ :imm t)  ; x1 = tag
                 (arm64:cmp :x1 #.+tag-symbol+ :imm t)     ; tag == 2?
                 (arm64:cset :x0 arm64:+eq+)               ; x0 = (tag == 2)
                 (arm64:cmp :x1 #.+tag-keyword+ :imm t)    ; tag == 10?
                 (arm64:cset :x1 arm64:+eq+)               ; x1 = (tag == 10)
                 (arm64:orr :x0 :x0 :x1)                   ; x0 = symbol OR keyword
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (vectorp
       ;; Test if value is vector (tag == 4)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:and* :x0 :x0 #.+tag-mask+ :imm t)
                 (arm64:cmp :x0 #.+tag-vector+ :imm t)
                 (arm64:cset :x0 arm64:+eq+)
                 (gen-bool-to-tagged)
                 (linear-save-temp dst))))

      (null-check
       ;; Test if value is nil (== 0 in hybrid scheme)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:cmp :x0 #.+nil-value+ :imm t) ; compare to nil (0)
                 (arm64:cset :x0 arm64:+eq+)      ; set x0 to 1 if equal, 0 otherwise
                 (gen-bool-to-tagged)             ; convert to t or nil
                 (linear-save-temp dst))))
      (string-length
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:sub :x0 :x0 #.+tag-string+ :imm t)      ; remove string tag
                 (arm64:ldr :x0 :x0 :offset 0)    ; load length
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)      ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)      ; set fixnum bit
                 (linear-save-temp dst))))

      (string-ref
       (let ((dst (cadr instr))
             (str-src (caddr instr))
             (idx-src (cadddr instr)))
         (append (linear-load-temp :x0 str-src)
                 (linear-load-temp :x1 idx-src)
                 ;; Untag index (shift right 1)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)
                 ;; Untag string pointer
                 (arm64:sub :x0 :x0 #.+tag-string+ :imm t)
                 ;; Add 8 for header, then add index
                 (arm64:add :x0 :x0 8 :imm t)
                 (arm64:add :x0 :x0 :x1)
                 ;; Load byte at [x0]
                 (arm64:ldrb :x0 :x0 0)
                 ;; Tag as fixnum
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-save-temp dst))))

      ;; nthcdr: (nthcdr n list) - take cdr n times
      (nthcdr
       (let ((dst (cadr instr))
             (n-src (caddr instr))
             (list-src (cadddr instr)))
         ;; Generate inline loop: while n > 0, list = cdr(list), n--
         (append (linear-load-temp :x0 n-src)      ; x0 = n (tagged)
                 (linear-load-temp :x1 list-src)   ; x1 = list
                 ;; Loop: while x0 > 0
                 ;; Generate label references (will be fixed up)
                 (list (list :local-loop-start))
                 (arm64:cmp :x0 0 :imm t)           ; compare n to 0
                 (list (list :local-branch-le))     ; if n <= 0, exit
                 ;; x1 = cdr(x1)
                 ;; cons tag is 0, so no need to untag
                 (arm64:ldr :x1 :x1 :offset 8)      ; load cdr
                 ;; x0 = x0 - 1 (decrement n, keep tagged)
                 (arm64:sub :x0 :x0 2 :imm t)      ; subtract tagged 1 (1 << 1)
                 (list (list :local-jump-back))     ; jump to loop start
                 (list (list :local-loop-end))
                 ;; Result is in x1
                 (arm64:mov :x0 :x1)
                 (linear-save-temp dst))))

      ;; substring: (substring str start end) - extract substring
      (substring
       (let ((dst (cadr instr))
             (str-src (caddr instr))
             (start-src (cadddr instr))
             (end-src (car (cddddr instr))))
         ;; Call external substring implementation or inline it
         ;; For now, generate a runtime call (simplified - requires runtime function)
         (append (linear-load-temp :x0 str-src)
                 (linear-load-temp :x1 start-src)
                 (linear-load-temp :x2 end-src)
                 ;; Call runtime function SUBSTRING
                 (list (list :call-fn 'SUBSTRING))
                 (linear-save-temp dst))))

      ;; println: print value with newline
      (println
       (let ((dst (cadr instr))
             (val-src (caddr instr)))
         ;; Call runtime function PRINTLN
         (append (linear-load-temp :x0 val-src)
                 (list (list :call-fn 'PRINTLN))
                 (linear-save-temp dst))))

      ;; system: execute system command (string)
      (system
       (let ((dst (cadr instr))
             (cmd-src (caddr instr)))
         ;; Call C system() function via extern
         (append (linear-load-temp :x0 cmd-src)
                 ;; Untag string and add 8 to get C string pointer
                 (arm64:sub :x0 :x0 #.+tag-string+ :imm t)
                 (arm64:add :x0 :x0 8 :imm t)
                 (list (list :extern-call "_system"))
                 ;; Tag result as fixnum
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-save-temp dst))))

      ;; setcar/setcdr mutations
      (setcar
       (let ((dst (cadr instr))
             (cons-src (caddr instr))
             (val-src (cadddr instr)))
         (append (linear-load-temp :x0 cons-src)
                 (linear-load-temp :x1 val-src)
                 ;; cons tag is 0, so no need to untag/retag
                 (arm64:str :x1 :x0 :offset 0) ; store car
                 (arm64:mov :x0 :x0)           ; x0 already has the cons pointer
                 (linear-save-temp dst))))

      (setcdr
       (let ((dst (cadr instr))
             (cons-src (caddr instr))
             (val-src (cadddr instr)))
         (append (linear-load-temp :x0 cons-src)
                 (linear-load-temp :x1 val-src)
                 ;; cons tag is 0, so no need to untag/retag
                 (arm64:str :x1 :x0 :offset 8) ; store cdr
                 (arm64:mov :x0 :x0)           ; x0 already has the cons pointer
                 (linear-save-temp dst))))

      ;; Control flow
      (label
       ;; Labels are handled during assembly - just record position
       nil)

      (jump
       ;; Unconditional branch - will be fixed up later
       (let ((label (cadr instr)))
         ;; Placeholder B instruction (offset filled in later)
         (arm64:b 0)))

      (jump-if-nil
       (let ((src (cadr instr))
             (label (caddr instr)))
         ;; Compare with nil (0), branch if equal
         (append (linear-load-temp :x0 src)
                 (arm64:cmp :x0 #.+nil-value+ :imm t)
                 (arm64:b.eq 0))))  ; offset filled in later

      (move
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (linear-save-temp dst))))

      ;; Block/return-from control flow
      (block-start
       ;; Handled in main codegen loop
       nil)

      (return-from
       ;; Handled in main codegen loop
       nil)

      ;; Loop control flow (TCO)
      (loop-start
       ;; Just a marker - position recorded for continue jumps
       nil)

      (store-param
       ;; (store-param src-temp param-idx) - store temp to param slot at [env - param-idx*8]
       (let ((src-temp (cadr instr))
             (param-idx (caddr instr)))
         (append (linear-load-temp :x16 src-temp)
                 (arm64:sub :x10 :env (* param-idx 8) :imm t)
                 (arm64:str :x16 :x10 :offset 0))))

      (continue
       ;; TCO continue - emit branch marker (args already stored by store-param)
       (list (list :continue)))

      ;; Bindings - env is stable (not moved), offsets are absolute
      (bind
       ;; No-op: env stays fixed, bindings use absolute offsets
       nil)

      (store-binding
       (let ((offset (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 ;; Store at env - offset*8 (like recursive codegen)
                 (arm64:sub :x1 :env (* offset 8) :imm t)
                 (arm64:str :x0 :x1 :offset 0))))

      (unbind
       ;; No-op: env stays fixed
       nil)

      (setq
       (let ((offset (cadr instr))
             (src (caddr instr)))
         (append (linear-load-temp :x0 src)
                 (arm64:sub :x1 :env (* offset 8) :imm t)
                 (arm64:str :x0 :x1 :offset 0))))

      ;; Function calls - emit marker for later resolution
      (call
       (let* ((dst (cadr instr))
              (name (caddr instr))
              (arg-temps (cdddr instr)))
         ;; Load args into x0-x7
         (append
          (loop for temp in arg-temps
                for reg in '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7)
                append (linear-load-temp reg temp))
          ;; Emit call marker (resolved later by resolve-calls)
          (list (list :call-fn name))
          (linear-save-temp dst))))

      ;; Indirect function call (funcall)
      (funcall
       (let* ((dst (cadr instr))
              (fn-temp (caddr instr))
              (arg-temps (cdddr instr)))
         ;; Load args into x0-x7
         (append
          (loop for temp in arg-temps
                for reg in '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7)
                append (linear-load-temp reg temp))
          ;; Load function (closure) into x16 (IP0), then call
          (linear-load-temp :x16 fn-temp)
          ;; Extract code pointer from closure
          (arm64:sub :x16 :x16 #.+tag-closure+ :imm t)    ; remove closure tag
          (arm64:ldr :x16 :x16 :offset 0)   ; load tagged fn offset
          (arm64:asr :x16 :x16 #.+fixnum-bit+ :imm t)    ; untag to get raw offset
          (arm64:add :x16 :x26 :x16)        ; add code base to get address
          (arm64:blr :x16)                 ; call through register
          (linear-save-temp dst))))

      ;; Result marker - just load result temp to x0
      (result
       (let ((src (cadr instr)))
         (linear-load-temp :x0 src)))

      ;; Make-vector: allocate vector on heap
      ;; Vector layout: [length (8 bytes)] [data (n * 8 bytes)]
      (make-vector
       (let ((dst (cadr instr))
             (size-temp (caddr instr)))
         (append
          (linear-load-temp :x0 size-temp)
          ;; GC pre-check
          (gc-trigger-code)
          ;; x0 = tagged size, store untagged length at [x28+0]
          (arm64:asr :x1 :x0 #.+fixnum-bit+ :imm t)           ; x1 = untagged length
          (arm64:str :x1 :heap :offset 0)        ; [x28+0] = length
          ;; Calculate allocation size: 8 + length*8
          (arm64:lsl :x1 :x1 3 :imm t)           ; x1 = length * 8
          (arm64:add :x1 :x1 8 :imm t)           ; x1 = 8 + data_size
          ;; Round to 16-byte alignment
          (arm64:add :x1 :x1 15 :imm t)
          (arm64:and* :x1 :x1 -16 :imm t)
          ;; Return tagged pointer, bump heap
          (arm64:mov :x0 :heap)
          (arm64:add :heap :heap :x1)
          ;; Tag with vector tag
          (arm64:movz :x1 #.+tag-vector+)
          (arm64:orr :x0 :x0 :x1)
          ;; Note: NO post-allocation GC check - x0 is unrooted until saved
          (linear-save-temp dst))))

      ;; Get-global-vars: load from [x27 + 104]
      (get-global-vars
       (let ((dst (cadr instr)))
         (append (arm64:ldr :x0 :gc :offset 104)
                 (linear-save-temp dst))))

      ;; Get-intern-table: load from [x27 + 0]
      (get-intern-table
       (let ((dst (cadr instr)))
         (append (arm64:ldr :x0 :gc :offset 0)
                 (linear-save-temp dst))))

      ;; Set-global-vars: store to [x27 + 104]
      (set-global-vars
       (let ((val-temp (cadr instr)))
         (append (linear-load-temp :x0 val-temp)
                 (arm64:str :x0 :gc :offset 104))))

      ;; Set-intern-table: store to [x27 + 0]
      (set-intern-table
       (let ((val-temp (cadr instr)))
         (append (linear-load-temp :x0 val-temp)
                 (arm64:str :x0 :gc :offset 0))))

      ;; Get-keyword-table: load from [x27 + 128]
      (get-keyword-table
       (let ((dst (cadr instr)))
         (append (arm64:ldr :x0 :gc :offset 128)
                 (linear-save-temp dst))))

      ;; Set-keyword-table: store to [x27 + 128]
      (set-keyword-table
       (let ((val-temp (cadr instr)))
         (append (linear-load-temp :x0 val-temp)
                 (arm64:str :x0 :gc :offset 128))))

      ;; Get-cmdline-args: load argc/argv from [x27 + 64/72]
      (get-cmdline-args
       (let ((dst (cadr instr)))
         ;; Return nil for now - full implementation would build list from argv
         (append (arm64:movz :x0 +nil-value+)  ; nil = 0
                 (linear-save-temp dst))))

      ;; Sys-exit: exit with value (via libSystem stub)
      (sys-exit
       (let ((val-temp (cadr instr)))
         (append (linear-load-temp :x0 val-temp)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; untag
                 (list (list :extern-call "_exit")))))

      ;; Sys-open: open file (path flags mode) -> fd (via libSystem stub)
      (sys-open
       (let ((dst (cadr instr))
             (path-temp (caddr instr))
             (flags-temp (cadddr instr)))
         (append (linear-load-temp :x0 path-temp)
                 (arm64:sub :x0 :x0 #.+tag-string+ :imm t)  ; untag string
                 (arm64:add :x0 :x0 8 :imm t)  ; skip length
                 (linear-load-temp :x1 flags-temp)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag flags
                 (arm64:movz :x2 0)            ; mode = 0
                 (list (list :extern-call "_open"))
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      ;; Sys-read: read(fd, buf, len) -> bytes read (via libSystem stub)
      (sys-read
       (let ((dst (cadr instr))
             (fd-temp (caddr instr))
             (buf-temp (cadddr instr))
             (len-temp (car (cddddr instr))))
         (append (linear-load-temp :x0 fd-temp)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-load-temp :x1 buf-temp)
                 (arm64:sub :x1 :x1 #.+tag-vector+ :imm t)  ; untag vector
                 (arm64:add :x1 :x1 8 :imm t)  ; skip length
                 (linear-load-temp :x2 len-temp)
                 (arm64:asr :x2 :x2 #.+fixnum-bit+ :imm t)
                 (list (list :extern-call "_read"))
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-save-temp dst))))

      ;; Sys-write: write(fd, buf, len) -> bytes written (via libSystem stub)
      (sys-write
       (let ((dst (cadr instr))
             (fd-temp (caddr instr))
             (buf-temp (cadddr instr))
             (len-temp (car (cddddr instr))))
         (append (linear-load-temp :x0 fd-temp)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-load-temp :x1 buf-temp)
                 (arm64:sub :x1 :x1 #.+tag-vector+ :imm t)  ; untag vector
                 (arm64:add :x1 :x1 8 :imm t)  ; skip length
                 (linear-load-temp :x2 len-temp)
                 (arm64:asr :x2 :x2 #.+fixnum-bit+ :imm t)
                 (list (list :extern-call "_write"))
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-save-temp dst))))

      ;; Sys-close: close(fd) (via libSystem stub)
      (sys-close
       (let ((dst (cadr instr))
             (fd-temp (caddr instr)))
         (append (linear-load-temp :x0 fd-temp)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (list (list :extern-call "_close"))
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-save-temp dst))))

      ;; Vector-ref: (vector-ref dst vec idx)
      ;; Vector-length: get length of vector (returns tagged fixnum)
      (vector-length
       (let ((dst (cadr instr))
             (vec-temp (caddr instr)))
         (append (linear-load-temp :x0 vec-temp)
                 (arm64:sub :x0 :x0 #.+tag-vector+ :imm t)     ; untag vector
                 (arm64:ldr :x0 :x0 :offset 0)    ; x0 = raw length
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)     ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)     ; set fixnum bit
                 (linear-save-temp dst))))

      (vector-ref
       (let ((dst (cadr instr))
             (vec-temp (caddr instr))
             (idx-temp (cadddr instr)))
         (append (linear-load-temp :x1 vec-temp)
                 (arm64:sub :x1 :x1 #.+tag-vector+ :imm t)  ; untag vector
                 (linear-load-temp :x0 idx-temp)
                 ;; Untag index and calculate offset: idx_untagged * 8 + 8
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)  ; x0 = idx_untagged
                 (arm64:lsl :x0 :x0 3 :imm t)  ; x0 = idx_untagged * 8
                 (arm64:add :x0 :x0 8 :imm t)  ; x0 = 8 + idx_untagged * 8
                 (arm64:add :x1 :x1 :x0)       ; x1 = address
                 (arm64:ldr :x0 :x1 :offset 0) ; x0 = [x1] = element
                 (linear-save-temp dst))))

      ;; Vector-set: (vector-set dst vec idx val)
      (vector-set
       (let ((dst (cadr instr))
             (vec-temp (caddr instr))
             (idx-temp (cadddr instr))
             (val-temp (car (cddddr instr))))
         (append (linear-load-temp :x0 vec-temp)
                 (arm64:sub :x0 :x0 #.+tag-vector+ :imm t)  ; untag vector
                 (linear-load-temp :x1 idx-temp)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag index
                 (arm64:add :x1 :x1 1 :imm t)  ; skip length slot
                 (arm64:lsl :x1 :x1 3 :imm t)  ; *8 for offset
                 (arm64:add :x0 :x0 :x1)       ; x0 = address
                 (linear-load-temp :x2 val-temp)
                 (arm64:str :x2 :x0 :offset 0)
                 (arm64:mov :x0 :x2)           ; return value
                 (linear-save-temp dst))))

      ;; Buffer-byte-ref: get raw byte at index from vector data area
      ;; Vector layout: [length (8 bytes)][raw bytes...]
      (buffer-byte-ref
       (let ((dst (cadr instr))
             (buf-temp (caddr instr))
             (idx-temp (cadddr instr)))
         (append (linear-load-temp :x0 buf-temp)
                 (arm64:sub :x0 :x0 #.+tag-vector+ :imm t)  ; untag vector
                 (arm64:add :x0 :x0 8 :imm t)  ; skip length
                 (linear-load-temp :x1 idx-temp)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)  ; untag index
                 (arm64:add :x0 :x0 :x1)       ; x0 = address
                 (arm64:ldrb :x0 :x0 0)        ; load byte
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      ;; Buffer-byte-set: set byte in buffer
      (buffer-byte-set
       (let ((dst (cadr instr))
             (buf-temp (caddr instr))
             (idx-temp (cadddr instr))
             (val-temp (car (cddddr instr))))
         (append (linear-load-temp :x0 buf-temp)
                 (arm64:sub :x0 :x0 #.+tag-vector+ :imm t)  ; untag vector
                 (arm64:add :x0 :x0 8 :imm t)  ; skip length
                 (linear-load-temp :x1 idx-temp)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)
                 (arm64:add :x0 :x0 :x1)       ; x0 = address
                 (linear-load-temp :x2 val-temp)
                 (arm64:asr :x2 :x2 #.+fixnum-bit+ :imm t)
                 (arm64:strb :x2 :x0 :offset 0)
                 (linear-save-temp dst))))

      ;; Mem-set-byte: set byte at pointer + offset
      (mem-set-byte
       (let ((dst (cadr instr))
             (ptr-temp (caddr instr))
             (off-temp (cadddr instr))
             (val-temp (car (cddddr instr))))
         (append (linear-load-temp :x0 ptr-temp)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-load-temp :x1 off-temp)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)
                 (arm64:add :x0 :x0 :x1)
                 (linear-load-temp :x2 val-temp)
                 (arm64:asr :x2 :x2 #.+fixnum-bit+ :imm t)
                 (arm64:strb :x2 :x0 :offset 0)
                 (arm64:movz :x0 +nil-value+)  ; return nil = 0
                 (linear-save-temp dst))))

      ;; Mem-load-64: load 64-bit value from pointer + offset
      (mem-load-64
       (let ((dst (cadr instr))
             (ptr-temp (caddr instr))
             (off-temp (cadddr instr)))
         (append (linear-load-temp :x0 ptr-temp)
                 (arm64:asr :x0 :x0 #.+fixnum-bit+ :imm t)
                 (linear-load-temp :x1 off-temp)
                 (arm64:asr :x1 :x1 #.+fixnum-bit+ :imm t)
                 (arm64:add :x0 :x0 :x1)
                 (arm64:ldr :x0 :x0 :offset 0)
                 (arm64:lsl :x0 :x0 #.+fixnum-bit+ :imm t)  ; shift left
                 (arm64:orr :x0 :x0 #.+fixnum-bit+ :imm t)  ; set fixnum bit
                 (linear-save-temp dst))))

      ;; NOTE: bnot (bitwise NOT) handler is at line ~1627
      ;; This was a duplicate that was unreachable (case only uses first match)
      ;; Boolean NOT uses (cmp-eq x nil-ir) in compiler.lisp, not bnot

      ;; symbol-name: get string name from symbol or keyword
      ;; Symbol has tag 2, Keyword has tag 10, String has tag 6
      ;; Symbols and keywords have same layout as strings (length + chars)
      ;; Just need to change the tag to 6
      (symbol-name
       (let ((dst (cadr instr))
             (sym-temp (caddr instr)))
         (append (linear-load-temp :x0 sym-temp)
                 (arm64:and* :x0 :x0 -16 :imm t)           ; clear all tag bits
                 (arm64:add :x0 :x0 #.+tag-string+ :imm t) ; add string tag
                 (linear-save-temp dst))))

      ;; string-equal: compare two strings for equality
      ;; Returns tagged t or nil
      (string-equal
       (let ((dst (cadr instr))
             (str1-temp (caddr instr))
             (str2-temp (cadddr instr)))
         (append
          ;; Load both strings
          (linear-load-temp :x0 str1-temp)
          (linear-load-temp :x8 str2-temp)
          ;; Check for nil (= 0)
          (arm64:cmp :x0 #.+nil-value+ :imm t) ; is str1 nil?
          (arm64:b.eq 4)                     ; yes, jump to nil_check (+4)
          (arm64:cmp :x8 #.+nil-value+ :imm t) ; is str2 nil?
          (arm64:b.eq 25)                    ; yes, str1!=nil so return false (+25)
          (arm64:b 5)                        ; both non-nil, skip to compare (+5)
          ;; nil_check: str1 is nil
          (arm64:cmp :x8 #.+nil-value+ :imm t) ; is str2 also nil?
          (arm64:b.ne 22)                    ; no, return false (+22)
          ;; Both are nil, return true
          (arm64:movz :x0 +t-value+)         ; x0 = t
          (arm64:b 21)                       ; jump to end (+21)
          ;; compare: both are valid strings
          (arm64:and* :x2 :x8 -16 :imm t)    ; x2 = str2 & ~0xF (untagged)
          (arm64:and* :x1 :x0 -16 :imm t)    ; x1 = str1 & ~0xF (untagged)
          ;; Load lengths
          (arm64:ldr :x3 :x1 :offset 0)      ; x3 = len1
          (arm64:ldr :x4 :x2 :offset 0)      ; x4 = len2
          ;; Compare lengths
          (arm64:cmp :x3 :x4)                ; cmp len1, len2
          (arm64:b.ne 14)                    ; if len1 != len2, jump to return_false
          ;; Lengths equal, setup for loop
          (arm64:add :x1 :x1 8 :imm t)       ; x1 = str1 data start
          (arm64:add :x2 :x2 8 :imm t)       ; x2 = str2 data start
          (arm64:movz :x4 0)                 ; x4 = 0 (loop counter)
          ;; loop_start:
          (arm64:cmp :x4 :x3)                ; cmp counter, len
          (arm64:b.ge 7)                     ; if counter >= len, return_true
          ;; Load bytes from both strings
          (arm64:ldrb :x5 :x1 :x4 :reg t)    ; x5 = str1[counter]
          (arm64:ldrb :x6 :x2 :x4 :reg t)    ; x6 = str2[counter]
          ;; Compare bytes
          (arm64:cmp :x5 :x6)                ; cmp char1, char2
          (arm64:b.ne 5)                     ; if not equal, return_false
          ;; Increment counter
          (arm64:add :x4 :x4 1 :imm t)       ; x4++
          (arm64:b -7)                       ; back to loop_start (-7)
          ;; return_true:
          (arm64:movz :x0 +t-value+)         ; x0 = t (tagged 1)
          (arm64:b 2)                        ; skip return_false
          ;; return_false:
          (arm64:movz :x0 +nil-value+)       ; x0 = nil
          (linear-save-temp dst))))

      ;; make-string-from-vector: convert vector of char codes to string
      ;; Simplified: only handles vectors, not lists
      (make-string-from-vector
       (let ((dst (cadr instr))
             (vec-temp (caddr instr)))
         (append
          ;; Load vector
          (linear-load-temp :x1 vec-temp)
          (arm64:sub :x1 :x1 #.+tag-vector+ :imm t)           ; untag vector
          ;; x5 = vec length
          (arm64:ldr :x5 :x1 :offset 0)
          ;; GC pre-check
          (gc-trigger-code)
          ;; Allocate string: store length at [x28]
          (arm64:str :x5 :heap :offset 0)
          ;; x4 = alloc size = (8 + len + 15) & ~15
          (arm64:add :x4 :x5 23 :imm t)
          (arm64:and* :x4 :x4 -16 :imm t)
          ;; x0 = string base, bump heap
          (arm64:mov :x0 :heap)
          (arm64:add :heap :heap :x4)
          ;; x2 = string data = x0 + 8
          (arm64:add :x2 :x0 8 :imm t)
          ;; x3 = loop counter = 0
          (arm64:movz :x3 0)
          ;; Loop: copy chars from vector to string
          ;; Offsets: cmp=0, b.ge=1, body=2-8, b=9, exit=10
          (arm64:cmp :x3 :x5)                    ; 0: compare counter with length
          (arm64:b.ge 9)                         ; 1: skip 9 instrs to exit (instr 10)
          ;; Load vec[x3]: address = x1 + 8 + x3*8
          (arm64:lsl :x4 :x3 3 :imm t)           ; 2: x4 = x3 * 8
          (arm64:add :x4 :x4 8 :imm t)           ; 3: x4 = 8 + x3*8
          (arm64:add :x4 :x1 :x4)                ; 4: x4 = vec_base + offset
          (arm64:ldr :x4 :x4 :offset 0)          ; 5: x4 = tagged fixnum
          (arm64:asr :x4 :x4 #.+fixnum-bit+ :imm t)           ; 6: x4 = char value
          (arm64:strb :x4 :x2 :x3 :reg t)        ; 7: [x2 + x3] = x4 (byte)
          (arm64:add :x3 :x3 1 :imm t)           ; 8: x3++
          (arm64:b -9)                           ; 9: back to cmp (instr 0)
          ;; Tag result with string tag
          (arm64:movz :x4 #.+tag-string+)
          (arm64:orr :x0 :x0 :x4)
          (linear-save-temp dst))))

      ;; make-symbol: convert string to symbol (just change tag)
      (make-symbol
       (let ((dst (cadr instr))
             (str-temp (caddr instr)))
         (append (linear-load-temp :x0 str-temp)
                 (arm64:sub :x0 :x0 #.+tag-string+ :imm t)  ; untag string
                 (arm64:add :x0 :x0 #.+tag-symbol+ :imm t)  ; add symbol tag
                 (linear-save-temp dst))))

      ;; load-lambda: create closure on heap
      ;; Format: (load-lambda dst name free-offsets)
      (load-lambda
       (let* ((dst (cadr instr))
              (name (caddr instr))
              (free-offsets (cadddr instr))
              ;; Look up function offset
              (fn-entry (lookup-string name fnoffs)))
         (unless fn-entry
           (error "Function not found in fnoffs: ~A" name))
         (let* ((fn-offset (cdr fn-entry)))
         (if (null free-offsets)
             ;; No captures - simple closure
             (append
              (gc-trigger-code)
              (load-addr-8 :x0 (ash fn-offset 4))
              (arm64:str :x0 :heap :offset 0)
              (arm64:movz :x0 0)              ; nil for empty env
              (arm64:str :x0 :heap :offset 8)
              (arm64:mov :x0 :heap)
              (arm64:add :x0 :x0 #.+tag-closure+ :imm t)    ; closure tag
              (arm64:add :heap :heap 16 :imm t)
              ;; Note: NO post-allocation GC check - x0 is unrooted until saved
              (linear-save-temp dst))
             ;; Has captures - build cons chain of captured values
             (labels ((gen-cons-chain (offs)
                        (if (null offs)
                            (arm64:movz :x0 0)  ; nil
                            (let* ((off (car offs))
                                   (off8 (* off 8))
                                   (rest-code (gen-cons-chain (cdr offs))))
                              (append
                               rest-code
                               ;; Save cdr (rest of list) in heap
                               (arm64:str :x0 :heap :offset 8)
                               ;; Load current value from [env - offset*8]
                               (arm64:sub :x1 :env off8 :imm t)
                               (arm64:ldr :x0 :x1 :offset 0)
                               ;; Store as car
                               (arm64:str :x0 :heap :offset 0)
                               ;; Make cons pointer (tag 0)
                               (arm64:mov :x0 :heap)
                               ;; cons tag is 0, so no add needed
                               (arm64:add :heap :heap 16 :imm t))))))
               (append
                (gc-trigger-code)
                ;; Build captured env cons chain
                (gen-cons-chain free-offsets)
                ;; x0 now has captured env, save to heap+8
                (arm64:str :x0 :heap :offset 8)
                ;; Store fn-offset at heap+0
                (load-addr-8 :x16 (ash fn-offset 4))
                (arm64:str :x16 :heap :offset 0)
                ;; Create closure pointer
                (arm64:mov :x0 :heap)
                (arm64:add :x0 :x0 #.+tag-closure+ :imm t)
                (arm64:add :heap :heap 16 :imm t)
                ;; Note: NO post-allocation GC check - x0 is unrooted until saved
                (linear-save-temp dst)))))))

      ;; buffer-to-string: convert raw byte buffer to string (inline)
      ;; Allocates string on heap, copies bytes from buffer
      (buffer-to-string
       (let ((dst (cadr instr))
             (buf-temp (caddr instr))
             (len-temp (cadddr instr)))
         (append
          ;; Load buf to x1, len to x5 (untagged)
          (linear-load-temp :x1 buf-temp)
          (arm64:sub :x1 :x1 #.+tag-vector+ :imm t)      ; untag vector
          (arm64:add :x1 :x1 8 :imm t)      ; skip length header, x1 = data ptr
          (linear-load-temp :x5 len-temp)
          (arm64:asr :x5 :x5 #.+fixnum-bit+ :imm t)      ; untag length
          ;; GC pre-check
          (gc-trigger-code)
          ;; Allocate string: store length at [x28]
          (arm64:str :x5 :heap :offset 0)
          ;; x4 = alloc size = (8 + len + 15) & ~15
          (arm64:add :x4 :x5 23 :imm t)     ; x4 = len + 23
          (arm64:and* :x4 :x4 -16 :imm t)   ; x4 = (len + 23) & ~15
          ;; x0 = string base, bump heap
          (arm64:mov :x0 :heap)
          (arm64:add :heap :heap :x4)
          ;; x2 = string data base = x0 + 8
          (arm64:add :x2 :x0 8 :imm t)
          ;; x3 = loop counter = 0
          (arm64:movz :x3 0)
          ;; Loop: copy bytes from buf to string
          ;; loop_start: cmp x3, x5; b.ge +6; ldrb; strb; add; b loop_start
          (arm64:cmp :x3 :x5)               ; 0
          (arm64:b.ge 6)                    ; +6 = 24 bytes = 6 instrs
          (arm64:add :x4 :x1 :x3)           ; x4 = buf_data + x3
          (arm64:ldrb :x4 :x4 0)            ; x4 = byte at [x4]
          (arm64:strb :x4 :x2 :x3 :reg t)   ; [x2 + x3] = x4 (register offset)
          (arm64:add :x3 :x3 1 :imm t)      ; x3++
          (arm64:b -6)                      ; back to cmp (-6 = -24 bytes)
          ;; loop_end: tag result with string tag (4)
          (arm64:movz :x4 4)                ; x4 = string tag
          (arm64:orr :x0 :x0 :x4)           ; x0 |= tag
          (linear-save-temp dst))))

      (get-symtab-offset
       ;; Load symtab offset from [x27+112]
       ;; Note: value is ALREADY tagged (pre-shifted << 4) in wrapper storage
       (let ((dst (cadr instr)))
         (append
          (arm64:ldr :x0 :gc :offset 112)    ; load pre-tagged value from x27+112
          (linear-save-temp dst))))

      (otherwise
       (error "codegen-linear-instr: unknown instruction ~S" op)))))

#+sbcl
(defun codegen (linear-ir rtaddrs fnoffs)
  "Generate ARM64 code from linear IR.
   Simple iteration over instructions - no recursion.
   Returns flat list of instruction bytes."
  (setf *linear-labels* nil)
  (setf *linear-fixups* nil)
  (setf *linear-block-info* nil)
  (setf *linear-loop-stack* nil)
  (let ((code-parts nil)  ;; Use push/nreverse for O(n) instead of append
        (offset 0))
    ;; First pass: generate code, record label positions and fixups
    (dolist (instr linear-ir)
      (let ((op (car instr)))
        (cond
          ;; Record label position
          ((eq op 'label)
           (let ((label-name (cadr instr)))
             (push (cons label-name offset) *linear-labels*)))

          ;; Generate code and track fixups for jumps
          ((eq op 'jump)
           (let ((label (cadr instr)))
             (push (cons offset label) *linear-fixups*)
             (let ((instr-code (arm64:b 0)))
               (push instr-code code-parts)
               (incf offset (length instr-code)))))

          ((eq op 'jump-if-nil)
           (let ((src (cadr instr))
                 (label (caddr instr)))
             ;; Generate compare + conditional branch
             (let ((cmp-code (append (linear-load-temp :x0 src)
                                     (arm64:cmp :x0 #.+nil-value+ :imm t))))
               (push cmp-code code-parts)
               (incf offset (length cmp-code)))
             ;; Record fixup for the branch
             (push (cons offset label) *linear-fixups*)
             (let ((branch-code (arm64:b.eq 0)))
               (push branch-code code-parts)
               (incf offset (length branch-code)))))

          ;; Block start - record block info for return-from
          ((eq op 'block-start)
           (let ((block-id (cadr instr))
                 (end-label (caddr instr))
                 (dst-temp (cadddr instr)))
             (push (cons block-id (cons end-label dst-temp)) *linear-block-info*)))

          ;; Return-from - save value to block's dst and jump to end
          ((eq op 'return-from)
           (let* ((block-id (cadr instr))
                  (val-temp (caddr instr))
                  (block-info (cdr (assoc block-id *linear-block-info* :test #'equal)))
                  (end-label (car block-info))
                  (dst-temp (cdr block-info)))
             ;; Generate: load value, save to dst, then branch
             (let ((save-code (append (linear-load-temp :x0 val-temp)
                                      (linear-save-temp dst-temp))))
               (push save-code code-parts)
               (incf offset (length save-code)))
             ;; Record fixup for the jump
             (push (cons offset end-label) *linear-fixups*)
             (let ((branch-code (arm64:b 0)))
               (push branch-code code-parts)
               (incf offset (length branch-code)))))

          ;; Loop start - push label onto loop stack for continue
          ((eq op 'loop-start)
           (let ((loop-label (cadr instr)))
             (push loop-label *linear-loop-stack*)))

          ;; Continue - store-params already done, jump to loop start
          ((eq op 'continue)
           (let ((loop-label (car *linear-loop-stack*)))
             (unless loop-label
               (error "continue outside of loop"))
             ;; Emit jump to loop start (fixup later)
             (push (cons offset loop-label) *linear-fixups*)
             (let ((branch-code (arm64:b 0)))
               (push branch-code code-parts)
               (incf offset (length branch-code)))))

          ;; Store-param - store temp value back to parameter slot for TCO
          ((eq op 'store-param)
           (let* ((src-temp (cadr instr))
                  (param-idx (caddr instr))
                  ;; Parameters are at env - idx*8 (same as load-var)
                  (param-offset (* param-idx 8)))
             ;; Load from temp, calculate param address, store
             (let ((store-code (append (linear-load-temp :x0 src-temp)
                                       (arm64:sub :x1 :env param-offset :imm t)
                                       (arm64:str :x0 :x1 :offset 0))))
               (push store-code code-parts)
               (incf offset (length store-code)))))

          ;; Normal instruction
          (t
           (let ((instr-code (codegen-linear-instr instr rtaddrs fnoffs)))
             (when instr-code
               (push instr-code code-parts)
               (incf offset (length instr-code))))))))

    ;; Flatten code-parts: reverse and flatten into single list
    (let ((code (apply #'append (nreverse code-parts))))
      ;; Second pass: fix up branch offsets
      (dolist (fixup *linear-fixups*)
        (let* ((branch-offset (car fixup))
               (label (cdr fixup))
               (target-offset (cdr (assoc label *linear-labels*))))
          (unless target-offset
            (error "codegen: unresolved branch to label ~S (fixups: ~S, labels: ~S)"
                   label *linear-fixups* *linear-labels*))
          ;; Calculate relative offset in instructions (bytes / 4)
          (let ((rel-offset (ash (- target-offset branch-offset) -2)))
              ;; Patch the branch instruction
              (let ((old-instr (logior (ash (elt code branch-offset) 0)
                                      (ash (elt code (+ branch-offset 1)) 8)
                                      (ash (elt code (+ branch-offset 2)) 16)
                                      (ash (elt code (+ branch-offset 3)) 24))))
                ;; Check if conditional branch (B.cond) vs unconditional (B/BL)
                ;; B.cond has opcode 0x54 in bits [31:24]
                (let ((new-instr
                        (if (= (logand (ash old-instr -24) #xFF) #x54)
                            ;; B.cond: imm19 in bits [23:5], cond in bits [3:0]
                            (logior (logand old-instr #xFF00001F)  ; keep opcode and cond
                                    (ash (logand rel-offset #x7FFFF) 5))  ; imm19 << 5
                            ;; B/BL: imm26 in bits [25:0]
                            (logior (logand old-instr #xFC000000)
                                    (logand rel-offset #x3FFFFFF)))))
                  (setf (elt code branch-offset) (logand new-instr #xFF))
                  (setf (elt code (+ branch-offset 1)) (logand (ash new-instr -8) #xFF))
                  (setf (elt code (+ branch-offset 2)) (logand (ash new-instr -16) #xFF))
                  (setf (elt code (+ branch-offset 3)) (logand (ash new-instr -24) #xFF)))))))

      code)))

;;; ============================================================
;;; Main Codegen Function (handles all IR nodes)
;;; ============================================================

;;; ============================================================
;;; Prologue and Epilogue
;;; ============================================================

;;; Dynamic function prologue/epilogue with sized frames.
;;; Small functions get small frames to allow deeper recursion.

(defconstant +fn-header-size+ 64
  "Fixed overhead for saved registers: fp, lr, x19-x24, x26 = 8 × 8 bytes")

(defconstant +min-env-slots+ 16
  "Minimum environment slots (128 bytes) for small functions")

(defvar *current-frame-layout* nil
  "Current function's frame layout, set during prologue for epilogue/spills to use")

;;; Frame Layout Calculation
;;;
;;; The frame-layout ADT is the SINGLE SOURCE OF TRUTH for all frame offsets.
;;; This function computes it once; all codegen uses the accessors.

(defun make-frame-layout (spill-count env-slots)
  "Calculate frame layout based on actual needs.
   Returns a frame-layout ADT instance.

   Layout (16-byte aligned):
     sp+0:           callee-saved (x19,x20,x21,x22,x23,x24,x26) = 64 bytes
     sp+64:          spill slots (spill-count × 8 bytes)
     sp+env-base:    x20 points here; env slots use NEGATIVE offsets from x20
                     (env[-8] = first slot, env[-16] = second, etc.)
     sp+fp-offset:   saved frame pointer
     sp+lr-offset:   saved link register
     sp+frame-size:  original sp

   IMPORTANT: env-base must be high enough that env[-N*8] doesn't overlap
   with spill slots. Since env uses negative offsets, we place env-base
   at spill_end + env_bytes, so env[-env_bytes] = spill_end."
  (let* ((callee-base 0)
         (callee-size 72)  ; 9 slots: unused(0,8), x19(16), x20(24), x21(32), x22(40), x23(48), x24(56), x26(64)
         (spill-base (+ callee-base callee-size))
         (spill-bytes (* spill-count 8))
         (actual-env-slots (if (> env-slots +min-env-slots+) env-slots +min-env-slots+))
         (env-bytes (* actual-env-slots 8))
         ;; env-base must be high enough for negative env offsets
         ;; env[-env-bytes] should be at spill-base + spill-bytes (no overlap)
         (env-base (+ spill-base spill-bytes env-bytes))
         (fp-offset (+ env-base 8))  ; fp right after env-base
         (lr-offset (+ fp-offset 8))
         (raw-size (+ lr-offset 8))
         ;; Round up to 16-byte alignment
         (frame-size (logand (+ raw-size 15) (lognot 15))))
    (let ((layout (fl-layout frame-size fp-offset lr-offset
                              callee-base callee-size
                              spill-base spill-count
                              env-base actual-env-slots)))
      ;; Validate at construction time - crashes early on frame slot collision
      (validate-frame-layout layout)
      layout)))

(defun frame-spill-offset (layout slot)
  "Get offset for spill slot N from frame layout."
  (+ (fl-layout-spill-base layout) (* slot 8)))

;;; Fixed 16KB frame layout (legacy compatibility)
(defvar *fixed-16k-layout*
  (fl-layout #x4000      ; frame-size
             #x3FF0      ; fp-offset
             #x3FF8      ; lr-offset
             0           ; callee-base
             72          ; callee-size (9 slots: unused(0,8), x19-x24, x26)
             72          ; spill-base (0x48)
             479         ; spill-count (adjusted for new callee-size)
             #x3F80      ; env-base
             16)         ; env-slots
  "Pre-computed layout for legacy 16KB frames")

;;; Typed Prologue/Epilogue - use frame-layout ADT for all offsets
;;;
;;; These are the ONLY functions that should generate prologue/epilogue code.
;;; They take a frame-layout and use its accessors, ensuring consistency.

(defun gen-prologue (layout)
  "Generate function prologue from frame-layout.
   Sets *current-frame-layout* for epilogue and spill access."
  (setf *current-frame-layout* layout)
  (let ((frame-size (fl-layout-frame-size layout))
        (fp-offset (fl-layout-fp-offset layout))
        (lr-offset (fl-layout-lr-offset layout))
        (env-base (fl-layout-env-base layout)))
    (if (<= frame-size 4095)
        ;; Small frame - single SUB, direct offsets
        (append
         (arm64:sub :sp :sp frame-size :imm t)
         (arm64:stp :x19 :env :sp :offset 16)
         (arm64:stp :x21 :x22 :sp :offset 32)
         (arm64:stp :x23 :closure :sp :offset 48)
         (arm64:str :code-base :sp :offset 64)
         (arm64:str :fp :sp :offset fp-offset)
         (arm64:str :lr :sp :offset lr-offset)
         (arm64:add :fp :sp fp-offset :imm t)
         (arm64:add :env :sp env-base :imm t))
        ;; Large frame - use shifted immediate plus optional remainder
        (let ((shift-val (ash frame-size -12))
              (rem (logand frame-size #xFFF)))
          (append
           ;; Subtract 4K-aligned part
           (when (> shift-val 0)
             (arm64:sub :sp :sp shift-val :imm t :shift12 t))
           ;; Subtract remainder if any
           (when (> rem 0)
             (arm64:sub :sp :sp rem :imm t))
           ;; Save callee-saved registers at low offsets
           (arm64:stp :x19 :env :sp :offset 16)
           (arm64:stp :x21 :x22 :sp :offset 32)
           (arm64:stp :x23 :closure :sp :offset 48)
           (arm64:str :code-base :sp :offset 64)
           ;; fp/lr at their computed offsets
           (gen-store-at-large-offset :fp fp-offset)
           (gen-store-at-large-offset :lr lr-offset)
           ;; Set fp to point to fp slot
           (gen-add-large-offset :fp :sp fp-offset)
           ;; Set env to point to env area
           (gen-add-large-offset :env :sp env-base))))))

(defun gen-epilogue (layout)
  "Generate function epilogue from frame-layout."
  (let ((frame-size (fl-layout-frame-size layout))
        (fp-offset (fl-layout-fp-offset layout))
        (lr-offset (fl-layout-lr-offset layout)))
    (if (<= frame-size 4095)
        (append
         (arm64:ldr :code-base :sp :offset 64)
         (arm64:ldp :x23 :closure :sp :offset 48)
         (arm64:ldp :x21 :x22 :sp :offset 32)
         (arm64:ldp :x19 :env :sp :offset 16)
         (arm64:ldr :fp :sp :offset fp-offset)
         (arm64:ldr :lr :sp :offset lr-offset)
         (arm64:add :sp :sp frame-size :imm t)
         (arm64:ret))
        (let ((shift-val (ash frame-size -12))
              (rem (logand frame-size #xFFF)))
          (append
           (arm64:ldr :code-base :sp :offset 64)
           (arm64:ldp :x23 :closure :sp :offset 48)
           (arm64:ldp :x21 :x22 :sp :offset 32)
           (arm64:ldp :x19 :env :sp :offset 16)
           (gen-load-from-large-offset :fp fp-offset)
           (gen-load-from-large-offset :lr lr-offset)
           ;; Restore sp: add remainder first, then shifted part
           (when (> rem 0)
             (arm64:add :sp :sp rem :imm t))
           (when (> shift-val 0)
             (arm64:add :sp :sp shift-val :imm t :shift12 t))
           (arm64:ret))))))

;;; Helpers for large offset addressing
(defun gen-store-at-large-offset (reg offset)
  "Generate store to [sp + large-offset] using x8 as scratch."
  (let ((shift (ash offset -12))
        (rem (logand offset #xFFF)))
    (if (= 0 shift)
        (arm64:str reg :sp :offset offset)
        (append
         (arm64:add :x8 :sp shift :imm t :shift12 t)
         (arm64:str reg :x8 :offset rem)))))

(defun gen-load-from-large-offset (reg offset)
  "Generate load from [sp + large-offset] using x8 as scratch."
  (let ((shift (ash offset -12))
        (rem (logand offset #xFFF)))
    (if (= 0 shift)
        (arm64:ldr reg :sp :offset offset)
        (append
         (arm64:add :x8 :sp shift :imm t :shift12 t)
         (arm64:ldr reg :x8 :offset rem)))))

(defun gen-add-large-offset (dest base offset)
  "Generate dest = base + large-offset."
  (let ((shift (ash offset -12))
        (rem (logand offset #xFFF)))
    (if (= 0 shift)
        (arm64:add dest base offset :imm t)
        (if (= 0 rem)
            (arm64:add dest base shift :imm t :shift12 t)
            (append
             (arm64:add dest base shift :imm t :shift12 t)
             (arm64:add dest dest rem :imm t))))))

(defun fn-fixed-prologue-internal ()
  "Internal: Generate 16KB frame prologue (legacy fallback)"
  (setf *current-frame-size* #x4000)
  (append
   (arm64:sub :sp :sp #x4 :imm t :shift12 t)
   (arm64:str :fp :sp :offset #x3FF0)
   (arm64:str :lr :sp :offset #x3FF8)
   (arm64:add :fp :sp #x3 :imm t :shift12 t)
   (arm64:add :fp :fp #xFF0 :imm t)
   (arm64:stp :x19 :env :sp :offset 16)
   (arm64:stp :x21 :x22 :sp :offset 32)
   (arm64:stp :x23 :closure :sp :offset 48)
   (arm64:str :code-base :sp :offset 64)
   (arm64:add :env :sp #x3 :imm t :shift12 t)
   (arm64:add :env :env #xF80 :imm t)))

(defun fn-fixed-epilogue-internal ()
  "Internal: Generate 16KB frame epilogue (legacy fallback)"
  (append
   (arm64:ldr :code-base :sp :offset 64)
   (arm64:ldp :x23 :closure :sp :offset 48)
   (arm64:ldp :x21 :x22 :sp :offset 32)
   (arm64:ldp :x19 :env :sp :offset 16)
   (arm64:ldr :fp :sp :offset #x3FF0)
   (arm64:ldr :lr :sp :offset #x3FF8)
   (arm64:add :sp :sp #x4 :imm t :shift12 t)
   (arm64:ret)))

(defun fn-fixed-prologue (&optional (spill-count 0) (env-slots 32))
  "Generate function prologue.
   When spill-count=0 and env-slots=32, uses fixed 16KB layout for compatibility.
   Otherwise computes a dynamic layout based on actual needs."
  (if (and (= spill-count 0) (= env-slots 32))
      ;; Use fixed 16KB layout for backward compatibility
      (gen-prologue *fixed-16k-layout*)
      ;; Compute dynamic layout
      (gen-prologue (make-frame-layout spill-count env-slots))))

(defun fn-fixed-epilogue ()
  "Generate function epilogue matching fn-fixed-prologue.
   Uses *current-frame-layout* set by gen-prologue."
  (gen-epilogue (or *current-frame-layout* *fixed-16k-layout*)))

;;; ============================================================
;;; Function Codegen
;;; ============================================================

(defun codegen-fn (fn rtaddrs fnoffs)
  "Generate code for a function using register allocation.
   Accepts two formats:
   - Native: (name params body-ir param-base) - 4 elements
   - SBCL:   (name params body-ir free-vars free-offsets) - 5 elements
   For SBCL format, param-base = (length free-vars).

   NOTE: Only uses register-allocated codegen. No fallback."
  (declare (ignore rtaddrs fnoffs))
  (let ((reg-alloc-code (codegen-fn-reg-alloc fn)))
    (if reg-alloc-code
        reg-alloc-code
        (error "codegen-fn-reg-alloc failed for ~A - unsupported IR in body" (car fn)))))

;;; Dead code - unused
#+sbcl
(defun resolve-tco-branches (code loop-label-offset)
  "Resolve :tco-branch markers into actual B (unconditional branch) instructions.
   Each marker is (:tco-branch loop-label-offset) and needs to become a backward branch.
   Preserves :call-fn, :tail-call-fn, and :extern-call markers for later resolution.
   Returns flattened code with TCO markers replaced by B instructions."
  (labels ((emit-b-back (offset)
             ;; B instruction: 0x14 | (imm26 & 0x3FFFFFF)
             ;; offset is in bytes, convert to instructions (divide by 4)
             ;; and it's negative (backward branch)
             (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
                    (b-instr (logior #x14000000 imm26)))
               (list (logand (ash b-instr -24) #xFF)
                     (logand (ash b-instr -16) #xFF)
                     (logand (ash b-instr -8) #xFF)
                     (logand b-instr #xFF))))
           (marker-p (item)
             ;; Check if item is a marker that should be preserved
             (and (consp item)
                  (or (eq (car item) :call-fn)
                      (eq (car item) :tail-call-fn)
                      (eq (car item) :extern-call)
                      (eq (car item) :loop-start)
                      (eq (car item) :loop-continue)
                      (eq (car item) :block-start)
                      (eq (car item) :block-end)
                      (eq (car item) :return-from))))
           (process (items pos acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ;; TCO branch marker - replace with B instruction
                     ((and (consp item) (eq (car item) :tco-branch))
                      ;; Calculate backward offset: target - current
                      ;; target = loop-label-offset, current = pos + 4 (after this instruction)
                      ;; But we're at pos, so offset = loop-label-offset - pos
                      (let* ((offset (- loop-label-offset pos))
                             (b-bytes (emit-b-back offset)))
                        (process (cdr items) (+ pos 4) (append (reverse b-bytes) acc))))
                     ;; :continue marker (from linear codegen) - same as :tco-branch
                     ((and (consp item) (eq (car item) :continue))
                      (let* ((offset (- loop-label-offset pos))
                             (b-bytes (emit-b-back offset)))
                        (process (cdr items) (+ pos 4) (append (reverse b-bytes) acc))))
                     ;; Call/extern markers - preserve as single items (4 bytes each)
                     ((marker-p item)
                      (process (cdr items) (+ pos 4) (cons item acc)))
                     ;; Nested list - flatten recursively
                     ((consp item)
                      (let* ((flattened (process item 0 nil))
                             (size (length flattened)))
                        (process (cdr items) (+ pos size)
                                 (append (reverse flattened) acc))))
                     ;; Regular byte
                     (t
                      (process (cdr items) (+ pos 1) (cons item acc))))))))
    (process code 0 nil)))

;;; Dead code - replaced by gen-capture-loads-reg in reg-alloc.lisp
#+sbcl
(defun gen-capture-loads (num-captures)
  "Generate code to load captured values from x24 cons list into env slots.
   x24 = (v0 . (v1 . (v2 . nil))) - load into offsets 0, 1, 2, etc."
  (labels ((gen-loads (idx acc)
             (if (>= idx num-captures)
                 acc
                 (let* ((offset (* idx 8))
                        ;; x24 points to current cons cell
                        ;; Load car into x9, store at [x20 - offset*8]
                        ;; Then advance: x24 = cdr(x24)
                        (load-car (append
                                   (arm64:sub :x16 :closure 1 :imm t)      ; untag cons
                                   (arm64:ldr :x16 :x16 :offset 0)))  ; x9 = car
                        (store-env (append
                                    (arm64:sub :x10 :env offset :imm t)
                                    (arm64:str :x16 :x10 :offset 0))) ; [x20-off] = x9
                        (advance (append
                                  (arm64:sub :x16 :closure 1 :imm t)       ; untag cons
                                  (arm64:ldr :closure :x16 :offset 8)))) ; x24 = cdr
                   (gen-loads (+ idx 1)
                              (append-all (list acc load-car store-env advance)))))))
    (gen-loads 0 nil)))

;;; Old fnoffs builder (SBCL only - uses code-size)
#+sbcl
(defun build-fnoffs-pass (fns offset fnoffs acc)
  "Build function offset table: ((name-string . byte-offset) ...)
   Names are normalized to strings at this boundary.
   Uses fnoffs for accurate size calculation (may be nil for first pass)."
  (if (null fns)
      (reverse acc)
      (let* ((fn (car fns))
             (name-str (normalize-fn-name (car fn)))  ; Normalize at creation
             (code (codegen-fn fn nil fnoffs))
             (size (code-size code))
             (entry (cons name-str offset)))
        (build-fnoffs-pass (cdr fns) (+ offset size) fnoffs (cons entry acc)))))

#+sbcl
(defun fnoffs-equal (a b)
  "Compare two fnoffs tables for equality"
  (cond
    ((and (null a) (null b)) t)
    ((or (null a) (null b)) nil)
    (t (let ((ea (car a))
             (eb (car b)))
         (if (and (equal (car ea) (car eb))
                  (= (cdr ea) (cdr eb)))
             (fnoffs-equal (cdr a) (cdr b))
             nil)))))

#+sbcl
(defun build-fnoffs (fns offset)
  "Build function offset table with iteration until stable.
   Code size depends on function offsets (load-addr size varies),
   so we iterate until the table stabilizes."
  (labels ((iterate (prev-fnoffs iterations)
             (if (> iterations 10)
                 prev-fnoffs  ; Safety limit
                 (let ((new-fnoffs (build-fnoffs-pass fns offset prev-fnoffs nil)))
                   (if (fnoffs-equal prev-fnoffs new-fnoffs)
                       new-fnoffs
                       (iterate new-fnoffs (+ iterations 1)))))))
    ;; First pass with nil fnoffs, then iterate
    (let ((first-pass (build-fnoffs-pass fns offset nil nil)))
      (iterate first-pass 1))))

;;; ============================================================
;;; Resolve Calls (simple version without function linking)
;;; ============================================================

#+sbcl
(defun resolve-calls-simple (code)
  "Simple resolve - just flatten the code list.
   For now, this just removes the :call-fn and :extern-call markers.
   Full version needs function offset table."
  (labels ((flatten (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item) (or (eq (car item) :call-fn)
                                              (eq (car item) :extern-call)))
                       ;; Keep extern-call markers for later processing
                       (if (eq (car item) :extern-call)
                           (flatten (cdr items) (cons item acc))
                           ;; Regular call - emit placeholder for now
                           (flatten (cdr items)
                                    (append (reverse (arm64:bl 0)) acc)))
                       (if (consp item)
                           (flatten (cdr items) (append (reverse item) acc))
                           (flatten (cdr items) (cons item acc))))))))
    (flatten code nil)))

;;; ============================================================
;;; Delivery Functions
;;; ============================================================

#+sbcl
(defun deliver (source output-path &optional (heap-size #x4000000))
  "Compile source string to native executable.
   Heap is pre-allocated in __DATA segment (mapped by dyld at load time).
   HEAP-SIZE: heap size in bytes (default 64MB).
   Supports: defun, lambda, funcall, GC runtime."
  #-sbcl (register-compiler-symbols)
  (reset-symbol-table)
  (reset-lambda-counter)
  #+sbcl (reset-compile-warnings)
  (let* ((forms (read-all source))
         (result (compile-forms forms))
         (defuns-orig (car result))
         (main-ir-orig (cadr result))
         (wrapper-size +heap-wrapper-size+)  ;; from macho.lisp - single source of truth
         ;; Lift lambdas from main-ir
         (main-lift-result #+sbcl (lift-lambdas-2 main-ir-orig nil)
                           #-sbcl (lift-lambdas main-ir-orig nil))
         (main-ir (car main-lift-result))
         (main-lambdas (cdr main-lift-result))
         ;; Lift lambdas from defun bodies
         (defun-lift-result (lift-lambdas-from-defuns defuns-orig nil nil))
         (defuns (car defun-lift-result))
         (defun-lambdas (cdr defun-lift-result))
         ;; Combine all lambdas
         (all-lambdas (append main-lambdas defun-lambdas)))

    ;; Full compilation path with GC runtime
    (let* ((lambda-as-defuns (lambdas-to-defuns all-lambdas nil))
           (all-fns-raw (append defuns lambda-as-defuns))
           ;; Apply TCO to all functions (converts self-tail-calls to loops)
           (all-fns (apply-tco-to-all-functions all-fns-raw))
           ;; Link-time verification: check all call-fn targets resolve
           (_ #+sbcl (when (and (not *skip-link-verification*)
                                (verify-link-references (mapcar #'car all-fns)))
                       (error "Link failed: undefined function references")))
           ;; Generate main code using linear codegen (simpler, well-tested)
           (main-linear (linearize main-ir))
           (main-code-temp (append-all
                            (list (fn-fixed-prologue)
                                  (codegen main-linear nil nil)
                                  (fn-fixed-epilogue))))
           (main-size (code-size main-code-temp))
           ;; Build fnoffs starting after main code
           (fnoffs (build-fnoffs all-fns main-size))
           ;; Regenerate main with fnoffs
           (main-code (append-all
                       (list (fn-fixed-prologue)
                             (codegen main-linear nil fnoffs)
                             (fn-fixed-epilogue))))
           ;; Generate all function code
           (fn-code (codegen-all-fns all-fns nil fnoffs nil))
           ;; Generate GC runtime code
           (gc-code (gc-runtime-code))
           ;; Combine all code
           (all-code (append main-code fn-code gc-code))
           ;; Flatten with markers
           (bytes-with-markers (flatten-code-keep-markers-and-calls all-code))
           ;; Collect extern calls
           (extern-calls (collect-extern-calls bytes-with-markers))
           (imports-raw (get-unique-imports extern-calls))
           ;; Always ensure _exit is in imports (wrapper needs it)
           (imports (if (member "_exit" imports-raw :test #'string=)
                        imports-raw
                        (cons "_exit" imports-raw)))
           ;; Calculate layout
           (code-offset #x400)
           (exact-flat-size (count-actual-bytes bytes-with-markers))
           (stubs-total (* (length imports) 12))
           (stubs-offset-unaligned (+ code-offset wrapper-size exact-flat-size))
           (stubs-offset (* (ceiling stubs-offset-unaligned 4) 4))
           (stub-size 12)
           (stubs-end (+ stubs-offset stubs-total))
           ;; Build stub alist
           (stub-alist (build-stub-alist imports stubs-offset stub-size))
           ;; Convert fnoffs to byte addresses
           (fn-addr-base (+ code-offset wrapper-size))
           (fn-alist-base (build-fn-addr-alist fnoffs fn-addr-base nil))
           ;; Extract GC function labels
           (gc-fn-alist (extract-fn-labels bytes-with-markers fn-addr-base))
           ;; Merge user functions + GC functions
           (fn-alist (append fn-alist-base gc-fn-alist))
           ;; Flatten all call markers
           (flatten-result (flatten-all-calls bytes-with-markers fn-alist stub-alist fn-addr-base))
           (flat-code (car flatten-result))
           ;; Build all-fnoffs early (needed for symtab)
           (all-fnoffs (append fnoffs
                               (mapcar (lambda (entry)
                                         (cons (car entry)
                                               (- (cdr entry) fn-addr-base)))
                                       gc-fn-alist)))
           ;; Emit symbol table bytes - MUST be before heap-page-offset calculation
           (symtab-bytes (emit-symbol-table all-fnoffs main-size))
           (symtab-size (length symtab-bytes))
           ;; Calculate heap page offset for ADRP instruction
           ;; MUST include symtab-size since build-macho adds symtab after stubs
           (text-content-end (+ stubs-end symtab-size))
           (text-vmsize (* (ceiling text-content-end #x4000) #x4000))
           (text-pages-4kb (/ text-vmsize #x1000))
           (heap-page-offset (+ text-pages-4kb 4))  ;; +4 for __DATA_CONST pages (16KB = 4 pages)
           ;; Calculate symtab offset relative to user code start (x26)
           ;; x26 points to user code start (code-offset + wrapper-size)
           ;; symtab is at stubs-end
           (symtab-offset (- stubs-end (+ code-offset wrapper-size)))
           (symtab-count (1+ (length all-fnoffs)))  ; +1 for _main entry
           ;; Append symtab to code (after stubs will be added by linker)
           (code-with-symtab flat-code)  ; symtab goes after stubs in linker
           ;; Wrap with __DATA segment heap initialization
           (wrapped-code (wrap-bytecode-with-heap-for-imports code-with-symtab
                                                               heap-page-offset
                                                               symtab-offset
                                                               symtab-count)))

      ;; Write executable with imports, heap, and symbol table
      (write-macho-executable-with-imports-and-heap output-path wrapped-code imports heap-size
                                                    all-fnoffs symtab-bytes)
      #+sbcl (write-symbol-map output-path all-fnoffs main-size imports stubs-offset)
      ;; Extract and write debug info (nanopass)
      #+sbcl (let* ((debug-vars (extract-debug-vars all-fns))
                    (debug-table (emit-debug-table debug-vars all-fnoffs)))
               (write-debug-info output-path debug-vars debug-table)))))

#+sbcl
(defun deliver-file (source-path output-path &optional (heap-size #x4000000))
  "Compile Lisp file to native executable.
   Usage: (habu:deliver-file \"program.lisp\" \"program\")"
  (deliver (native-read-file source-path) output-path heap-size))

#+sbcl
(defun deliver-forms (forms output-path &optional (heap-size #x4000000))
  "Compile pre-parsed forms to native executable.
   Use when you need SBCL's reader for package-qualified symbols.
   Usage: (habu:deliver-forms (read-forms-with-sbcl source) \"output\")

   This is identical to deliver but takes pre-parsed forms instead of source string."
  (reset-symbol-table)
  (reset-lambda-counter)
  (reset-compile-warnings)
  (let* ((result (compile-forms forms))
         (defuns-orig (car result))
         (main-ir-orig (cadr result))
         (wrapper-size +heap-wrapper-size+)
         ;; Lift lambdas from main-ir
         (main-lift-result (lift-lambdas-2 main-ir-orig nil))
         (main-ir (car main-lift-result))
         (main-lambdas (cdr main-lift-result))
         ;; Lift lambdas from defun bodies
         (defun-lift-result (lift-lambdas-from-defuns defuns-orig nil nil))
         (defuns (car defun-lift-result))
         (defun-lambdas (cdr defun-lift-result))
         ;; Combine all lambdas
         (all-lambdas (append main-lambdas defun-lambdas)))

    ;; Full compilation path with GC runtime
    (let* ((lambda-as-defuns (lambdas-to-defuns all-lambdas nil))
           (all-fns-raw (append defuns lambda-as-defuns))
           ;; Apply TCO to all functions
           (all-fns (apply-tco-to-all-functions all-fns-raw))
           ;; Link-time verification
           (_ #+sbcl (when (verify-link-references (mapcar #'car all-fns))
                       (error "Link failed: undefined function references")))
           ;; Generate main code using linear codegen
           (main-linear (linearize main-ir))
           (main-code-temp (append-all
                            (list (fn-fixed-prologue)
                                  (codegen main-linear nil nil)
                                  (fn-fixed-epilogue))))
           (main-size (code-size main-code-temp))
           ;; Build fnoffs starting after main code
           (fnoffs (build-fnoffs all-fns main-size))
           ;; Regenerate main with fnoffs
           (main-code (append-all
                       (list (fn-fixed-prologue)
                             (codegen main-linear nil fnoffs)
                             (fn-fixed-epilogue))))
           ;; Generate all function code
           (fn-code (codegen-all-fns all-fns nil fnoffs nil))
           ;; Generate GC runtime code
           (gc-code (gc-runtime-code))
           ;; Combine all code
           (all-code (append main-code fn-code gc-code))
           ;; Flatten with markers
           (bytes-with-markers (flatten-code-keep-markers-and-calls all-code))
           ;; Collect extern calls
           (extern-calls (collect-extern-calls bytes-with-markers))
           (imports-raw (get-unique-imports extern-calls))
           ;; Always ensure _exit is in imports
           (imports (if (member "_exit" imports-raw :test #'string=)
                        imports-raw
                        (cons "_exit" imports-raw)))
           ;; Calculate layout
           (code-offset #x400)
           (exact-flat-size (count-actual-bytes bytes-with-markers))
           (stubs-total (* (length imports) 12))
           (stubs-offset-unaligned (+ code-offset wrapper-size exact-flat-size))
           (stubs-offset (* (ceiling stubs-offset-unaligned 4) 4))
           (stub-size 12)
           (stubs-end (+ stubs-offset stubs-total))
           ;; Build stub alist
           (stub-alist (build-stub-alist imports stubs-offset stub-size))
           ;; Convert fnoffs to byte addresses
           (fn-addr-base (+ code-offset wrapper-size))
           (fn-alist-base (build-fn-addr-alist fnoffs fn-addr-base nil))
           ;; Extract GC function labels
           (gc-fn-alist (extract-fn-labels bytes-with-markers fn-addr-base))
           ;; Merge user functions + GC functions
           (fn-alist (append fn-alist-base gc-fn-alist))
           ;; Flatten all call markers
           (flatten-result (flatten-all-calls bytes-with-markers fn-alist stub-alist fn-addr-base))
           (flat-code (car flatten-result))
           ;; Build all-fnoffs (needed for symtab)
           (all-fnoffs (append fnoffs
                               (mapcar (lambda (entry)
                                         (cons (car entry)
                                               (- (cdr entry) fn-addr-base)))
                                       gc-fn-alist)))
           ;; Emit symbol table bytes
           (symtab-bytes (emit-symbol-table all-fnoffs main-size))
           (symtab-size (length symtab-bytes))
           ;; Calculate heap page offset
           (text-content-end (+ stubs-end symtab-size))
           (text-vmsize (* (ceiling text-content-end #x4000) #x4000))
           (text-pages-4kb (/ text-vmsize #x1000))
           (heap-page-offset (+ text-pages-4kb 4))
           ;; Calculate symtab offset
           (symtab-offset (- stubs-end (+ code-offset wrapper-size)))
           (symtab-count (1+ (length all-fnoffs)))
           ;; Wrap with heap initialization
           (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code
                                                               heap-page-offset
                                                               symtab-offset
                                                               symtab-count)))

      ;; Write executable
      (write-macho-executable-with-imports-and-heap output-path wrapped-code imports heap-size
                                                    all-fnoffs symtab-bytes)
      #+sbcl (write-symbol-map output-path all-fnoffs main-size imports stubs-offset)
      #+sbcl (let* ((debug-vars (extract-debug-vars all-fns))
                    (debug-table (emit-debug-table debug-vars all-fnoffs)))
               (write-debug-info output-path debug-vars debug-table)))))

#+sbcl
(defun write-symbol-map (output-path fnoffs main-size imports stubs-offset)
  "Write a symbol map file for debugging.
   Format: HEX_OFFSET NAME (one per line)
   HEX_OFFSET is relative to __TEXT segment start (0x100000000 on macOS).
   To find function from PC: offset = PC - 0x100000468 (VM_BASE + code_offset + wrapper)"
  (let ((map-path (concatenate 'string output-path ".map"))
        (wrapper-size +heap-wrapper-size+)  ;; from macho.lisp - single source of truth
        (code-offset #x400))
    (with-open-file (f map-path :direction :output :if-exists :supersede)
      ;; Header comment
      (format f ";; Symbol map for ~A~%" output-path)
      (format f ";; PC to offset: (PC - 0x1000003E8) for functions~%")
      (format f ";; Offset is relative to code start (after wrapper)~%~%")
      ;; Main entry
      (format f "0x~8,'0X _main~%" (+ code-offset wrapper-size))
      (format f "0x~8,'0X _main_end~%" (+ code-offset wrapper-size main-size))
      ;; Functions from fnoffs (names are already strings)
      (dolist (entry fnoffs)
        (let* ((name-str (car entry))  ; Already normalized
               (offset (cdr entry))
               (abs-offset (+ code-offset wrapper-size offset)))
          (format f "0x~8,'0X ~A~%" abs-offset name-str)))
      ;; Import stubs
      (let ((stub-off stubs-offset))
        (dolist (imp imports)
          (format f "0x~8,'0X stub_~A~%" stub-off imp)
          (setf stub-off (+ stub-off 12))))
      (format t "Symbol map written to ~A~%" map-path))))

;;; ============================================================
;;; Embedded Symbol Table for Runtime Symbolication
;;; ============================================================

#+sbcl
(defun emit-symbol-table (fnoffs main-size)
  "Emit symbol table bytes for embedding in binary.
   Returns a list of bytes containing:
   - u64 count (number of entries)
   - For each entry (sorted by offset):
     - u64 offset (relative to wrapper end = user code start)
     - u64 name_len
     - name bytes (null-terminated, padded to 8)
   The table includes _main at offset 0.
   fnoffs names are already normalized to strings."
  (declare (ignore main-size))
  (let* ((sorted-entries (sort (copy-list fnoffs) #'< :key #'cdr))
         ;; Add _main entry at the beginning (use string, not symbol)
         (all-entries (cons (cons "MAIN" 0) sorted-entries))
         (count (length all-entries)))
    (labels ((u64-bytes (val)
               (list (logand val #xFF)
                     (logand (ash val -8) #xFF)
                     (logand (ash val -16) #xFF)
                     (logand (ash val -24) #xFF)
                     (logand (ash val -32) #xFF)
                     (logand (ash val -40) #xFF)
                     (logand (ash val -48) #xFF)
                     (logand (ash val -56) #xFF)))
             (name-bytes (name-str)
               ;; name-str is already a string
               (let* ((chars (loop for c across name-str collect (char-code c)))
                      (len (length chars))
                      (padded-len (* (ceiling (+ len 1) 8) 8)))  ; +1 for null, pad to 8
                 (append chars
                         (make-list (- padded-len len) :initial-element 0))))
             (emit-entry (entry)
               (let* ((name-str (car entry))  ; Already normalized
                      (offset (cdr entry)))
                 (append (u64-bytes offset)
                         (u64-bytes (length name-str))
                         (name-bytes name-str)))))
      (append (u64-bytes count)
              (apply #'append (mapcar #'emit-entry all-entries))))))

#+sbcl
(defun symbol-table-size (fnoffs)
  "Calculate the size of the embedded symbol table in bytes.
   fnoffs names are already normalized to strings."
  (let ((count (1+ (length fnoffs))))  ; +1 for _main
    (labels ((entry-size (entry)
               (let* ((name-str (car entry))  ; Already normalized
                      (len (length name-str))
                      (padded-len (* (ceiling (+ len 1) 8) 8)))
                 (+ 8 8 padded-len))))  ; offset + name_len + padded_name
      (+ 8  ; count
         16 ; _main entry (8+8+8 padded "MAIN\0\0\0\0")
         (reduce #'+ (mapcar #'entry-size fnoffs) :initial-value 0)))))

(defun count-actual-bytes (items)
  "Count actual bytes in a flattened list, excluding markers.
   Markers are conses like (:extern-call ...), (:fn-label ...), etc.
   Note: placeholder zeros for call markers are already in the list."
  (labels ((count-bytes (lst acc)
             (if (null lst)
                 acc
                 (let ((item (car lst)))
                   (if (consp item)
                       ;; Marker - don't count (placeholder zeros already counted)
                       (count-bytes (cdr lst) acc)
                       ;; Byte - count it
                       (count-bytes (cdr lst) (+ acc 1)))))))
    (count-bytes items 0)))

(defun build-fn-addr-alist (fnoffs base acc)
  "Convert fnoffs to absolute addresses"
  (if (null fnoffs)
      (reverse acc)
      (let* ((entry (car fnoffs))
             (name (car entry))
             (offset (cdr entry))
             (addr (+ base offset)))
        (build-fn-addr-alist (cdr fnoffs) base
                                   (cons (cons name addr) acc)))))

#+sbcl
(defun flatten-code-keep-markers-and-calls (code)
  "Flatten code lists but keep both :extern-call, :call-fn, :tco-branch, :loop-start, :loop-continue and :fn-label markers with positions.
   ITERATIVE version using explicit work stack to avoid deep recursion."
  ;; Work stack entries: (items pos acc parent-state)
  ;; parent-state: nil or (remaining-items parent-pos parent-acc parent-parent)
  (let ((work-stack (list (list code 0 nil nil))))
    (loop
      (when (null work-stack)
        (error "flatten-code-keep-markers-and-calls: work stack unexpectedly empty"))
      (let* ((state (car work-stack))
             (items (first state))
             (pos (second state))
             (acc (third state))
             (parent (fourth state)))
        (setf work-stack (cdr work-stack))
        (cond
          ;; Done with current list
          ((null items)
           (if parent
               ;; Return to parent with flattened result
               (let* ((flattened (reverse acc))
                      (size (length flattened))
                      (parent-items (first parent))
                      (parent-pos (second parent))
                      (parent-acc (third parent))
                      (parent-parent (fourth parent)))
                 (push (list parent-items
                             (+ parent-pos size)
                             (append (reverse flattened) parent-acc)
                             parent-parent)
                       work-stack))
               ;; Top level done - return result
               (return (reverse acc))))
          ;; Process next item
          (t
           (let ((item (car items)))
             (cond
               ;; Extern call marker - reserve 4 bytes for BL instruction
               ((and (consp item) (eq (car item) :extern-call))
                (let ((marker (list :extern-call (cadr item) pos)))
                  (push (list (cdr items)
                              (+ pos 4)
                              (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc)))))
                              parent)
                        work-stack)))
               ;; Function call marker - reserve 4 bytes for BL instruction
               ((and (consp item) (eq (car item) :call-fn))
                (let ((marker (list :call-fn (cadr item) pos)))
                  (push (list (cdr items)
                              (+ pos 4)
                              (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc)))))
                              parent)
                        work-stack)))
               ;; TCO branch marker - reserve 4 bytes for B instruction
               ((and (consp item) (eq (car item) :tco-branch))
                (let ((marker (list :tco-branch (cadr item) pos)))
                  (push (list (cdr items)
                              (+ pos 4)
                              (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc)))))
                              parent)
                        work-stack)))
               ;; Loop start marker - records position for loop continue to jump to
               ((and (consp item) (eq (car item) :loop-start))
                (let ((marker (list :loop-start pos)))
                  (push (list (cdr items) pos (cons marker acc) parent) work-stack)))
               ;; Loop continue marker - reserve 4 bytes for B instruction
               ((and (consp item) (eq (car item) :loop-continue))
                (let ((marker (list :loop-continue pos)))
                  (push (list (cdr items)
                              (+ pos 4)
                              (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc)))))
                              parent)
                        work-stack)))
               ;; Block start marker - records position for block-end target
               ((and (consp item) (eq (car item) :block-start))
                (let ((marker (list :block-start (cadr item) pos)))
                  (push (list (cdr items) pos (cons marker acc) parent) work-stack)))
               ;; Block end marker - position where return-from jumps to
               ((and (consp item) (eq (car item) :block-end))
                (let ((marker (list :block-end (cadr item) pos)))
                  (push (list (cdr items) pos (cons marker acc) parent) work-stack)))
               ;; Return-from marker - reserve 4 bytes for B instruction
               ((and (consp item) (eq (car item) :return-from))
                (let ((marker (list :return-from (cadr item) pos)))
                  (push (list (cdr items)
                              (+ pos 4)
                              (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc)))))
                              parent)
                        work-stack)))
               ;; Function label marker - used by GC runtime
               ((and (consp item) (eq (car item) :fn-label))
                (let ((marker (list :fn-label (cadr item) pos)))
                  (push (list (cdr items) pos (cons marker acc) parent) work-stack)))
               ;; Internal label marker - skip entirely
               ((and (consp item) (eq (car item) :label))
                (push (list (cdr items) pos acc parent) work-stack))
               ;; Lambda-ref marker - reserve 4 bytes for ADR instruction
               ((and (consp item) (eq (car item) :lambda-ref-marker))
                (let ((marker (list :lambda-ref-marker (cadr item) (caddr item) pos)))
                  (push (list (cdr items)
                              (+ pos 4)
                              (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc)))))
                              parent)
                        work-stack)))
               ;; Nested list - push current state as parent, process nested list
               ((consp item)
                (push (list item 0 nil (list (cdr items) pos acc parent)) work-stack))
               ;; Byte
               (t
                (push (list (cdr items) (+ pos 1) (cons item acc) parent) work-stack))))))))))

(defun flatten-all-calls (code fn-alist stub-alist code-base-addr)
  "Replace :call-fn, :extern-call, :loop-start/:loop-continue, :block-start/:block-end/:return-from markers with actual instructions.
   Returns (cons flattened-code positions)."
  (labels ((lookup-fn (name)
             (alist-lookup name fn-alist))
           (lookup-stub (name)
             (alist-lookup name stub-alist))
           (emit-bl (bl-addr target-addr acc)
             (let* ((rel-offset (- target-addr bl-addr))
                    (off-s (ash rel-offset -2))
                    (off-m (logand off-s #x3FFFFFF))
                    (bl-instr (logior #x94000000 off-m)))
               (cons (logand (ash bl-instr -24) #xFF)
                     (cons (logand (ash bl-instr -16) #xFF)
                           (cons (logand (ash bl-instr -8) #xFF)
                                 (cons (logand bl-instr #xFF) acc))))))
           (emit-b (b-addr target-addr acc)
             ;; Emit unconditional branch (B instruction) for loop continue
             (let* ((rel-offset (- target-addr b-addr))
                    (off-s (ash rel-offset -2))
                    (off-m (logand off-s #x3FFFFFF))
                    (b-instr (logior #x14000000 off-m)))
               (cons (logand (ash b-instr -24) #xFF)
                     (cons (logand (ash b-instr -16) #xFF)
                           (cons (logand (ash b-instr -8) #xFF)
                                 (cons (logand b-instr #xFF) acc))))))
           ;; First pass: collect block-end positions
           (collect-block-ends (items acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (if (and (consp item) (eq (car item) :block-end))
                       (collect-block-ends (cdr items)
                                           (cons (cons (cadr item) (caddr item)) acc))
                       (collect-block-ends (cdr items) acc)))))
           ;; Lookup block-end position by block-id
           (lookup-block-end (block-id block-ends)
             (let ((entry (assoc block-id block-ends :test #'equal)))
               (if entry (cdr entry) nil)))
           (process (items skip result positions loop-stack block-ends)
             (if (null items)
                 (cons (reverse result) positions)
                 (let ((item (car items)))
                   (cond
                     ;; Skip placeholder zeros
                     ((> skip 0)
                      (process (cdr items) (- skip 1) result positions loop-stack block-ends))
                     ;; Loop start marker - record position on stack, no bytes emitted
                     ((and (consp item) (eq (car item) :loop-start))
                      (let ((pos (cadr item)))
                        (process (cdr items) 0 result positions (cons pos loop-stack) block-ends)))
                     ;; Loop continue marker - emit B instruction to jump back to loop start
                     ((and (consp item) (eq (car item) :loop-continue))
                      (let* ((pos (cadr item))
                             (b-addr (+ code-base-addr pos))
                             (target-pos (car loop-stack))
                             (target-addr (+ code-base-addr target-pos))
                             (new-result (emit-b b-addr target-addr result)))
                        (process (cdr items) 4 new-result positions loop-stack block-ends)))
                     ;; Block start marker - skip (no bytes)
                     ((and (consp item) (eq (car item) :block-start))
                      (process (cdr items) 0 result positions loop-stack block-ends))
                     ;; Block end marker - skip (no bytes)
                     ((and (consp item) (eq (car item) :block-end))
                      (process (cdr items) 0 result positions loop-stack block-ends))
                     ;; Return-from marker - emit B instruction to jump forward to block-end
                     ((and (consp item) (eq (car item) :return-from))
                      (let* ((block-id (cadr item))
                             (pos (caddr item))
                             (b-addr (+ code-base-addr pos))
                             (target-pos (lookup-block-end block-id block-ends))
                             (target-addr (+ code-base-addr target-pos))
                             (new-result (emit-b b-addr target-addr result)))
                        (process (cdr items) 4 new-result positions loop-stack block-ends)))
                     ;; Extern call marker - skip 4 placeholder zeros
                     ((and (consp item) (eq (car item) :extern-call))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (stub-addr (lookup-stub name))
                             (new-result (if stub-addr
                                            (emit-bl bl-addr stub-addr result)
                                            (cons #x94 (cons 0 (cons 0 (cons 0 result)))))))
                        (process (cdr items) 4 new-result (cons (cons name pos) positions) loop-stack block-ends)))
                     ;; Function call marker - skip 4 placeholder zeros
                     ((and (consp item) (eq (car item) :call-fn))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (fn-addr (lookup-fn name)))
                        (unless fn-addr
                          (error "Function not found during code flattening: ~A" name))
                        (let ((new-result (emit-bl bl-addr fn-addr result)))
                          (process (cdr items) 4 new-result (cons (cons name pos) positions) loop-stack block-ends))))
                     ;; Function label marker - skip (no bytes)
                     ((and (consp item) (eq (car item) :fn-label))
                      (process (cdr items) 0 result positions loop-stack block-ends))
                    ;; Internal label marker - skip (no bytes)
                    ((and (consp item) (eq (car item) :label))
                     (process (cdr items) 0 result positions loop-stack block-ends))
                    ;; Lambda-ref marker - emit ADR instruction
                    ((and (consp item) (eq (car item) :lambda-ref-marker))
                     (let* ((dest-reg (cadr item))
                            (lambda-name (caddr item))
                            (pos (cadddr item))
                            (adr-addr (+ code-base-addr pos))
                            (fn-addr (lookup-fn lambda-name)))
                       (unless fn-addr
                         (error "Lambda not found during code flattening: ~A" lambda-name))
                       (let* ((rel-offset (- fn-addr adr-addr))
                              ;; ADR: rd = PC + imm21
                              ;; Format: 0 immlo[2] 10000 immhi[19] Rd[5]
                              (immlo (logand (ash rel-offset 0) #x3))
                              (immhi (logand (ash rel-offset -2) #x7FFFF))
                              (rd (arm64:reg dest-reg))
                              (adr-instr (logior (ash immlo 29)
                                                 #x10000000
                                                 (ash immhi 5)
                                                 rd)))
                         (process (cdr items) 4
                                  (cons (logand (ash adr-instr -24) #xFF)
                                        (cons (logand (ash adr-instr -16) #xFF)
                                              (cons (logand (ash adr-instr -8) #xFF)
                                                    (cons (logand adr-instr #xFF) result))))
                                  positions loop-stack block-ends))))
                     ;; Regular byte
                     (t
                      (process (cdr items) 0 (cons item result) positions loop-stack block-ends)))))))
    ;; First collect block-end positions, then process
    (let ((block-ends (collect-block-ends code nil)))
      (process code 0 nil nil nil block-ends))))

(defun extract-fn-labels (code base-addr)
  "Extract :fn-label markers from flattened code and build fn-alist.
   BASE-ADDR is the absolute address where code starts.
   Returns alist of (name-string . addr). Names are normalized to strings."
  (labels ((collect (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item) (eq (car item) :fn-label))
                       (let* ((name-str (normalize-fn-name (cadr item)))
                              (pos (caddr item))
                              (addr (+ base-addr pos)))
                         (collect (cdr items) (cons (cons name-str addr) acc)))
                       (collect (cdr items) acc))))))
    (collect code nil)))

(defun alist-lookup (key alist)
  "Look up key in alist, return value or nil.
   Keys are strings (fnoffs/stub names are normalized)."
  (if (null alist)
      nil
      (if (string-equal key (caar alist))
          (cdar alist)
          (alist-lookup key (cdr alist)))))

#+sbcl
(defun flatten-code-keep-markers (code)
  "Flatten nested code lists but keep :extern-call markers intact.
   Tracks position and transforms (:extern-call name) to (:extern-call name pos).
   Each marker followed by 4 zeros = 4 bytes total for BL instruction.
   ITERATIVE version using explicit work stack to avoid deep recursion."
  (let ((work-stack (list (list code 0 nil nil))))
    (loop
      (when (null work-stack)
        (error "flatten-code-keep-markers: work stack unexpectedly empty"))
      (let* ((state (car work-stack))
             (items (first state))
             (pos (second state))
             (acc (third state))
             (parent (fourth state)))
        (setf work-stack (cdr work-stack))
        (cond
          ((null items)
           (if parent
               (let* ((flattened (reverse acc))
                      (size (length flattened))
                      (parent-items (first parent))
                      (parent-pos (second parent))
                      (parent-acc (third parent))
                      (parent-parent (fourth parent)))
                 (push (list parent-items
                             (+ parent-pos size)
                             (append (reverse flattened) parent-acc)
                             parent-parent)
                       work-stack))
               (return (reverse acc))))
          (t
           (let ((item (car items)))
             (cond
               ((and (consp item) (eq (car item) :extern-call))
                (let ((marker (list :extern-call (cadr item) pos)))
                  (push (list (cdr items)
                              (+ pos 4)
                              (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc)))))
                              parent)
                        work-stack)))
               ((consp item)
                (push (list item 0 nil (list (cdr items) pos acc parent)) work-stack))
               (t
                (push (list (cdr items) (+ pos 1) (cons item acc) parent) work-stack))))))))))

#-sbcl
(defun flatten-extern-calls (code stub-alist code-base-addr)
  "Replace extern call markers with BL instructions using stub-alist.
   stub-alist is ((name . stub-addr) ...).
   Returns (cons flattened-code extern-call-positions).
   Native Habu version - SBCL uses hash-table version in compiler-sbcl.lisp."
  (labels ((lookup (name alist)
             (if (null alist)
                 nil
                 (if (string= name (caar alist))
                     (cdar alist)
                     (lookup name (cdr alist)))))
           (emit-bl (bl-addr stub-addr acc)
             ;; Calculate BL instruction
             (let* ((rel-offset (- stub-addr bl-addr))
                    (off-s (ash rel-offset -2))
                    (off-m (logand off-s #x3FFFFFF))
                    (bl-instr (logior #x94000000 off-m)))
               ;; Emit in little-endian order (reversed for cons)
               (cons (logand (ash bl-instr -24) #xFF)
                     (cons (logand (ash bl-instr -16) #xFF)
                           (cons (logand (ash bl-instr -8) #xFF)
                                 (cons (logand bl-instr #xFF) acc))))))
           (process (items skip result positions)
             (if (null items)
                 (cons (reverse result) positions)
                 (let ((item (car items)))
                   (cond
                     ;; Skip placeholder zeros after extern-call marker
                     ((> skip 0)
                      (process (cdr items) (- skip 1) result positions))
                     ;; Extern call marker - emit BL, skip next 4 placeholder zeros
                     ((and (consp item) (eq (car item) :extern-call))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (stub-addr (lookup name stub-alist))
                             (new-result (if stub-addr
                                            (emit-bl bl-addr stub-addr result)
                                            ;; Placeholder if no stub found
                                            (cons #x94 (cons 0 (cons 0 (cons 0 result)))))))
                        (process (cdr items) 4 new-result (cons (cons name pos) positions))))
                     ;; Regular byte
                     (t
                      (process (cdr items) 0 (cons item result) positions)))))))
    (process code 0 nil nil)))

;;; ============================================================
;;; Native resolve-calls (for compile-program in compiler.lisp)
;;; ============================================================

#-sbcl
(defun resolve-calls (code fnoffs)
  "Resolve call, loop, and block markers to branch instructions.
   Handles: (:call-fn name), (:tail-call-fn name), (:loop-start), (:loop-continue),
            (:block-start id), (:block-end id), (:return-from id)
   Note: (:extern-call name) markers are kept as-is for later resolution.
   Native version using arm64 intrinsics."
  (labels ((calc-size (item)
             ;; Calculate byte size of an item
             (cond ((and (consp item) (eq (car item) :call-fn)) 4)
                   ((and (consp item) (eq (car item) :tail-call-fn)) 4)
                   ((and (consp item) (eq (car item) :extern-call)) 4)
                   ((and (consp item) (eq (car item) :loop-start)) 0) ; marker only, no code
                   ((and (consp item) (eq (car item) :loop-continue)) 4) ; B instruction
                   ((and (consp item) (eq (car item) :block-start)) 0) ; marker only
                   ((and (consp item) (eq (car item) :block-end)) 0) ; marker only
                   ((and (consp item) (eq (car item) :return-from)) 4) ; B instruction
                   ((and (consp item) (eq (car item) :tco-branch)) 4)
                   (t 1)))
           (lookup-fn (name fnoffs)
             ;; Look up function offset by name (symbol)
             (if (null fnoffs)
                 nil
                 (if (eq name (caar fnoffs))
                     (cdar fnoffs)
                     (lookup-fn name (cdr fnoffs)))))
           ;; First pass: collect block-end positions
           (collect-block-ends (items pos acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (cond
                     ((and (consp item) (eq (car item) :block-end))
                      (collect-block-ends (cdr items) pos
                                          (cons (cons (cadr item) pos) acc)))
                     (t
                      (collect-block-ends (cdr items) (+ pos (calc-size item)) acc))))))
           (lookup-block-end (block-id block-ends)
             (if (null block-ends)
                 nil
                 (if (equal block-id (caar block-ends))
                     (cdar block-ends)
                     (lookup-block-end block-id (cdr block-ends)))))
           (resolve-at (items pos acc loop-stack block-ends)
             ;; Iterate through items, tracking position, resolving markers
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ;; Loop start - record position on stack, emit nothing
                     ((and (consp item) (eq (car item) :loop-start))
                      (resolve-at (cdr items) pos acc (cons pos loop-stack) block-ends))
                     ;; Loop continue - emit backward branch to loop start
                     ((and (consp item) (eq (car item) :loop-continue))
                      (let* ((loop-start (car loop-stack))
                             (rel-offset (- loop-start pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-stack block-ends)))
                     ;; Block start - skip (no bytes)
                     ((and (consp item) (eq (car item) :block-start))
                      (resolve-at (cdr items) pos acc loop-stack block-ends))
                     ;; Block end - skip (no bytes)
                     ((and (consp item) (eq (car item) :block-end))
                      (resolve-at (cdr items) pos acc loop-stack block-ends))
                     ;; Return-from - emit forward branch to block end
                     ((and (consp item) (eq (car item) :return-from))
                      (let* ((block-id (cadr item))
                             (block-end-pos (lookup-block-end block-id block-ends))
                             (rel-offset (- block-end-pos pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-stack block-ends)))
                     ;; TCO branch - similar to loop-continue but uses stored target
                     ((and (consp item) (eq (car item) :tco-branch))
                      (let* ((target (cadr item))
                             (rel-offset (- target pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-stack block-ends)))
                     ;; Internal call - resolve to BL (or BRK if undefined)
                     ((and (consp item) (eq (car item) :call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-pos (lookup-fn fn-name fnoffs))
                             (call-bytes (if fn-pos
                                             (let* ((rel-offset (- fn-pos pos)))
                                               (arm64:bl (ash rel-offset -2)))
                                             ;; Undefined function: emit BRK #0xF01 (SIGTRAP)
                                             (arm64:brk #xF01))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse call-bytes) acc)
                                    loop-stack block-ends)))
                     ;; Tail call - resolve to B (or BRK if undefined)
                     ((and (consp item) (eq (car item) :tail-call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-pos (lookup-fn fn-name fnoffs))
                             (call-bytes (if fn-pos
                                             (let* ((rel-offset (- fn-pos pos)))
                                               (arm64:b (ash rel-offset -2)))
                                             ;; Undefined function: emit BRK #0xF01 (SIGTRAP)
                                             (arm64:brk #xF01))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse call-bytes) acc)
                                    loop-stack block-ends)))
                     ;; External call - emit marker with position + 3 zero bytes
                     ;; CRITICAL: Must emit 4 bytes to maintain position consistency
                     ((and (consp item) (eq (car item) :extern-call))
                      (resolve-at (cdr items)
                                  (+ pos 4)
                                  (cons 0 (cons 0 (cons 0 (cons (list :extern-call (cadr item) pos) acc))))
                                  loop-stack block-ends))
                     ;; Regular byte
                     (t
                      (resolve-at (cdr items)
                                  (+ pos 1)
                                  (cons item acc)
                                  loop-stack block-ends)))))))
    ;; First collect block-end positions, then resolve
    (let ((block-ends (collect-block-ends code 0 nil)))
      (resolve-at code 0 nil nil block-ends))))

;;; ============================================================
;;; Export Functions
;;; ============================================================
