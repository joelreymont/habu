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
        (lift-lambdas-from-defuns (cdr defuns)
                                  (cons new-defun acc-defuns)
                                  (append acc-lambdas new-lambdas)))))

#+sbcl
(defun lambdas-to-defuns (lambdas acc)
  "Convert lifted lambda entries to defun format (SBCL version).
   Lambda entry: (name params body free-vars free-offsets) or (name . lambda-ir)
   Defun format: (name params body param-base)"
  (if (null lambdas)
      (reverse acc)
      (let* ((entry (car lambdas))
             (name (car entry))
             ;; Handle both formats: (name params body fv fo) and (name . lambda-ir)
             (rest (cdr entry))
             (params (if (and (consp rest) (consp (car rest)) (eq (car (car rest)) 'lambda-ir))
                         ;; (name . lambda-ir) format
                         (cadr (car rest))
                         (car rest)))
             (body (if (and (consp rest) (consp (car rest)) (eq (car (car rest)) 'lambda-ir))
                       (caddr (car rest))
                       (cadr rest)))
             (free-vars (if (and (consp rest) (consp (car rest)) (eq (car (car rest)) 'lambda-ir))
                            (cadddr (car rest))
                            (caddr rest)))
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
         (has-tag ir 'buffer-byte-ref-ir))
     (let* ((left (cadr ir))
            (right (caddr ir))
            (left-result (lift-lambdas left lambdas))
            (new-left (car left-result))
            (l1 (cdr left-result))
            (right-result (lift-lambdas right l1))
            (new-right (car right-result))
            (l2 (cdr right-result)))
       (cons (list (car ir) new-left new-right) l2)))

    ;; Ternary ops (vector-set-ir, buffer-byte-set-ir)
    ((or (has-tag ir 'vector-set-ir)
         (has-tag ir 'buffer-byte-set-ir))
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
         (has-tag ir 'set-global-vars-ir))
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

    ;; Default - return unchanged
    (t (cons ir lambdas))))

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
;;; ARM64 Instruction Wrappers
;;; These thin wrappers call arm64:* functions in both SBCL and native modes.
;;; They provide convenience (e.g., converting shift16 to :lsl, byte to
;;; instruction offsets) while encoding is done in arm64/asm.lisp.
;;; ============================================================

(defun movz (rd imm)
  (arm64:movz rd imm))

(defun movk (rd imm shift16)
  "MOVK Rd, #imm, LSL #shift16 - shift16 is 0, 1, 2, or 3 (for 0, 16, 32, 48)"
  (arm64:movk rd imm :lsl (* shift16 16)))

;;; REMOVED: All ARM64 wrapper functions
;;; Now using arm64:* intrinsics directly with keyword arguments.
;;; See arm64/asm.lisp for the full API.
;;;
;;; Examples:
;;;   (arm64:add rd rn imm :imm t)     - ADD immediate
;;;   (arm64:ldr rt rn :offset off)    - LDR with offset
;;;   (arm64:cmp rn imm :imm t)        - CMP immediate
;;;   (arm64:b.eq offset)              - Branch if equal (instruction count)
;;;   arm64:+eq+                       - Condition code constants


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

;;; Toggle for register-allocated codegen
;;; When t, uses register allocator for function code generation
;;; Falls back to accumulator-based codegen for unsupported IR
#+sbcl (defvar *use-register-allocation* nil)

(defun gc-trigger-code ()
  "Generate inline GC trigger check. Insert after allocations.
   Uses x9 as scratch. Emits :call-fn marker if GC needed.
   In generational mode: checks nursery-end, calls GEN-MINOR-GC.
   In simple mode: checks from-end, calls GC-COLLECT."
  #+sbcl
  (if *use-generational-gc*
      ;; Generational GC: compare against nursery-end
      (append-all
       (list (arm64:ldr 9 27 :offset +gen-nursery-end-offset+)  ; x9 = nursery_end
             (arm64:cmp 28 9)                                   ; compare x28, nursery_end
             (arm64:b.lo 2)                                     ; skip if x28 < nursery_end
             (list (list :call-fn 'GEN-MINOR-GC))))
      ;; Simple GC: compare against from-end
      (append-all
       (list (arm64:ldr 9 27 :offset +gc-from-end-offset+)  ; x9 = from_end
             (arm64:cmp 28 9)                               ; compare x28, from_end
             (arm64:b.lo 2)
             (list (list :call-fn 'GC-COLLECT)))))
  #-sbcl
  ;; Native mode: always use simple GC for now
  (append-all
   (list (arm64:ldr 9 27 :offset +gc-from-end-offset+)
         (arm64:cmp 28 9)
         (arm64:b.lo 2)
         (list (list :call-fn 'GC-COLLECT)))))

(defun gen-write-barrier-code (target-reg)
  "Generate write barrier for stores to heap objects.
   TARGET-REG is the register containing the target object address.
   Call after every heap store that may create an old->young pointer.

   The barrier:
   1. Checks if target is in old space (address >= nursery_end)
   2. If so, computes card index and marks card dirty

   Uses x9, x10 as scratch. Only generated in generational GC mode."
  #+sbcl
  (if *use-generational-gc*
      (append-all
       (list
        ;; Clear tag bits to get base address
        (arm64:and* 9 target-reg -16 :imm t)     ; x9 = base address
        ;; Load nursery_end (old space starts here)
        (arm64:ldr 10 27 :offset +gen-nursery-end-offset+)  ; x10 = nursery_end
        ;; Check if target < nursery_end (in nursery, no barrier needed)
        (arm64:cmp 9 10)
        (arm64:b.lo 7)                           ; skip barrier if in nursery (7 instrs)
        ;; Target is in old space - mark card dirty
        ;; card_index = (addr - old_space_start) >> 9
        (arm64:sub 9 9 10)                       ; x9 = addr - old_space_start
        (arm64:lsr 9 9 +gen-card-shift+ :imm t)  ; x9 = card index
        ;; card_addr = card_table + card_index
        (arm64:ldr 10 27 :offset +gen-card-table-offset+)   ; x10 = card_table
        (arm64:add 9 9 10)                       ; x9 = card address
        ;; Mark card dirty (store 1)
        (arm64:movz 10 1)
        (arm64:strb 10 9 0)))                    ; card[index] = 1
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
   Temp slots occupy 0x40-0xC0 (16 slots, 128 bytes)."
  (if (>= td 16)
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
      (movz rd addr)
      (if (< addr #x100000000)
          (append (movz rd (logand addr #xFFFF))
                  (movk rd (ash addr -16) 1))
          (if (< addr #x1000000000000)
              ;; 48-bit address
              (append-all (list (movz rd (logand addr #xFFFF))
                                (movk rd (logand (ash addr -16) #xFFFF) 1)
                                (movk rd (logand (ash addr -32) #xFFFF) 2)))
              ;; 64-bit address (for packed string data)
              (append-all (list (movz rd (logand addr #xFFFF))
                                (movk rd (logand (ash addr -16) #xFFFF) 1)
                                (movk rd (logand (ash addr -32) #xFFFF) 2)
                                (movk rd (logand (ash addr -48) #xFFFF) 3)))))))

(defun load-addr-8 (rd addr)
  "Load address into register, always producing 8 bytes (2 instructions).
   Used for lambda/function references where consistent code size is needed."
  (append (movz rd (logand addr #xFFFF))
          (movk rd (ash addr -16) 1)))

(defun gen-string-lit (str len total-size)
  "Generate code to allocate string literal on heap.
   String layout: [length:8][data:N]
   Returns tagged string pointer in x0, bumps x28."
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
                       (load-addr 9 val)
                       (arm64:str 9 28 :offset offset)))))))
       ;; Convert string to list of bytes
       (str-to-bytes (s i acc)
         (if (>= i (string-length s))
             (reverse acc)
             (str-to-bytes s (+ i 1) (cons (string-ref s i) acc)))))
    (let* ((bytes (str-to-bytes str 0 nil))
           ;; Add null terminator for C string compatibility
           (bytes-with-nul (append bytes (list 0)))
           ;; Store length first, then data starting at offset 8
           (len-code (append-all
                      (list (load-addr 9 len)
                            (arm64:str 9 28 :offset 0))))
           (data-code (gen-store-bytes 8 bytes-with-nul nil))
           ;; Return tagged pointer and bump heap
           (result-code (append-all
                         (list (arm64:mov 0 28)
                               (arm64:add 0 0 4 :imm t)  ; string tag
                               (arm64:add 28 28 total-size :imm t)
                               (gc-trigger-code)))))
      (append-all (list len-code data-code result-code)))))

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
  (arm64:str 0 31 :offset (temp-slot td)))

#-sbcl
(defun load-temp (rd td)
  (arm64:ldr rd 31 :offset (temp-slot td)))

(defun strb (rt rn offset)
  "Store byte from rt to [rn + offset]"
  (arm64:strb rt rn offset))

(defun strb-reg (rt rn rm)
  "STRB Wt, [Xn, Xm] - store byte to address Xn+Xm"
  (arm64:strb rt rn rm :reg t))

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
         (load-byte (arm64:ldrb 4 1 0))
         (store-byte (strb 4 3 0))
         (inc-src (arm64:add 1 1 1 :imm t))
         (inc-dst (arm64:add 3 3 1 :imm t))
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
    ((has-tag ir 'setq-ir) (ir-may-call (caddr ir)))
    ((has-tag ir 'setcar-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'setcdr-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'symbol-name-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'make-symbol-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'string-length-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'string-ref-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'string-concat-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'string-equal-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
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
    ((has-tag ir 'set-global-vars-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'str-lit) nil)
    ((has-tag ir 'if-ir) t)
    ((has-tag ir 'while-ir) t)
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
    (t nil)))

;;; ============================================================
;;; String Lookup in Fnoffs (lambda names are strings)
;;; ============================================================

(defun lookup-string (name fnoffs)
  "Look up a string name in fnoffs alist.
   fnoffs entries can have either symbol or string keys.
   Returns (name . offset) or nil if not found."
  (labels ((str-match (s1 s2)
             ;; Compare two strings (or string to symbol name)
             (cond
               ((and (stringp s1) (stringp s2))
                (string-equal s1 s2))
               ((and (stringp s1) (symbolp s2))
                (string-equal s1 (symbol-name s2)))
               ((and (symbolp s1) (stringp s2))
                (string-equal (symbol-name s1) s2))
               (t (eq s1 s2))))
           (search-list (lst)
             (if (null lst)
                 nil
                 (let ((entry (car lst)))
                   (if (str-match name (car entry))
                       entry
                       (search-list (cdr lst)))))))
    (search-list fnoffs)))

;;; ============================================================
;;; Build Captures for Closure Creation
;;; ============================================================

(defun build-captures (free-offsets)
  "Generate code to build a cons list of captured values.
   free-offsets = list of stack offsets where captured values live.
   Result in x0 is a tagged cons list."
  (if (null free-offsets)
      (movz 0 0)  ;; nil
      (labels ((build-list (offs acc)
                 ;; Build list in reverse, then we cons onto it
                 ;; Each captured value is loaded from [x20 - offset*8]
                 (if (null offs)
                     acc
                     (let* ((off (car offs))
                            (off8 (* off 8))
                            ;; Load value from stack
                            (load-code (append (arm64:sub 1 20 off8 :imm t)
                                                    (arm64:ldr 0 1 :offset 0)))
                            ;; Save in temp if not first
                            (store-code (if (null (cdr offs))
                                           nil  ;; Last one, keep in x0
                                           (append-all
                                            (list load-code
                                                  ;; Store car
                                                  (arm64:str 0 28 :offset 0)
                                                  ;; Load/cons previous result
                                                  (arm64:ldr 0 28 :offset 8)  ; get cdr (prev result)
                                                  ;; This doesn't work... need different approach
                                                  nil)))))
                       (build-list (cdr offs)
                                   (append acc load-code))))))
        ;; Simpler approach: build cons list iteratively
        ;; Start with nil, then cons each value
        ;; NOTE: No GC trigger here - the variable-length cons chain causes
        ;; issues with function offset calculations. Closures with captures
        ;; are rare enough that skipping GC check here is acceptable.
        (labels ((gen-cons-chain (offs)
                   (if (null offs)
                       (movz 0 0)
                       (let* ((off (car offs))
                              (off8 (* off 8))
                              (rest-code (gen-cons-chain (cdr offs))))
                         ;; First build rest of list, then cons current onto it
                         (append-all
                          (list rest-code
                                ;; Save cdr in heap
                                (arm64:str 0 28 :offset 8)
                                ;; Load current value
                                (arm64:sub 1 20 off8 :imm t)
                                (arm64:ldr 0 1 :offset 0)
                                ;; Store as car
                                (arm64:str 0 28 :offset 0)
                                ;; Make cons pointer
                                (arm64:mov 0 28)
                                (arm64:add 0 0 1 :imm t)  ;; cons tag
                                (arm64:add 28 28 16 :imm t)))))))
          ;; Don't reverse - gen-cons-chain builds in correct order
          ;; for free-vars list (first offset becomes car)
          (gen-cons-chain free-offsets)))))

;;; ============================================================
;;; Binary Operation Codegen Helper
;;; ============================================================

#-sbcl
(defun codegen-binop (left-ir right-ir op-instrs rtaddrs fnoffs td)
  "Generate code for binary operation"
  (let* ((left-may-call (ir-may-call left-ir))
         (right-may-call (ir-may-call right-ir)))
    (cond
      ;; Left may call - need to save x24
      ;; Left is evaluated at nd, saves result to nd, so right must use nd+1
      (left-may-call
       (let* ((xs (temp-slot td))
              (nd (+ td 1))
              (lc (codegen left-ir rtaddrs fnoffs nd))
              (rc (codegen right-ir rtaddrs fnoffs (+ nd 1))))
         (append-all
          (list (arm64:str 24 31 :offset xs)
                lc
                (save-temp nd)
                (arm64:ldr 24 31 :offset xs)
                rc
                (arm64:mov 1 0)
                (load-temp 0 nd)
                op-instrs))))
      ;; Right may call - need to save x24
      ;; Left is evaluated, saved at nd, so right must use nd+1
      (right-may-call
       (let* ((xs (temp-slot td))
              (nd (+ td 1))
              (lc (codegen left-ir rtaddrs fnoffs nd))
              (rc (codegen right-ir rtaddrs fnoffs (+ nd 1))))
         (append-all
          (list lc
                (save-temp nd)
                (arm64:str 24 31 :offset xs)
                rc
                (arm64:mov 1 0)
                (load-temp 0 nd)
                (arm64:ldr 24 31 :offset xs)
                op-instrs))))
      ;; Neither calls - simple case
      ;; IMPORTANT: Right must use td+1 to avoid clobbering left's temp slot
      (t
       (let* ((nd (+ td 1))
              (lc (codegen left-ir rtaddrs fnoffs td))
              (rc (codegen right-ir rtaddrs fnoffs nd)))
         (append-all
          (list lc
                (save-temp td)
                rc
                (arm64:mov 1 0)
                (load-temp 0 td)
                op-instrs)))))))

;;; ============================================================
;;; Main Codegen Function (handles all IR nodes)
;;; ============================================================

#-sbcl
(defun codegen (ir rtaddrs fnoffs td)
  "Generate ARM64 code from IR"
  (cond
    ;; Literal
    ((has-tag ir 'lit)
     (let* ((v (cadr ir))
            (tg (ash v 4)))
       (if (and (>= tg 0) (< tg #x10000))
           (movz 0 tg)
           (load-addr 0 tg))))

    ;; Nil - use tag 6 to distinguish from fixnum 0
    ;; nil = 0x06 (tag 6), fixnum 0 = 0x00 (tag 0)
    ((has-tag ir 'nil-ir)
     (movz 0 6))

    ;; Symbol literal
    ((has-tag ir 'sym-lit)
     (let* ((name (cadr ir))
            (id (intern-symbol name))
            (tagged (logior (ash id 4) 2)))
       (if (< tagged #x10000)
           (movz 0 tagged)
           (load-addr 0 tagged))))

    ;; String literal - allocate on heap
    ;; String layout: [length:8][data:N][padding to 16]
    ;; Total size must be 16-byte aligned to keep heap aligned for cons cells
    ((has-tag ir 'str-lit)
     (let* ((str (cadr ir))
            (len (string-length str))
            ;; Align (header + data) to 16 bytes
            (total-size (logand (+ len 8 15) (lognot 15))))
       ;; Generate code to:
       ;; 1. Store length at x28
       ;; 2. Copy string bytes to x28+8
       ;; 3. Return tagged pointer, bump x28
       (gen-string-lit str len total-size)))

    ;; Variable reference
    ((has-tag ir 'var)
     (let* ((off (cadr ir))
            (off8 (* off 8)))
       (append (arm64:sub 1 20 off8 :imm t)
                    (arm64:ldr 0 1 :offset 0))))

    ;; Variable assignment (setq)
    ((has-tag ir 'setq-ir)
     (let* ((off (cadr ir))
            (val-ir (caddr ir))
            (off8 (* off 8))
            (val-code (codegen val-ir rtaddrs fnoffs td)))
       ;; Compile value to x0, then store at x20 - offset*8
       (append-all
        (list val-code
              (arm64:sub 1 20 off8 :imm t)
              (arm64:str 0 1 :offset 0)))))

    ;; Addition
    ((has-tag ir 'add)
     (codegen-binop (cadr ir) (caddr ir)
                         (arm64:add 0 0 1)
                         rtaddrs fnoffs td))

    ;; Subtraction
    ((has-tag ir 'sub)
     (codegen-binop (cadr ir) (caddr ir)
                         (arm64:sub 0 0 1)
                         rtaddrs fnoffs td))

    ;; Multiplication (untag one operand)
    ((has-tag ir 'mul)
     (codegen-binop (cadr ir) (caddr ir)
                         (append (arm64:lsr 1 1 4 :imm t)
                                      (arm64:mul 0 0 1))
                         rtaddrs fnoffs td))

    ;; Division
    ((has-tag ir 'div)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (arm64:lsr 0 0 4 :imm t)
                                (arm64:lsr 1 1 4 :imm t)
                                (arm64:sdiv 0 0 1)
                                (arm64:lsl 0 0 4 :imm t)))
                         rtaddrs fnoffs td))

    ;; Modulo: a mod b = a - (a/b)*b
    ((has-tag ir 'mod)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (arm64:lsr 9 0 4 :imm t)    ; x9 = a untagged
                                (arm64:lsr 10 1 4 :imm t)   ; x10 = b untagged
                                (arm64:sdiv 11 9 10) ; x11 = a/b
                                (arm64:mul 11 11 10) ; x11 = (a/b)*b
                                (arm64:sub 0 9 11)   ; x0 = a - (a/b)*b
                                (arm64:lsl 0 0 4 :imm t)))  ; tag result
                         rtaddrs fnoffs td))

    ;; Comparison (equality)
    ((has-tag ir 'cmp-eq)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (arm64:cmp 0 1)
                                (arm64:cset 0 arm64:+eq+)
                                (arm64:lsl 0 0 4 :imm t)))
                         rtaddrs fnoffs td))

    ;; Less than
    ((has-tag ir 'cmp-lt)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (arm64:cmp 0 1)
                                (arm64:cset 0 arm64:+lt+)
                                (arm64:lsl 0 0 4 :imm t)))
                         rtaddrs fnoffs td))

    ;; Greater than
    ((has-tag ir 'cmp-gt)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (arm64:cmp 0 1)
                                (arm64:cset 0 arm64:+gt+)
                                (arm64:lsl 0 0 4 :imm t)))
                         rtaddrs fnoffs td))

    ;; Less than or equal
    ((has-tag ir 'cmp-le)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (arm64:cmp 0 1)
                                (arm64:cset 0 arm64:+le+)
                                (arm64:lsl 0 0 4 :imm t)))
                         rtaddrs fnoffs td))

    ;; Greater than or equal
    ((has-tag ir 'cmp-ge)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (arm64:cmp 0 1)
                                (arm64:cset 0 arm64:+ge+)
                                (arm64:lsl 0 0 4 :imm t)))
                         rtaddrs fnoffs td))

    ;; Cons cell (inline heap allocation)
    ((has-tag ir 'cons-ir)
     (let* ((car-ir (cadr ir))
            (cdr-ir (caddr ir))
            (xs (temp-slot td))
            (cs (temp-slot (+ td 1)))
            (nd (+ td 2))
            (car-code (codegen car-ir rtaddrs fnoffs nd))
            (cdr-code (codegen cdr-ir rtaddrs fnoffs nd)))
       (append-all
        (list (arm64:str 24 31 :offset xs)
              car-code
              (arm64:str 0 31 :offset cs)
              (arm64:ldr 24 31 :offset xs)
              cdr-code
              (arm64:str 0 28 :offset 8)
              (arm64:ldr 0 31 :offset cs)
              (arm64:str 0 28 :offset 0)
              (arm64:mov 0 28)
              (arm64:add 0 0 1 :imm t)
              (arm64:add 28 28 16 :imm t)
              (gc-trigger-code)     ; GC check after allocation
              (arm64:ldr 24 31 :offset xs)))))

    ;; Car
    ((has-tag ir 'car-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append inner-code
                    (append (arm64:sub 0 0 1 :imm t)
                                 (arm64:ldr 0 0 :offset 0)))))

    ;; Cdr
    ((has-tag ir 'cdr-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append inner-code
                    (append (arm64:sub 0 0 1 :imm t)
                                 (arm64:ldr 0 0 :offset 8)))))

    ;; String-length: string layout is [length:8][data...]
    ;; String tag is 4, so untag and load length from offset 0
    ((has-tag ir 'string-length-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append-all
        (list inner-code
              (arm64:sub 0 0 4 :imm t)        ; untag string
              (arm64:ldr 0 0 :offset 0)     ; load length
              (arm64:lsl 0 0 4 :imm t)))))    ; tag as fixnum

    ;; String-ref: get character at index
    ;; (string-ref-ir str-ir idx-ir)
    ((has-tag ir 'string-ref-ir)
     (let* ((str-ir (cadr ir))
            (idx-ir (caddr ir))
            (spill-off (spill-base td))
            (str-code (codegen str-ir rtaddrs fnoffs td))
            (idx-code (codegen idx-ir rtaddrs fnoffs td)))
       (append-all
        (list str-code
              ;; Spill string pointer
              (arm64:str 0 31 :offset spill-off)
              idx-code
              ;; x0 = tagged index, x1 = string pointer
              (arm64:ldr 1 31 :offset spill-off)
              ;; Untag index (shift right 4)
              (arm64:lsr 0 0 4 :imm t)
              ;; Untag string pointer
              (arm64:sub 1 1 4 :imm t)
              ;; Add 8 for header, then add index
              (arm64:add 1 1 8 :imm t)
              (arm64:add 1 1 0)
              ;; Load byte at [x1]
              (arm64:ldrb 0 1 0)
              ;; Tag as fixnum
              (arm64:lsl 0 0 4 :imm t)))))

    ;; String-concat: concatenate two strings
    ;; Result is a new string on heap
    ((has-tag ir 'string-concat-ir)
     (let* ((str1-ir (cadr ir))
            (str2-ir (caddr ir))
            (spill1 (spill-base td))
            (spill2 (+ spill1 8))
            (spill3 (+ spill1 16))
            (str1-code (codegen str1-ir rtaddrs fnoffs td))
            (str2-code (codegen str2-ir rtaddrs fnoffs (+ td 1))))
       (append-all
        (list str1-code
              ;; Spill str1
              (arm64:str 0 31 :offset spill1)
              str2-code
              ;; Spill str2
              (arm64:str 0 31 :offset spill2)
              ;; Load str1, get len1 into x9
              (arm64:ldr 1 31 :offset spill1)
              (arm64:sub 1 1 4 :imm t)            ; untag
              (arm64:ldr 9 1 :offset 0)         ; x9 = len1
              ;; Load str2, get len2 into x10
              (arm64:ldr 2 31 :offset spill2)
              (arm64:sub 2 2 4 :imm t)            ; untag
              (arm64:ldr 10 2 :offset 0)        ; x10 = len2
              ;; x11 = len1 + len2 (total length)
              (arm64:add 11 9 10)
              ;; Save total length
              (arm64:str 11 31 :offset spill3)
              ;; Store total length at heap[0]
              (arm64:str 11 28 :offset 0)
              ;; Save heap start for result
              (arm64:mov 0 28)
              ;; Calculate aligned size: (8 + total + 15) & ~15 for 16-byte alignment
              (arm64:add 12 11 23 :imm t)         ; +8 header +15 for 16-byte alignment
              (arm64:and* 12 12 -16 :imm t)           ; align to 16 bytes
              ;; Bump heap by aligned size
              (arm64:add 28 28 12)
              ;; Now copy str1 bytes to result+8
              ;; x1 = src1 (str1+8), x3 = dst (result+8), x9 = len1
              (arm64:ldr 1 31 :offset spill1)
              (arm64:sub 1 1 4 :imm t)            ; untag str1
              (arm64:add 1 1 8 :imm t)            ; skip header
              (arm64:add 3 0 8 :imm t)            ; dst = result + 8
              ;; Copy loop for str1 (x9 = count)
              ;; This is a simple byte-by-byte copy
              (gen-memcpy-inline 9)
              ;; Now copy str2 bytes
              ;; x3 already points past str1 data
              ;; x1 = src2 (str2+8), x10 = len2
              (arm64:ldr 1 31 :offset spill2)
              (arm64:sub 1 1 4 :imm t)            ; untag str2
              (arm64:add 1 1 8 :imm t)            ; skip header
              (arm64:mov 9 10)             ; count = len2
              (gen-memcpy-inline 9)
              ;; Return tagged result
              (arm64:add 0 0 4 :imm t)))))        ; string tag

    ;; String-equal: compare two strings for equality
    ;; Returns tagged fixnum: 16 (true) or 0 (false)
    ((has-tag ir 'string-equal-ir)
     (let* ((str1-ir (cadr ir))
            (str2-ir (caddr ir))
            (spill-off (spill-base td))
            (str1-code (codegen str1-ir rtaddrs fnoffs td))
            (str2-code (codegen str2-ir rtaddrs fnoffs td)))
       (append-all
        (list str1-code
              ;; Spill str1
              (arm64:str 0 31 :offset spill-off)
              str2-code
              ;; x2 = str2 base (untagged)
              (arm64:and* 2 0 -16 :imm t)               ; x2 = str2 & ~0xF
              ;; x1 = str1 base (untagged)
              (arm64:ldr 0 31 :offset spill-off)  ; x0 = str1 (tagged)
              (arm64:and* 1 0 -16 :imm t)               ; x1 = str1 & ~0xF
              ;; Load lengths
              (arm64:ldr 3 1 :offset 0)           ; x3 = len1
              (arm64:ldr 4 2 :offset 0)           ; x4 = len2
              ;; Compare lengths
              (arm64:cmp 3 4)                ; cmp len1, len2
              (arm64:b.ne 14)                    ; if len1 != len2, jump to return_false
              ;; Lengths equal, setup for loop
              (arm64:add 1 1 8 :imm t)              ; x1 = str1 data start
              (arm64:add 2 2 8 :imm t)              ; x2 = str2 data start
              (movz 4 0)                   ; x4 = 0 (loop counter)
              ;; loop_start:
              (arm64:cmp 4 3)                ; cmp counter, len
              (arm64:b.ge 7)                     ; if counter >= len, return_true
              ;; Load bytes from both strings
              (arm64:ldrb 5 1 4 :reg t)             ; x5 = str1[counter]
              (arm64:ldrb 6 2 4 :reg t)             ; x6 = str2[counter]
              ;; Compare bytes
              (arm64:cmp 5 6)                ; cmp char1, char2
              (arm64:b.ne 5)                     ; if not equal, return_false
              ;; Increment counter
              (arm64:add 4 4 1 :imm t)              ; x4++
              (arm64:b -6)                 ; back to loop_start
              ;; return_true:
              (movz 0 16)                  ; x0 = 16 (tagged 1)
              (arm64:b 2)                  ; skip return_false
              ;; return_false:
              (movz 0 6)))))               ; x0 = 6 (nil tag)

    ;; Make-vector: allocate vector on heap
    ;; Vector layout: [length (8 bytes)] [data (n * 8 bytes)]
    ;; Total size = 8 + (untagged_size * 8), rounded to 16 for tagging
    ((has-tag ir 'make-vector-ir)
     (let* ((size-ir (cadr ir))
            (sc (codegen size-ir rtaddrs fnoffs td)))
       (append-all
        (list sc
              ;; x0 = tagged size, store untagged length at [x28+0]
              (arm64:lsr 1 0 4 :imm t)           ; x1 = untagged length
              (arm64:str 1 28 :offset 0)       ; [x28+0] = length
              ;; Calculate allocation size: 8 + (x0 >> 1)
              (arm64:lsr 1 0 1 :imm t)           ; x1 = x0 >> 1 = untagged_size * 8
              (arm64:add 1 1 8 :imm t)           ; x1 = 8 + data_size = total size
              ;; Round to 16-byte alignment: (x1 + 15) & ~15
              (arm64:add 1 1 15 :imm t)          ; x1 = total + 15
              (arm64:and* 1 1 -16 :imm t)             ; x1 = x1 & ~15 (clear low 4 bits)
              ;; Return tagged pointer, bump heap
              (arm64:mov 0 28)            ; x0 = current heap ptr
              (arm64:add 28 28 1)         ; x28 += total size (now 16-aligned)
              ;; Tag with vector tag (0x3)
              (movz 1 3)
              (arm64:orr 0 0 1)
              ;; GC trigger check (x0 is tagged, safe for GC)
              (gc-trigger-code)))))

    ;; Vector-set: set element at index
    ;; (vector-set-ir vec-ir idx-ir val-ir)
    ((has-tag ir 'vector-set-ir)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (val-ir (cadddr ir))
            (xs (temp-slot td))
            (xs2 (temp-slot (+ td 1)))
            (nd (+ td 2))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd))
            (si (arm64:str 0 31 :offset xs2))
            (vlc (codegen val-ir rtaddrs fnoffs nd)))
       ;; After codegen: val in x0, vec at [sp+xs], idx at [sp+xs2]
       (append-all
        (list vc sv ic si vlc
              ;; x0 = val, load vec -> x1, idx -> x2
              (arm64:ldr 1 31 :offset xs)         ; x1 = vec (tagged with 3)
              (arm64:ldr 2 31 :offset xs2)        ; x2 = idx (tagged)
              ;; Clear tag from vec by subtracting 3
              (arm64:sub 1 1 3 :imm t)              ; x1 = vec_ptr (untagged)
              ;; Save vec base for write barrier
              (arm64:mov 3 1)                       ; x3 = vec_ptr (for barrier)
              ;; Calculate offset: x2 = (idx >> 1) + 8
              (arm64:lsr 2 2 1 :imm t)              ; x2 = idx >> 1 = idx_untagged * 8
              (arm64:add 2 2 8 :imm t)              ; x2 = offset = 8 + idx_untagged * 8
              ;; Store val at vec_ptr + offset
              (arm64:add 1 1 2)              ; x1 = address
              (arm64:str 0 1 :offset 0)            ; [x1] = val
              ;; Write barrier for generational GC (x3 = vec base address)
              (gen-write-barrier-code 3)))))

    ;; Vector-ref: get element at index
    ;; (vector-ref-ir vec-ir idx-ir)
    ((has-tag ir 'vector-ref-ir)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (xs (temp-slot td))
            (nd (+ td 1))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd)))
       ;; After codegen: idx in x0, vec at [sp+xs]
       (append-all
        (list vc sv ic
              ;; x0 = idx, load vec -> x1
              (arm64:ldr 1 31 :offset xs)         ; x1 = vec (tagged with 3)
              ;; Clear tag from vec by subtracting 3
              (arm64:sub 1 1 3 :imm t)              ; x1 = vec_ptr (untagged)
              ;; Calculate offset: x0 = (idx >> 1) + 8
              (arm64:lsr 0 0 1 :imm t)              ; x0 = idx >> 1 = idx_untagged * 8
              (arm64:add 0 0 8 :imm t)              ; x0 = offset = 8 + idx_untagged * 8
              ;; Load element from vec_ptr + offset
              (arm64:add 1 1 0)              ; x1 = address
              (arm64:ldr 0 1 :offset 0)))))       ; x0 = [x1] = element (already tagged)

    ;; Vector-length: get vector size
    ;; (vector-length-ir vec-ir)
    ((has-tag ir 'vector-length-ir)
     (let* ((vec-ir (cadr ir))
            (vc (codegen vec-ir rtaddrs fnoffs td)))
       (append-all
        (list vc
              ;; x0 = vec (tagged with 3)
              ;; Clear tag by subtracting 3
              (arm64:sub 0 0 3 :imm t)              ; x0 = vec_ptr (untagged)
              ;; Load length: x0 = [x0+0]
              (arm64:ldr 0 0 :offset 0)           ; x0 = raw length (untagged integer)
              ;; Tag as fixnum: x0 = x0 << 4
              (arm64:lsl 0 0 4 :imm t)))))          ; x0 = tagged fixnum length

    ;; Buffer-byte-ref: get raw byte at index from vector data area
    ;; (buffer-byte-ref-ir vec-ir idx-ir)
    ;; Used for reading bytes stored by sys-read
    ;; Vector layout: [length (8 bytes)][raw bytes...]
    ((has-tag ir 'buffer-byte-ref-ir)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (xs (temp-slot td))
            (nd (+ td 1))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd)))
       ;; After codegen: idx in x0, vec at [sp+xs]
       (append-all
        (list vc sv ic
              ;; x0 = idx (tagged), load vec -> x1
              (arm64:ldr 1 31 :offset xs)         ; x1 = vec (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (arm64:and* 1 1 -16 :imm t)         ; x1 = vec_ptr (untagged)
              ;; Untag idx: x0 = x0 >> 4
              (arm64:lsr 0 0 4 :imm t)            ; x0 = raw index
              ;; Add 8 to skip length header, add index
              (arm64:add 0 0 8 :imm t)            ; x0 = 8 + idx
              (arm64:add 1 1 0)                   ; x1 = vec_base + 8 + idx
              ;; Load byte
              (arm64:ldrb 0 1 0)                  ; x0 = byte
              ;; Tag as fixnum
              (arm64:lsl 0 0 4 :imm t)))))        ; x0 = tagged fixnum

    ;; Buffer-byte-set: store byte at index in vector data area
    ;; (buffer-byte-set-ir vec-ir idx-ir val-ir)
    ;; Used for storing bytes (e.g., from sys-read-byte)
    ;; Vector layout: [length (8 bytes)][raw bytes...]
    ((has-tag ir 'buffer-byte-set-ir)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (val-ir (cadddr ir))
            (xs (temp-slot td))
            (xs2 (temp-slot (+ td 1)))
            (nd (+ td 2))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (arm64:str 0 31 :offset xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd))
            (si (arm64:str 0 31 :offset xs2))
            (vlc (codegen val-ir rtaddrs fnoffs nd)))
       ;; After codegen: val in x0, vec at [sp+xs], idx at [sp+xs2]
       (append-all
        (list vc sv ic si vlc
              ;; x0 = val (tagged), load vec -> x1, idx -> x2
              (arm64:ldr 1 31 :offset xs)         ; x1 = vec (tagged)
              (arm64:ldr 2 31 :offset xs2)        ; x2 = idx (tagged)
              ;; Clear tag from vec: x1 = x1 & ~0xF
              (arm64:and* 1 1 -16 :imm t)         ; x1 = vec_ptr (untagged)
              ;; Untag idx: x2 = x2 >> 4
              (arm64:lsr 2 2 4 :imm t)            ; x2 = raw index
              ;; Untag val: x0 = x0 >> 4
              (arm64:lsr 0 0 4 :imm t)            ; x0 = raw byte value
              ;; Add 8 to skip length header, add index
              (arm64:add 2 2 8 :imm t)            ; x2 = 8 + idx
              (arm64:add 1 1 2)                   ; x1 = vec_base + 8 + idx
              ;; Store byte
              (arm64:strb 0 1 0)                  ; [x1] = byte
              ;; Return nil (stored value already consumed)
              (movz 0 6)))))                      ; x0 = nil (0x06)

    ;; Make-string-from-vector: convert vector OR list of char codes to string
    ;; (make-string-from-vector-ir seq-ir)
    ;; Handles both vectors (tag 3) and lists (tag 1) per Lisp spec
    ((has-tag ir 'make-string-from-vector-ir)
     (let* ((seq-ir (cadr ir))
            (sc (codegen seq-ir rtaddrs fnoffs td)))
       (append-all
        (list sc
              ;; x0 = sequence (could be vector tag 3 or list tag 1)
              ;; Check tag: x6 = x0 & 0xF
              (arm64:and* 6 0 #xF :imm t)               ; x6 = tag
              (arm64:cmp 6 1 :imm t)                ; is it a list (tag 1)?
              (arm64:b.eq 24)        ; if list, jump to list handler (+24 instrs = 96 bytes)

              ;; === VECTOR PATH (tag 3) ===
              ;; x1 = untagged vec base
              (arm64:sub 1 0 3 :imm t)              ; x1 = vec_ptr (untagged)
              ;; x5 = vec length (raw)
              (arm64:ldr 5 1 :offset 0)           ; x5 = [x1+0] = length
              ;; Allocate string: store length at [x28], compute alloc size
              (arm64:str 5 28 :offset 0)          ; [x28+0] = length
              ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
              (arm64:add 4 5 23 :imm t)             ; x4 = len + 23 (= len + 8 + 15)
              (arm64:lsr 4 4 4 :imm t)              ; x4 = x4 >> 4
              (arm64:lsl 4 4 4 :imm t)              ; x4 = (x4 >> 4) << 4 = x4 & ~15
              ;; Save string ptr (will be result), bump heap
              (arm64:mov 0 28)               ; x0 = string base (untagged)
              (arm64:add 28 28 4)            ; x28 += alloc_size
              ;; x2 = string data base = x0 + 8
              (arm64:add 2 0 8 :imm t)              ; x2 = string data start
              ;; x3 = loop counter = 0
              (movz 3 0)                   ; x3 = 0
              ;; vec_loop_start:
              (arm64:cmp 3 5)                ; cmp x3, x5
              (arm64:b.ge 9)        ; if x3 >= x5, jump to vec_loop_end (+9 instrs)
              ;; Load vec[x3]: address = x1 + 8 + x3*8
              (arm64:lsl 4 3 3 :imm t)              ; x4 = x3 * 8
              (arm64:add 4 4 8 :imm t)              ; x4 = 8 + x3*8 (offset in vec)
              (arm64:add 4 1 4)              ; x4 = vec_base + offset
              (arm64:ldr 4 4 :offset 0)           ; x4 = [x4] = tagged fixnum
              (arm64:lsr 4 4 4 :imm t)              ; x4 = char value (untagged)
              (strb-reg 4 2 3)             ; [x2 + x3] = x4 (byte)
              (arm64:add 3 3 1 :imm t)              ; x3++
              (arm64:b -9)               ; back to vec_loop_start
              ;; vec_loop_end: tag result
              (movz 4 4)                   ; x4 = 4
              (arm64:orr 0 0 4)              ; x0 = string (tagged)
              (arm64:b 29)               ; jump to end (+29 instrs = 116 bytes)

              ;; === LIST PATH (tag 1) ===
              ;; First count list length
              ;; x1 = list ptr, x5 = count
              (arm64:mov 1 0)                ; x1 = list (tagged)
              (movz 5 0)                   ; x5 = 0
              ;; count_loop:
              (arm64:cmp 1 6 :imm t)                ; compare with nil (0x06)
              (arm64:b.eq 5)        ; if nil, jump to count_done (+5 instrs)
              (arm64:add 5 5 1 :imm t)              ; x5++
              (arm64:sub 4 1 1 :imm t)              ; x4 = untag cons
              (arm64:ldr 1 4 :offset 8)           ; x1 = cdr (tagged)
              (arm64:b -5)               ; back to count_loop
              ;; count_done: x5 = length
              ;; Allocate string
              (arm64:str 5 28 :offset 0)          ; [x28+0] = length
              (arm64:add 4 5 23 :imm t)             ; x4 = len + 23
              (arm64:lsr 4 4 4 :imm t)
              (arm64:lsl 4 4 4 :imm t)              ; x4 = aligned size
              (arm64:mov 6 28)               ; x6 = string base (save for result)
              (arm64:add 28 28 4)            ; bump heap
              ;; x2 = string data = x6 + 8
              (arm64:add 2 6 8 :imm t)
              ;; x1 = list (from x0), x3 = index = 0
              (arm64:mov 1 0)
              (movz 3 0)
              ;; copy_loop:
              (arm64:cmp 1 6 :imm t)                ; compare with nil
              (arm64:b.eq 8)        ; if nil, jump to copy_done (+8 instrs)
              (arm64:sub 4 1 1 :imm t)              ; x4 = untag cons
              (arm64:ldr 7 4 :offset 0)           ; x7 = car (tagged fixnum)
              (arm64:lsr 7 7 4 :imm t)              ; x7 = char value (untagged)
              (strb-reg 7 2 3)             ; [x2 + x3] = byte
              (arm64:add 3 3 1 :imm t)              ; x3++
              (arm64:ldr 1 4 :offset 8)           ; x1 = cdr (tagged)
              (arm64:b -8)               ; back to copy_loop
              ;; copy_done: x0 = result = x6 | 4
              (arm64:mov 0 6)
              (movz 4 4)
              (arm64:orr 0 0 4)))))

    ;; Setcar - mutate car of cons cell
    ;; (setcar-ir cons-ir val-ir)
    ((has-tag ir 'setcar-ir)
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (spill-off (spill-base td))
            (cons-code (codegen cons-ir rtaddrs fnoffs td))
            (val-code (codegen val-ir rtaddrs fnoffs td)))
       (append-all
        (list cons-code
              ;; Spill cons to stack (SP = x31)
              (arm64:str 0 31 :offset spill-off)
              val-code
              ;; Restore cons to x1
              (arm64:ldr 1 31 :offset spill-off)
              ;; Untag cons (subtract 1)
              (arm64:sub 1 1 1 :imm t)
              ;; Store val at car position (offset 0)
              (arm64:str 0 1 :offset 0)
              ;; Write barrier for generational GC (x1 = cons base address)
              (gen-write-barrier-code 1)))))

    ;; Setcdr - mutate cdr of cons cell
    ;; (setcdr-ir cons-ir val-ir)
    ((has-tag ir 'setcdr-ir)
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (spill-off (spill-base td))
            (cons-code (codegen cons-ir rtaddrs fnoffs td))
            (val-code (codegen val-ir rtaddrs fnoffs td)))
       (append-all
        (list cons-code
              ;; Spill cons to stack (SP = x31)
              (arm64:str 0 31 :offset spill-off)
              val-code
              ;; Restore cons to x1
              (arm64:ldr 1 31 :offset spill-off)
              ;; Untag cons (subtract 1)
              (arm64:sub 1 1 1 :imm t)
              ;; Store val at cdr position (offset 8)
              (arm64:str 0 1 :offset 8)
              ;; Write barrier for generational GC (x1 = cons base address)
              (gen-write-barrier-code 1)))))

    ;; Symbol-name - get string name from symbol
    ;; Symbols are stored as (string-pointer | 2), so untag to get string
    ((has-tag ir 'symbol-name-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       ;; Untag symbol (subtract 2), then add string tag (4)
       ;; Result: string-pointer | 4
       (append inner-code
                    (append (arm64:sub 0 0 2 :imm t)
                                 (arm64:add 0 0 4 :imm t)))))

    ;; Make-symbol-from-string - create symbol from string
    ;; Strings are (pointer | 4), symbols are (pointer | 2)
    ((has-tag ir 'make-symbol-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       ;; Untag string (subtract 4), then add symbol tag (2)
       (append inner-code
                    (append (arm64:sub 0 0 4 :imm t)
                                 (arm64:add 0 0 2 :imm t)))))

    ;; Get-tag (extract low 4 bits as tagged fixnum)
    ((has-tag ir 'get-tag)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append inner-code
                    ;; AND x0, x0, #0xF to extract tag bits
                    ;; Then LSL x0, x0, #4 to tag as fixnum
                    (append (arm64:and* 0 0 #xF :imm t)
                                 (arm64:lsl 0 0 4 :imm t)))))

    ;; If-IR
    ((has-tag ir 'if-ir)
     (let* ((cond-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (cond-code (codegen cond-ir rtaddrs fnoffs td))
            (then-code (codegen then-ir rtaddrs fnoffs td))
            (else-code (codegen else-ir rtaddrs fnoffs td))
            ;; Must use code-size to handle :call-fn markers (4 bytes each)
            (else-size (code-size else-code))
            (then-size (code-size then-code)))
       (append-all
        (list cond-code
              (arm64:cmp 0 0 :imm t)
              ;; Branch if cond==0 (false) to skip then + unconditional branch
              (arm64:b.eq (ash (+ then-size 8) -2))
              then-code
              ;; Unconditional branch to skip else
              (arm64:b (ash (+ else-size 4) -2))
              else-code))))

    ;; While-IR: (while-ir test body) - true iteration, no stack growth
    ((has-tag ir 'while-ir)
     (let* ((test-ir (cadr ir))
            (body-ir (caddr ir))
            (test-code (codegen test-ir rtaddrs fnoffs td))
            (body-code (codegen body-ir rtaddrs fnoffs td))
            (test-size (code-size test-code))
            (body-size (code-size body-code)))
       ;; Layout: test-code, cmp, b.eq(exit), body-code, b(back-to-test)
       ;; From b.eq at X: body starts at X+4, backward-b at X+4+body_size,
       ;; exit at X+4+body_size+4. So skip = (body_size+8)/4 instructions
       (append-all
        (list test-code
              (arm64:cmp 0 0 :imm t)
              ;; If test is false (x0==0), skip body and back-branch
              (arm64:b.eq (ash (+ body-size 8) -2))
              body-code
              ;; Jump back to start of test
              (arm64:b (ash (- 0 (+ test-size 8 body-size)) -2))))))

    ;; Let-IR: (let-ir vals body count offs)
    ((has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (body-ir (caddr ir))
            (offs (nth 3 (cdr ir)))  ; offs is at index 4: (let-ir vals body count offs)
            (xs (temp-slot td))
            (nd (+ td 1))
            (save-x24 (arm64:str 24 31 :offset xs)))
       ;; Generate bindings with proper offsets
       (labels ((gen-binds (vs os acc)
                  (if (null vs)
                      acc
                      (let* ((restore-x24 (if acc (arm64:ldr 24 31 :offset xs) nil))
                             (val-code (codegen (car vs) rtaddrs fnoffs nd))
                             (store-code (append (arm64:sub 1 20 (* (car os) 8) :imm t)
                                                      (arm64:str 0 1 :offset 0))))
                        (gen-binds (cdr vs) (cdr os)
                                   (append-all (list acc restore-x24 val-code store-code)))))))
         (let* ((bindings-code (gen-binds vals offs nil))
                (restore-final (arm64:ldr 24 31 :offset xs))
                (body-code (codegen body-ir rtaddrs fnoffs nd)))
           (append-all (list save-x24 bindings-code restore-final body-code))))))

    ;; Progn-IR
    ((has-tag ir 'progn-ir)
     (let ((forms (cadr ir)))
       (codegen-progn-forms forms rtaddrs fnoffs td)))

    ;; sys-exit-IR
    ((has-tag ir 'sys-exit-ir)
     (let ((arg-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append arg-code
                    (append (arm64:lsr 0 0 4 :imm t)
                                 (list (list :extern-call "_exit"))))))

    ;; get-cmdline-args-IR: returns list of command-line argument strings
    ;; ARM64 implementation that builds the list in correct order
    ;; Iterates from i=argc-1 down to 0, consing each string onto result
    ((has-tag ir 'get-cmdline-args-ir)
     (let ((slot0 (temp-slot td))
           (slot1 (temp-slot (+ td 1)))
           (slot2 (temp-slot (+ td 2)))
           (slot3 (temp-slot (+ td 3)))
           (slot4 (temp-slot (+ td 4))))
       ;; Register usage:
       ;; x9 = argc (untagged), x10 = argv, x11 = loop index i
       ;; x12 = current argv[i] pointer, x13 = string scan pointer
       ;; x14 = string length, x15-x19 = scratch
       ;; x20 = saved string start for tagging
       ;; x0 = result list accumulator
       (append-all
        (list
         ;; Save callee-saved registers we'll use (x20)
         (arm64:str 20 31 :offset slot0)
         ;; Load argc and argv from GC globals
         (arm64:ldr 9 27 :offset 64)   ; x9 = argc (at [x27+64])
         (arm64:ldr 10 27 :offset 72)  ; x10 = argv (at [x27+72])
         ;; result = nil (tagged 0x06)
         (movz 0 6)
         ;; i = argc - 1, set flags
         (arm64:subs 11 9 1 :imm t)
         ;; if argc <= 0, skip to done (branch forward 44 instructions)
         (arm64:b.lt 44)

         ;; === MAIN LOOP (instruction 5) ===
         ;; Load argv[i]: x12 = [x10 + x11*8]
         (arm64:ldr-reg 12 10 11 :shift 3)

         ;; === STRLEN LOOP ===
         ;; x13 = scan pointer (start at x12), x14 = length counter
         (arm64:mov 13 12)
         (movz 14 0)
         ;; strlen_loop (instruction 8):
         (arm64:ldrb 15 13 0)
         (arm64:cbz 15 4)   ; if zero, skip 4 instructions to strlen_done
         (arm64:add 13 13 1 :imm t)
         (arm64:add 14 14 1 :imm t)
         (arm64:b -4)       ; back to strlen_loop

         ;; === STRLEN_DONE (instruction 13) ===
         ;; Save loop state to stack
         (arm64:str 0 31 :offset slot1)
         (arm64:str 11 31 :offset slot2)
         (arm64:str 10 31 :offset slot3)
         (arm64:str 14 31 :offset slot4)

         ;; === ALLOCATE STRING ===
         ;; x20 = string address (before bump)
         (arm64:mov 20 28)
         ;; Store tagged length at string header
         (arm64:lsl 15 14 4 :imm t)
         (arm64:str 15 28 :offset 0)

         ;; === COPY BYTES LOOP ===
         ;; x16 = dest (x28+8), x17 = src (x12), x18 = remaining count
         (arm64:add 16 28 8 :imm t)
         (arm64:mov 17 12)
         (arm64:mov 18 14)
         ;; copy_loop (instruction 23):
         (arm64:cbz 18 5)   ; if count == 0, skip 5 to copy_done
         (arm64:ldrb-post 19 17 1)
         (arm64:strb-post 19 16 1)
         (arm64:sub 18 18 1 :imm t)
         (arm64:b -4)       ; back to copy_loop

         ;; === COPY_DONE (instruction 28) ===
         ;; Bump heap pointer: size = 8 + ((length + 7) & ~7)
         (arm64:ldr 14 31 :offset slot4)
         (arm64:add 15 14 15 :imm t)
         (arm64:and* 15 15 -8 :imm t)
         (arm64:add 28 28 15)

         ;; Tag string pointer
         (arm64:orr-imm 21 20 4)

         ;; === CONS STRING ONTO RESULT ===
         ;; Restore result list
         (arm64:ldr 0 31 :offset slot1)
         ;; Allocate cons cell at x28
         (arm64:str 21 28 :offset 0)
         (arm64:str 0 28 :offset 8)
         ;; Tag cons and update result
         (arm64:orr-imm 0 28 1)
         (arm64:add 28 28 16 :imm t)

         ;; === DECREMENT AND LOOP ===
         ;; Restore loop counter and argv
         (arm64:ldr 11 31 :offset slot2)
         (arm64:ldr 10 31 :offset slot3)
         ;; i--, set flags
         (arm64:subs 11 11 1 :imm t)
         ;; if i >= 0, loop back (branch backward 39 instructions)
         (arm64:b.ge -39)

         ;; === DONE (instruction 44) ===
         ;; Restore callee-saved registers
         (arm64:ldr 20 31 :offset slot0)))))

    ;; sys-open-IR: open(path, flags, mode) -> fd
    ((has-tag ir 'sys-open-ir)
     (let* ((path-ir (cadr ir))
            (flags-ir (caddr ir))
            (mode-ir (cadddr ir))
            (nd (+ td 3))
            (path-code (codegen path-ir rtaddrs fnoffs nd))
            (save-path (arm64:str 0 31 :offset (temp-slot td)))
            (flags-code (codegen flags-ir rtaddrs fnoffs nd))
            (save-flags (arm64:str 0 31 :offset (temp-slot (+ td 1))))
            (mode-code (codegen mode-ir rtaddrs fnoffs nd))
            (save-mode (arm64:str 0 31 :offset (temp-slot (+ td 2)))))
       (append-all
        (list path-code save-path flags-code save-flags mode-code save-mode
              (arm64:ldr 0 31 :offset (temp-slot td))
              (arm64:and* 0 0 -8 :imm t)             ; clear string tag (mask ~7)
              (arm64:add 0 0 8 :imm t)           ; skip length field
              (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))
              (arm64:lsr 1 1 4 :imm t)           ; untag flags
              (arm64:ldr 2 31 :offset (temp-slot (+ td 2)))
              (arm64:lsr 2 2 4 :imm t)           ; untag mode
              (list (list :extern-call "_open"))
              (arm64:lsl 0 0 4 :imm t)))))       ; tag result

    ;; sys-write-IR: write(fd, buf, len) -> bytes written
    ((has-tag ir 'sys-write-ir)
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (nd (+ td 3))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (arm64:str 0 31 :offset (temp-slot td)))
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (arm64:str 0 31 :offset (temp-slot (+ td 1))))
            (len-code (codegen len-ir rtaddrs fnoffs nd))
            (save-len (arm64:str 0 31 :offset (temp-slot (+ td 2)))))
       (append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              (arm64:ldr 0 31 :offset (temp-slot td))
              (arm64:lsr 0 0 4 :imm t)           ; untag fd
              (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))
              (arm64:and* 1 1 -8 :imm t)             ; clear string/vector tag (mask ~7)
              (arm64:add 1 1 8 :imm t)           ; skip length field
              (arm64:ldr 2 31 :offset (temp-slot (+ td 2)))
              (arm64:lsr 2 2 4 :imm t)           ; untag len
              (list (list :extern-call "_write"))
              (arm64:lsl 0 0 4 :imm t)))))       ; tag result

    ;; sys-write-char-IR: write a single character (fixnum) to fd
    ((has-tag ir 'sys-write-char-ir)
     (let* ((fd-ir (cadr ir))
            (char-ir (caddr ir))
            (nd (+ td 2))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (arm64:str 0 31 :offset (temp-slot td)))
            (char-code (codegen char-ir rtaddrs fnoffs nd))
            (save-char (arm64:str 0 31 :offset (temp-slot (+ td 1)))))
       (append-all
        (list fd-code save-fd char-code save-char
              ;; Load fd -> x0, untag
              (arm64:ldr 0 31 :offset (temp-slot td))
              (arm64:lsr 0 0 4 :imm t)
              ;; Load char -> x3, untag, store byte to stack
              (arm64:ldr 3 31 :offset (temp-slot (+ td 1)))
              (arm64:lsr 3 3 4 :imm t)
              (arm64:strb 3 31 (temp-slot (+ td 1)))  ; store byte
              ;; x1 = pointer to the byte on stack
              (arm64:add 1 31 (temp-slot (+ td 1)) :imm t)
              ;; x2 = 1 (length)
              (arm64:movz 2 1)
              ;; Call write(fd, &byte, 1)
              (list (list :extern-call "_write"))
              ;; Tag result as fixnum
              (arm64:lsl 0 0 4 :imm t)))))

    ;; sys-read-byte-IR: read a single byte from fd -> byte (0-255) or -1 on EOF/error
    ((has-tag ir 'sys-read-byte-ir)
     (let* ((fd-ir (cadr ir))
            (nd (+ td 1))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd)))
       (append-all
        (list fd-code
              ;; fd -> x0, untag
              (arm64:lsr 0 0 4 :imm t)
              ;; x1 = pointer to stack slot for the byte
              (arm64:add 1 31 (temp-slot td) :imm t)
              ;; x2 = 1 (length)
              (arm64:movz 2 1)
              ;; Call read(fd, &byte, 1)
              (list (list :extern-call "_read"))
              ;; Check return value: if <= 0, return -1 (as fixnum)
              ;; x0 = bytes read (1) or error (<= 0)
              (arm64:cmp 0 1 :imm t)  ; cmp x0, #1
              (arm64:b.lt 4)           ; if x0 < 1, skip to error case
              ;; Success: load the byte from stack, tag as fixnum
              (arm64:ldrb 0 31 (temp-slot td))
              (arm64:lsl 0 0 4 :imm t)  ; tag as fixnum
              (arm64:b 2)              ; skip error case
              ;; Error: return -1 as fixnum (-1 << 4 = -16)
              (arm64:sub 0 31 16 :imm t)))))  ; x0 = xzr - 16 = -16

    ;; sys-read-IR: read(fd, buf, len) -> bytes read
    ((has-tag ir 'sys-read-ir)
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (nd (+ td 3))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (arm64:str 0 31 :offset (temp-slot td)))
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (arm64:str 0 31 :offset (temp-slot (+ td 1))))
            (len-code (codegen len-ir rtaddrs fnoffs nd))
            (save-len (arm64:str 0 31 :offset (temp-slot (+ td 2)))))
       (append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              (arm64:ldr 0 31 :offset (temp-slot td))
              (arm64:lsr 0 0 4 :imm t)           ; untag fd
              (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))
              (arm64:and* 1 1 -8 :imm t)             ; clear vector tag (mask ~7)
              (arm64:add 1 1 8 :imm t)           ; skip length field
              (arm64:ldr 2 31 :offset (temp-slot (+ td 2)))
              (arm64:lsr 2 2 4 :imm t)           ; untag len
              (list (list :extern-call "_read"))
              (arm64:lsl 0 0 4 :imm t)))))       ; tag result

    ;; sys-close-IR: close(fd) -> 0 on success
    ((has-tag ir 'sys-close-ir)
     (let* ((fd-ir (cadr ir))
            (fd-code (codegen fd-ir rtaddrs fnoffs td)))
       (append-all
        (list fd-code
              (arm64:lsr 0 0 4 :imm t)           ; untag fd
              (list (list :extern-call "_close"))
              (arm64:lsl 0 0 4 :imm t)))))       ; tag result

    ;; === JIT Memory Primitives (ARM64 macOS) ===

    ;; mmap-ir: mmap(addr, len, prot, flags, fd, offset) -> addr or -1
    ;; All args are untagged raw values (no tagging/untagging)
    ((has-tag ir 'mmap-ir)
     (let* ((addr-ir (cadr ir))
            (len-ir (caddr ir))
            (prot-ir (cadddr ir))
            (flags-ir (nth 4 ir))
            (fd-ir (nth 5 ir))
            (offset-ir (nth 6 ir))
            (nd (+ td 6))
            (addr-code (codegen addr-ir rtaddrs fnoffs nd))
            (len-code (codegen len-ir rtaddrs fnoffs nd))
            (prot-code (codegen prot-ir rtaddrs fnoffs nd))
            (flags-code (codegen flags-ir rtaddrs fnoffs nd))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (offset-code (codegen offset-ir rtaddrs fnoffs nd)))
       (append-all
        (list
         ;; Compute and save all args to stack slots
         addr-code (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset (temp-slot td))
         len-code (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset (temp-slot (+ td 1)))
         prot-code (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset (temp-slot (+ td 2)))
         flags-code (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset (temp-slot (+ td 3)))
         fd-code (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset (temp-slot (+ td 4)))
         offset-code (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset (temp-slot (+ td 5)))
         ;; Load into arg registers x0-x5
         (arm64:ldr 0 31 :offset (temp-slot td))
         (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))
         (arm64:ldr 2 31 :offset (temp-slot (+ td 2)))
         (arm64:ldr 3 31 :offset (temp-slot (+ td 3)))
         (arm64:ldr 4 31 :offset (temp-slot (+ td 4)))
         (arm64:ldr 5 31 :offset (temp-slot (+ td 5)))
         (list (list :extern-call "_mmap"))))))  ; returns raw pointer in x0

    ;; munmap-ir: munmap(addr, len) -> 0 on success
    ;; NOTE: addr is a RAW pointer (from mmap), len is tagged
    ((has-tag ir 'munmap-ir)
     (let* ((addr-ir (cadr ir))
            (len-ir (caddr ir))
            (nd (+ td 2))
            (addr-code (codegen addr-ir rtaddrs fnoffs nd))
            (len-code (codegen len-ir rtaddrs fnoffs nd)))
       (append-all
        (list addr-code (arm64:str 0 31 :offset (temp-slot td))  ; addr is RAW, no untagging
              len-code (arm64:lsr 0 0 4 :imm t)
              (arm64:mov 1 0)                     ; x1 = len
              (arm64:ldr 0 31 :offset (temp-slot td))  ; x0 = addr (raw)
              (list (list :extern-call "_munmap"))))))

    ;; pthread-jit-write-protect-np-ir: pthread_jit_write_protect_np(enabled)
    ;; enabled = 0: allow write, 1: allow execute
    ((has-tag ir 'pthread-jit-write-protect-np-ir)
     (let* ((enabled-ir (cadr ir))
            (enabled-code (codegen enabled-ir rtaddrs fnoffs td)))
       (append-all
        (list enabled-code
              (arm64:lsr 0 0 4 :imm t)           ; untag enabled
              (list (list :extern-call "_pthread_jit_write_protect_np"))))))

    ;; sys-dcache-flush-ir: sys_dcache_flush(start, size)
    ;; NOTE: start is a RAW pointer, size is tagged
    ((has-tag ir 'sys-dcache-flush-ir)
     (let* ((start-ir (cadr ir))
            (size-ir (caddr ir))
            (nd (+ td 2))
            (start-code (codegen start-ir rtaddrs fnoffs nd))
            (size-code (codegen size-ir rtaddrs fnoffs nd)))
       (append-all
        (list start-code (arm64:str 0 31 :offset (temp-slot td))  ; start is RAW, no untagging
              size-code (arm64:lsr 0 0 4 :imm t)
              (arm64:mov 1 0)                     ; x1 = size
              (arm64:ldr 0 31 :offset (temp-slot td))  ; x0 = start (raw)
              (list (list :extern-call "_sys_dcache_flush"))))))

    ;; sys-icache-invalidate-ir: sys_icache_invalidate(start, size)
    ;; NOTE: start is a RAW pointer, size is tagged
    ((has-tag ir 'sys-icache-invalidate-ir)
     (let* ((start-ir (cadr ir))
            (size-ir (caddr ir))
            (nd (+ td 2))
            (start-code (codegen start-ir rtaddrs fnoffs nd))
            (size-code (codegen size-ir rtaddrs fnoffs nd)))
       (append-all
        (list start-code (arm64:str 0 31 :offset (temp-slot td))  ; start is RAW, no untagging
              size-code (arm64:lsr 0 0 4 :imm t)
              (arm64:mov 1 0)                     ; x1 = size
              (arm64:ldr 0 31 :offset (temp-slot td))  ; x0 = start (raw)
              (list (list :extern-call "_sys_icache_invalidate"))))))

    ;; funcall-ptr-ir: call function pointer, return tagged fixnum
    ;; The function pointer is a RAW address (from mmap), NOT tagged
    ;; Returns: tags the raw x0 as a fixnum (x0 << 4)
    ((has-tag ir 'funcall-ptr-ir)
     (let* ((ptr-ir (cadr ir))
            (ptr-code (codegen ptr-ir rtaddrs fnoffs td)))
       (append-all
        (list ptr-code
              (arm64:blr 0)                       ; branch-link to x0
              (arm64:lsl 0 0 4 :imm t)))))        ; tag result as fixnum

    ;; mem-set-byte-ir: store byte at ptr+offset
    ;; (mem-set-byte ptr offset byte-value)
    ;; NOTE: ptr is a RAW pointer (from mmap), NOT tagged
    ;; offset and byte-value are tagged fixnums
    ((has-tag ir 'mem-set-byte-ir)
     (let* ((ptr-ir (cadr ir))
            (offset-ir (caddr ir))
            (byte-ir (cadddr ir))
            (nd (+ td 3))
            (ptr-code (codegen ptr-ir rtaddrs fnoffs nd))
            (offset-code (codegen offset-ir rtaddrs fnoffs nd))
            (byte-code (codegen byte-ir rtaddrs fnoffs nd)))
       (append-all
        (list ptr-code (arm64:str 0 31 :offset (temp-slot td))  ; ptr is RAW, no untagging
              offset-code (arm64:lsr 0 0 4 :imm t) (arm64:str 0 31 :offset (temp-slot (+ td 1)))
              byte-code (arm64:lsr 0 0 4 :imm t)
              ;; x0 = byte value, x1 = offset, x2 = ptr
              (arm64:mov 3 0)                     ; x3 = byte
              (arm64:ldr 1 31 :offset (temp-slot (+ td 1)))  ; x1 = offset
              (arm64:ldr 0 31 :offset (temp-slot td))  ; x0 = ptr (raw)
              (arm64:add 0 0 1)                   ; x0 = ptr + offset
              (arm64:strb 3 0 0)))))              ; store byte at [x0]

    ;; mem-load-64-ir: load 64-bit word from ptr+offset
    ;; (mem-load-64 ptr offset)
    ;; NOTE: ptr is a RAW pointer (from mmap), NOT tagged
    ;; offset is a tagged fixnum
    ((has-tag ir 'mem-load-64-ir)
     (let* ((ptr-ir (cadr ir))
            (offset-ir (caddr ir))
            (nd (+ td 2))
            (ptr-code (codegen ptr-ir rtaddrs fnoffs nd))
            (offset-code (codegen offset-ir rtaddrs fnoffs nd)))
       (append-all
        (list ptr-code (arm64:str 0 31 :offset (temp-slot td))  ; ptr is RAW, no untagging
              offset-code (arm64:lsr 0 0 4 :imm t)
              ;; x0 = offset, load ptr, compute address, load word
              (arm64:mov 1 0)                     ; x1 = offset
              (arm64:ldr 0 31 :offset (temp-slot td))  ; x0 = ptr (raw)
              (arm64:add 0 0 1)                   ; x0 = ptr + offset
              (arm64:ldr 0 0 :offset 0)))))       ; x0 = [x0] (raw 64-bit value)

    ;; buffer-to-string-ir - convert raw byte buffer to string (inline)
    ((has-tag ir 'buffer-to-string-ir)
     ;; buffer-to-string-ir = (buffer-to-string-ir buf-ir len-ir)
     ;; Inline implementation: allocate string on heap, copy raw bytes from buffer
     ;; Buffer layout: [length (8 bytes)][raw bytes...] (sys-read writes raw bytes)
     ;; String layout: [length (8 bytes)][char data (n bytes)]
     ;; Register usage:
     ;;   x0: result (tagged string)
     ;;   x1: untagged buf base + 8 (raw data start)
     ;;   x2: string data base (untagged string ptr + 8)
     ;;   x3: loop counter (0 to len-1)
     ;;   x4: temp for loading/storing bytes
     ;;   x5: length (untagged)
     (let* ((buf-ir (cadr ir))
            (len-ir (caddr ir))
            (buf-slot (temp-slot td))
            (nd (+ td 1))
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (len-code (codegen len-ir rtaddrs fnoffs nd)))
       (append-all
        (list
         ;; Evaluate buf, save to slot
         buf-code
         (arm64:str 0 31 :offset buf-slot)
         ;; Evaluate len
         len-code
         ;; x5 = length (untagged)
         (arm64:lsr 5 0 4 :imm t)                 ; x5 = len >> 4 (untag)
         ;; x1 = buf data start (untagged buf base + 8)
         (arm64:ldr 1 31 :offset buf-slot)      ; x1 = buf (tagged)
         (arm64:and* 1 1 -8 :imm t)                   ; x1 = buf & ~7 (clear tag)
         (arm64:add 1 1 8 :imm t)                 ; x1 = buf + 8 (skip length header)
         ;; Allocate string: store length at [x28]
         (arm64:str 5 28 :offset 0)             ; [x28+0] = length
         ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
         (arm64:add 4 5 23 :imm t)                ; x4 = len + 23 (= len + 8 + 15)
         (arm64:and* 4 4 -16 :imm t)                  ; x4 = (len + 23) & ~15
         ;; Save string ptr (will be result), bump heap
         (arm64:mov 0 28)                  ; x0 = string base (untagged)
         (arm64:add 28 28 4)               ; x28 += alloc_size
         ;; x2 = string data base = x0 + 8
         (arm64:add 2 0 8 :imm t)                 ; x2 = string data start
         ;; x3 = loop counter = 0
         (movz 3 0)                      ; x3 = 0
         ;; Loop: while x3 < x5
         ;; loop_start: (offset 0 from here)
         (arm64:cmp 3 5)                   ; cmp x3, x5
         (arm64:b.ge 6)           ; if x3 >= x5, jump to loop_end (+6 instructions = 24 bytes)
         ;; Load buf[x3] - raw byte
         (arm64:add 4 1 3)                 ; x4 = buf_data + x3
         (arm64:ldrb 4 4 0)                    ; x4 = byte at [x4]
         ;; Store byte: str_data[x3] = x4
         (strb-reg 4 2 3)                ; [x2 + x3] = x4 (byte)
         ;; x3++
         (arm64:add 3 3 1 :imm t)                 ; x3++
         ;; Jump back to loop_start (cmp instruction)
         (arm64:b -6)                  ; back 6 instructions = -24 bytes
         ;; loop_end:
         ;; Tag result with string tag (0x4)
         (movz 4 4)                      ; x4 = 4
         (arm64:orr 0 0 4)))))

    ;; Function call
    ((has-tag ir 'call-fn)
     (let* ((fn-name (cadr ir))
            (args (caddr ir))
            (num-args (length args))
            (arg-code (codegen-call-args args rtaddrs fnoffs td))
            ;; Load spilled args into registers x1-x7 before call
            (load-code (gen-arg-loads num-args td)))
       ;; Emit call marker that will be resolved by resolve-calls
       ;; NOTE: must double-wrap so append-all keeps marker as single item
       (append-all (list arg-code load-code (list (list :call-fn fn-name))))))

    ;; Lambda reference (closure creation)
    ;; lambda-ref = (lambda-ref name free-offsets)
    ;; After lambda lifting, name is a string that we look up in fnoffs
    ;; Uses load-addr-8 for consistent code size (fnoffs depends on code size)
    ((has-tag ir 'lambda-ref)
     (let* ((name (cadr ir))
            (free-offsets (caddr ir))
            ;; Look up function offset in fnoffs
            (fn-entry (lookup-string name fnoffs))
            (fn-offset (if fn-entry (cdr fn-entry) 0)))
       ;; Build closure on heap: (fn-offset . captured-env)
       ;; First, build captured environment on heap (list of captured values)
       (if (null free-offsets)
           ;; No captures - simple closure
           (append-all
            (list (load-addr-8 0 (ash fn-offset 4))
                  (arm64:str 0 28 :offset 0)
                  (movz 0 0)  ;; nil for empty env
                  (arm64:str 0 28 :offset 8)
                  (arm64:mov 0 28)
                  (arm64:add 0 0 5 :imm t)  ;; closure tag
                  (arm64:add 28 28 16 :imm t)
                  (gc-trigger-code)))
           ;; Has captures - build env cons list first
           (let* ((capture-code (build-captures free-offsets))
                  (xs (temp-slot td)))
             (append-all
              (list ;; Save x24 before building captures
                    (arm64:str 24 31 :offset xs)
                    ;; Build captured env (result in x0)
                    capture-code
                    ;; Save captured env
                    (arm64:str 0 28 :offset 8)
                    ;; Store fn-offset
                    (load-addr-8 0 (ash fn-offset 4))
                    (arm64:str 0 28 :offset 0)
                    ;; Create closure pointer
                    (arm64:mov 0 28)
                    (arm64:add 0 0 5 :imm t)
                    (arm64:add 28 28 16 :imm t)
                    (gc-trigger-code)  ; GC check after closure allocation
                    ;; Restore x24
                    (arm64:ldr 24 31 :offset xs)))))))

    ;; Function reference (closure for named function)
    ;; fn-ref-ir = (fn-ref-ir name) where name is a symbol
    ;; Creates a closure with empty env pointing to the named function
    ;; Uses load-addr-8 for consistent code size
    ((has-tag ir 'fn-ref-ir)
     (let* ((name (cadr ir))
            ;; Look up function offset in fnoffs (symbol key)
            (fn-entry (lookup-string name fnoffs))
            (fn-offset (if fn-entry (cdr fn-entry) 0)))
       ;; Build closure on heap: (fn-offset . nil)
       ;; No captures, so env is nil
       (append-all
        (list (load-addr-8 0 (ash fn-offset 4))
              (arm64:str 0 28 :offset 0)
              (movz 0 0)  ;; nil for empty env
              (arm64:str 0 28 :offset 8)
              (arm64:mov 0 28)
              (arm64:add 0 0 5 :imm t)  ;; closure tag
              (arm64:add 28 28 16 :imm t)
              (gc-trigger-code)))))

    ;; Funcall-IR
    ((has-tag ir 'funcall-ir)
     (let* ((fn-ir (cadr ir))
            (args (caddr ir))
            (num-args (length args))
            (fn-code (codegen fn-ir rtaddrs fnoffs td))
            (cs (temp-slot td))
            (nd (+ td 1))
            (arg-code (codegen-funcall-args args rtaddrs fnoffs nd 0))
            ;; Load args from spill slots to registers x0-x7
            ;; Note: funcall-args uses nd for spill, so load from nd
            (load-code (gen-arg-loads num-args nd)))
       (append-all
        (list fn-code
              (arm64:str 0 31 :offset cs)  ;; Save closure to temp
              arg-code                    ;; Eval and spill args
              load-code                   ;; Load args to x0-x7
              ;; Use x9 for closure to avoid clobbering x0-x7 (args)
              (arm64:ldr 9 31 :offset cs)  ;; x9 = closure
              (arm64:sub 9 9 5 :imm t)       ;; Untag closure
              (arm64:ldr 24 9 :offset 8)   ;; x24 = [x9 + 8] = env
              (arm64:ldr 9 9 :offset 0)    ;; x9 = [x9 + 0] = fn-offset
              (arm64:lsr 9 9 4 :imm t)       ;; Untag fn-offset
              (arm64:add 9 9 26)      ;; x9 = x26 + fn-offset = absolute addr
              (arm64:blr 9)))))

    ;; Get-intern-table: load intern table from [x27 + 0]
    ((has-tag ir 'get-intern-table-ir)
     (arm64:ldr 0 27 :offset 0))

    ;; Set-intern-table: store value to [x27 + 0], return value
    ((has-tag ir 'set-intern-table-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 0))))

    ;; Get-lambda-counter: load counter from [x27 + 8]
    ;; Returns untagged fixnum (the counter is stored pre-tagged at heap+8)
    ((has-tag ir 'get-lambda-counter-ir)
     (arm64:ldr 0 27 :offset 8))

    ;; Set-lambda-counter: store value to [x27 + 8], return value
    ;; Value should already be tagged as fixnum
    ((has-tag ir 'set-lambda-counter-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 8))))

    ;; Get-symbol-counter: load counter from [x27 + 48]
    ;; Returns tagged fixnum (counter stored pre-tagged at offset 48)
    ((has-tag ir 'get-symbol-counter-ir)
     (arm64:ldr 0 27 :offset 48))

    ;; Set-symbol-counter: store value to [x27 + 48], return value
    ;; Value should already be tagged as fixnum
    ((has-tag ir 'set-symbol-counter-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 48))))

    ;; Get-symbol-table-sym: load table pointer from [x27 + 56]
    ;; Returns alist pointer (or nil)
    ((has-tag ir 'get-symbol-table-sym-ir)
     (arm64:ldr 0 27 :offset 56))

    ;; Set-symbol-table-sym: store table to [x27 + 56], return value
    ((has-tag ir 'set-symbol-table-sym-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 56))))

    ;; Get-packages: load packages list from [x27 + 80]
    ((has-tag ir 'get-packages-ir)
     (arm64:ldr 0 27 :offset 80))

    ;; Set-packages: store packages list to [x27 + 80], return value
    ((has-tag ir 'set-packages-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 80))))

    ;; Get-current-package: load current package from [x27 + 88]
    ((has-tag ir 'get-current-package-ir)
     (arm64:ldr 0 27 :offset 88))

    ;; Set-current-package: store current package to [x27 + 88], return value
    ((has-tag ir 'set-current-package-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 88))))

    ;; Get-global-vars: load global variables table from [x27 + 104]
    ;; Returns alist of (symbol-name-string . value) pairs
    ((has-tag ir 'get-global-vars-ir)
     (arm64:ldr 0 27 :offset 104))

    ;; Set-global-vars: store global variables table to [x27 + 104], return value
    ((has-tag ir 'set-global-vars-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (arm64:str 0 27 :offset 104))))

    ;; TCO: loop-ir wraps body that may contain continue-ir nodes
    ;; Just generate body code - the loop-start is resolved at function level
    ((has-tag ir 'loop-ir)
     (codegen (cadr ir) rtaddrs fnoffs td))

    ;; TCO: continue-ir jumps back to loop-start after setting params
    ;; Emits a :tco-branch marker that gets resolved in codegen-fn
    ((has-tag ir 'continue-ir)
     (let* ((arg-irs (cadr ir))
            (nargs (length arg-irs))
            ;; Evaluate args into temp slots first
            (args-code (codegen-tco-args arg-irs rtaddrs fnoffs td 0))
            ;; Copy from temps to params
            (copy-code (codegen-tco-copy-args nargs 0))
            ;; Emit marker to be resolved by resolve-tco-branches
            (branch-code (list (list :tco-branch))))
       (append-all (list args-code copy-code branch-code))))

    ;; Default - return empty
    (t nil)))

;;; ============================================================
;;; Helper: TCO Args Codegen
;;; ============================================================

(defun codegen-tco-args (arg-irs rtaddrs fnoffs td idx)
  "Evaluate args and store to temp slots for TCO continue.
   Uses temp slots at sp+0x40+idx*8 to avoid overwriting params."
  (if (null arg-irs)
      nil
      (let* ((arg-ir (car arg-irs))
             (arg-code (codegen arg-ir rtaddrs fnoffs td))
             ;; Store to temp slot at sp + 0x40 + idx*8
             (slot-offset (+ #x40 (* idx 8)))
             (store-code (arm64:str 0 31 :offset slot-offset))
             (rest-code (codegen-tco-args (cdr arg-irs) rtaddrs fnoffs td (+ idx 1))))
        (append-all (list arg-code store-code rest-code)))))

(defun codegen-tco-copy-args (nargs idx)
  "Copy from temp slots to param slots (at x20 - idx*8).
   Must be done after all args are evaluated to handle cases like
   (f b a) where we're swapping parameters."
  (if (>= idx nargs)
      nil
      (let* (;; Load from temp slot
             (slot-offset (+ #x40 (* idx 8)))
             (load-code (arm64:ldr 9 31 :offset slot-offset))
             ;; Store to param slot [x20 - idx*8]
             (param-offset (* idx 8))
             (store-code (append (arm64:sub 10 20 param-offset :imm t)
                                 (arm64:str 9 10 :offset 0)))
             (rest-code (codegen-tco-copy-args nargs (+ idx 1))))
        (append-all (list load-code store-code rest-code)))))

;;; ============================================================
;;; Helper: Let Bindings Codegen
;;; ============================================================

(defun codegen-let-bindings (bindings rtaddrs fnoffs td idx)
  "Generate code to evaluate and store let bindings"
  (if (null bindings)
      nil
      (let* ((val-ir (car bindings))
             (val-code (codegen val-ir rtaddrs fnoffs td))
             (store-code (append (arm64:sub 1 20 (* idx 8) :imm t)
                                      (arm64:str 0 1 :offset 0)))
             (rest-code (codegen-let-bindings (cdr bindings) rtaddrs fnoffs td (+ idx 1))))
        (append-all (list val-code store-code rest-code)))))

;;; ============================================================
;;; Helper: Progn Forms Codegen
;;; ============================================================

(defun codegen-progn-forms (forms rtaddrs fnoffs td)
  "Generate code for sequence of forms, return value of last"
  (if (null forms)
      nil
      (if (null (cdr forms))
          (codegen (car forms) rtaddrs fnoffs td)
          (let* ((first-code (codegen (car forms) rtaddrs fnoffs td))
                 (rest-code (codegen-progn-forms (cdr forms) rtaddrs fnoffs td)))
            (append first-code rest-code)))))

;;; ============================================================
;;; Helper: Call Arguments Codegen
;;; ============================================================

#-sbcl
(defun spill-base (td)
  "Calculate spill area base for temp depth td.
   Spill area is 0x100-0x1F0 (240 bytes = 30 slots).
   Each nesting level gets 64 bytes (8 slots) of spill area."
  (+ #x100 (* td 64)))

(defun codegen-call-args (args rtaddrs fnoffs td)
  "Generate code for function call arguments"
  (codegen-args-iter args rtaddrs fnoffs td 0))

(defun codegen-args-iter (args rtaddrs fnoffs td argnum)
  "Generate code for args, storing ALL args to spill slots.
   This ensures arg 0 isn't clobbered when evaluating later args.
   Uses td-based offset so nested calls don't clobber each other."
  (if (null args)
      nil
      (let* ((arg-ir (car args))
             ;; Eval arg with incremented td so nested calls use different spill area
             (arg-code (codegen arg-ir rtaddrs fnoffs (+ td 1)))
             ;; Store to spill slot based on current td
             (spill-offset (+ (spill-base td) (* argnum 8)))
             (save-code (arm64:str 0 31 :offset spill-offset)))
        (append-all
         (list arg-code
               save-code
               (codegen-args-iter (cdr args) rtaddrs fnoffs td (+ argnum 1)))))))

;;; ============================================================
;;; Helper: Load Arguments into Registers Before Call
;;; ============================================================

(defun gen-arg-loads (num-args td)
  "Generate code to load spilled args from spill area into registers x0-x7.
   Uses td-based offset to match where args were stored."
  (if (= num-args 0)
      nil
      (let ((base (spill-base td)))
        (labels ((gen-load (i acc)
                   (if (>= i num-args)
                       acc
                       (gen-load (+ i 1)
                                 (append acc
                                              (arm64:ldr i 31 :offset (+ base (* i 8))))))))
          (gen-load 0 nil)))))

;;; ============================================================
;;; Helper: Funcall Arguments Codegen
;;; ============================================================

(defun codegen-funcall-args (args rtaddrs fnoffs td argnum)
  "Generate code for funcall arguments.
   Uses td-based spill area so nested calls don't clobber each other."
  (if (null args)
      nil
      (let* ((arg-ir (car args))
             ;; Eval arg with incremented td so nested calls use different spill area
             (arg-code (codegen arg-ir rtaddrs fnoffs (+ td 1))))
        (if (< argnum 8)
            ;; Args 0-7: store to td-based spill slot
            (let* ((spill-offset (+ (spill-base td) (* argnum 8)))
                   (save-code (arm64:str 0 31 :offset spill-offset)))
              (append-all
               (list arg-code
                     save-code
                     (codegen-funcall-args (cdr args) rtaddrs fnoffs td (+ argnum 1)))))
            ;; Args 8+ go on stack (not yet implemented)
            nil))))

;;; ============================================================
;;; Prologue and Epilogue
;;; ============================================================

#-sbcl
(defun prologue ()
  "Generate function prologue.
   Frame layout after prologue (0x200 bytes = 512 bytes):
   sp+0x1F0: x29 (fp)
   sp+0x1F8: x30 (lr)
   sp+0x10:  x19, x20
   sp+0x20:  x21, x22
   sp+0x30:  x23, x24
   sp+0x40:  temp slots (16 max = 128 bytes, to 0xC0)
   sp+0x0C0: environment base (x20, 8 params = 64 bytes)
   sp+0x100: spill area (240 bytes = 30 slots, to 0x1F0)
   Reduced from 2KB to 512 bytes to allow ~16K nested calls."
  (append-all
   (list (arm64:sub 31 31 #x200 :imm t)           ;; Create 512-byte frame
         (arm64:str 29 31 :offset #x1F0)        ;; Save fp at sp+0x1F0
         (arm64:str 30 31 :offset #x1F8)        ;; Save lr at sp+0x1F8
         (arm64:add 29 31 0 :imm t)               ;; fp = sp
         (arm64:stp 19 20 31 :offset 16)
         (arm64:stp 21 22 31 :offset 32)
         (arm64:stp 23 24 31 :offset 48)
         (arm64:add 20 31 #xC0 :imm t))))

#-sbcl
(defun epilogue ()
  "Generate function epilogue"
  (append-all
   (list (arm64:ldp 23 24 31 :offset 48)
         (arm64:ldp 21 22 31 :offset 32)
         (arm64:ldp 19 20 31 :offset 16)
         (arm64:ldr 29 31 :offset #x1F0)        ;; Restore fp from sp+0x1F0
         (arm64:ldr 30 31 :offset #x1F8)        ;; Restore lr from sp+0x1F8
         (arm64:add 31 31 #x200 :imm t)           ;; Restore 512-byte frame
         (arm64:ret))))

;;; ============================================================
;;; Function Codegen
;;; ============================================================

(defun codegen-fn (fn rtaddrs fnoffs)
  "Generate code for a function.
   Accepts two formats:
   - Native: (name params body-ir param-base) - 4 elements
   - SBCL:   (name params body-ir free-vars free-offsets) - 5 elements
   For SBCL format, param-base = (length free-vars).
   Uses simple fixed frame layout. Supports TCO for self-recursive functions.

   When *use-register-allocation* is true, tries register-allocated codegen first,
   falling back to accumulator-based codegen if IR not fully supported.

   TCO Architecture:
   - Nanopass: apply-tco-to-function (optimize.lisp) transforms tail calls to loop-ir/continue-ir
   - Codegen: handles loop-ir and continue-ir as regular IR nodes, emits :tco-branch markers
   - Emission: resolve-tco-branches converts markers to actual B instructions"
  ;; Try register-allocated codegen if enabled
  #+sbcl
  (when *use-register-allocation*
    (let ((reg-alloc-code (codegen-fn-reg-alloc fn)))
      (when reg-alloc-code
        (return-from codegen-fn reg-alloc-code))))
  ;; Fall back to accumulator-based codegen
  (let* ((params (cadr fn))
         (body-ir (caddr fn))
         (fourth (cadddr fn))
         ;; Detect format: if fourth element is a number, it's param-base (native format)
         ;; If it's a list (or nil), it's free-vars (SBCL format) - use length as param-base
         (param-base (if (numberp fourth)
                         fourth
                         (if fourth (length fourth) 0)))
         ;; For lifted lambdas (param-base > 0), load captured values from x24
         (capture-code (if (> param-base 0)
                           (gen-capture-loads param-base)
                           nil))
         ;; Generate param stores: move x0-x7 to [x20 - offset*8]
         (param-code (gen-param-stores params param-base 0 nil))
         ;; Calculate loop label offset for TCO: prologue + capture + params
         ;; This is where continue-ir branches back to
         (prologue-size (code-size (prologue)))
         (capture-size (if capture-code (code-size capture-code) 0))
         (param-size (if param-code (code-size param-code) 0))
         (loop-label-offset (+ prologue-size capture-size param-size))
         ;; Generate body code - codegen handles loop-ir/continue-ir directly
         (body-code (codegen body-ir rtaddrs fnoffs 0))
         ;; Combine all code
         (all-code (append-all (list (prologue) capture-code param-code body-code (epilogue))))
         ;; Resolve TCO branch markers into actual B instructions
         (resolved-code (resolve-tco-branches all-code loop-label-offset)))
    resolved-code))

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
                      (eq (car item) :loop-continue))))
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
                                   (arm64:sub 9 24 1 :imm t)      ; untag cons
                                   (arm64:ldr 9 9 :offset 0)))  ; x9 = car
                        (store-env (append
                                    (arm64:sub 10 20 offset :imm t)
                                    (arm64:str 9 10 :offset 0))) ; [x20-off] = x9
                        (advance (append
                                  (arm64:sub 9 24 1 :imm t)       ; untag cons
                                  (arm64:ldr 24 9 :offset 8)))) ; x24 = cdr
                   (gen-loads (+ idx 1)
                              (append-all (list acc load-car store-env advance)))))))
    (gen-loads 0 nil)))

#-sbcl
(defun gen-param-stores (params base idx acc)
  "Generate stores from registers x0-x7 to environment slots"
  (if (null params)
      acc
      (if (< idx 8)
          (let* ((offset (* (+ base idx) 8))
                 (store (append (arm64:sub 9 20 offset :imm t)
                                     (arm64:str idx 9 :offset 0))))
            (gen-param-stores (cdr params) base (+ idx 1)
                                   (append acc store)))
          ;; Args 8+ would need stack loading - skip for now
          acc)))

#-sbcl
(defun code-size (code)
  "Calculate size of code in bytes, accounting for markers.
   Markers: :call-fn, :extern-call, :tco-branch, :loop-continue = 4 bytes each.
   :loop-start = 0 bytes (position marker only)."
  (labels ((tally (items acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (cond
                     ((and (consp item) (eq (car item) :call-fn))
                      (tally (cdr items) (+ acc 4)))
                     ((and (consp item) (eq (car item) :extern-call))
                      (tally (cdr items) (+ acc 4)))
                     ((and (consp item) (eq (car item) :tco-branch))
                      (tally (cdr items) (+ acc 4)))
                     ((and (consp item) (eq (car item) :loop-continue))
                      (tally (cdr items) (+ acc 4)))
                     ((and (consp item) (eq (car item) :loop-start))
                      (tally (cdr items) acc)) ; 0 bytes - position marker only
                     ((consp item)
                      (tally (cdr items) (+ acc (tally item 0))))
                     (t
                      (tally (cdr items) (+ acc 1))))))))
    (tally code 0)))

(defun build-fnoffs-pass (fns offset fnoffs acc)
  "Build function offset table: ((name . byte-offset) ...)
   Uses fnoffs for accurate size calculation (may be nil for first pass)."
  (if (null fns)
      (reverse acc)
      (let* ((fn (car fns))
             (name (car fn))
             (code (codegen-fn fn nil fnoffs))
             (size (code-size code))
             (entry (cons name offset)))
        (build-fnoffs-pass (cdr fns) (+ offset size) fnoffs (cons entry acc)))))

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

#-sbcl
(defun codegen-all-fns (fns rtaddrs fnoffs acc)
  "Generate code for all functions with fnoffs"
  (if (null fns)
      acc
      (let* ((fn (car fns))
             (code (codegen-fn fn rtaddrs fnoffs)))
        (codegen-all-fns (cdr fns) rtaddrs fnoffs
                              (append acc code)))))

;;; ============================================================
;;; Main Codegen Entry Point
;;; ============================================================

#-sbcl
(defun codegen-main (mir rtaddrs)
  "Generate main code with prologue/epilogue"
  (append-all
   (list (prologue)
         (codegen mir rtaddrs nil 0)
         (epilogue))))

;;; ============================================================
;;; Resolve Calls (simple version without function linking)
;;; ============================================================

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

(defun deliver (source output-path &optional (heap-size #x4000000))
  "Compile source string to native executable.
   Heap is allocated via mmap at runtime.
   HEAP-SIZE: runtime heap size in bytes (default 64MB).
   Supports: defun, lambda, funcall, GC runtime."
  #-sbcl (register-compiler-symbols)
  (reset-symbol-table)
  (reset-lambda-counter)
  (let* ((forms (read-all source))
         (result (compile-forms forms))
         (defuns-orig (car result))
         (main-ir-orig (cadr result))
         (wrapper-size 210)  ;; mmap heap initialization wrapper
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
           (all-fns (append defuns lambda-as-defuns))
           ;; Generate main code first (with nil fnoffs to get size)
           (main-code-temp (append-all
                            (list (prologue)
                                  (codegen main-ir nil nil 0)
                                  (epilogue))))
           (main-size (code-size main-code-temp))
           ;; Build fnoffs starting after main code
           (fnoffs (build-fnoffs all-fns main-size))
           ;; Regenerate main with fnoffs
           (main-code (append-all
                       (list (prologue)
                             (codegen main-ir nil fnoffs 0)
                             (epilogue))))
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
           (imports (get-unique-imports extern-calls))
           (imports (if (null imports) '("_exit") imports))
           ;; Calculate stubs
           (code-offset #x400)
           (exact-flat-size (count-actual-bytes bytes-with-markers))
           (exact-code-size (+ exact-flat-size wrapper-size))
           (stubs-offset-unaligned (+ code-offset exact-code-size))
           (stubs-offset (* (ceiling stubs-offset-unaligned 4) 4))
           (stub-size 12)
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
           ;; Wrap with mmap-heap initialization
           (wrapped-code (wrap-bytecode-with-mmap-heap flat-code heap-size)))

      ;; Write executable
      (let ((all-fnoffs (append fnoffs
                                (mapcar (lambda (entry)
                                          (cons (car entry)
                                                (- (cdr entry) fn-addr-base)))
                                        gc-fn-alist))))
        (write-macho-executable-mmap-heap output-path wrapped-code imports all-fnoffs)
        #+sbcl (write-symbol-map output-path all-fnoffs main-size imports stubs-offset)))))

(defun deliver-file (source-path output-path &optional (heap-size #x4000000))
  "Compile Lisp file to native executable.
   Usage: (habu:deliver-file \"program.lisp\" \"program\")"
  (deliver (native-read-file source-path) output-path heap-size))

#+sbcl
(defun write-symbol-map (output-path fnoffs main-size imports stubs-offset)
  "Write a symbol map file for debugging.
   Format: HEX_OFFSET NAME (one per line)
   HEX_OFFSET is relative to __TEXT segment start (0x100000000 on macOS).
   To find function from PC: offset = PC - 0x100000454 (base + code_offset + wrapper)"
  (let ((map-path (concatenate 'string output-path ".map"))
        (wrapper-size 116)
        (code-offset #x400))
    (with-open-file (f map-path :direction :output :if-exists :supersede)
      ;; Header comment
      (format f ";; Symbol map for ~A~%" output-path)
      (format f ";; PC to offset: (PC - 0x10000044C) for functions~%")
      (format f ";; Offset is relative to code start (after wrapper)~%~%")
      ;; Main entry
      (format f "0x~8,'0X _main~%" (+ code-offset wrapper-size))
      (format f "0x~8,'0X _main_end~%" (+ code-offset wrapper-size main-size))
      ;; Functions from fnoffs
      (dolist (entry fnoffs)
        (let* ((name (car entry))
               (offset (cdr entry))
               (abs-offset (+ code-offset wrapper-size offset))
               (name-str (if (symbolp name) (symbol-name name) name)))
          (format f "0x~8,'0X ~A~%" abs-offset name-str)))
      ;; Import stubs
      (let ((stub-off stubs-offset))
        (dolist (imp imports)
          (format f "0x~8,'0X stub_~A~%" stub-off imp)
          (setf stub-off (+ stub-off 12))))
      (format t "Symbol map written to ~A~%" map-path))))

(defun count-actual-bytes (items)
  "Count actual bytes in a flattened list, excluding markers.
   Markers are conses like (:extern-call ...), (:fn-label ...), etc."
  (labels ((count-bytes (lst acc)
             (if (null lst)
                 acc
                 (let ((item (car lst)))
                   (if (consp item)
                       ;; Marker - don't count
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

(defun flatten-code-keep-markers-and-calls (code)
  "Flatten code lists but keep both :extern-call, :call-fn, :tco-branch, :loop-start, :loop-continue and :fn-label markers with positions."
  (labels ((flatten (items pos acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ;; Extern call marker - reserve 4 bytes for BL instruction
                     ((and (consp item) (eq (car item) :extern-call))
                      (let ((marker (list :extern-call (cadr item) pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc))))))))
                     ;; Function call marker - reserve 4 bytes for BL instruction
                     ((and (consp item) (eq (car item) :call-fn))
                      (let ((marker (list :call-fn (cadr item) pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc))))))))
                     ;; TCO branch marker - reserve 4 bytes for B instruction
                     ((and (consp item) (eq (car item) :tco-branch))
                      (let ((marker (list :tco-branch (cadr item) pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc))))))))
                     ;; Loop start marker - records position for loop continue to jump to
                     ;; No bytes emitted, just position recorded
                     ((and (consp item) (eq (car item) :loop-start))
                      (let ((marker (list :loop-start pos)))
                        (flatten (cdr items) pos (cons marker acc))))
                     ;; Loop continue marker - reserve 4 bytes for B instruction
                     ((and (consp item) (eq (car item) :loop-continue))
                      (let ((marker (list :loop-continue pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc))))))))
                     ;; Function label marker - used by GC runtime
                     ;; Just record position, no bytes generated
                     ((and (consp item) (eq (car item) :fn-label))
                      (let ((marker (list :fn-label (cadr item) pos)))
                        (flatten (cdr items)
                                 pos
                                 (cons marker acc))))
                    ;; Internal label marker - GC internal jumps use hardcoded offsets
                    ;; Skip entirely, no bytes, no position change
                    ((and (consp item) (eq (car item) :label))
                     (flatten (cdr items) pos acc))
                     ;; Nested list
                     ((consp item)
                      (let* ((flattened (flatten item 0 nil))
                             (size (length flattened)))
                        (flatten (cdr items)
                                 (+ pos size)
                                 (append (reverse flattened) acc))))
                     ;; Byte
                     (t
                      (flatten (cdr items)
                               (+ pos 1)
                               (cons item acc))))))))
    (flatten code 0 nil)))

(defun flatten-all-calls (code fn-alist stub-alist code-base-addr)
  "Replace :call-fn, :extern-call, :loop-start/:loop-continue markers with actual instructions.
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
           (process (items skip result positions loop-stack)
             (if (null items)
                 (cons (reverse result) positions)
                 (let ((item (car items)))
                   (cond
                     ;; Skip placeholder zeros
                     ((> skip 0)
                      (process (cdr items) (- skip 1) result positions loop-stack))
                     ;; Loop start marker - record position on stack, no bytes emitted
                     ((and (consp item) (eq (car item) :loop-start))
                      (let ((pos (cadr item)))
                        (process (cdr items) 0 result positions (cons pos loop-stack))))
                     ;; Loop continue marker - emit B instruction to jump back to loop start
                     ((and (consp item) (eq (car item) :loop-continue))
                      (let* ((pos (cadr item))
                             (b-addr (+ code-base-addr pos))
                             (target-pos (car loop-stack))
                             (target-addr (+ code-base-addr target-pos))
                             (new-result (emit-b b-addr target-addr result)))
                        (process (cdr items) 4 new-result positions loop-stack)))
                     ;; Extern call marker - skip 4 placeholder zeros
                     ((and (consp item) (eq (car item) :extern-call))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (stub-addr (lookup-stub name))
                             (new-result (if stub-addr
                                            (emit-bl bl-addr stub-addr result)
                                            (cons #x94 (cons 0 (cons 0 (cons 0 result)))))))
                        (process (cdr items) 4 new-result (cons (cons name pos) positions) loop-stack)))
                     ;; Function call marker - skip 4 placeholder zeros
                     ((and (consp item) (eq (car item) :call-fn))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (fn-addr (lookup-fn name))
                             (new-result (if fn-addr
                                            (emit-bl bl-addr fn-addr result)
                                            ;; Function not found - emit NOP
                                            (cons #xD5 (cons #x03 (cons #x20 (cons #x1F result)))))))
                        (process (cdr items) 4 new-result (cons (cons name pos) positions) loop-stack)))
                     ;; Function label marker - skip (no bytes)
                     ((and (consp item) (eq (car item) :fn-label))
                      (process (cdr items) 0 result positions loop-stack))
                    ;; Internal label marker - skip (no bytes)
                    ((and (consp item) (eq (car item) :label))
                     (process (cdr items) 0 result positions loop-stack))
                     ;; Regular byte
                     (t
                      (process (cdr items) 0 (cons item result) positions loop-stack)))))))
    (process code 0 nil nil nil)))

(defun extract-fn-labels (code base-addr)
  "Extract :fn-label markers from flattened code and build fn-alist.
   BASE-ADDR is the absolute address where code starts.
   Returns alist of (name . addr)."
  (labels ((collect (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item) (eq (car item) :fn-label))
                       (let* ((name (cadr item))
                              (pos (caddr item))
                              (addr (+ base-addr pos)))
                         (collect (cdr items) (cons (cons name addr) acc)))
                       (collect (cdr items) acc))))))
    (collect code nil)))

(defun alist-lookup (key alist)
  "Look up key in alist, return value or nil"
  (if (null alist)
      nil
      (if (if (symbolp key)
              (eq key (caar alist))
              (equal key (caar alist)))
          (cdar alist)
          (alist-lookup key (cdr alist)))))

(defun flatten-code-keep-markers (code)
  "Flatten nested code lists but keep :extern-call markers intact.
   Tracks position and transforms (:extern-call name) to (:extern-call name pos).
   Each marker followed by 4 zeros = 4 bytes total for BL instruction."
  (labels ((flatten (items pos acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ((and (consp item) (eq (car item) :extern-call))
                      (let ((marker (list :extern-call (cadr item) pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons 0 (cons marker acc))))))))
                     ((consp item)
                      (let* ((flattened (flatten item 0 nil))
                             (size (length flattened)))
                        (flatten (cdr items)
                                 (+ pos size)
                                 (append (reverse flattened) acc))))
                     (t
                      (flatten (cdr items)
                               (+ pos 1)
                               (cons item acc))))))))
    (flatten code 0 nil)))

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
  "Resolve call and loop markers to branch instructions.
   Handles: (:call-fn name), (:tail-call-fn name), (:loop-start), (:loop-continue)
   Note: (:extern-call name) markers are kept as-is for later resolution.
   Native version using arm64 intrinsics."
  (labels ((calc-size (item)
             ;; Calculate byte size of an item
             (cond ((and (consp item) (eq (car item) :call-fn)) 4)
                   ((and (consp item) (eq (car item) :tail-call-fn)) 4)
                   ((and (consp item) (eq (car item) :extern-call)) 4)
                   ((and (consp item) (eq (car item) :loop-start)) 0) ; marker only, no code
                   ((and (consp item) (eq (car item) :loop-continue)) 4) ; B instruction
                   ((and (consp item) (eq (car item) :tco-branch)) 4)
                   (t 1)))
           (lookup-fn (name fnoffs)
             ;; Look up function offset by name (symbol)
             (if (null fnoffs)
                 nil
                 (if (eq name (caar fnoffs))
                     (cdar fnoffs)
                     (lookup-fn name (cdr fnoffs)))))
           (resolve-at (items pos acc loop-stack)
             ;; Iterate through items, tracking position, resolving markers
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ;; Loop start - record position on stack, emit nothing
                     ((and (consp item) (eq (car item) :loop-start))
                      (resolve-at (cdr items) pos acc (cons pos loop-stack)))
                     ;; Loop continue - emit backward branch to loop start
                     ((and (consp item) (eq (car item) :loop-continue))
                      (let* ((loop-start (car loop-stack))
                             (rel-offset (- loop-start pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-stack)))
                     ;; TCO branch - similar to loop-continue but uses stored target
                     ((and (consp item) (eq (car item) :tco-branch))
                      (let* ((target (cadr item))
                             (rel-offset (- target pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-stack)))
                     ;; Internal call - resolve to BL
                     ((and (consp item) (eq (car item) :call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-pos (lookup-fn fn-name fnoffs))
                             (fn-pos (if fn-pos fn-pos 0))
                             (rel-offset (- fn-pos pos))
                             (bl-bytes (arm64:bl (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse bl-bytes) acc)
                                    loop-stack)))
                     ;; Tail call - resolve to B (branch without link)
                     ((and (consp item) (eq (car item) :tail-call-fn))
                      (let* ((fn-name (cadr item))
                             (fn-pos (lookup-fn fn-name fnoffs))
                             (fn-pos (if fn-pos fn-pos 0))
                             (rel-offset (- fn-pos pos))
                             (b-bytes (arm64:b (ash rel-offset -2))))
                        (resolve-at (cdr items)
                                    (+ pos 4)
                                    (append (reverse b-bytes) acc)
                                    loop-stack)))
                     ;; External call - emit marker with position + 3 zero bytes
                     ;; CRITICAL: Must emit 4 bytes to maintain position consistency
                     ((and (consp item) (eq (car item) :extern-call))
                      (resolve-at (cdr items)
                                  (+ pos 4)
                                  (list* 0 0 0 (list :extern-call (cadr item) pos) acc)
                                  loop-stack))
                     ;; Regular byte
                     (t
                      (resolve-at (cdr items)
                                  (+ pos 1)
                                  (cons item acc)
                                  loop-stack)))))))
    (resolve-at code 0 nil nil)))

;;; ============================================================
;;; Export Functions
;;; ============================================================
