;;; Pure ARM64 Codegen - Uses ONLY Habu primitives (no SBCL dependencies)
;;; No defparameter, no dotimes, no dolist, no loop
;;; This can be compiled to native and run without SBCL

#+sbcl (in-package :habu)

;;; ============================================================
;;; State Management using Cons Cells (not defparameter)
;;; ============================================================

;; Symbol table state: ((name . id) ...)
;; Counter stored in first cons cell: ((counter . table) . nil)
;; Access: (car (car state)) = counter, (cdr (car state)) = table

#+sbcl (defvar *symbol-state* (cons (cons 1 nil) nil))

(defun reset-symbol-table ()
  "Reset symbol table state"
  #+sbcl (progn
           (setf (car (car *symbol-state*)) 1)
           (setf (cdr (car *symbol-state*)) nil))
  #-sbcl (progn
           (setcar (car *symbol-state*) 1)
           (setcdr (car *symbol-state*) nil)))

(defun intern-symbol (name)
  "Get or create a symbol ID for NAME"
  (let* ((state *symbol-state*)
         (counter (car (car state)))
         (table (cdr (car state))))
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
              #+sbcl (progn
                       (setf (car (car state)) (+ counter 1))
                       (setf (cdr (car state)) (cons (cons name id) table)))
              #-sbcl (progn
                       (setcar (car state) (+ counter 1))
                       (setcdr (car state) (cons (cons name id) table)))
              id))))))

;;; ============================================================
;;; Lambda Counter State (for lambda lifting)
;;; ============================================================

#+sbcl (defvar *lambda-state* (cons 0 nil))

(defun reset-lambda-counter ()
  "Reset lambda counter"
  #+sbcl (setf (car *lambda-state*) 0)
  #-sbcl (setcar *lambda-state* 0))

(defun gensym-lambda ()
  "Generate unique lambda name as a string like LAMBDA-1, LAMBDA-2, etc."
  (let* ((state *lambda-state*)
         (counter (car state))
         (new-count (+ counter 1)))
    #+sbcl (setf (car state) new-count)
    #-sbcl (setcar state new-count)
    ;; In SBCL mode, use format to create proper string
    ;; In native mode, build from char codes and convert to string
    #+sbcl (format nil "LAMBDA-~D" new-count)
    #-sbcl
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
         (has-tag ir 'vector-ref-ir))
     (let* ((left (cadr ir))
            (right (caddr ir))
            (left-result (lift-lambdas left lambdas))
            (new-left (car left-result))
            (l1 (cdr left-result))
            (right-result (lift-lambdas right l1))
            (new-right (car right-result))
            (l2 (cdr right-result)))
       (cons (list (car ir) new-left new-right) l2)))

    ;; Ternary ops (vector-set-ir)
    ((has-tag ir 'vector-set-ir)
     (let* ((arg1 (cadr ir))
            (arg2 (caddr ir))
            (arg3 (cadddr ir))
            (r1 (lift-lambdas arg1 lambdas))
            (r2 (lift-lambdas arg2 (cdr r1)))
            (r3 (lift-lambdas arg3 (cdr r2))))
       (cons (list 'vector-set-ir (car r1) (car r2) (car r3)) (cdr r3))))

    ;; Unary ops
    ((or (has-tag ir 'car-ir) (has-tag ir 'cdr-ir) (has-tag ir 'get-tag)
         (has-tag ir 'symbol-name-ir) (has-tag ir 'make-symbol-ir)
         (has-tag ir 'string-length-ir)
         (has-tag ir 'make-vector-ir) (has-tag ir 'vector-length-ir)
         (has-tag ir 'make-string-from-vector-ir))
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
;;; ARM64 Instruction Encoders (copied from compiler.lisp)
;;; All pure functions - no state dependencies
;;; ============================================================

(defun encode-word (word)
  (let* ((b0 (logand word #xFF))
         (s1 (ash word -8))
         (b1 (logand s1 #xFF))
         (s2 (ash word -16))
         (b2 (logand s2 #xFF))
         (s3 (ash word -24))
         (b3 (logand s3 #xFF)))
    (list b0 b1 b2 b3)))

(defun movz (rd imm)
  (let* ((masked (logand imm #xFFFF))
         (shifted (ash masked 5))
         (ored (logior #xD2800000 shifted))
         (word (logior ored rd)))
    (encode-word word)))

(defun movk (rd imm shift16)
  "MOVK Rd, #imm, LSL #shift16 - shift16 is 0, 1, 2, or 3 (for 0, 16, 32, 48)"
  (let* ((hw-bits (ash shift16 21))
         (imm-bits (ash (logand imm #xFFFF) 5))
         (base (logior #xF2800000 hw-bits))
         (word (logior base imm-bits rd)))
    (encode-word word)))

(defun add-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x8B000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (encode-word word)))

(defun sub-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #xCB000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (encode-word word)))

(defun mul-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9B007C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (encode-word word)))

(defun sdiv-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9AC00C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (encode-word word)))

(defun lsl-imm (rd rn shift)
  (let* ((s1 (- #x40 shift))
         (immr (logand s1 #x3F))
         (imms (- #x3F shift))
         (immr-shift (ash immr 16))
         (imms-shift (ash imms 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xD3400000 immr-shift))
         (or2 (logior or1 imms-shift))
         (or3 (logior or2 rn-shift))
         (word (logior or3 rd)))
    (encode-word word)))

(defun lsr-imm (rd rn shift)
  (let* ((shift-s (ash shift 16))
         (rn-s (ash rn 5))
         (or1 (logior #xD340FC00 shift-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (encode-word word)))

(defun add-imm (rd rn imm)
  (let* ((imm12 (logand imm #xFFF))
         (shifted (ash imm12 10))
         (rn-shift (ash rn 5))
         (or1 (logior #x91000000 shifted))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (encode-word word)))

(defun sub-imm (rd rn imm)
  (let* ((imm12 (logand imm #xFFF))
         (shifted (ash imm12 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xD1000000 shifted))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (encode-word word)))

(defun str-offset (rt rn offset)
  (let* ((off-scaled (ash offset -3))
         (off-bits (ash (logand off-scaled #xFFF) 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xF9000000 off-bits))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rt)))
    (encode-word word)))

(defun ldr-offset (rt rn offset)
  (let* ((off-scaled (ash offset -3))
         (off-bits (ash (logand off-scaled #xFFF) 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xF9400000 off-bits))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rt)))
    (encode-word word)))

(defun ldrb (rt rn offset)
  "Load byte from [rn + offset] into rt (zero-extended)"
  (let* ((off-bits (ash (logand offset #xFFF) 10))
         (rn-shift (ash rn 5))
         (or1 (logior #x39400000 off-bits))  ; LDRB unsigned offset
         (or2 (logior or1 rn-shift))
         (word (logior or2 rt)))
    (encode-word word)))

(defun stp-offset (rt1 rt2 rn offset)
  (let* ((off7 (logand (ash offset -3) #x7F))
         (off-bits (ash off7 15))
         (rt2-bits (ash rt2 10))
         (rn-bits (ash rn 5))
         (or1 (logior #xA9000000 off-bits))
         (or2 (logior or1 rt2-bits))
         (or3 (logior or2 rn-bits))
         (word (logior or3 rt1)))
    (encode-word word)))

(defun ldp-offset (rt1 rt2 rn offset)
  (let* ((off7 (logand (ash offset -3) #x7F))
         (off-bits (ash off7 15))
         (rt2-bits (ash rt2 10))
         (rn-bits (ash rn 5))
         (or1 (logior #xA9400000 off-bits))
         (or2 (logior or1 rt2-bits))
         (or3 (logior or2 rn-bits))
         (word (logior or3 rt1)))
    (encode-word word)))

(defun cmp-reg (rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #xEB00001F rm-shift rn-shift)))
    (encode-word word)))

(defun cmp-imm (rn imm)
  (let* ((imm12 (logand imm #xFFF))
         (shifted (ash imm12 10))
         (rn-shift (ash rn 5))
         (word (logior #xF100001F shifted rn-shift)))
    (encode-word word)))

(defun cset (rd cond-code)
  (let* ((inv-cond (logxor cond-code 1))
         (cond-bits (logior (ash inv-cond 12) (ash inv-cond 16)))
         (word (logior #x9A9F07E0 cond-bits rd)))
    (encode-word word)))

;; Condition codes
(defun cond-eq () 0)
(defun cond-ne () 1)
(defun cond-lt () 11)
(defun cond-ge () 10)
(defun cond-le () 13)
(defun cond-gt () 12)

(defun b-offset (offset)
  (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
         (word (logior #x14000000 imm26)))
    (encode-word word)))

(defun b-cond (cond-code offset)
  (let* ((imm19 (logand (ash offset -2) #x7FFFF))
         (imm-bits (ash imm19 5))
         (word (logior #x54000000 imm-bits cond-code)))
    (encode-word word)))

(defun bl (offset)
  (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
         (word (logior #x94000000 imm26)))
    (encode-word word)))

(defun blr (rn)
  (let* ((rn-bits (ash rn 5))
         (word (logior #xD63F0000 rn-bits)))
    (encode-word word)))

(defun ret ()
  (encode-word #xD65F03C0))

(defun mov-reg (rd rn)
  "MOV Rd, Rn (alias for ORR Rd, XZR, Rn)"
  (let* ((rn-shift (ash rn 16))
         (word (logior #xAA0003E0 rn-shift rd)))
    (encode-word word)))

(defun and-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #x8A000000 rm-shift rn-shift rd)))
    (encode-word word)))

(defun and-imm (rd rn imm &optional imms immr)
  "AND Xd, Xn, #imm - Bitwise AND with immediate.
   Two forms:
   1. (and-imm rd rn imm) - for simple masks like #xF, #xFF
   2. (and-imm rd rn N imms immr) - full logical immediate encoding
   For clearing low 4 bits (~0xF): (and-imm rd rn 1 #x3C #x3B)"
  (let* ((rn-shift (ash rn 5)))
    (if imms
        ;; 5-arg form: imm=N, imms=imms, immr=immr
        ;; Encoding: 0x92000000 | (N << 22) | (immr << 16) | (imms << 10) | Rn | Rd
        (let* ((n imm)  ; First optional is actually N
               (n-shift (ash n 22))
               (immr-shift (ash immr 16))
               (imms-shift (ash imms 10))
               (word (logior #x92000000 n-shift immr-shift imms-shift rn-shift rd)))
          (encode-word word))
        ;; 3-arg form: simple masks
        (let ((word (if (= imm #xF)
                        ;; For #xF: N=1, immr=0, imms=3 (4 bits of 1s at position 0)
                        (logior #x92400C00 rn-shift rd)
                        (if (= imm #xFF)
                            ;; For #xFF (8 bits): imms=7
                            (logior #x92401C00 rn-shift rd)
                            ;; Unsupported immediate
                            #xD503201F))))
          (encode-word word)))))

(defun orr-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #xAA000000 rm-shift rn-shift rd)))
    (encode-word word)))

(defun eor-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #xCA000000 rm-shift rn-shift rd)))
    (encode-word word)))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

#-sbcl
(defun reverse-helper (lst acc)
  "Tail-recursive reverse helper"
  (if (null lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

#-sbcl
(defun reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

#-sbcl
(defun append (lst1 lst2)
  "Append two lists"
  (labels ((append-iter (l acc)
             (if (null l)
                 acc
                 (append-iter (cdr l) (cons (car l) acc)))))
    (append-iter (reverse-helper lst1 nil) lst2)))

#-sbcl
(defun length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

(defun append-all (lists)
  "Append all lists in LISTS"
  (if (null lists)
      nil
      (if (null (cdr lists))
          (car lists)
          (append (car lists) (append-all (cdr lists))))))

(defun temp-slot (td)
  "Calculate temp slot offset for depth TD"
  (if (>= td 60)
      (progn
        ;; Error: too many temp slots - but we can't use format in pure code
        ;; Just return a safe value
        #x240)
      (+ #x40 (* td 8))))

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
                       (str-offset 9 28 offset)))))))
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
                            (str-offset 9 28 0))))
           (data-code (gen-store-bytes 8 bytes-with-nul nil))
           ;; Return tagged pointer and bump heap
           (result-code (append-all
                         (list (mov-reg 0 28)
                               (add-imm 0 0 4)  ; string tag
                               (add-imm 28 28 total-size)))))
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

(defun save-temp (td)
  (str-offset 0 31 (temp-slot td)))

(defun load-temp (rd td)
  (ldr-offset rd 31 (temp-slot td)))

(defun strb (rt rn offset)
  "Store byte from rt to [rn + offset]"
  (let* ((off-bits (ash (logand offset #xFFF) 10))
         (rn-shift (ash rn 5))
         (or1 (logior #x39000000 off-bits))  ; STRB unsigned offset
         (or2 (logior or1 rn-shift))
         (word (logior or2 rt)))
    (encode-word word)))

(defun strb-reg (rt rn rm)
  "STRB Wt, [Xn, Xm] - store byte to address Xn+Xm"
  ;; Encoding: 00 111 0 00 00 1 Rm 011 0 10 Rn Rt
  ;; #x38206800 = base + shifted register mode
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (word (logior #x38206800 rm-s rn-s rt)))
    (encode-word word)))

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
  (let* ((cbz-instr (cbz count-reg 28))  ; branch +28 bytes (7 instructions) if zero
         (ldrb-instr (ldrb 4 1 0))
         (strb-instr (strb 4 3 0))
         (inc-src (add-imm 1 1 1))
         (inc-dst (add-imm 3 3 1))
         (dec-count (sub-imm count-reg count-reg 1))
         (branch-back (b -24)))  ; branch back 24 bytes (6 instructions)
    (append-all (list cbz-instr ldrb-instr strb-instr
                           inc-src inc-dst dec-count branch-back))))

(defun cbz (rt offset)
  "CBZ rt, offset - compare and branch if zero"
  (let* ((imm19 (logand (ash offset -2) #x7FFFF))
         (imm-bits (ash imm19 5))
         (or1 (logior #xB4000000 imm-bits))
         (word (logior or1 rt)))
    (encode-word word)))

(defun b (offset)
  "B offset - unconditional branch"
  (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
         (word (logior #x14000000 imm26)))
    (encode-word word)))

;;; ============================================================
;;; IR Tag Predicates
;;; ============================================================

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
    ((has-tag ir 'make-vector-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'vector-ref-ir) (or (ir-may-call (cadr ir)) (ir-may-call (caddr ir))))
    ((has-tag ir 'vector-set-ir) (or (ir-may-call (cadr ir))
                                      (ir-may-call (caddr ir))
                                      (ir-may-call (cadddr ir))))
    ((has-tag ir 'vector-length-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'make-string-from-vector-ir) (ir-may-call (cadr ir)))
    ((has-tag ir 'str-lit) nil)
    ((has-tag ir 'if-ir) t)
    ((has-tag ir 'let-ir) t)
    ((has-tag ir 'let*-ir) t)
    ((has-tag ir 'progn-ir) t)
    ;; Syscalls act like function calls (clobber registers)
    ((has-tag ir 'sys-open-ir) t)
    ((has-tag ir 'sys-write-ir) t)
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
                            (load-code (append (sub-imm 1 20 off8)
                                                    (ldr-offset 0 1 0)))
                            ;; Save in temp if not first
                            (store-code (if (null (cdr offs))
                                           nil  ;; Last one, keep in x0
                                           (append-all
                                            (list load-code
                                                  ;; Store car
                                                  (str-offset 0 28 0)
                                                  ;; Load/cons previous result
                                                  (ldr-offset 0 28 8)  ; get cdr (prev result)
                                                  ;; This doesn't work... need different approach
                                                  nil)))))
                       (build-list (cdr offs)
                                   (append acc load-code))))))
        ;; Simpler approach: build cons list iteratively
        ;; Start with nil, then cons each value
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
                                (str-offset 0 28 8)
                                ;; Load current value
                                (sub-imm 1 20 off8)
                                (ldr-offset 0 1 0)
                                ;; Store as car
                                (str-offset 0 28 0)
                                ;; Make cons pointer
                                (mov-reg 0 28)
                                (add-imm 0 0 1)  ;; cons tag
                                (add-imm 28 28 16)))))))
          ;; Don't reverse - gen-cons-chain builds in correct order
          ;; for free-vars list (first offset becomes car)
          (gen-cons-chain free-offsets)))))

;;; ============================================================
;;; Binary Operation Codegen Helper
;;; ============================================================

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
          (list (str-offset 24 31 xs)
                lc
                (save-temp nd)
                (ldr-offset 24 31 xs)
                rc
                (mov-reg 1 0)
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
                (str-offset 24 31 xs)
                rc
                (mov-reg 1 0)
                (load-temp 0 nd)
                (ldr-offset 24 31 xs)
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
                (mov-reg 1 0)
                (load-temp 0 td)
                op-instrs)))))))

;;; ============================================================
;;; Main Codegen Function (handles all IR nodes)
;;; ============================================================

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

    ;; Nil
    ((has-tag ir 'nil-ir)
     (movz 0 0))

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
       (append (sub-imm 1 20 off8)
                    (ldr-offset 0 1 0))))

    ;; Variable assignment (setq)
    ((has-tag ir 'setq-ir)
     (let* ((off (cadr ir))
            (val-ir (caddr ir))
            (off8 (* off 8))
            (val-code (codegen val-ir rtaddrs fnoffs td)))
       ;; Compile value to x0, then store at x20 - offset*8
       (append-all
        (list val-code
              (sub-imm 1 20 off8)
              (str-offset 0 1 0)))))

    ;; Addition
    ((has-tag ir 'add)
     (codegen-binop (cadr ir) (caddr ir)
                         (add-reg 0 0 1)
                         rtaddrs fnoffs td))

    ;; Subtraction
    ((has-tag ir 'sub)
     (codegen-binop (cadr ir) (caddr ir)
                         (sub-reg 0 0 1)
                         rtaddrs fnoffs td))

    ;; Multiplication (untag one operand)
    ((has-tag ir 'mul)
     (codegen-binop (cadr ir) (caddr ir)
                         (append (lsr-imm 1 1 4)
                                      (mul-reg 0 0 1))
                         rtaddrs fnoffs td))

    ;; Division
    ((has-tag ir 'div)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (lsr-imm 0 0 4)
                                (lsr-imm 1 1 4)
                                (sdiv-reg 0 0 1)
                                (lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Modulo: a mod b = a - (a/b)*b
    ((has-tag ir 'mod)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (lsr-imm 9 0 4)    ; x9 = a untagged
                                (lsr-imm 10 1 4)   ; x10 = b untagged
                                (sdiv-reg 11 9 10) ; x11 = a/b
                                (mul-reg 11 11 10) ; x11 = (a/b)*b
                                (sub-reg 0 9 11)   ; x0 = a - (a/b)*b
                                (lsl-imm 0 0 4)))  ; tag result
                         rtaddrs fnoffs td))

    ;; Comparison (equality)
    ((has-tag ir 'cmp-eq)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (cmp-reg 0 1)
                                (cset 0 (cond-eq))
                                (lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Less than
    ((has-tag ir 'cmp-lt)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (cmp-reg 0 1)
                                (cset 0 (cond-lt))
                                (lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Greater than
    ((has-tag ir 'cmp-gt)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (cmp-reg 0 1)
                                (cset 0 (cond-gt))
                                (lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Less than or equal
    ((has-tag ir 'cmp-le)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (cmp-reg 0 1)
                                (cset 0 (cond-le))
                                (lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Greater than or equal
    ((has-tag ir 'cmp-ge)
     (codegen-binop (cadr ir) (caddr ir)
                         (append-all
                          (list (cmp-reg 0 1)
                                (cset 0 (cond-ge))
                                (lsl-imm 0 0 4)))
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
        (list (str-offset 24 31 xs)
              car-code
              (str-offset 0 31 cs)
              (ldr-offset 24 31 xs)
              cdr-code
              (str-offset 0 28 8)
              (ldr-offset 0 31 cs)
              (str-offset 0 28 0)
              (mov-reg 0 28)
              (add-imm 0 0 1)
              (add-imm 28 28 16)
              (ldr-offset 24 31 xs)))))

    ;; Car
    ((has-tag ir 'car-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append inner-code
                    (append (sub-imm 0 0 1)
                                 (ldr-offset 0 0 0)))))

    ;; Cdr
    ((has-tag ir 'cdr-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append inner-code
                    (append (sub-imm 0 0 1)
                                 (ldr-offset 0 0 8)))))

    ;; String-length: string layout is [length:8][data...]
    ;; String tag is 4, so untag and load length from offset 0
    ((has-tag ir 'string-length-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append-all
        (list inner-code
              (sub-imm 0 0 4)        ; untag string
              (ldr-offset 0 0 0)     ; load length
              (lsl-imm 0 0 4)))))    ; tag as fixnum

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
              (str-offset 0 31 spill-off)
              idx-code
              ;; x0 = tagged index, x1 = string pointer
              (ldr-offset 1 31 spill-off)
              ;; Untag index (shift right 4)
              (lsr-imm 0 0 4)
              ;; Untag string pointer
              (sub-imm 1 1 4)
              ;; Add 8 for header, then add index
              (add-imm 1 1 8)
              (add-reg 1 1 0)
              ;; Load byte at [x1]
              (ldrb 0 1 0)
              ;; Tag as fixnum
              (lsl-imm 0 0 4)))))

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
              (str-offset 0 31 spill1)
              str2-code
              ;; Spill str2
              (str-offset 0 31 spill2)
              ;; Load str1, get len1 into x9
              (ldr-offset 1 31 spill1)
              (sub-imm 1 1 4)            ; untag
              (ldr-offset 9 1 0)         ; x9 = len1
              ;; Load str2, get len2 into x10
              (ldr-offset 2 31 spill2)
              (sub-imm 2 2 4)            ; untag
              (ldr-offset 10 2 0)        ; x10 = len2
              ;; x11 = len1 + len2 (total length)
              (add-reg 11 9 10)
              ;; Save total length
              (str-offset 11 31 spill3)
              ;; Store total length at heap[0]
              (str-offset 11 28 0)
              ;; Save heap start for result
              (mov-reg 0 28)
              ;; Calculate aligned size: (8 + total + 7) & ~7
              (add-imm 12 11 15)         ; +8 header +7 for alignment
              (and-imm 12 12 #xFFFFFFF8) ; align to 8
              ;; Bump heap by aligned size
              (add-reg 28 28 12)
              ;; Now copy str1 bytes to result+8
              ;; x1 = src1 (str1+8), x3 = dst (result+8), x9 = len1
              (ldr-offset 1 31 spill1)
              (sub-imm 1 1 4)            ; untag str1
              (add-imm 1 1 8)            ; skip header
              (add-imm 3 0 8)            ; dst = result + 8
              ;; Copy loop for str1 (x9 = count)
              ;; This is a simple byte-by-byte copy
              (gen-memcpy-inline 9)
              ;; Now copy str2 bytes
              ;; x3 already points past str1 data
              ;; x1 = src2 (str2+8), x10 = len2
              (ldr-offset 1 31 spill2)
              (sub-imm 1 1 4)            ; untag str2
              (add-imm 1 1 8)            ; skip header
              (mov-reg 9 10)             ; count = len2
              (gen-memcpy-inline 9)
              ;; Return tagged result
              (add-imm 0 0 4)))))        ; string tag

    ;; Make-vector: allocate vector on heap
    ;; Vector layout: [length (8 bytes)] [data (n * 8 bytes)]
    ;; Total size = 8 + (untagged_size * 8), rounded to 16 for tagging
    ((has-tag ir 'make-vector-ir)
     (let* ((size-ir (cadr ir))
            (sc (codegen size-ir rtaddrs fnoffs td)))
       (append-all
        (list sc
              ;; x0 = tagged size, store untagged length at [x28+0]
              (lsr-imm 1 0 4)           ; x1 = untagged length
              (str-offset 1 28 0)       ; [x28+0] = length
              ;; Calculate allocation size: 8 + (x0 >> 1)
              (lsr-imm 1 0 1)           ; x1 = x0 >> 1 = untagged_size * 8
              (add-imm 1 1 8)           ; x1 = 8 + data_size = total size
              ;; Round to 16-byte alignment: (x1 + 15) & ~15
              (add-imm 1 1 15)          ; x1 = total + 15
              (and-imm 1 1 1 59 60) ; x1 = x1 & ~15 (immr=60 = rotate left by 4, mask 0xFFF...F0)
              ;; Return tagged pointer, bump heap
              (mov-reg 0 28)            ; x0 = current heap ptr
              (add-reg 28 28 1)         ; x28 += total size (now 16-aligned)
              ;; Tag with vector tag (0x3)
              (movz 1 3)
              (orr-reg 0 0 1)))))

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
            (sv (str-offset 0 31 xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd))
            (si (str-offset 0 31 xs2))
            (vlc (codegen val-ir rtaddrs fnoffs nd)))
       ;; After codegen: val in x0, vec at [sp+xs], idx at [sp+xs2]
       (append-all
        (list vc sv ic si vlc
              ;; x0 = val, load vec -> x1, idx -> x2
              (ldr-offset 1 31 xs)         ; x1 = vec (tagged with 3)
              (ldr-offset 2 31 xs2)        ; x2 = idx (tagged)
              ;; Clear tag from vec by subtracting 3
              (sub-imm 1 1 3)              ; x1 = vec_ptr (untagged)
              ;; Calculate offset: x2 = (idx >> 1) + 8
              (lsr-imm 2 2 1)              ; x2 = idx >> 1 = idx_untagged * 8
              (add-imm 2 2 8)              ; x2 = offset = 8 + idx_untagged * 8
              ;; Store val at vec_ptr + offset
              (add-reg 1 1 2)              ; x1 = address
              (str-offset 0 1 0)))))       ; [x1] = val

    ;; Vector-ref: get element at index
    ;; (vector-ref-ir vec-ir idx-ir)
    ((has-tag ir 'vector-ref-ir)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (xs (temp-slot td))
            (nd (+ td 1))
            (vc (codegen vec-ir rtaddrs fnoffs nd))
            (sv (str-offset 0 31 xs))
            (ic (codegen idx-ir rtaddrs fnoffs nd)))
       ;; After codegen: idx in x0, vec at [sp+xs]
       (append-all
        (list vc sv ic
              ;; x0 = idx, load vec -> x1
              (ldr-offset 1 31 xs)         ; x1 = vec (tagged with 3)
              ;; Clear tag from vec by subtracting 3
              (sub-imm 1 1 3)              ; x1 = vec_ptr (untagged)
              ;; Calculate offset: x0 = (idx >> 1) + 8
              (lsr-imm 0 0 1)              ; x0 = idx >> 1 = idx_untagged * 8
              (add-imm 0 0 8)              ; x0 = offset = 8 + idx_untagged * 8
              ;; Load element from vec_ptr + offset
              (add-reg 1 1 0)              ; x1 = address
              (ldr-offset 0 1 0)))))       ; x0 = [x1] = element (already tagged)

    ;; Vector-length: get vector size
    ;; (vector-length-ir vec-ir)
    ((has-tag ir 'vector-length-ir)
     (let* ((vec-ir (cadr ir))
            (vc (codegen vec-ir rtaddrs fnoffs td)))
       (append-all
        (list vc
              ;; x0 = vec (tagged with 3)
              ;; Clear tag by subtracting 3
              (sub-imm 0 0 3)              ; x0 = vec_ptr (untagged)
              ;; Load length: x0 = [x0+0]
              (ldr-offset 0 0 0)           ; x0 = raw length (untagged integer)
              ;; Tag as fixnum: x0 = x0 << 4
              (lsl-imm 0 0 4)))))          ; x0 = tagged fixnum length

    ;; Make-string-from-vector: convert vector of char codes to string
    ;; (make-string-from-vector-ir vec-ir)
    ((has-tag ir 'make-string-from-vector-ir)
     (let* ((vec-ir (cadr ir))
            (vc (codegen vec-ir rtaddrs fnoffs td)))
       (append-all
        (list vc
              ;; x0 = vec (tagged with 3)
              ;; x1 = untagged vec base
              (sub-imm 1 0 3)              ; x1 = vec_ptr (untagged)
              ;; x5 = vec length (raw)
              (ldr-offset 5 1 0)           ; x5 = [x1+0] = length
              ;; Allocate string: store length at [x28], compute alloc size
              (str-offset 5 28 0)          ; [x28+0] = length
              ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
              (add-imm 4 5 23)             ; x4 = len + 23 (= len + 8 + 15)
              ;; Clear low 4 bits: x4 = x4 & ~15
              ;; Using AND with #xF mask then subtract (since and-imm encoding is complex)
              ;; Actually, simpler: x4 = (x4 >> 4) << 4
              (lsr-imm 4 4 4)              ; x4 = x4 >> 4
              (lsl-imm 4 4 4)              ; x4 = (x4 >> 4) << 4 = x4 & ~15
              ;; Save string ptr (will be result), bump heap
              (mov-reg 0 28)               ; x0 = string base (untagged)
              (add-reg 28 28 4)            ; x28 += alloc_size
              ;; x2 = string data base = x0 + 8
              (add-imm 2 0 8)              ; x2 = string data start
              ;; x3 = loop counter = 0
              (movz 3 0)                   ; x3 = 0
              ;; Loop: while x3 < x5
              ;; loop_start: (offset 0 from here)
              (cmp-reg 3 5)                ; cmp x3, x5
              (b-cond (cond-ge) 36)        ; if x3 >= x5, jump to loop_end (+9 instructions = 36 bytes)
              ;; Load vec[x3]: address = x1 + 8 + x3*8
              (lsl-imm 4 3 3)              ; x4 = x3 * 8
              (add-imm 4 4 8)              ; x4 = 8 + x3*8 (offset in vec)
              (add-reg 4 1 4)              ; x4 = vec_base + offset
              (ldr-offset 4 4 0)           ; x4 = [x4] = tagged fixnum
              ;; Untag: x4 = x4 >> 4
              (lsr-imm 4 4 4)              ; x4 = char value (untagged)
              ;; Store byte: str_data[x3] = x4
              (strb-reg 4 2 3)             ; [x2 + x3] = x4 (byte)
              ;; x3++
              (add-imm 3 3 1)              ; x3++
              ;; Jump back to loop_start (cmp instruction)
              (b-offset -36)               ; back 9 instructions = -36 bytes
              ;; loop_end:
              ;; Tag result with string tag (0x4)
              (movz 4 4)                   ; x4 = 4
              (orr-reg 0 0 4)))))

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
              (str-offset 0 31 spill-off)
              val-code
              ;; Restore cons to x1
              (ldr-offset 1 31 spill-off)
              ;; Untag cons (subtract 1)
              (sub-imm 1 1 1)
              ;; Store val at car position (offset 0)
              (str-offset 0 1 0)))))

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
              (str-offset 0 31 spill-off)
              val-code
              ;; Restore cons to x1
              (ldr-offset 1 31 spill-off)
              ;; Untag cons (subtract 1)
              (sub-imm 1 1 1)
              ;; Store val at cdr position (offset 8)
              (str-offset 0 1 8)))))

    ;; Symbol-name - get string name from symbol
    ;; Symbols are stored as (string-pointer | 2), so untag to get string
    ((has-tag ir 'symbol-name-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       ;; Untag symbol (subtract 2), then add string tag (4)
       ;; Result: string-pointer | 4
       (append inner-code
                    (append (sub-imm 0 0 2)
                                 (add-imm 0 0 4)))))

    ;; Make-symbol-from-string - create symbol from string
    ;; Strings are (pointer | 4), symbols are (pointer | 2)
    ((has-tag ir 'make-symbol-ir)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       ;; Untag string (subtract 4), then add symbol tag (2)
       (append inner-code
                    (append (sub-imm 0 0 4)
                                 (add-imm 0 0 2)))))

    ;; Get-tag (extract low 4 bits as tagged fixnum)
    ((has-tag ir 'get-tag)
     (let ((inner-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append inner-code
                    ;; AND x0, x0, #0xF to extract tag bits
                    ;; Then LSL x0, x0, #4 to tag as fixnum
                    (append (and-imm 0 0 #xF)
                                 (lsl-imm 0 0 4)))))

    ;; If-IR
    ((has-tag ir 'if-ir)
     (let* ((cond-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (cond-code (codegen cond-ir rtaddrs fnoffs td))
            (then-code (codegen then-ir rtaddrs fnoffs td))
            (else-code (codegen else-ir rtaddrs fnoffs td))
            ;; Must use code-size to handle :call markers (4 bytes each)
            (else-size (code-size else-code))
            (then-size (code-size then-code)))
       (append-all
        (list cond-code
              (cmp-imm 0 0)
              ;; Branch if cond==0 (false) to skip then + unconditional branch
              (b-cond (cond-eq) (+ then-size 8))
              then-code
              ;; Unconditional branch to skip else
              (b-offset (+ else-size 4))
              else-code))))

    ;; Let-IR: (let-ir vals body count offs)
    ((has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (body-ir (caddr ir))
            (offs (nth 3 (cdr ir)))  ; offs is at index 4: (let-ir vals body count offs)
            (xs (temp-slot td))
            (nd (+ td 1))
            (save-x24 (str-offset 24 31 xs)))
       ;; Generate bindings with proper offsets
       (labels ((gen-binds (vs os acc)
                  (if (null vs)
                      acc
                      (let* ((restore-x24 (if acc (ldr-offset 24 31 xs) nil))
                             (val-code (codegen (car vs) rtaddrs fnoffs nd))
                             (store-code (append (sub-imm 1 20 (* (car os) 8))
                                                      (str-offset 0 1 0))))
                        (gen-binds (cdr vs) (cdr os)
                                   (append-all (list acc restore-x24 val-code store-code)))))))
         (let* ((bindings-code (gen-binds vals offs nil))
                (restore-final (ldr-offset 24 31 xs))
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
                    (append (lsr-imm 0 0 4)
                                 (list (list :extern-call "_exit"))))))

    ;; sys-open-IR: open(path, flags, mode) -> fd
    ((has-tag ir 'sys-open-ir)
     (let* ((path-ir (cadr ir))
            (flags-ir (caddr ir))
            (mode-ir (cadddr ir))
            (nd (+ td 3))
            (path-code (codegen path-ir rtaddrs fnoffs nd))
            (save-path (str-offset 0 31 (temp-slot td)))
            (flags-code (codegen flags-ir rtaddrs fnoffs nd))
            (save-flags (str-offset 0 31 (temp-slot (+ td 1))))
            (mode-code (codegen mode-ir rtaddrs fnoffs nd))
            (save-mode (str-offset 0 31 (temp-slot (+ td 2)))))
       (append-all
        (list path-code save-path flags-code save-flags mode-code save-mode
              (ldr-offset 0 31 (temp-slot td))
              (and-imm 0 0 1 60 61)     ; clear string tag (mask ~7 = 0xFFFFFFFFFFFFFFF8)
              (add-imm 0 0 8)           ; skip length field
              (ldr-offset 1 31 (temp-slot (+ td 1)))
              (lsr-imm 1 1 4)           ; untag flags
              (ldr-offset 2 31 (temp-slot (+ td 2)))
              (lsr-imm 2 2 4)           ; untag mode
              (list (list :extern-call "_open"))
              (lsl-imm 0 0 4)))))       ; tag result

    ;; sys-write-IR: write(fd, buf, len) -> bytes written
    ((has-tag ir 'sys-write-ir)
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (nd (+ td 3))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (str-offset 0 31 (temp-slot td)))
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (str-offset 0 31 (temp-slot (+ td 1))))
            (len-code (codegen len-ir rtaddrs fnoffs nd))
            (save-len (str-offset 0 31 (temp-slot (+ td 2)))))
       (append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              (ldr-offset 0 31 (temp-slot td))
              (lsr-imm 0 0 4)           ; untag fd
              (ldr-offset 1 31 (temp-slot (+ td 1)))
              (and-imm 1 1 1 60 61)     ; clear string/vector tag (mask ~7 = 0xFFFFFFFFFFFFFFF8)
              (add-imm 1 1 8)           ; skip length field
              (ldr-offset 2 31 (temp-slot (+ td 2)))
              (lsr-imm 2 2 4)           ; untag len
              (list (list :extern-call "_write"))
              (lsl-imm 0 0 4)))))       ; tag result

    ;; sys-read-IR: read(fd, buf, len) -> bytes read
    ((has-tag ir 'sys-read-ir)
     (let* ((fd-ir (cadr ir))
            (buf-ir (caddr ir))
            (len-ir (cadddr ir))
            (nd (+ td 3))
            (fd-code (codegen fd-ir rtaddrs fnoffs nd))
            (save-fd (str-offset 0 31 (temp-slot td)))
            (buf-code (codegen buf-ir rtaddrs fnoffs nd))
            (save-buf (str-offset 0 31 (temp-slot (+ td 1))))
            (len-code (codegen len-ir rtaddrs fnoffs nd))
            (save-len (str-offset 0 31 (temp-slot (+ td 2)))))
       (append-all
        (list fd-code save-fd buf-code save-buf len-code save-len
              (ldr-offset 0 31 (temp-slot td))
              (lsr-imm 0 0 4)           ; untag fd
              (ldr-offset 1 31 (temp-slot (+ td 1)))
              (and-imm 1 1 1 60 61)     ; clear vector tag (mask ~7 = 0xFFFFFFFFFFFFFFF8)
              (add-imm 1 1 8)           ; skip length field
              (ldr-offset 2 31 (temp-slot (+ td 2)))
              (lsr-imm 2 2 4)           ; untag len
              (list (list :extern-call "_read"))
              (lsl-imm 0 0 4)))))       ; tag result

    ;; sys-close-IR: close(fd) -> 0 on success
    ((has-tag ir 'sys-close-ir)
     (let* ((fd-ir (cadr ir))
            (fd-code (codegen fd-ir rtaddrs fnoffs td)))
       (append-all
        (list fd-code
              (lsr-imm 0 0 4)           ; untag fd
              (list (list :extern-call "_close"))
              (lsl-imm 0 0 4)))))       ; tag result

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
         (str-offset 0 31 buf-slot)
         ;; Evaluate len
         len-code
         ;; x5 = length (untagged)
         (lsr-imm 5 0 4)                 ; x5 = len >> 4 (untag)
         ;; x1 = buf data start (untagged buf base + 8)
         (ldr-offset 1 31 buf-slot)      ; x1 = buf (tagged)
         (and-imm 1 1 1 60 61)           ; x1 = buf & ~7 (clear tag)
         (add-imm 1 1 8)                 ; x1 = buf + 8 (skip length header)
         ;; Allocate string: store length at [x28]
         (str-offset 5 28 0)             ; [x28+0] = length
         ;; x4 = alloc size = (8 + len + 15) & ~15 for 16-byte alignment
         (add-imm 4 5 23)                ; x4 = len + 23 (= len + 8 + 15)
         (and-imm 4 4 1 59 60)           ; x4 = (len + 23) & ~15 (immr=60 = rotate left by 4)
         ;; Save string ptr (will be result), bump heap
         (mov-reg 0 28)                  ; x0 = string base (untagged)
         (add-reg 28 28 4)               ; x28 += alloc_size
         ;; x2 = string data base = x0 + 8
         (add-imm 2 0 8)                 ; x2 = string data start
         ;; x3 = loop counter = 0
         (movz 3 0)                      ; x3 = 0
         ;; Loop: while x3 < x5
         ;; loop_start: (offset 0 from here)
         (cmp-reg 3 5)                   ; cmp x3, x5
         (b-cond (cond-ge) 24)           ; if x3 >= x5, jump to loop_end (+6 instructions = 24 bytes)
         ;; Load buf[x3] - raw byte
         (add-reg 4 1 3)                 ; x4 = buf_data + x3
         (ldrb 4 4 0)                    ; x4 = byte at [x4]
         ;; Store byte: str_data[x3] = x4
         (strb-reg 4 2 3)                ; [x2 + x3] = x4 (byte)
         ;; x3++
         (add-imm 3 3 1)                 ; x3++
         ;; Jump back to loop_start (cmp instruction)
         (b-offset -24)                  ; back 6 instructions = -24 bytes
         ;; loop_end:
         ;; Tag result with string tag (0x4)
         (movz 4 4)                      ; x4 = 4
         (orr-reg 0 0 4)))))

    ;; Function call
    ((has-tag ir 'call-fn)
     (let* ((fn-name (cadr ir))
            (args (caddr ir))
            (num-args (length args))
            (arg-code (codegen-call-args args rtaddrs fnoffs td))
            ;; Load spilled args into registers x1-x7 before call
            (load-code (gen-arg-loads num-args td)))
       ;; Emit call marker that will be resolved by resolve-calls
       (append-all (list arg-code load-code (list (list :call fn-name))))))

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
                  (str-offset 0 28 0)
                  (movz 0 0)  ;; nil for empty env
                  (str-offset 0 28 8)
                  (mov-reg 0 28)
                  (add-imm 0 0 5)  ;; closure tag
                  (add-imm 28 28 16)))
           ;; Has captures - build env cons list first
           (let* ((capture-code (build-captures free-offsets))
                  (xs (temp-slot td)))
             (append-all
              (list ;; Save x24 before building captures
                    (str-offset 24 31 xs)
                    ;; Build captured env (result in x0)
                    capture-code
                    ;; Save captured env
                    (str-offset 0 28 8)
                    ;; Store fn-offset
                    (load-addr-8 0 (ash fn-offset 4))
                    (str-offset 0 28 0)
                    ;; Create closure pointer
                    (mov-reg 0 28)
                    (add-imm 0 0 5)
                    (add-imm 28 28 16)
                    ;; Restore x24
                    (ldr-offset 24 31 xs)))))))

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
              (str-offset 0 28 0)
              (movz 0 0)  ;; nil for empty env
              (str-offset 0 28 8)
              (mov-reg 0 28)
              (add-imm 0 0 5)  ;; closure tag
              (add-imm 28 28 16)))))

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
              (str-offset 0 31 cs)  ;; Save closure to temp
              arg-code                    ;; Eval and spill args
              load-code                   ;; Load args to x0-x7
              ;; Use x9 for closure to avoid clobbering x0-x7 (args)
              (ldr-offset 9 31 cs)  ;; x9 = closure
              (sub-imm 9 9 5)       ;; Untag closure
              (ldr-offset 24 9 8)   ;; x24 = [x9 + 8] = env
              (ldr-offset 9 9 0)    ;; x9 = [x9 + 0] = fn-offset
              (lsr-imm 9 9 4)       ;; Untag fn-offset
              (add-reg 9 9 26)      ;; x9 = x26 + fn-offset = absolute addr
              (blr 9)))))

    ;; Get-intern-table: load intern table from [x27 + 0]
    ((has-tag ir 'get-intern-table-ir)
     (ldr-offset 0 27 0))

    ;; Set-intern-table: store value to [x27 + 0], return value
    ((has-tag ir 'set-intern-table-ir)
     (let ((val-code (codegen (cadr ir) rtaddrs fnoffs td)))
       (append val-code
               (str-offset 0 27 0))))

    ;; Default - return empty
    (t nil)))

;;; ============================================================
;;; Helper: Let Bindings Codegen
;;; ============================================================

(defun codegen-let-bindings (bindings rtaddrs fnoffs td idx)
  "Generate code to evaluate and store let bindings"
  (if (null bindings)
      nil
      (let* ((val-ir (car bindings))
             (val-code (codegen val-ir rtaddrs fnoffs td))
             (store-code (append (sub-imm 1 20 (* idx 8))
                                      (str-offset 0 1 0)))
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

(defun spill-base (td)
  "Calculate spill area base for temp depth td.
   Each nesting level gets 64 bytes (8 slots) of spill area."
  (+ #x240 (* td 64)))

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
             (save-code (str-offset 0 31 spill-offset)))
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
                                              (ldr-offset i 31 (+ base (* i 8))))))))
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
                   (save-code (str-offset 0 31 spill-offset)))
              (append-all
               (list arg-code
                     save-code
                     (codegen-funcall-args (cdr args) rtaddrs fnoffs td (+ argnum 1)))))
            ;; Args 8+ go on stack (not yet implemented)
            nil))))

;;; ============================================================
;;; Prologue and Epilogue
;;; ============================================================

(defun prologue ()
  "Generate function prologue.
   Frame layout after prologue (0x400 bytes):
   sp+0x3F0: x29 (fp)
   sp+0x3F8: x30 (lr)
   sp+0x10:  x19, x20
   sp+0x20:  x21, x22
   sp+0x30:  x23, x24
   sp+0x40:  temp slots (td*8)
   sp+0x180: environment base (x20)
   sp+0x240: spill area (td*64)
   NOTE: Using STR/LDR instead of STP/LDP for fp/lr because STP's 7-bit
   signed offset can only reach -512 to +504 bytes, but we need 0x3F0 (1008)."
  (append-all
   (list (sub-imm 31 31 #x400)           ;; Create frame first
         (str-offset 29 31 #x3F0)        ;; Save fp at sp+0x3F0
         (str-offset 30 31 #x3F8)        ;; Save lr at sp+0x3F8
         (add-imm 29 31 0)               ;; fp = sp
         (stp-offset 19 20 31 16)
         (stp-offset 21 22 31 32)
         (stp-offset 23 24 31 48)
         (add-imm 20 31 #x180))))

(defun epilogue ()
  "Generate function epilogue"
  (append-all
   (list (ldp-offset 23 24 31 48)
         (ldp-offset 21 22 31 32)
         (ldp-offset 19 20 31 16)
         (ldr-offset 29 31 #x3F0)        ;; Restore fp from sp+0x3F0
         (ldr-offset 30 31 #x3F8)        ;; Restore lr from sp+0x3F8
         (add-imm 31 31 #x400)
         (ret))))

;;; ============================================================
;;; Function Codegen
;;; ============================================================

(defun codegen-fn (fn rtaddrs fnoffs)
  "Generate code for a function: (name params body-ir param-base)
   Uses simple fixed frame layout."
  (let* ((name (car fn))
         (params (cadr fn))
         (body-ir (caddr fn))
         (param-base (cadddr fn))
         ;; For lifted lambdas (param-base > 0), load captured values from x24
         ;; x24 points to a cons list of captured values
         (capture-code (if (> param-base 0)
                           (gen-capture-loads param-base)
                           nil))
         ;; Generate param stores: move x0-x7 to [x20 - offset*8]
         (param-code (gen-param-stores params param-base 0 nil))
         ;; Generate body code
         (body-code (codegen body-ir rtaddrs fnoffs 0)))
    (append-all
     (list (prologue)
           capture-code
           param-code
           body-code
           (epilogue)))))  ;; epilogue includes ret

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
                                   (sub-imm 9 24 1)      ; untag cons
                                   (ldr-offset 9 9 0)))  ; x9 = car
                        (store-env (append
                                    (sub-imm 10 20 offset)
                                    (str-offset 9 10 0))) ; [x20-off] = x9
                        (advance (append
                                  (sub-imm 9 24 1)       ; untag cons
                                  (ldr-offset 24 9 8)))) ; x24 = cdr
                   (gen-loads (+ idx 1)
                              (append-all (list acc load-car store-env advance)))))))
    (gen-loads 0 nil)))

(defun gen-param-stores (params base idx acc)
  "Generate stores from registers x0-x7 to environment slots"
  (if (null params)
      acc
      (if (< idx 8)
          (let* ((offset (* (+ base idx) 8))
                 (store (append (sub-imm 9 20 offset)
                                     (str-offset idx 9 0))))
            (gen-param-stores (cdr params) base (+ idx 1)
                                   (append acc store)))
          ;; Args 8+ would need stack loading - skip for now
          acc)))

(defun code-size (code)
  "Calculate size of code in bytes, accounting for markers"
  (labels ((tally (items acc)
             (if (null items)
                 acc
                 (let ((item (car items)))
                   (cond
                     ((and (consp item) (eq (car item) :call))
                      (tally (cdr items) (+ acc 4)))
                     ((and (consp item) (eq (car item) :extern-call))
                      (tally (cdr items) (+ acc 4)))
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

(defun build-fnoffs (fns offset acc)
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
   For now, this just removes the :call and :extern-call markers.
   Full version needs function offset table."
  (labels ((flatten (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item) (or (eq (car item) :call)
                                              (eq (car item) :extern-call)))
                       ;; Keep extern-call markers for later processing
                       (if (eq (car item) :extern-call)
                           (flatten (cdr items) (cons item acc))
                           ;; Regular call - emit placeholder for now
                           (flatten (cdr items)
                                    (append (reverse (bl 0)) acc)))
                       (if (consp item)
                           (flatten (cdr items) (append (reverse item) acc))
                           (flatten (cdr items) (cons item acc))))))))
    (flatten code nil)))

;;; ============================================================
;;; Pure Delivery (using all pure components)
;;; ============================================================

(defun deliver-v2 (source output-path)
  "Compile source string to native executable using all pure components.
   Uses: compile-forms (pure compiler), codegen (pure codegen),
   wrap-bytecode-with-heap-for-imports (macho), write-macho-executable-with-imports-and-heap.
   Works in both SBCL and native Habu (no SBCL dependencies)."
  (reset-symbol-table)
  (let* ((forms (read-all source))
         (result (compile-forms forms))
         (main-ir (cadr result))
         ;; Generate code using pure codegen
         (code (codegen-main main-ir nil))
         ;; First pass: flatten code lists but keep :extern-call markers
         (bytes-with-markers (flatten-code-keep-markers code))
         ;; Collect extern calls
         (extern-calls (collect-extern-calls bytes-with-markers))
         (imports (get-unique-imports extern-calls))
         (wrapper-size 72))  ;; 18 instructions × 4 bytes

    ;; Always use imports path for consistent Mach-O
    (let ((imports (if (null imports) '("_exit") imports)))

      ;; Calculate stub offsets
      (let* ((num-imports (length imports))
             (stubs-total (if (> num-imports 0) (* num-imports 12) 0))
             (code-offset #x400)
             (exact-flat-size (length bytes-with-markers))
             (exact-code-size (+ exact-flat-size wrapper-size))
             (stubs-offset (+ code-offset exact-code-size))
             (stub-size 12))

        ;; Build stub offset alist
        (let* ((stub-alist (build-stub-alist imports stubs-offset stub-size))
               (flatten-result (flatten-extern-calls bytes-with-markers stub-alist (+ code-offset wrapper-size)))
               (flat-code (car flatten-result)))

          ;; Calculate heap page offset
          (let* ((total-size (+ (length flat-code) wrapper-size))
                 (stubs-end (+ code-offset total-size stubs-total))
                 (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
                 (text-pages-4kb (/ text-vmsize #x1000))
                 (data-const-pages-4kb (/ #x4000 #x1000))
                 (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
                 (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code heap-page-offset)))

            ;; Write Mach-O executable (handles chmod+codesign via native-write-executable)
            (write-macho-executable-with-imports-and-heap output-path wrapped-code imports #x800000)))))))

(defun deliver-v3 (source output-path)
  "Compile source string with function definitions to native executable.
   Supports: defun, lambda, funcall, function calls, all v2 features.
   Layout: wrapper(72) + main-code + function-code + lambda-code + stubs
   Works in both SBCL and native Habu (no SBCL dependencies)."
  (reset-symbol-table)
  (reset-lambda-counter)
  (let* ((forms (read-all source))
         (result (compile-forms forms))
         (defuns-orig (car result))
         (main-ir-orig (cadr result))
         (wrapper-size 72)  ;; 18 instructions × 4 bytes
         ;; Lift lambdas from main-ir
         (main-lift-result (lift-lambdas main-ir-orig nil))
         (main-ir (car main-lift-result))
         (main-lambdas (cdr main-lift-result))
         ;; Lift lambdas from defun bodies
         (defun-lift-result (lift-lambdas-from-defuns defuns-orig nil nil))
         (defuns (car defun-lift-result))
         (defun-lambdas (cdr defun-lift-result))
         ;; Combine all lambdas
         (all-lambdas (append main-lambdas defun-lambdas))
         ;; Check if we have any functions at all
         (has-fns (or (not (null defuns)) (not (null all-lambdas)))))

    (if (not has-fns)
        ;; No functions or lambdas - use v2
        (deliver-v2 source output-path)

        ;; Has functions/lambdas - full compilation
        ;; Combine defuns and lambdas (lambdas need to be converted to defun format)
        (let* ((lambda-as-defuns (lambdas-to-defuns all-lambdas nil))
               (all-fns (append defuns lambda-as-defuns))
               ;; Generate main code first (with nil fnoffs to get size)
               (main-code-temp (append-all
                                (list (prologue)
                                      (codegen main-ir nil nil 0)
                                      (epilogue))))
               (main-size (code-size main-code-temp))
               ;; Build fnoffs starting after main code (relative to code start after wrapper)
               (fnoffs (build-fnoffs all-fns main-size nil))
               ;; Regenerate main with fnoffs
               (main-code (append-all
                           (list (prologue)
                                 (codegen main-ir nil fnoffs 0)
                                 (epilogue))))
               ;; Generate all function code (defuns + lambdas)
               (fn-code (codegen-all-fns all-fns nil fnoffs nil))
               ;; Combine all code
               (all-code (append main-code fn-code))
               ;; Flatten with markers tracking positions
               (bytes-with-markers (flatten-code-keep-markers-and-calls all-code))
               ;; Collect extern calls
               (extern-calls (collect-extern-calls bytes-with-markers))
               (imports (get-unique-imports extern-calls))
               (imports (if (null imports) '("_exit") imports))
               ;; Calculate stubs
               (num-imports (length imports))
               (stubs-total (* num-imports 12))
               (code-offset #x400)
               (exact-flat-size (length bytes-with-markers))
               (exact-code-size (+ exact-flat-size wrapper-size))
               (stubs-offset (+ code-offset exact-code-size))
               (stub-size 12)
               ;; Build stub alist
               (stub-alist (build-stub-alist imports stubs-offset stub-size))
               ;; Convert fnoffs to byte addresses (relative to code-offset + wrapper-size)
               (fn-addr-base (+ code-offset wrapper-size))
               (fn-alist (build-fn-addr-alist fnoffs fn-addr-base nil))
               ;; Flatten both :call and :extern-call markers
               (flatten-result (flatten-all-calls bytes-with-markers fn-alist stub-alist fn-addr-base))
               (flat-code (car flatten-result))
               ;; Calculate heap
               (total-size (+ (length flat-code) wrapper-size))
               (stubs-end (+ code-offset total-size stubs-total))
               (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
               (text-pages-4kb (/ text-vmsize #x1000))
               (data-const-pages-4kb (/ #x4000 #x1000))
               (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
               (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code heap-page-offset)))

          ;; Write executable (handles chmod+codesign via native-write-executable)
          (write-macho-executable-with-imports-and-heap output-path wrapped-code imports #x800000)))))

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
  "Flatten code lists but keep both :extern-call and :call markers with positions."
  (labels ((flatten (items pos acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ;; Extern call marker
                     ((and (consp item) (eq (car item) :extern-call))
                      (let ((marker (list :extern-call (cadr item) pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons marker acc)))))))
                     ;; Function call marker
                     ((and (consp item) (eq (car item) :call))
                      (let ((marker (list :call (cadr item) pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons marker acc)))))))
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
  "Replace both :call and :extern-call markers with BL instructions.
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
           (process (items skip result positions)
             (if (null items)
                 (cons (reverse result) positions)
                 (let ((item (car items)))
                   (cond
                     ;; Skip placeholder zeros
                     ((> skip 0)
                      (process (cdr items) (- skip 1) result positions))
                     ;; Extern call marker
                     ((and (consp item) (eq (car item) :extern-call))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (stub-addr (lookup-stub name))
                             (new-result (if stub-addr
                                            (emit-bl bl-addr stub-addr result)
                                            (cons #x94 (cons 0 (cons 0 (cons 0 result)))))))
                        (process (cdr items) 3 new-result (cons (cons name pos) positions))))
                     ;; Function call marker
                     ((and (consp item) (eq (car item) :call))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (fn-addr (lookup-fn name))
                             (new-result (if fn-addr
                                            (emit-bl bl-addr fn-addr result)
                                            ;; Function not found - emit NOP
                                            (cons #xD5 (cons #x03 (cons #x20 (cons #x1F result)))))))
                        (process (cdr items) 3 new-result (cons (cons name pos) positions))))
                     ;; Regular byte
                     (t
                      (process (cdr items) 0 (cons item result) positions)))))))
    (process code 0 nil nil)))

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
   Each marker followed by 3 zeros = 4 bytes total."
  (labels ((flatten (items pos acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (cond
                     ;; Extern call marker - add position, then 3 zeros
                     ((and (consp item) (eq (car item) :extern-call))
                      (let ((name (cadr item))
                            (marker (list :extern-call (cadr item) pos)))
                        (flatten (cdr items)
                                 (+ pos 4)
                                 (cons 0 (cons 0 (cons 0 (cons marker acc)))))))
                     ;; Nested list - recursively flatten
                     ((consp item)
                      (let* ((flattened (flatten item 0 nil))
                             (size (length flattened)))
                        (flatten (cdr items)
                                 (+ pos size)
                                 (append (reverse flattened) acc))))
                     ;; Byte - add directly
                     (t
                      (flatten (cdr items)
                               (+ pos 1)
                               (cons item acc))))))))
    (flatten code 0 nil)))

(defun flatten-extern-calls (code stub-alist code-base-addr)
  "Replace extern call markers with BL instructions using stub-alist.
   stub-alist is ((name . stub-addr) ...).
   Returns (cons flattened-code extern-call-positions)."
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
                     ;; Extern call marker - emit BL, skip next 3 zeros
                     ((and (consp item) (eq (car item) :extern-call))
                      (let* ((name (cadr item))
                             (pos (caddr item))
                             (bl-addr (+ code-base-addr pos))
                             (stub-addr (lookup name stub-alist))
                             (new-result (if stub-addr
                                            (emit-bl bl-addr stub-addr result)
                                            ;; Placeholder if no stub found
                                            (cons #x94 (cons 0 (cons 0 (cons 0 result)))))))
                        (process (cdr items) 3 new-result (cons (cons name pos) positions))))
                     ;; Regular byte
                     (t
                      (process (cdr items) 0 (cons item result) positions)))))))
    (process code 0 nil nil)))

;;; ============================================================
;;; Export Functions
;;; ============================================================

#+sbcl (export '(codegen codegen-main reset-symbol-table
                 resolve-calls-simple prologue epilogue
                 deliver-v2 deliver-v3) :habu)
