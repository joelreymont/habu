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

#+sbcl (defvar *pure-symbol-state* (cons (cons 1 nil) nil))

(defun pure-reset-symbol-table ()
  "Reset symbol table state"
  #+sbcl (progn
           (setf (car (car *pure-symbol-state*)) 1)
           (setf (cdr (car *pure-symbol-state*)) nil))
  #-sbcl (progn
           (setcar (car *pure-symbol-state*) 1)
           (setcdr (car *pure-symbol-state*) nil)))

(defun pure-intern-symbol (name)
  "Get or create a symbol ID for NAME"
  (let* ((state *pure-symbol-state*)
         (counter (car (car state)))
         (table (cdr (car state))))
    (labels ((find-in-table (lst)
               (if (null lst)
                   nil
                   (if (pure-string-equal name (car (car lst)))
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

#+sbcl (defvar *pure-lambda-state* (cons 0 nil))

(defun pure-reset-lambda-counter ()
  "Reset lambda counter"
  #+sbcl (setf (car *pure-lambda-state*) 0)
  #-sbcl (setcar *pure-lambda-state* 0))

(defun pure-gensym-lambda ()
  "Generate unique lambda name as a string like LAMBDA-1, LAMBDA-2, etc."
  (let* ((state *pure-lambda-state*)
         (counter (car state))
         (new-count (+ counter 1)))
    #+sbcl (setf (car state) new-count)
    #-sbcl (setcar state new-count)
    ;; Build name string: "LAMBDA-" + number
    (labels ((digits (n acc)
               (if (= n 0)
                   (if (null acc) (cons 48 nil) acc)  ;; 48 = '0'
                   (digits (truncate n 10)
                           (cons (+ 48 (mod n 10)) acc))))
             (chars-to-str (cs)
               (let ((len (pure-length cs)))
                 (labels ((build (i cs vec)
                            (if (null cs)
                                vec
                                (progn
                                  (vector-set vec i (car cs))
                                  (build (+ i 1) (cdr cs) vec)))))
                   (build 0 cs (make-vector len))))))
      (let* ((num-chars (digits new-count nil))
             (prefix (list 76 65 77 66 68 65 45))  ;; "LAMBDA-"
             (all-chars (pure-append prefix num-chars)))
        (chars-to-str all-chars)))))

;;; ============================================================
;;; Lambda Lifting (extract lambdas, replace with references)
;;; ============================================================

(defun pure-lift-lambdas (ir lambdas)
  "Extract lambda-ir nodes from IR, replacing with lambda-ref.
   Returns (cons transformed-ir lambdas) where lambdas is alist of (name params body free-vars free-offsets)"
  (cond
    ((null ir) (cons ir lambdas))
    ((not (consp ir)) (cons ir lambdas))

    ;; Found a lambda - extract it
    ((pure-has-tag ir 'lambda-ir)
     (let* ((name (pure-gensym-lambda))
            (params (cadr ir))
            (body (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth 4 ir)))
       ;; Recursively lift from body
       (let* ((body-result (pure-lift-lambdas body lambdas))
              (new-body (car body-result))
              (more-lambdas (cdr body-result))
              (lambda-entry (list name params new-body free-vars free-offsets)))
         (cons (list 'lambda-ref name free-offsets)
               (cons lambda-entry more-lambdas)))))

    ;; let-ir: (let-ir vals body count offs)
    ((pure-has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (body (caddr ir))
            (count (cadddr ir))
            (offs (nth 4 ir))
            (vals-result (pure-lift-list vals lambdas))
            (new-vals (car vals-result))
            (l1 (cdr vals-result))
            (body-result (pure-lift-lambdas body l1))
            (new-body (car body-result))
            (l2 (cdr body-result)))
       (cons (list 'let-ir new-vals new-body count offs) l2)))

    ;; if-ir
    ((pure-has-tag ir 'if-ir)
     (let* ((test (cadr ir))
            (then (caddr ir))
            (else (cadddr ir))
            (test-result (pure-lift-lambdas test lambdas))
            (new-test (car test-result))
            (l1 (cdr test-result))
            (then-result (pure-lift-lambdas then l1))
            (new-then (car then-result))
            (l2 (cdr then-result))
            (else-result (pure-lift-lambdas else l2))
            (new-else (car else-result))
            (l3 (cdr else-result)))
       (cons (list 'if-ir new-test new-then new-else) l3)))

    ;; progn-ir
    ((pure-has-tag ir 'progn-ir)
     (let* ((forms (cadr ir))
            (forms-result (pure-lift-list forms lambdas))
            (new-forms (car forms-result))
            (new-lambdas (cdr forms-result)))
       (cons (list 'progn-ir new-forms) new-lambdas)))

    ;; funcall-ir
    ((pure-has-tag ir 'funcall-ir)
     (let* ((fn-ir (cadr ir))
            (args (caddr ir))
            (fn-result (pure-lift-lambdas fn-ir lambdas))
            (new-fn (car fn-result))
            (l1 (cdr fn-result))
            (args-result (pure-lift-list args l1))
            (new-args (car args-result))
            (l2 (cdr args-result)))
       (cons (list 'funcall-ir new-fn new-args) l2)))

    ;; call-fn
    ((pure-has-tag ir 'call-fn)
     (let* ((name (cadr ir))
            (args (caddr ir))
            (args-result (pure-lift-list args lambdas))
            (new-args (car args-result))
            (new-lambdas (cdr args-result)))
       (cons (list 'call-fn name new-args) new-lambdas)))

    ;; Binary ops
    ((or (pure-has-tag ir 'add) (pure-has-tag ir 'sub)
         (pure-has-tag ir 'mul) (pure-has-tag ir 'div)
         (pure-has-tag ir 'mod) (pure-has-tag ir 'cmp-eq)
         (pure-has-tag ir 'cmp-lt) (pure-has-tag ir 'cmp-gt)
         (pure-has-tag ir 'cons-ir))
     (let* ((left (cadr ir))
            (right (caddr ir))
            (left-result (pure-lift-lambdas left lambdas))
            (new-left (car left-result))
            (l1 (cdr left-result))
            (right-result (pure-lift-lambdas right l1))
            (new-right (car right-result))
            (l2 (cdr right-result)))
       (cons (list (car ir) new-left new-right) l2)))

    ;; Unary ops
    ((or (pure-has-tag ir 'car-ir) (pure-has-tag ir 'cdr-ir) (pure-has-tag ir 'get-tag))
     (let* ((arg (cadr ir))
            (arg-result (pure-lift-lambdas arg lambdas))
            (new-arg (car arg-result))
            (new-lambdas (cdr arg-result)))
       (cons (list (car ir) new-arg) new-lambdas)))

    ;; sys-exit-ir
    ((pure-has-tag ir 'sys-exit-ir)
     (let* ((arg (cadr ir))
            (arg-result (pure-lift-lambdas arg lambdas))
            (new-arg (car arg-result))
            (new-lambdas (cdr arg-result)))
       (cons (list 'sys-exit-ir new-arg) new-lambdas)))

    ;; setq-ir: (setq-ir offset val-ir)
    ((pure-has-tag ir 'setq-ir)
     (let* ((off (cadr ir))
            (val-ir (caddr ir))
            (val-result (pure-lift-lambdas val-ir lambdas))
            (new-val (car val-result))
            (new-lambdas (cdr val-result)))
       (cons (list 'setq-ir off new-val) new-lambdas)))

    ;; Default - return unchanged
    (t (cons ir lambdas))))

(defun pure-lift-list (lst lambdas)
  "Lift lambdas from a list of IR nodes"
  (if (null lst)
      (cons nil lambdas)
      (let* ((first-result (pure-lift-lambdas (car lst) lambdas))
             (new-first (car first-result))
             (l1 (cdr first-result))
             (rest-result (pure-lift-list (cdr lst) l1))
             (new-rest (car rest-result))
             (l2 (cdr rest-result)))
        (cons (cons new-first new-rest) l2))))

(defun pure-lift-lambdas-from-defuns (defuns acc-defuns acc-lambdas)
  "Lift lambdas from all defun bodies.
   Defun format: (name params body param-base)
   Must preserve param-base after lifting."
  (if (null defuns)
      (cons (pure-reverse acc-defuns) acc-lambdas)
      (let* ((defun (car defuns))
             (name (car defun))
             (params (cadr defun))
             (body (caddr defun))
             (param-base (cadddr defun))  ;; Preserve param-base!
             (body-result (pure-lift-lambdas body acc-lambdas))
             (new-body (car body-result))
             (more-lambdas (cdr body-result))
             (new-defun (list name params new-body param-base)))  ;; Keep 4 elements
        (pure-lift-lambdas-from-defuns (cdr defuns)
                                        (cons new-defun acc-defuns)
                                        more-lambdas))))

(defun pure-lambdas-to-defuns (lambdas acc)
  "Convert lifted lambda entries to defun format.
   Lambda entry: (name params body free-vars free-offsets)
   Defun format: (name params body param-base)
   The param-base for lambdas is the number of captured variables,
   since params are stored after captured vars in the environment."
  (if (null lambdas)
      (pure-reverse acc)
      (let* ((lambda-entry (car lambdas))
             (name (car lambda-entry))
             (params (cadr lambda-entry))
             (body (caddr lambda-entry))
             (free-vars (cadddr lambda-entry))
             ;; param-base = number of free vars (params come after captures)
             (param-base (pure-length free-vars))
             (defun-entry (list name params body param-base)))
        (pure-lambdas-to-defuns (cdr lambdas) (cons defun-entry acc)))))

;;; ============================================================
;;; ARM64 Instruction Encoders (copied from compiler.lisp)
;;; All pure functions - no state dependencies
;;; ============================================================

(defun pure-encode-word (word)
  (let* ((b0 (logand word #xFF))
         (s1 (ash word -8))
         (b1 (logand s1 #xFF))
         (s2 (ash word -16))
         (b2 (logand s2 #xFF))
         (s3 (ash word -24))
         (b3 (logand s3 #xFF)))
    (list b0 b1 b2 b3)))

(defun pure-movz (rd imm)
  (let* ((masked (logand imm #xFFFF))
         (shifted (ash masked 5))
         (ored (logior #xD2800000 shifted))
         (word (logior ored rd)))
    (pure-encode-word word)))

(defun pure-movk (rd imm shift16)
  "MOVK Rd, #imm, LSL #shift16 - shift16 is 0, 1, 2, or 3 (for 0, 16, 32, 48)"
  (let* ((hw-bits (ash shift16 21))
         (imm-bits (ash (logand imm #xFFFF) 5))
         (base (logior #xF2800000 hw-bits))
         (word (logior base imm-bits rd)))
    (pure-encode-word word)))

(defun pure-add-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x8B000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (pure-encode-word word)))

(defun pure-sub-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #xCB000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (pure-encode-word word)))

(defun pure-mul-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9B007C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (pure-encode-word word)))

(defun pure-sdiv-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9AC00C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (pure-encode-word word)))

(defun pure-lsl-imm (rd rn shift)
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
    (pure-encode-word word)))

(defun pure-lsr-imm (rd rn shift)
  (let* ((shift-s (ash shift 16))
         (rn-s (ash rn 5))
         (or1 (logior #xD340FC00 shift-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (pure-encode-word word)))

(defun pure-add-imm (rd rn imm)
  (let* ((imm12 (logand imm #xFFF))
         (shifted (ash imm12 10))
         (rn-shift (ash rn 5))
         (or1 (logior #x91000000 shifted))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (pure-encode-word word)))

(defun pure-sub-imm (rd rn imm)
  (let* ((imm12 (logand imm #xFFF))
         (shifted (ash imm12 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xD1000000 shifted))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (pure-encode-word word)))

(defun pure-str-offset (rt rn offset)
  (let* ((off-scaled (ash offset -3))
         (off-bits (ash (logand off-scaled #xFFF) 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xF9000000 off-bits))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rt)))
    (pure-encode-word word)))

(defun pure-ldr-offset (rt rn offset)
  (let* ((off-scaled (ash offset -3))
         (off-bits (ash (logand off-scaled #xFFF) 10))
         (rn-shift (ash rn 5))
         (or1 (logior #xF9400000 off-bits))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rt)))
    (pure-encode-word word)))

(defun pure-stp-offset (rt1 rt2 rn offset)
  (let* ((off7 (logand (ash offset -3) #x7F))
         (off-bits (ash off7 15))
         (rt2-bits (ash rt2 10))
         (rn-bits (ash rn 5))
         (or1 (logior #xA9000000 off-bits))
         (or2 (logior or1 rt2-bits))
         (or3 (logior or2 rn-bits))
         (word (logior or3 rt1)))
    (pure-encode-word word)))

(defun pure-ldp-offset (rt1 rt2 rn offset)
  (let* ((off7 (logand (ash offset -3) #x7F))
         (off-bits (ash off7 15))
         (rt2-bits (ash rt2 10))
         (rn-bits (ash rn 5))
         (or1 (logior #xA9400000 off-bits))
         (or2 (logior or1 rt2-bits))
         (or3 (logior or2 rn-bits))
         (word (logior or3 rt1)))
    (pure-encode-word word)))

(defun pure-cmp-reg (rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #xEB00001F rm-shift rn-shift)))
    (pure-encode-word word)))

(defun pure-cmp-imm (rn imm)
  (let* ((imm12 (logand imm #xFFF))
         (shifted (ash imm12 10))
         (rn-shift (ash rn 5))
         (word (logior #xF100001F shifted rn-shift)))
    (pure-encode-word word)))

(defun pure-cset (rd cond-code)
  (let* ((inv-cond (logxor cond-code 1))
         (cond-bits (logior (ash inv-cond 12) (ash inv-cond 16)))
         (word (logior #x9A9F07E0 cond-bits rd)))
    (pure-encode-word word)))

;; Condition codes
(defun pure-cond-eq () 0)
(defun pure-cond-ne () 1)
(defun pure-cond-lt () 11)
(defun pure-cond-ge () 10)
(defun pure-cond-le () 13)
(defun pure-cond-gt () 12)

(defun pure-b-offset (offset)
  (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
         (word (logior #x14000000 imm26)))
    (pure-encode-word word)))

(defun pure-b-cond (cond-code offset)
  (let* ((imm19 (logand (ash offset -2) #x7FFFF))
         (imm-bits (ash imm19 5))
         (word (logior #x54000000 imm-bits cond-code)))
    (pure-encode-word word)))

(defun pure-bl (offset)
  (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
         (word (logior #x94000000 imm26)))
    (pure-encode-word word)))

(defun pure-blr (rn)
  (let* ((rn-bits (ash rn 5))
         (word (logior #xD63F0000 rn-bits)))
    (pure-encode-word word)))

(defun pure-ret ()
  (pure-encode-word #xD65F03C0))

(defun pure-mov-reg (rd rn)
  "MOV Rd, Rn (alias for ORR Rd, XZR, Rn)"
  (let* ((rn-shift (ash rn 16))
         (word (logior #xAA0003E0 rn-shift rd)))
    (pure-encode-word word)))

(defun pure-and-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #x8A000000 rm-shift rn-shift rd)))
    (pure-encode-word word)))

(defun pure-and-imm (rd rn imm)
  "AND Xd, Xn, #imm - Bitwise AND with immediate.
   Note: Only supports specific immediate values that are valid logical immediates.
   Currently supports: #xF (low 4 bits)"
  (let* ((rn-shift (ash rn 5))
         ;; For #xF: N=1, immr=0, imms=3 (4 bits of 1s at position 0)
         ;; Base encoding: 0x92400000 for 64-bit AND immediate
         ;; imms=3 at bits [15:10] = 0xC00
         (word (if (= imm #xF)
                   (logior #x92400C00 rn-shift rd)
                   ;; For #xFF (8 bits): imms=7 at bits [15:10] = 0x1C00
                   (if (= imm #xFF)
                       (logior #x92401C00 rn-shift rd)
                       ;; Unsupported immediate - return 0 (NOP-ish)
                       #xD503201F))))
    (pure-encode-word word)))

(defun pure-orr-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #xAA000000 rm-shift rn-shift rd)))
    (pure-encode-word word)))

(defun pure-eor-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (word (logior #xCA000000 rm-shift rn-shift rd)))
    (pure-encode-word word)))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun pure-reverse-helper (lst acc)
  "Tail-recursive reverse helper"
  (if (null lst)
      acc
      (pure-reverse-helper (cdr lst) (cons (car lst) acc))))

(defun pure-reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

(defun pure-append (lst1 lst2)
  "Append two lists"
  (labels ((append-iter (l acc)
             (if (null l)
                 acc
                 (append-iter (cdr l) (cons (car l) acc)))))
    (append-iter (pure-reverse-helper lst1 nil) lst2)))

(defun pure-length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

(defun pure-append-all (lists)
  "Append all lists in LISTS"
  (if (null lists)
      nil
      (if (null (cdr lists))
          (car lists)
          (pure-append (car lists) (pure-append-all (cdr lists))))))

(defun pure-temp-slot (td)
  "Calculate temp slot offset for depth TD"
  (if (>= td 60)
      (progn
        ;; Error: too many temp slots - but we can't use format in pure code
        ;; Just return a safe value
        #x240)
      (+ #x40 (* td 8))))

(defun pure-load-addr (rd addr)
  "Load large address into register"
  (if (< addr #x10000)
      (pure-movz rd addr)
      (if (< addr #x100000000)
          (pure-append (pure-movz rd (logand addr #xFFFF))
                       (pure-movk rd (ash addr -16) 1))
          ;; 48-bit address
          (pure-append-all (list (pure-movz rd (logand addr #xFFFF))
                                 (pure-movk rd (logand (ash addr -16) #xFFFF) 1)
                                 (pure-movk rd (logand (ash addr -32) #xFFFF) 2))))))

(defun pure-save-temp (td)
  (pure-str-offset 0 31 (pure-temp-slot td)))

(defun pure-load-temp (rd td)
  (pure-ldr-offset rd 31 (pure-temp-slot td)))

;;; ============================================================
;;; IR Tag Predicates
;;; ============================================================

(defun pure-has-tag (ir tag)
  "Check if IR has the given tag"
  (and (consp ir) (eq (car ir) tag)))

(defun pure-ir-may-call (ir)
  "Check if IR may involve a function call"
  (cond
    ((null ir) nil)
    ((not (consp ir)) nil)
    ((pure-has-tag ir 'lit) nil)
    ((pure-has-tag ir 'var) nil)
    ((pure-has-tag ir 'sym-lit) nil)
    ((pure-has-tag ir 'call-fn) t)
    ((pure-has-tag ir 'funcall-ir) t)
    ((pure-has-tag ir 'sys-exit-ir) (pure-ir-may-call (cadr ir)))
    ((pure-has-tag ir 'add) (or (pure-ir-may-call (cadr ir)) (pure-ir-may-call (caddr ir))))
    ((pure-has-tag ir 'sub) (or (pure-ir-may-call (cadr ir)) (pure-ir-may-call (caddr ir))))
    ((pure-has-tag ir 'mul) (or (pure-ir-may-call (cadr ir)) (pure-ir-may-call (caddr ir))))
    ((pure-has-tag ir 'mod) (or (pure-ir-may-call (cadr ir)) (pure-ir-may-call (caddr ir))))
    ((pure-has-tag ir 'cons-ir) (or (pure-ir-may-call (cadr ir)) (pure-ir-may-call (caddr ir))))
    ((pure-has-tag ir 'car-ir) (pure-ir-may-call (cadr ir)))
    ((pure-has-tag ir 'cdr-ir) (pure-ir-may-call (cadr ir)))
    ((pure-has-tag ir 'get-tag) (pure-ir-may-call (cadr ir)))
    ((pure-has-tag ir 'setq-ir) (pure-ir-may-call (caddr ir)))
    ((pure-has-tag ir 'if-ir) t)
    ((pure-has-tag ir 'let-ir) t)
    ((pure-has-tag ir 'let*-ir) t)
    ((pure-has-tag ir 'progn-ir) t)
    (t nil)))

;;; ============================================================
;;; String Lookup in Fnoffs (lambda names are strings)
;;; ============================================================

(defun pure-lookup-string (name fnoffs)
  "Look up a string name in fnoffs alist.
   fnoffs entries can have either symbol or string keys.
   Returns (name . offset) or nil if not found."
  (labels ((str-match (s1 s2)
             ;; Compare two strings (or string to symbol name)
             (cond
               ((and (stringp s1) (stringp s2))
                (pure-string-equal s1 s2))
               ((and (stringp s1) (symbolp s2))
                (pure-string-equal s1 (symbol-name s2)))
               ((and (symbolp s1) (stringp s2))
                (pure-string-equal (symbol-name s1) s2))
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

(defun pure-build-captures (free-offsets)
  "Generate code to build a cons list of captured values.
   free-offsets = list of stack offsets where captured values live.
   Result in x0 is a tagged cons list."
  (if (null free-offsets)
      (pure-movz 0 0)  ;; nil
      (labels ((build-list (offs acc)
                 ;; Build list in reverse, then we cons onto it
                 ;; Each captured value is loaded from [x20 - offset*8]
                 (if (null offs)
                     acc
                     (let* ((off (car offs))
                            (off8 (* off 8))
                            ;; Load value from stack
                            (load-code (pure-append (pure-sub-imm 1 20 off8)
                                                    (pure-ldr-offset 0 1 0)))
                            ;; Save in temp if not first
                            (store-code (if (null (cdr offs))
                                           nil  ;; Last one, keep in x0
                                           (pure-append-all
                                            (list load-code
                                                  ;; Store car
                                                  (pure-str-offset 0 28 0)
                                                  ;; Load/cons previous result
                                                  (pure-ldr-offset 0 28 8)  ; get cdr (prev result)
                                                  ;; This doesn't work... need different approach
                                                  nil)))))
                       (build-list (cdr offs)
                                   (pure-append acc load-code))))))
        ;; Simpler approach: build cons list iteratively
        ;; Start with nil, then cons each value
        (labels ((gen-cons-chain (offs)
                   (if (null offs)
                       (pure-movz 0 0)
                       (let* ((off (car offs))
                              (off8 (* off 8))
                              (rest-code (gen-cons-chain (cdr offs))))
                         ;; First build rest of list, then cons current onto it
                         (pure-append-all
                          (list rest-code
                                ;; Save cdr in heap
                                (pure-str-offset 0 28 8)
                                ;; Load current value
                                (pure-sub-imm 1 20 off8)
                                (pure-ldr-offset 0 1 0)
                                ;; Store as car
                                (pure-str-offset 0 28 0)
                                ;; Make cons pointer
                                (pure-mov-reg 0 28)
                                (pure-add-imm 0 0 1)  ;; cons tag
                                (pure-add-imm 28 28 16)))))))
          ;; Don't reverse - gen-cons-chain builds in correct order
          ;; for free-vars list (first offset becomes car)
          (gen-cons-chain free-offsets)))))

;;; ============================================================
;;; Binary Operation Codegen Helper
;;; ============================================================

(defun pure-codegen-binop (left-ir right-ir op-instrs rtaddrs fnoffs td)
  "Generate code for binary operation"
  (let* ((left-may-call (pure-ir-may-call left-ir))
         (right-may-call (pure-ir-may-call right-ir)))
    (cond
      ;; Left may call - need to save x24
      ;; Left is evaluated at nd, saves result to nd, so right must use nd+1
      (left-may-call
       (let* ((xs (pure-temp-slot td))
              (nd (+ td 1))
              (lc (pure-codegen left-ir rtaddrs fnoffs nd))
              (rc (pure-codegen right-ir rtaddrs fnoffs (+ nd 1))))
         (pure-append-all
          (list (pure-str-offset 24 31 xs)
                lc
                (pure-save-temp nd)
                (pure-ldr-offset 24 31 xs)
                rc
                (pure-mov-reg 1 0)
                (pure-load-temp 0 nd)
                op-instrs))))
      ;; Right may call - need to save x24
      ;; Left is evaluated, saved at nd, so right must use nd+1
      (right-may-call
       (let* ((xs (pure-temp-slot td))
              (nd (+ td 1))
              (lc (pure-codegen left-ir rtaddrs fnoffs nd))
              (rc (pure-codegen right-ir rtaddrs fnoffs (+ nd 1))))
         (pure-append-all
          (list lc
                (pure-save-temp nd)
                (pure-str-offset 24 31 xs)
                rc
                (pure-mov-reg 1 0)
                (pure-load-temp 0 nd)
                (pure-ldr-offset 24 31 xs)
                op-instrs))))
      ;; Neither calls - simple case
      ;; IMPORTANT: Right must use td+1 to avoid clobbering left's temp slot
      (t
       (let* ((nd (+ td 1))
              (lc (pure-codegen left-ir rtaddrs fnoffs td))
              (rc (pure-codegen right-ir rtaddrs fnoffs nd)))
         (pure-append-all
          (list lc
                (pure-save-temp td)
                rc
                (pure-mov-reg 1 0)
                (pure-load-temp 0 td)
                op-instrs)))))))

;;; ============================================================
;;; Main Codegen Function (handles all IR nodes)
;;; ============================================================

(defun pure-codegen (ir rtaddrs fnoffs td)
  "Generate ARM64 code from IR"
  (cond
    ;; Literal
    ((pure-has-tag ir 'lit)
     (let* ((v (cadr ir))
            (tg (ash v 4)))
       (if (and (>= tg 0) (< tg #x10000))
           (pure-movz 0 tg)
           (pure-load-addr 0 tg))))

    ;; Nil
    ((pure-has-tag ir 'nil-ir)
     (pure-movz 0 0))

    ;; Symbol literal
    ((pure-has-tag ir 'sym-lit)
     (let* ((name (cadr ir))
            (id (pure-intern-symbol name))
            (tagged (logior (ash id 4) 2)))
       (if (< tagged #x10000)
           (pure-movz 0 tagged)
           (pure-load-addr 0 tagged))))

    ;; Variable reference
    ((pure-has-tag ir 'var)
     (let* ((off (cadr ir))
            (off8 (* off 8)))
       (pure-append (pure-sub-imm 1 20 off8)
                    (pure-ldr-offset 0 1 0))))

    ;; Variable assignment (setq)
    ((pure-has-tag ir 'setq-ir)
     (let* ((off (cadr ir))
            (val-ir (caddr ir))
            (off8 (* off 8))
            (val-code (pure-codegen val-ir rtaddrs fnoffs td)))
       ;; Compile value to x0, then store at x20 - offset*8
       (pure-append-all
        (list val-code
              (pure-sub-imm 1 20 off8)
              (pure-str-offset 0 1 0)))))

    ;; Addition
    ((pure-has-tag ir 'add)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-add-reg 0 0 1)
                         rtaddrs fnoffs td))

    ;; Subtraction
    ((pure-has-tag ir 'sub)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-sub-reg 0 0 1)
                         rtaddrs fnoffs td))

    ;; Multiplication (untag one operand)
    ((pure-has-tag ir 'mul)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-append (pure-lsr-imm 1 1 4)
                                      (pure-mul-reg 0 0 1))
                         rtaddrs fnoffs td))

    ;; Division
    ((pure-has-tag ir 'div)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-append-all
                          (list (pure-lsr-imm 0 0 4)
                                (pure-lsr-imm 1 1 4)
                                (pure-sdiv-reg 0 0 1)
                                (pure-lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Modulo: a mod b = a - (a/b)*b
    ((pure-has-tag ir 'mod)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-append-all
                          (list (pure-lsr-imm 9 0 4)    ; x9 = a untagged
                                (pure-lsr-imm 10 1 4)   ; x10 = b untagged
                                (pure-sdiv-reg 11 9 10) ; x11 = a/b
                                (pure-mul-reg 11 11 10) ; x11 = (a/b)*b
                                (pure-sub-reg 0 9 11)   ; x0 = a - (a/b)*b
                                (pure-lsl-imm 0 0 4)))  ; tag result
                         rtaddrs fnoffs td))

    ;; Comparison (equality)
    ((pure-has-tag ir 'cmp-eq)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-append-all
                          (list (pure-cmp-reg 0 1)
                                (pure-cset 0 (pure-cond-eq))
                                (pure-lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Less than
    ((pure-has-tag ir 'cmp-lt)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-append-all
                          (list (pure-cmp-reg 0 1)
                                (pure-cset 0 (pure-cond-lt))
                                (pure-lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Greater than
    ((pure-has-tag ir 'cmp-gt)
     (pure-codegen-binop (cadr ir) (caddr ir)
                         (pure-append-all
                          (list (pure-cmp-reg 0 1)
                                (pure-cset 0 (pure-cond-gt))
                                (pure-lsl-imm 0 0 4)))
                         rtaddrs fnoffs td))

    ;; Cons cell (inline heap allocation)
    ((pure-has-tag ir 'cons-ir)
     (let* ((car-ir (cadr ir))
            (cdr-ir (caddr ir))
            (xs (pure-temp-slot td))
            (cs (pure-temp-slot (+ td 1)))
            (nd (+ td 2))
            (car-code (pure-codegen car-ir rtaddrs fnoffs nd))
            (cdr-code (pure-codegen cdr-ir rtaddrs fnoffs nd)))
       (pure-append-all
        (list (pure-str-offset 24 31 xs)
              car-code
              (pure-str-offset 0 31 cs)
              (pure-ldr-offset 24 31 xs)
              cdr-code
              (pure-str-offset 0 28 8)
              (pure-ldr-offset 0 31 cs)
              (pure-str-offset 0 28 0)
              (pure-mov-reg 0 28)
              (pure-orr-reg 0 0 28)
              (pure-add-imm 0 0 1)
              (pure-add-imm 28 28 16)
              (pure-ldr-offset 24 31 xs)))))

    ;; Car
    ((pure-has-tag ir 'car-ir)
     (let ((inner-code (pure-codegen (cadr ir) rtaddrs fnoffs td)))
       (pure-append inner-code
                    (pure-append (pure-sub-imm 0 0 1)
                                 (pure-ldr-offset 0 0 0)))))

    ;; Cdr
    ((pure-has-tag ir 'cdr-ir)
     (let ((inner-code (pure-codegen (cadr ir) rtaddrs fnoffs td)))
       (pure-append inner-code
                    (pure-append (pure-sub-imm 0 0 1)
                                 (pure-ldr-offset 0 0 8)))))

    ;; Get-tag (extract low 4 bits as tagged fixnum)
    ((pure-has-tag ir 'get-tag)
     (let ((inner-code (pure-codegen (cadr ir) rtaddrs fnoffs td)))
       (pure-append inner-code
                    ;; AND x0, x0, #0xF to extract tag bits
                    ;; Then LSL x0, x0, #4 to tag as fixnum
                    (pure-append (pure-and-imm 0 0 #xF)
                                 (pure-lsl-imm 0 0 4)))))

    ;; If-IR
    ((pure-has-tag ir 'if-ir)
     (let* ((cond-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (cond-code (pure-codegen cond-ir rtaddrs fnoffs td))
            (then-code (pure-codegen then-ir rtaddrs fnoffs td))
            (else-code (pure-codegen else-ir rtaddrs fnoffs td))
            ;; Must use pure-code-size to handle :call markers (4 bytes each)
            (else-size (pure-code-size else-code))
            (then-size (pure-code-size then-code)))
       (pure-append-all
        (list cond-code
              (pure-cmp-imm 0 0)
              ;; Branch if cond==0 (false) to skip then + unconditional branch
              (pure-b-cond (pure-cond-eq) (+ then-size 8))
              then-code
              ;; Unconditional branch to skip else
              (pure-b-offset (+ else-size 4))
              else-code))))

    ;; Let-IR: (let-ir vals body count offs)
    ((pure-has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (body-ir (caddr ir))
            (offs (nth 3 (cdr ir)))  ; offs is at index 4: (let-ir vals body count offs)
            (xs (pure-temp-slot td))
            (nd (+ td 1))
            (save-x24 (pure-str-offset 24 31 xs)))
       ;; Generate bindings with proper offsets
       (labels ((gen-binds (vs os acc)
                  (if (null vs)
                      acc
                      (let* ((restore-x24 (if acc (pure-ldr-offset 24 31 xs) nil))
                             (val-code (pure-codegen (car vs) rtaddrs fnoffs nd))
                             (store-code (pure-append (pure-sub-imm 1 20 (* (car os) 8))
                                                      (pure-str-offset 0 1 0))))
                        (gen-binds (cdr vs) (cdr os)
                                   (pure-append-all (list acc restore-x24 val-code store-code)))))))
         (let* ((bindings-code (gen-binds vals offs nil))
                (restore-final (pure-ldr-offset 24 31 xs))
                (body-code (pure-codegen body-ir rtaddrs fnoffs nd)))
           (pure-append-all (list save-x24 bindings-code restore-final body-code))))))

    ;; Progn-IR
    ((pure-has-tag ir 'progn-ir)
     (let ((forms (cadr ir)))
       (pure-codegen-progn-forms forms rtaddrs fnoffs td)))

    ;; sys-exit-IR
    ((pure-has-tag ir 'sys-exit-ir)
     (let ((arg-code (pure-codegen (cadr ir) rtaddrs fnoffs td)))
       (pure-append arg-code
                    (pure-append (pure-lsr-imm 0 0 4)
                                 (list (list :extern-call "_exit"))))))

    ;; Function call
    ((pure-has-tag ir 'call-fn)
     (let* ((fn-name (cadr ir))
            (args (caddr ir))
            (num-args (pure-length args))
            (arg-code (pure-codegen-call-args args rtaddrs fnoffs td))
            ;; Load spilled args into registers x1-x7 before call
            (load-code (pure-gen-arg-loads num-args td)))
       ;; Emit call marker that will be resolved by pure-resolve-calls
       (pure-append-all (list arg-code load-code (list (list :call fn-name))))))

    ;; Lambda reference (closure creation)
    ;; lambda-ref = (lambda-ref name free-offsets)
    ;; After lambda lifting, name is a string that we look up in fnoffs
    ((pure-has-tag ir 'lambda-ref)
     (let* ((name (cadr ir))
            (free-offsets (caddr ir))
            ;; Look up function offset in fnoffs
            (fn-entry (pure-lookup-string name fnoffs))
            (fn-offset (if fn-entry (cdr fn-entry) 0)))
       ;; Build closure on heap: (fn-offset . captured-env)
       ;; First, build captured environment on heap (list of captured values)
       (if (null free-offsets)
           ;; No captures - simple closure
           (pure-append-all
            (list (pure-load-addr 0 (ash fn-offset 4))
                  (pure-str-offset 0 28 0)
                  (pure-movz 0 0)  ;; nil for empty env
                  (pure-str-offset 0 28 8)
                  (pure-mov-reg 0 28)
                  (pure-add-imm 0 0 5)  ;; closure tag
                  (pure-add-imm 28 28 16)))
           ;; Has captures - build env cons list first
           (let* ((capture-code (pure-build-captures free-offsets))
                  (xs (pure-temp-slot td)))
             (pure-append-all
              (list ;; Save x24 before building captures
                    (pure-str-offset 24 31 xs)
                    ;; Build captured env (result in x0)
                    capture-code
                    ;; Save captured env
                    (pure-str-offset 0 28 8)
                    ;; Store fn-offset
                    (pure-load-addr 0 (ash fn-offset 4))
                    (pure-str-offset 0 28 0)
                    ;; Create closure pointer
                    (pure-mov-reg 0 28)
                    (pure-add-imm 0 0 5)
                    (pure-add-imm 28 28 16)
                    ;; Restore x24
                    (pure-ldr-offset 24 31 xs)))))))

    ;; Function reference (closure for named function)
    ;; fn-ref-ir = (fn-ref-ir name) where name is a symbol
    ;; Creates a closure with empty env pointing to the named function
    ((pure-has-tag ir 'fn-ref-ir)
     (let* ((name (cadr ir))
            ;; Look up function offset in fnoffs (symbol key)
            (fn-entry (pure-lookup-string name fnoffs))
            (fn-offset (if fn-entry (cdr fn-entry) 0)))
       ;; Build closure on heap: (fn-offset . nil)
       ;; No captures, so env is nil
       (pure-append-all
        (list (pure-load-addr 0 (ash fn-offset 4))
              (pure-str-offset 0 28 0)
              (pure-movz 0 0)  ;; nil for empty env
              (pure-str-offset 0 28 8)
              (pure-mov-reg 0 28)
              (pure-add-imm 0 0 5)  ;; closure tag
              (pure-add-imm 28 28 16)))))

    ;; Funcall-IR
    ((pure-has-tag ir 'funcall-ir)
     (let* ((fn-ir (cadr ir))
            (args (caddr ir))
            (num-args (pure-length args))
            (fn-code (pure-codegen fn-ir rtaddrs fnoffs td))
            (cs (pure-temp-slot td))
            (nd (+ td 1))
            (arg-code (pure-codegen-funcall-args args rtaddrs fnoffs nd 0))
            ;; Load args from spill slots to registers x0-x7
            ;; Note: funcall-args uses nd for spill, so load from nd
            (load-code (pure-gen-arg-loads num-args nd)))
       (pure-append-all
        (list fn-code
              (pure-str-offset 0 31 cs)  ;; Save closure to temp
              arg-code                    ;; Eval and spill args
              load-code                   ;; Load args to x0-x7
              ;; Use x9 for closure to avoid clobbering x0-x7 (args)
              (pure-ldr-offset 9 31 cs)  ;; x9 = closure
              (pure-sub-imm 9 9 5)       ;; Untag closure
              (pure-ldr-offset 24 9 8)   ;; x24 = [x9 + 8] = env
              (pure-ldr-offset 9 9 0)    ;; x9 = [x9 + 0] = fn-offset
              (pure-lsr-imm 9 9 4)       ;; Untag fn-offset
              (pure-add-reg 9 9 26)      ;; x9 = x26 + fn-offset = absolute addr
              (pure-blr 9)))))

    ;; Default - return empty
    (t nil)))

;;; ============================================================
;;; Helper: Let Bindings Codegen
;;; ============================================================

(defun pure-codegen-let-bindings (bindings rtaddrs fnoffs td idx)
  "Generate code to evaluate and store let bindings"
  (if (null bindings)
      nil
      (let* ((val-ir (car bindings))
             (val-code (pure-codegen val-ir rtaddrs fnoffs td))
             (store-code (pure-append (pure-sub-imm 1 20 (* idx 8))
                                      (pure-str-offset 0 1 0)))
             (rest-code (pure-codegen-let-bindings (cdr bindings) rtaddrs fnoffs td (+ idx 1))))
        (pure-append-all (list val-code store-code rest-code)))))

;;; ============================================================
;;; Helper: Progn Forms Codegen
;;; ============================================================

(defun pure-codegen-progn-forms (forms rtaddrs fnoffs td)
  "Generate code for sequence of forms, return value of last"
  (if (null forms)
      nil
      (if (null (cdr forms))
          (pure-codegen (car forms) rtaddrs fnoffs td)
          (let* ((first-code (pure-codegen (car forms) rtaddrs fnoffs td))
                 (rest-code (pure-codegen-progn-forms (cdr forms) rtaddrs fnoffs td)))
            (pure-append first-code rest-code)))))

;;; ============================================================
;;; Helper: Call Arguments Codegen
;;; ============================================================

(defun pure-spill-base (td)
  "Calculate spill area base for temp depth td.
   Each nesting level gets 64 bytes (8 slots) of spill area."
  (+ #x240 (* td 64)))

(defun pure-codegen-call-args (args rtaddrs fnoffs td)
  "Generate code for function call arguments"
  (pure-codegen-args-iter args rtaddrs fnoffs td 0))

(defun pure-codegen-args-iter (args rtaddrs fnoffs td argnum)
  "Generate code for args, storing ALL args to spill slots.
   This ensures arg 0 isn't clobbered when evaluating later args.
   Uses td-based offset so nested calls don't clobber each other."
  (if (null args)
      nil
      (let* ((arg-ir (car args))
             ;; Eval arg with incremented td so nested calls use different spill area
             (arg-code (pure-codegen arg-ir rtaddrs fnoffs (+ td 1)))
             ;; Store to spill slot based on current td
             (spill-offset (+ (pure-spill-base td) (* argnum 8)))
             (save-code (pure-str-offset 0 31 spill-offset)))
        (pure-append-all
         (list arg-code
               save-code
               (pure-codegen-args-iter (cdr args) rtaddrs fnoffs td (+ argnum 1)))))))

;;; ============================================================
;;; Helper: Load Arguments into Registers Before Call
;;; ============================================================

(defun pure-gen-arg-loads (num-args td)
  "Generate code to load spilled args from spill area into registers x0-x7.
   Uses td-based offset to match where args were stored."
  (if (= num-args 0)
      nil
      (let ((base (pure-spill-base td)))
        (labels ((gen-load (i acc)
                   (if (>= i num-args)
                       acc
                       (gen-load (+ i 1)
                                 (pure-append acc
                                              (pure-ldr-offset i 31 (+ base (* i 8))))))))
          (gen-load 0 nil)))))

;;; ============================================================
;;; Helper: Funcall Arguments Codegen
;;; ============================================================

(defun pure-codegen-funcall-args (args rtaddrs fnoffs td argnum)
  "Generate code for funcall arguments.
   Uses td-based spill area so nested calls don't clobber each other."
  (if (null args)
      nil
      (let* ((arg-ir (car args))
             ;; Eval arg with incremented td so nested calls use different spill area
             (arg-code (pure-codegen arg-ir rtaddrs fnoffs (+ td 1))))
        (if (< argnum 8)
            ;; Args 0-7: store to td-based spill slot
            (let* ((spill-offset (+ (pure-spill-base td) (* argnum 8)))
                   (save-code (pure-str-offset 0 31 spill-offset)))
              (pure-append-all
               (list arg-code
                     save-code
                     (pure-codegen-funcall-args (cdr args) rtaddrs fnoffs td (+ argnum 1)))))
            ;; Args 8+ go on stack (not yet implemented)
            nil))))

;;; ============================================================
;;; Prologue and Epilogue
;;; ============================================================

(defun pure-prologue ()
  "Generate function prologue"
  (pure-append-all
   (list (pure-stp-offset 29 30 31 -128)
         (pure-add-imm 29 31 0)
         (pure-sub-imm 31 31 #x400)
         (pure-stp-offset 19 20 31 16)
         (pure-stp-offset 21 22 31 32)
         (pure-stp-offset 23 24 31 48)
         (pure-add-imm 20 31 #x180))))

(defun pure-epilogue ()
  "Generate function epilogue"
  (pure-append-all
   (list (pure-ldp-offset 23 24 31 48)
         (pure-ldp-offset 21 22 31 32)
         (pure-ldp-offset 19 20 31 16)
         (pure-add-imm 31 31 #x400)
         (pure-ldp-offset 29 30 31 -128)
         (pure-ret))))

;;; ============================================================
;;; Function Codegen
;;; ============================================================

(defun pure-codegen-fn (fn rtaddrs fnoffs)
  "Generate code for a function: (name params body-ir param-base)
   Uses simple fixed frame layout."
  (let* ((name (car fn))
         (params (cadr fn))
         (body-ir (caddr fn))
         (param-base (cadddr fn))
         ;; For lifted lambdas (param-base > 0), load captured values from x24
         ;; x24 points to a cons list of captured values
         (capture-code (if (> param-base 0)
                           (pure-gen-capture-loads param-base)
                           nil))
         ;; Generate param stores: move x0-x7 to [x20 - offset*8]
         (param-code (pure-gen-param-stores params param-base 0 nil))
         ;; Generate body code
         (body-code (pure-codegen body-ir rtaddrs fnoffs 0)))
    (pure-append-all
     (list (pure-prologue)
           capture-code
           param-code
           body-code
           (pure-epilogue)
           (pure-ret)))))

(defun pure-gen-capture-loads (num-captures)
  "Generate code to load captured values from x24 cons list into env slots.
   x24 = (v0 . (v1 . (v2 . nil))) - load into offsets 0, 1, 2, etc."
  (labels ((gen-loads (idx acc)
             (if (>= idx num-captures)
                 acc
                 (let* ((offset (* idx 8))
                        ;; x24 points to current cons cell
                        ;; Load car into x9, store at [x20 - offset*8]
                        ;; Then advance: x24 = cdr(x24)
                        (load-car (pure-append
                                   (pure-sub-imm 9 24 1)      ; untag cons
                                   (pure-ldr-offset 9 9 0)))  ; x9 = car
                        (store-env (pure-append
                                    (pure-sub-imm 10 20 offset)
                                    (pure-str-offset 9 10 0))) ; [x20-off] = x9
                        (advance (pure-append
                                  (pure-sub-imm 9 24 1)       ; untag cons
                                  (pure-ldr-offset 24 9 8)))) ; x24 = cdr
                   (gen-loads (+ idx 1)
                              (pure-append-all (list acc load-car store-env advance)))))))
    (gen-loads 0 nil)))

(defun pure-gen-param-stores (params base idx acc)
  "Generate stores from registers x0-x7 to environment slots"
  (if (null params)
      acc
      (if (< idx 8)
          (let* ((offset (* (+ base idx) 8))
                 (store (pure-append (pure-sub-imm 9 20 offset)
                                     (pure-str-offset idx 9 0))))
            (pure-gen-param-stores (cdr params) base (+ idx 1)
                                   (pure-append acc store)))
          ;; Args 8+ would need stack loading - skip for now
          acc)))

(defun pure-code-size (code)
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

(defun pure-build-fnoffs (fns offset acc)
  "Build function offset table: ((name . byte-offset) ...)"
  (if (null fns)
      (pure-reverse acc)
      (let* ((fn (car fns))
             (name (car fn))
             (code (pure-codegen-fn fn nil nil))
             (size (pure-code-size code))
             (entry (cons name offset)))
        (pure-build-fnoffs (cdr fns) (+ offset size) (cons entry acc)))))

(defun pure-codegen-all-fns (fns rtaddrs fnoffs acc)
  "Generate code for all functions with fnoffs"
  (if (null fns)
      acc
      (let* ((fn (car fns))
             (code (pure-codegen-fn fn rtaddrs fnoffs)))
        (pure-codegen-all-fns (cdr fns) rtaddrs fnoffs
                              (pure-append acc code)))))

;;; ============================================================
;;; Main Codegen Entry Point
;;; ============================================================

(defun pure-codegen-main (mir rtaddrs)
  "Generate main code with prologue/epilogue"
  (pure-append-all
   (list (pure-prologue)
         (pure-codegen mir rtaddrs nil 0)
         (pure-epilogue))))

;;; ============================================================
;;; Resolve Calls (simple version without function linking)
;;; ============================================================

(defun pure-resolve-calls-simple (code)
  "Simple resolve - just flatten the code list.
   For now, this just removes the :call and :extern-call markers.
   Full version needs function offset table."
  (labels ((flatten (items acc)
             (if (null items)
                 (pure-reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item) (or (eq (car item) :call)
                                              (eq (car item) :extern-call)))
                       ;; Keep extern-call markers for later processing
                       (if (eq (car item) :extern-call)
                           (flatten (cdr items) (cons item acc))
                           ;; Regular call - emit placeholder for now
                           (flatten (cdr items)
                                    (pure-append (pure-reverse (pure-bl 0)) acc)))
                       (if (consp item)
                           (flatten (cdr items) (pure-append (pure-reverse item) acc))
                           (flatten (cdr items) (cons item acc))))))))
    (flatten code nil)))

;;; ============================================================
;;; Pure Delivery (using all pure components)
;;; ============================================================

(defun pure-deliver-v2 (source output-path)
  "Compile source string to native executable using all pure components.
   Uses: pure-compile-forms (pure compiler), pure-codegen (pure codegen),
   wrap-bytecode-with-heap-for-imports (macho), write-macho-executable-with-imports-and-heap.
   Works in both SBCL and native Habu (no SBCL dependencies)."
  (pure-reset-symbol-table)
  (let* ((forms (pure-read-all source))
         (result (pure-compile-forms forms))
         (main-ir (cadr result))
         ;; Generate code using pure codegen
         (code (pure-codegen-main main-ir nil))
         ;; First pass: flatten code lists but keep :extern-call markers
         (bytes-with-markers (pure-flatten-code-keep-markers code))
         ;; Collect extern calls
         (extern-calls (pure-collect-extern-calls bytes-with-markers))
         (imports (pure-get-unique-imports extern-calls))
         (wrapper-size 68))

    ;; Always use imports path for consistent Mach-O
    (let ((imports (if (null imports) '("_exit") imports)))

      ;; Calculate stub offsets
      (let* ((num-imports (pure-length imports))
             (stubs-total (if (> num-imports 0) (* num-imports 12) 0))
             (code-offset #x400)
             (exact-flat-size (pure-length bytes-with-markers))
             (exact-code-size (+ exact-flat-size wrapper-size))
             (stubs-offset (+ code-offset exact-code-size))
             (stub-size 12))

        ;; Build stub offset alist
        (let* ((stub-alist (pure-build-stub-alist imports stubs-offset stub-size))
               (flatten-result (pure-flatten-extern-calls bytes-with-markers stub-alist (+ code-offset wrapper-size)))
               (flat-code (car flatten-result)))

          ;; Calculate heap page offset
          (let* ((total-size (+ (pure-length flat-code) wrapper-size))
                 (stubs-end (+ code-offset total-size stubs-total))
                 (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
                 (text-pages-4kb (/ text-vmsize #x1000))
                 (data-const-pages-4kb (/ #x4000 #x1000))
                 (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
                 (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code heap-page-offset)))

            ;; Write Mach-O executable (handles chmod+codesign via native-write-executable)
            (write-macho-executable-with-imports-and-heap output-path wrapped-code imports #x800000)))))))

(defun pure-deliver-v3 (source output-path)
  "Compile source string with function definitions to native executable.
   Supports: defun, lambda, funcall, function calls, all v2 features.
   Layout: wrapper(68) + main-code + function-code + lambda-code + stubs
   Works in both SBCL and native Habu (no SBCL dependencies)."
  (pure-reset-symbol-table)
  (pure-reset-lambda-counter)
  (let* ((forms (pure-read-all source))
         (result (pure-compile-forms forms))
         (defuns-orig (car result))
         (main-ir-orig (cadr result))
         (wrapper-size 68)
         ;; Lift lambdas from main-ir
         (main-lift-result (pure-lift-lambdas main-ir-orig nil))
         (main-ir (car main-lift-result))
         (main-lambdas (cdr main-lift-result))
         ;; Lift lambdas from defun bodies
         (defun-lift-result (pure-lift-lambdas-from-defuns defuns-orig nil nil))
         (defuns (car defun-lift-result))
         (defun-lambdas (cdr defun-lift-result))
         ;; Combine all lambdas
         (all-lambdas (pure-append main-lambdas defun-lambdas))
         ;; Check if we have any functions at all
         (has-fns (or (not (null defuns)) (not (null all-lambdas)))))

    (if (not has-fns)
        ;; No functions or lambdas - use v2
        (pure-deliver-v2 source output-path)

        ;; Has functions/lambdas - full compilation
        ;; Combine defuns and lambdas (lambdas need to be converted to defun format)
        (let* ((lambda-as-defuns (pure-lambdas-to-defuns all-lambdas nil))
               (all-fns (pure-append defuns lambda-as-defuns))
               ;; Generate main code first (with nil fnoffs to get size)
               (main-code-temp (pure-append-all
                                (list (pure-prologue)
                                      (pure-codegen main-ir nil nil 0)
                                      (pure-epilogue))))
               (main-size (pure-code-size main-code-temp))
               ;; Build fnoffs starting after main code (relative to code start after wrapper)
               (fnoffs (pure-build-fnoffs all-fns main-size nil))
               ;; Regenerate main with fnoffs
               (main-code (pure-append-all
                           (list (pure-prologue)
                                 (pure-codegen main-ir nil fnoffs 0)
                                 (pure-epilogue))))
               ;; Generate all function code (defuns + lambdas)
               (fn-code (pure-codegen-all-fns all-fns nil fnoffs nil))
               ;; Combine all code
               (all-code (pure-append main-code fn-code))
               ;; Flatten with markers tracking positions
               (bytes-with-markers (pure-flatten-code-keep-markers-and-calls all-code))
               ;; Collect extern calls
               (extern-calls (pure-collect-extern-calls bytes-with-markers))
               (imports (pure-get-unique-imports extern-calls))
               (imports (if (null imports) '("_exit") imports))
               ;; Calculate stubs
               (num-imports (pure-length imports))
               (stubs-total (* num-imports 12))
               (code-offset #x400)
               (exact-flat-size (pure-length bytes-with-markers))
               (exact-code-size (+ exact-flat-size wrapper-size))
               (stubs-offset (+ code-offset exact-code-size))
               (stub-size 12)
               ;; Build stub alist
               (stub-alist (pure-build-stub-alist imports stubs-offset stub-size))
               ;; Convert fnoffs to byte addresses (relative to code-offset + wrapper-size)
               (fn-addr-base (+ code-offset wrapper-size))
               (fn-alist (pure-build-fn-addr-alist fnoffs fn-addr-base nil))
               ;; Flatten both :call and :extern-call markers
               (flatten-result (pure-flatten-all-calls bytes-with-markers fn-alist stub-alist fn-addr-base))
               (flat-code (car flatten-result))
               ;; Calculate heap
               (total-size (+ (pure-length flat-code) wrapper-size))
               (stubs-end (+ code-offset total-size stubs-total))
               (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
               (text-pages-4kb (/ text-vmsize #x1000))
               (data-const-pages-4kb (/ #x4000 #x1000))
               (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
               (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code heap-page-offset)))

          ;; Write executable (handles chmod+codesign via native-write-executable)
          (write-macho-executable-with-imports-and-heap output-path wrapped-code imports #x800000)))))

(defun pure-build-fn-addr-alist (fnoffs base acc)
  "Convert fnoffs to absolute addresses"
  (if (null fnoffs)
      (pure-reverse acc)
      (let* ((entry (car fnoffs))
             (name (car entry))
             (offset (cdr entry))
             (addr (+ base offset)))
        (pure-build-fn-addr-alist (cdr fnoffs) base
                                   (cons (cons name addr) acc)))))

(defun pure-flatten-code-keep-markers-and-calls (code)
  "Flatten code lists but keep both :extern-call and :call markers with positions."
  (labels ((flatten (items pos acc)
             (if (null items)
                 (pure-reverse acc)
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
                             (size (pure-length flattened)))
                        (flatten (cdr items)
                                 (+ pos size)
                                 (pure-append (pure-reverse flattened) acc))))
                     ;; Byte
                     (t
                      (flatten (cdr items)
                               (+ pos 1)
                               (cons item acc))))))))
    (flatten code 0 nil)))

(defun pure-flatten-all-calls (code fn-alist stub-alist code-base-addr)
  "Replace both :call and :extern-call markers with BL instructions.
   Returns (cons flattened-code positions)."
  (labels ((lookup-fn (name)
             (pure-alist-lookup name fn-alist))
           (lookup-stub (name)
             (pure-alist-lookup name stub-alist))
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
                 (cons (pure-reverse result) positions)
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

(defun pure-alist-lookup (key alist)
  "Look up key in alist, return value or nil"
  (if (null alist)
      nil
      (if (if (symbolp key)
              (eq key (caar alist))
              (equal key (caar alist)))
          (cdar alist)
          (pure-alist-lookup key (cdr alist)))))

(defun pure-flatten-code-keep-markers (code)
  "Flatten nested code lists but keep :extern-call markers intact.
   Tracks position and transforms (:extern-call name) to (:extern-call name pos).
   Each marker followed by 3 zeros = 4 bytes total."
  (labels ((flatten (items pos acc)
             (if (null items)
                 (pure-reverse acc)
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
                             (size (pure-length flattened)))
                        (flatten (cdr items)
                                 (+ pos size)
                                 (pure-append (pure-reverse flattened) acc))))
                     ;; Byte - add directly
                     (t
                      (flatten (cdr items)
                               (+ pos 1)
                               (cons item acc))))))))
    (flatten code 0 nil)))

(defun pure-flatten-extern-calls (code stub-alist code-base-addr)
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
                 (cons (pure-reverse result) positions)
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

#+sbcl (export '(pure-codegen pure-codegen-main pure-reset-symbol-table
                 pure-resolve-calls-simple pure-prologue pure-epilogue
                 pure-deliver-v2 pure-deliver-v3) :habu)
