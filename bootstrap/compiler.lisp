;;; ============================================================
;;; Habu Native Compiler - Self-Hosting ARM64 Lisp Compiler
;;; ============================================================
;;;
;;; Components: ARM64 asm, utils, reader, codegen, IR compiler
;;;
;;; This file defines the HABU package which provides:
;;; - System functions (string-length, string-ref, etc.)
;;; - Bootstrap compiler (nc-* functions)
;;; - ARM64 code generation

;;; ============================================================
;;; Part 0: Package Definition
;;; ============================================================

(defpackage :habu
  (:use :cl)
  ;; Shadow CL symbols we redefine
  (:shadow)
  ;; Export system functions (available in self-hosted Habu)
  (:export
   ;; String functions
   string-length string-ref make-string-from-vector
   ;; Vector functions
   make-vector vector-set
   ;; Compiler entry points
   nc-read-all nc-compile nc-eval-ir nc-eval-forms nc-codegen
   main))

(in-package :habu)

;;; ============================================================
;;; Part 0b: System Functions (SBCL compatibility layer)
;;; ============================================================

;; These functions exist in Habu runtime but not in standard CL
;; In self-hosted Habu, these would be primitives
(defun string-length (s) (length s))
(defun string-ref (s i) (char-code (char s i)))
(defun make-vector (n) (make-array n))
(defun vector-set (v i x) (setf (aref v i) x))
(defun make-string-from-vector (v)
  (map 'string #'code-char v))

;;; ============================================================
;;; Part 1: ARM64 Instruction Encoders (nc-asm-*)
;;; ============================================================

;; All encoder functions use let* to avoid nested calls in arg position
(defun nc-encode-word (word)
  (let* ((b0 (logand word #xFF))
         (s1 (ash word -8))
         (b1 (logand s1 #xFF))
         (s2 (ash word -16))
         (b2 (logand s2 #xFF))
         (s3 (ash word -24))
         (b3 (logand s3 #xFF)))
    (list b0 b1 b2 b3)))

(defun nc-movz (rd imm)
  (let* ((masked (logand imm #xFFFF))
         (shifted (ash masked 5))
         (ored (logior #xD2800000 shifted))
         (word (logior ored rd)))
    (nc-encode-word word)))

(defun nc-add-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x8B000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-sub-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #xCB000000 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-mul-reg (rd rn rm)
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9B007C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-sdiv-reg (rd rn rm)
  ;; SDIV Xd, Xn, Xm - Signed divide
  ;; Encoding: 1001 1010 110 Rm 00001 0 Rn Rd
  (let* ((rm-shift (ash rm 16))
         (rn-shift (ash rn 5))
         (or1 (logior #x9AC00C00 rm-shift))
         (or2 (logior or1 rn-shift))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-lsl-imm (rd rn shift)
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
    (nc-encode-word word)))

(defun nc-lsr-imm (rd rn shift)
  (let* ((shift-s (ash shift 16))
         (rn-s (ash rn 5))
         (or1 (logior #xD340FC00 shift-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-mov-reg (rd rm)
  (let* ((rm-s (ash rm 16))
         (or1 (logior #xAA0003E0 rm-s))
         (word (logior or1 rd)))
    (nc-encode-word word)))

(defun nc-and-reg (rd rn rm)
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #x8A000000 rm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-ldr-offset (rt rn offset)
  (let* ((off-s (ash offset -3))
         (off-ss (ash off-s 10))
         (rn-s (ash rn 5))
         (or1 (logior #xF9400000 off-ss))
         (or2 (logior or1 rn-s))
         (word (logior or2 rt)))
    (nc-encode-word word)))

(defun nc-str-offset (rt rn offset)
  (let* ((off-s (ash offset -3))
         (off-ss (ash off-s 10))
         (rn-s (ash rn 5))
         (or1 (logior #xF9000000 off-ss))
         (or2 (logior or1 rn-s))
         (word (logior or2 rt)))
    (nc-encode-word word)))

(defun nc-add-imm (rd rn imm)
  (let* ((imm-m (logand imm #xFFF))
         (imm-s (ash imm-m 10))
         (rn-s (ash rn 5))
         (or1 (logior #x91000000 imm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-sub-imm (rd rn imm)
  (let* ((imm-m (logand imm #xFFF))
         (imm-s (ash imm-m 10))
         (rn-s (ash rn 5))
         (or1 (logior #xD1000000 imm-s))
         (or2 (logior or1 rn-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-stp-offset (rt1 rt2 rn imm)
  (let* ((imm-s (ash imm -3))
         (imm-m (logand imm-s #x7F))
         (imm-ss (ash imm-m 15))
         (rt2-s (ash rt2 10))
         (rn-s (ash rn 5))
         (or1 (logior #xA9000000 imm-ss))
         (or2 (logior or1 rt2-s))
         (or3 (logior or2 rn-s))
         (word (logior or3 rt1)))
    (nc-encode-word word)))

(defun nc-ldp-offset (rt1 rt2 rn imm)
  (let* ((imm-s (ash imm -3))
         (imm-m (logand imm-s #x7F))
         (imm-ss (ash imm-m 15))
         (rt2-s (ash rt2 10))
         (rn-s (ash rn 5))
         (or1 (logior #xA9400000 imm-ss))
         (or2 (logior or1 rt2-s))
         (or3 (logior or2 rn-s))
         (word (logior or3 rt1)))
    (nc-encode-word word)))

(defun nc-cmp-reg (rn rm)
  (let* ((rm-s (ash rm 16))
         (rn-s (ash rn 5))
         (or1 (logior #xEB00001F rm-s))
         (word (logior or1 rn-s)))
    (nc-encode-word word)))

(defun nc-cset (rd cond-code)
  (let* ((cc-x (logxor cond-code 1))
         (cc-s (ash cc-x 12))
         (or1 (logior #x9A9F07E0 cc-s))
         (word (logior or1 rd)))
    (nc-encode-word word)))

(defun nc-b-offset (offset)
  (let* ((off-s (ash offset -2))
         (off-m (logand off-s #x3FFFFFF))
         (word (logior #x14000000 off-m)))
    (nc-encode-word word)))

(defun nc-bl-offset (offset)
  (let* ((off-s (ash offset -2))
         (off-m (logand off-s #x3FFFFFF))
         (word (logior #x94000000 off-m)))
    (nc-encode-word word)))

(defun nc-b-cond (cond-code offset)
  (let* ((off-s (ash offset -2))
         (off-m (logand off-s #x7FFFF))
         (off-ss (ash off-m 5))
         (or1 (logior #x54000000 off-ss))
         (word (logior or1 cond-code)))
    (nc-encode-word word)))

(defun nc-ret ()
  (nc-encode-word #xD65F03C0))

(defun nc-movk (rd imm shift)
  (let* ((shift-s (ash shift -4))
         (shift-ss (ash shift-s 21))
         (imm-m (logand imm #xFFFF))
         (imm-s (ash imm-m 5))
         (or1 (logior #xF2800000 shift-ss))
         (or2 (logior or1 imm-s))
         (word (logior or2 rd)))
    (nc-encode-word word)))

(defun nc-blr (rn)
  (let* ((rn-s (ash rn 5))
         (word (logior #xD63F0000 rn-s)))
    (nc-encode-word word)))

(defun nc-load-addr (rd addr)
  (let* ((lo16 (logand addr #xFFFF))
         (sh16 (ash addr -16))
         (hi16 (logand sh16 #xFFFF))
         (sh32 (ash addr -32))
         (hi32 (logand sh32 #xFFFF))
         (sh48 (ash addr -48))
         (hi48 (logand sh48 #xFFFF))
         (base (nc-movz rd lo16))
         (p1 (if (> hi16 0) (nc-movk rd hi16 16) nil))
         (r1 (append base p1))
         (p2 (if (> hi32 0) (nc-movk rd hi32 32) nil))
         (r2 (append r1 p2))
         (p3 (if (> hi48 0) (nc-movk rd hi48 48) nil)))
    (append r2 p3)))

(defun nc-cond-eq () 0)
(defun nc-cond-ne () 1)
(defun nc-cond-lt () 11)
(defun nc-cond-le () 13)
(defun nc-cond-gt () 12)
(defun nc-cond-ge () 10)

(defun nc-string-to-char-codes (str)
  "Convert string to list of character codes"
  (labels ((iter (i acc)
             (if (>= i (length str))
                 (reverse acc)
                 (iter (+ i 1) (cons (char-code (char str i)) acc)))))
    (iter 0 nil)))

(defun nc-codegen-string-from-chars (chars td)
  "Generate code to build a string from character codes.
   Returns code that leaves the string in x0."
  (let* ((len (length chars))
         (vec-slot (nc-temp-slot td))
         ;; Allocate vector: movz x0, len; ldr x11, [x19, #56]; blr x11
         ;; Runtime table index 7 = make_vector at offset 56
         (alloc (nc-append-all
                 (list (nc-movz 0 len)
                       (nc-ldr-offset 11 19 56)
                       (nc-blr 11)
                       (nc-str-offset 0 31 vec-slot)))))
    ;; Store each character: ldr x0, [sp, vec-slot]; movz x1, idx; movz x2, tagged-ch; ldr x11, [x19, #64]; blr x11
    ;; Runtime table index 8 = vector_set at offset 64
    (labels ((store-chars (chs idx acc)
               (if (null chs)
                   acc
                   (let* ((ch (car chs))
                          (tagged (ash ch 4))
                          (store-code (nc-append-all
                                       (list (nc-ldr-offset 0 31 vec-slot)
                                             (nc-movz 1 idx)
                                             (if (< tagged #x10000)
                                                 (nc-movz 2 tagged)
                                                 (nc-load-addr 2 tagged))
                                             (nc-ldr-offset 11 19 64)
                                             (nc-blr 11)))))
                     (store-chars (cdr chs) (+ idx 1) (append acc store-code))))))
      (let* ((stores (store-chars chars 0 nil))
             ;; Make string from vector: ldr x0, [sp, vec-slot]; ldr x9, [x19, #80]; blr x9
             ;; Runtime table index 10 = make_string_from_vector at offset 80
             (make-str (nc-append-all
                        (list (nc-ldr-offset 0 31 vec-slot)
                              (nc-ldr-offset 9 19 80)
                              (nc-blr 9)))))
        (nc-append-all (list alloc stores make-str))))))

;;; ============================================================
;;; Part 2: Utility Functions (nc-util-*)
;;; ============================================================

(defun nc-has-tag (ir tag)
  (and (consp ir) (eq (car ir) tag)))

(defun nc-env-lookup (sym env)
  (if (null env)
      nil
      (if (eq (caar env) sym)
          (cdar env)
          (nc-env-lookup sym (cdr env)))))

(defun nc-env-extend (bindings env)
  ;; Use let* to sequence operations - avoid nested recursive calls in args
  (labels ((max-off (e acc)
             (if (null e) acc
                 (let ((o (cdar e)))
                   (max-off (cdr e) (if (> o acc) o acc)))))
           (add-bs (bs off acc)
             (if (null bs) acc
                 (let ((entry (cons (caar bs) off)))
                   (add-bs (cdr bs) (+ off 1) (cons entry acc))))))
    (let* ((mx (if env (max-off env -1) -1))
           (bs-result (add-bs bindings (+ mx 1) nil))
           (rev-result (reverse bs-result)))
      (append rev-result env))))

(defun nc-count-instrs (code)
  (if (null code) 0 (ash (length code) -2)))

;; Append two lists - bind first, then append
(defun nc-append2 (a b)
  (let ((ar a))
    (append ar b)))

;; Append list of lists using fold - avoiding nested calls
(defun nc-append-all (lists)
  (labels ((iter (ls acc)
             (if (null ls) acc
                 (let* ((hd (car ls))
                        (tl (cdr ls))
                        (na (append acc hd)))
                   (iter tl na)))))
    (iter lists nil)))

;;; ============================================================
;;; Part 3: Reader (nc-read-*)
;;; ============================================================

(defun nc-whitespace-p (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
(defun nc-digit-p (ch) (and (>= ch #x30) (<= ch #x39)))
(defun nc-hex-digit-p (ch) (or (nc-digit-p ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
(defun nc-alpha-p (ch) (or (and (>= ch #x41) (<= ch #x5A)) (and (>= ch #x61) (<= ch #x7A))))
(defun nc-symbol-char-p (ch)
  (or (nc-alpha-p ch) (nc-digit-p ch) (= ch #x2D) (= ch #x5F) (= ch #x2B) (= ch #x2A)
      (= ch #x2F) (= ch #x3D) (= ch #x3C) (= ch #x3E) (= ch #x21) (= ch #x3F)
      (= ch #x26) (= ch #x25) (= ch #x3A)))

(defun nc-char-at (s pos)
  (if (< pos (string-length s)) (string-ref s pos) 0))

(defun nc-digit-val (ch) (- ch #x30))
(defun nc-hex-val (ch)
  (cond ((nc-digit-p ch) (- ch #x30))
        ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) 10))
        ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) 10))
        (t 0)))

(defun nc-skip-line (s pos)
  (let ((ch (nc-char-at s pos)))
    (if (or (= ch #x0A) (= ch 0)) (+ pos 1) (nc-skip-line s (+ pos 1)))))

(defun nc-skip-ws (s pos)
  (let ((ch (nc-char-at s pos)))
    (cond ((nc-whitespace-p ch) (nc-skip-ws s (+ pos 1)))
          ((= ch #x3B) (nc-skip-ws s (nc-skip-line s (+ pos 1))))
          (t pos))))

(defun nc-read-digits (s pos n)
  (let ((ch (nc-char-at s pos)))
    (if (nc-digit-p ch)
        (nc-read-digits s (+ pos 1) (+ (* n 10) (nc-digit-val ch)))
        (cons n pos))))

(defun nc-read-int (s pos)
  (let ((neg nil) (start pos))
    (let ((ch (nc-char-at s pos)))
      (cond ((= ch #x2D) (setq neg t) (setq start (+ pos 1)))
            ((= ch #x2B) (setq start (+ pos 1)))))
    (let* ((r (nc-read-digits s start 0))
           (val (car r))
           (end (cdr r)))
      (cons (if neg (- 0 val) val) end))))

(defun nc-read-hex-digits (s pos n)
  (let ((ch (nc-char-at s pos)))
    (if (nc-hex-digit-p ch)
        (nc-read-hex-digits s (+ pos 1) (+ (* n 16) (nc-hex-val ch)))
        (cons n pos))))

(defun nc-read-hex (s pos)
  (nc-read-hex-digits s pos 0))

(defun nc-chars-to-string (chars)
  (let* ((len (length chars))
         (vec (make-vector len)))
    (dotimes (i len)
      (vector-set vec i (nth i chars)))
    (make-string-from-vector vec)))

(defun nc-read-sym-chars (s pos chars)
  (let ((ch (nc-char-at s pos)))
    (if (nc-symbol-char-p ch)
        (nc-read-sym-chars s (+ pos 1) (cons ch chars))
        (cons chars pos))))

(defun nc-read-sym (s pos)
  (let* ((r (nc-read-sym-chars s pos nil))
         (chars (car r))
         (end (cdr r))
         (name (nc-chars-to-string (reverse chars)))
         (uname (string-upcase name)))
    (cons (cond ((string= uname "NIL") nil)
                ((string= uname "T") t)
                (t (intern uname)))
          end)))

(defun nc-read-str-chars (s pos chars)
  (let ((ch (nc-char-at s pos)))
    (cond
      ((= ch #x22) (cons chars (+ pos 1)))
      ((= ch #x5C)
       (let* ((esc (nc-char-at s (+ pos 1)))
              (ec (cond ((= esc #x6E) #x0A) ((= esc #x74) #x09) ((= esc #x72) #x0D) (t esc))))
         (nc-read-str-chars s (+ pos 2) (cons ec chars))))
      ((= ch 0) (cons chars pos))
      (t (nc-read-str-chars s (+ pos 1) (cons ch chars))))))

(defun nc-read-str (s pos)
  (let* ((r (nc-read-str-chars s (+ pos 1) nil))
         (chars (car r))
         (end (cdr r)))
    (cons (nc-chars-to-string (reverse chars)) end)))

(defun nc-read (source pos)
  (labels
      ((read-list-elems (p)
         (let* ((p2 (nc-skip-ws source p))
                (ch (nc-char-at source p2)))
           (cond
             ((= ch #x29) (cons nil (+ p2 1)))
             ((= ch #x2E)
              (let* ((r (read-one (+ p2 1)))
                     (cdr-val (car r))
                     (p3 (cdr r))
                     (p4 (nc-skip-ws source p3)))
                (cons cdr-val (+ p4 1))))
             ((= ch 0) (cons nil p2))
             (t (let* ((er (read-one p2))
                       (el (car er))
                       (p3 (cdr er))
                       (rr (read-list-elems p3)))
                  (cons (cons el (car rr)) (cdr rr)))))))
       (read-list (p) (read-list-elems (+ p 1)))
       (read-sharp (p)
         (let ((ch (nc-char-at source (+ p 1))))
           (cond
             ((or (= ch #x78) (= ch #x58)) (nc-read-hex source (+ p 2)))
             ((= ch #x27)
              (let ((r (read-one (+ p 2))))
                (cons (list 'function (car r)) (cdr r))))
             ((= ch #x5C)
              (let ((ch2 (nc-char-at source (+ p 2))))
                (if (nc-alpha-p (nc-char-at source (+ p 3)))
                    (let* ((r (nc-read-sym-chars source (+ p 2) nil))
                           (nm (nc-chars-to-string (reverse (car r)))))
                      (cons (cond ((string= nm "newline") #x0A) ((string= nm "space") #x20)
                                  ((string= nm "tab") #x09) (t ch2))
                            (cdr r)))
                    (cons ch2 (+ p 3)))))
             (t (cons nil (+ p 2))))))
       (read-one (p)
         (let* ((p2 (nc-skip-ws source p))
                (ch (nc-char-at source p2)))
           (if (>= p2 (string-length source))
               (cons nil p2)
               (cond
                 ((= ch #x22) (nc-read-str source p2))
                 ((= ch #x28) (read-list p2))
                 ((= ch #x27)
                  (let ((r (read-one (+ p2 1))))
                    (cons (list 'quote (car r)) (cdr r))))
                 ((= ch #x60)
                  (let ((r (read-one (+ p2 1))))
                    (cons (list 'quasiquote (car r)) (cdr r))))
                 ((= ch #x2C)
                  (if (= (nc-char-at source (+ p2 1)) #x40)
                      (let ((r (read-one (+ p2 2))))
                        (cons (list 'unquote-splicing (car r)) (cdr r)))
                      (let ((r (read-one (+ p2 1))))
                        (cons (list 'unquote (car r)) (cdr r)))))
                 ((= ch #x23) (read-sharp p2))
                 ((or (nc-digit-p ch)
                      (and (or (= ch #x2D) (= ch #x2B))
                           (nc-digit-p (nc-char-at source (+ p2 1)))))
                  (nc-read-int source p2))
                 ((nc-symbol-char-p ch) (nc-read-sym source p2))
                 ((= ch #x29) (cons nil (+ p2 1)))
                 (t (read-one (+ p2 1))))))))
    (read-one pos)))

(defun nc-read-from-string (source)
  (car (nc-read source 0)))

(defun nc-read-all (source)
  (let ((len (string-length source)))
    (labels ((ra (pos acc)
               (let ((p2 (nc-skip-ws source pos)))
                 (if (>= p2 len)
                     (reverse acc)
                     (let ((r (nc-read source p2)))
                       (ra (cdr r) (cons (car r) acc)))))))
      (ra 0 nil))))

;;; ============================================================
;;; Part 4: Stack Frame Constants (inlined for delivery)
;;; ============================================================

;; Constants inlined directly to avoid global variable initialization issues
;; Frame size: #xFF0, Env base: #x180, Temp base: #x40
;; Temp guard: #x180, Spill base: #x200

(defun nc-frame-size () #xFF0)
(defun nc-env-base () #x180)
(defun nc-temp-base () #x40)
(defun nc-temp-guard () #x180)
(defun nc-spill-base () #x200)

(defun nc-temp-slot (depth)
  (let ((off (+ #x40 (* depth 8))))  ; #x40 = nc-temp-base
    (if (>= off #x180)  ; #x180 = nc-temp-guard
        (+ #x180 (* (- depth (ash (- #x180 #x40) -3)) 8))
        off)))

(defun nc-spill-slot (idx)
  (+ #x200 (* idx 8)))  ; #x200 = nc-spill-base

;;; ============================================================
;;; Part 5: Prologue/Epilogue
;;; ============================================================

(defun nc-prologue ()
  (append
   (nc-stp-offset 29 30 31 (- #xFF0))  ; -frame-size
   (nc-sub-imm 31 31 #xFF0)  ; frame-size
   (nc-mov-reg 29 31)
   (nc-stp-offset 19 20 31 16)
   (nc-stp-offset 21 22 31 32)
   (nc-stp-offset 23 24 31 48)
   (nc-mov-reg 20 31)
   (nc-add-imm 20 20 #x180)))  ; env-base

(defun nc-epilogue ()
  (append
   (nc-ldp-offset 23 24 31 48)
   (nc-ldp-offset 21 22 31 32)
   (nc-ldp-offset 19 20 31 16)
   (nc-add-imm 31 31 #xFF0)  ; frame-size
   (nc-ldp-offset 29 30 31 0)
   (nc-ret)))

;;; ============================================================
;;; Part 5b: Free Variable Analysis
;;; ============================================================

(defun nc-find-free-vars (expr bound env)
  "Find variables referenced in expr that are in env but not in bound"
  (labels ((collect (e bnd acc)
             (cond
               ((null e) acc)
               ((symbolp e)
                ;; Check if it's a variable reference (in env but not bound)
                (if (and (nc-env-lookup e env)
                         (not (member e bnd)))
                    (if (member e acc) acc (cons e acc))
                    acc))
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)  ; Don't look inside quotes
               ((eq (car e) 'lambda)
                ;; Lambda binds its params - add to bound
                (let ((params (cadr e))
                      (body (caddr e)))
                  (collect body (append params bnd) acc)))
               ((eq (car e) 'LET)
                ;; Let binds variables
                (let* ((bindings (cadr e))
                       (body (caddr e))
                       (names (mapcar #'car bindings))
                       (vals (mapcar #'cadr bindings))
                       ;; Collect from values first
                       (acc2 (collect-list vals bnd acc))
                       ;; Then body with new bindings
                       (new-bnd (append names bnd)))
                  (collect body new-bnd acc2)))
               ((eq (car e) 'LET*)
                (let* ((bindings (cadr e))
                       (body (caddr e)))
                  (labels ((do-bindings (bs bnd acc)
                             (if (null bs)
                                 (collect body bnd acc)
                                 (let* ((b (car bs))
                                        (nm (car b))
                                        (vl (cadr b))
                                        (acc2 (collect vl bnd acc)))
                                   (do-bindings (cdr bs) (cons nm bnd) acc2)))))
                    (do-bindings bindings bnd acc))))
               (t
                ;; General case: collect from all subexpressions
                (collect-list e bnd acc))))
           (collect-list (lst bnd acc)
             (if (null lst)
                 acc
                 (collect-list (cdr lst) bnd (collect (car lst) bnd acc)))))
    (collect expr bound nil)))

;;; ============================================================
;;; Part 6: IR Compiler (nc-compile-*)
;;; ============================================================

(defun nc-quote-ir (obj)
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'nil-ir))  ;; Use nil-ir for proper nil, not (lit 0)
    ((symbolp obj) (list 'sym-lit (symbol-name obj)))
    ((consp obj) (list 'cons-ir (nc-quote-ir (car obj)) (nc-quote-ir (cdr obj))))
    (t (list 'lit 0))))

(defun nc-compile (expr env fenv)
  (cond
    ((numberp expr) (list 'lit expr))
    ((symbolp expr)
     ;; Use numberp since offset 0 is falsey in Habu
     (let ((off (nc-env-lookup expr env)))
       (if (numberp off)
           (list 'var off)
           ;; Check if it's a known function name - return as symbol reference
           (if (and fenv (assoc expr fenv))
               (list 'sym-lit (symbol-name expr))
               (list 'lit 0)))))
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ((eq op '+)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (nc-compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'add (nc-compile (car args) env fenv) (nc-compile (cadr args) env fenv))
                        (nc-compile (cons '+ (cons (list '+ (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op '-)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 0)
                (if (null (cdr args)) (list 'sub (list 'lit 0) (nc-compile (car args) env fenv))
                    (if (null (cddr args))
                        (list 'sub (nc-compile (car args) env fenv) (nc-compile (cadr args) env fenv))
                        (nc-compile (cons '- (cons (list '- (car args) (cadr args)) (cddr args))) env fenv))))))
         ((eq op '*)
          (let ((args (cdr expr)))
            (if (null args) (list 'lit 1)
                (if (null (cdr args)) (nc-compile (car args) env fenv)
                    (if (null (cddr args))
                        (list 'mul (nc-compile (car args) env fenv) (nc-compile (cadr args) env fenv))
                        (nc-compile (cons '* (cons (list '* (car args) (cadr args)) (cddr args))) env fenv))))))
         ;; division
         ((eq op '/)
          (list 'div (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ;; modulo
         ((eq op 'mod)
          (list 'mod-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op 'rem)
          (list 'mod-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ;; 1+ and 1-
         ((eq op '1+)
          (list 'add (nc-compile (cadr expr) env fenv) (list 'lit 1)))
         ((eq op '1-)
          (list 'sub (nc-compile (cadr expr) env fenv) (list 'lit 1)))
         ((eq op '=)
          (list 'cmp-eq (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '<)
          (list 'cmp-lt (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '>)
          (list 'cmp-gt (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '<=)
          (list 'cmp-le (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op '>=)
          (list 'cmp-ge (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op 'if)
          (list 'if-ir
                (nc-compile (cadr expr) env fenv)
                (nc-compile (caddr expr) env fenv)
                (if (cdddr expr) (nc-compile (cadddr expr) env fenv) (list 'lit 0))))
         ;; cond - multi-branch conditional
         ((eq op 'cond)
          (let ((clauses (cdr expr)))
            (if (null clauses)
                (list 'lit 0)
                (let* ((clause (car clauses))
                       (test (car clause))
                       (body (cdr clause)))
                  (if (eq test 't)
                      ;; t clause - always execute
                      (if (null body)
                          (list 'lit 1)
                          (if (null (cdr body))
                              (nc-compile (car body) env fenv)
                              (nc-compile (cons 'progn body) env fenv)))
                      (list 'if-ir
                            (nc-compile test env fenv)
                            (if (null body)
                                (nc-compile test env fenv)
                                (if (null (cdr body))
                                    (nc-compile (car body) env fenv)
                                    (nc-compile (cons 'progn body) env fenv)))
                            (nc-compile (cons 'cond (cdr clauses)) env fenv)))))))
         ;; when - if with implicit progn (no else branch)
         ((eq op 'when)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (list 'if-ir
                  (nc-compile test env fenv)
                  (if (null (cdr body))
                      (nc-compile (car body) env fenv)
                      (nc-compile (cons 'progn body) env fenv))
                  (list 'lit 0))))
         ;; unless - negated when
         ((eq op 'unless)
          (let ((test (cadr expr))
                (body (cddr expr)))
            (list 'if-ir
                  (nc-compile test env fenv)
                  (list 'lit 0)
                  (if (null (cdr body))
                      (nc-compile (car body) env fenv)
                      (nc-compile (cons 'progn body) env fenv)))))
         ;; dotimes - counted iteration using tail-recursive helper
         ((eq op 'dotimes)
          ;; (dotimes (var count [result]) body...)
          ;; Transform to recursive structure with explicit counter passing
          (let* ((spec (cadr expr))
                 (var (car spec))
                 (count-form (cadr spec))
                 (result-form (if (cddr spec) (caddr spec) 0))
                 (body (cddr expr)))
            ;; Create a dotimes-ir node
            (list 'dotimes-ir
                  var
                  (nc-compile count-form env fenv)
                  body  ; Keep body as source, compile during eval
                  result-form
                  env)))  ; Save env for body compilation
         ;; dolist - list iteration
         ((eq op 'dolist)
          ;; (dolist (var list [result]) body...)
          (let* ((spec (cadr expr))
                 (var (car spec))
                 (list-form (cadr spec))
                 (result-form (if (cddr spec) (caddr spec) nil))
                 (body (cddr expr)))
            ;; Create a dolist-ir node
            (list 'dolist-ir
                  var
                  (nc-compile list-form env fenv)
                  body  ; Keep body as source, compile during eval
                  result-form
                  env)))  ; Save env for body compilation
         ((eq op 'LET)  ; Changed to uppercase
          (let* ((bindings (cadr expr))
                 (body (caddr expr)))
            (labels ((proc (bs eacc vals names)
                       (if (null bs)
                           (list eacc (reverse vals) (reverse names))
                           (let* ((b (car bs))
                                  (nm (if (consp b) (car b) b))
                                  (vl (if (consp b) (cadr b) 0))
                                  (vi (nc-compile vl env fenv))
                                  (ne (nc-env-extend (list (list nm)) eacc)))
                             (proc (cdr bs) ne (cons vi vals) (cons nm names)))))
                     ;; Avoid mapcar - use labels recursion instead
                     (get-offs (ns e acc)
                       (if (null ns)
                           (reverse acc)
                           (get-offs (cdr ns) e (cons (nc-env-lookup (car ns) e) acc)))))
              (let* ((r (proc bindings env nil nil))
                     (nenv (car r))
                     (vals (cadr r))
                     (names (caddr r))
                     (offs (get-offs names nenv nil))
                     (bir (nc-compile body nenv fenv)))
                (list 'let-ir vals bir (length bindings) offs)))))
         ((eq op 'LET*)  ; Changed to uppercase
          (let ((bs (cadr expr)) (body (caddr expr)))
            (if (null bs)
                (nc-compile body env fenv)
                (nc-compile (list 'LET (list (car bs)) (list 'LET* (cdr bs) body)) env fenv))))
         ((eq op 'quote) (nc-quote-ir (cadr expr)))
         ;; function - return the function name (for passing to funcall)
         ((eq op 'function)
          (let ((fn-name (cadr expr)))
            (list 'sym-lit (symbol-name fn-name))))
         ;; lambda - anonymous function (closure)
         ((eq op 'lambda)
          (let* ((params (cadr expr))
                 (body (caddr expr))
                 ;; Find free variables (referenced but not in params)
                 (free-vars (nc-find-free-vars body params env))
                 ;; Get the offsets for each free var in current env
                 (free-offsets (mapcar (lambda (v) (nc-env-lookup v env)) free-vars)))
            (list 'lambda-ir params body free-vars free-offsets)))
         ((eq op 'cons)
          (list 'cons-ir (nc-compile (cadr expr) env fenv) (nc-compile (caddr expr) env fenv)))
         ((eq op 'car) (list 'car-ir (nc-compile (cadr expr) env fenv)))
         ((eq op 'cdr) (list 'cdr-ir (nc-compile (cadr expr) env fenv)))
         ((eq op 'list)
          (labels ((bl (args)
                     (if (null args) (list 'lit 0)
                         (list 'cons-ir (nc-compile (car args) env fenv) (bl (cdr args))))))
            (bl (cdr expr))))
         ((eq op 'null)
          (list 'cmp-eq (nc-compile (cadr expr) env fenv) (list 'lit 0)))
         ((eq op 'numberp)
          (list 'cmp-eq (list 'get-tag (nc-compile (cadr expr) env fenv)) (list 'lit 0)))
         ((eq op 'consp)
          (list 'cmp-eq (list 'get-tag (nc-compile (cadr expr) env fenv)) (list 'lit 1)))
         ;; progn - evaluate forms in sequence, return last
         ((eq op 'progn)
          (let ((forms (cdr expr)))
            (if (null forms)
                (list 'lit 0)
                (if (null (cdr forms))
                    (nc-compile (car forms) env fenv)
                    (list 'progn-ir
                          (mapcar (lambda (f) (nc-compile f env fenv)) forms))))))
         ;; and - short-circuit and
         ((eq op 'and)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'lit 1)
                (if (null (cdr args))
                    (nc-compile (car args) env fenv)
                    (list 'if-ir
                          (nc-compile (car args) env fenv)
                          (nc-compile (cons 'and (cdr args)) env fenv)
                          (list 'lit 0))))))
         ;; or - short-circuit or (returns first truthy value)
         ((eq op 'or)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'lit 0)
                (if (null (cdr args))
                    (nc-compile (car args) env fenv)
                    ;; Need to evaluate first arg, check if truthy, return it or continue
                    ;; Use a let to bind the value, then check and return
                    (let ((tmp (gensym "OR")))
                      (nc-compile
                       (list 'LET (list (list tmp (car args)))
                             (list 'if tmp tmp (cons 'or (cdr args))))
                       env fenv))))))
         ;; not - logical not
         ((eq op 'not)
          (list 'cmp-eq (nc-compile (cadr expr) env fenv) (list 'lit 0)))
         ;; funcall - call function by value
         ((eq op 'funcall)
          (list 'funcall-ir
                (nc-compile (cadr expr) env fenv)
                (mapcar (lambda (a) (nc-compile a env fenv)) (cddr expr))))
         ;; User function call or call via variable
         (t
          ;; Check if op is a known function name
          (if (and fenv (assoc op fenv))
              (list 'call-fn op (mapcar (lambda (a) (nc-compile a env fenv)) (cdr expr)))
              ;; Check if op is a variable (parameter) - compile as funcall
              (let ((off (nc-env-lookup op env)))
                (if (numberp off)
                    (list 'funcall-ir (list 'var off) (mapcar (lambda (a) (nc-compile a env fenv)) (cdr expr)))
                    (list 'lit 0))))))))
    (t (list 'lit 0))))

;;; ============================================================
;;; Part 6b: IR Evaluator (nc-eval-*)
;;; ============================================================

(defun nc-eval-ir (ir env)
  "Evaluate IR and return tagged value"
  (cond
    ((nc-has-tag ir 'lit) (cadr ir))
    ((nc-has-tag ir 'var)
     (let ((off (cadr ir)))
       (nth off env)))
    ((nc-has-tag ir 'add)
     (+ (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'sub)
     (- (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'mul)
     (* (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)))
    ((nc-has-tag ir 'cmp-eq)
     (if (= (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-lt)
     (if (< (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-gt)
     (if (> (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-le)
     (if (<= (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'cmp-ge)
     (if (>= (nc-eval-ir (cadr ir) env) (nc-eval-ir (caddr ir) env)) 1 0))
    ((nc-has-tag ir 'if-ir)
     (if (not (= (nc-eval-ir (cadr ir) env) 0))
         (nc-eval-ir (caddr ir) env)
         (nc-eval-ir (cadddr ir) env)))
    ((nc-has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count offs)
     ;; offs is at index 4, which is (nth 3 (cdr ir))
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir))))  ; Fixed: was (nth 4 ...)
       (labels ((bind (vs os e)
                  (if (null vs) e
                      (let ((v (nc-eval-ir (car vs) env)))
                        (bind (cdr vs) (cdr os)
                              (append e (list v)))))))
         (nc-eval-ir bir (bind vals offs env)))))
    (t 0)))

;; Global function environment for IR evaluation
(defvar *nc-fenv* nil)

(defun nc-eval-ir-with-fns (ir env fenv)
  "Evaluate IR with function environment"
  (cond
    ((nc-has-tag ir 'lit) (cadr ir))
    ((nc-has-tag ir 'nil-ir) nil)  ;; Evaluate to proper SBCL nil
    ((nc-has-tag ir 'sym-lit)
     ;; Return the symbol itself (interned)
     (intern (cadr ir)))
    ((nc-has-tag ir 'var)
     (let ((off (cadr ir)))
       (nth off env)))
    ((nc-has-tag ir 'add)
     (+ (nc-eval-ir-with-fns (cadr ir) env fenv)
        (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'sub)
     (- (nc-eval-ir-with-fns (cadr ir) env fenv)
        (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'mul)
     (* (nc-eval-ir-with-fns (cadr ir) env fenv)
        (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'div)
     (truncate (nc-eval-ir-with-fns (cadr ir) env fenv)
               (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'mod-ir)
     (mod (nc-eval-ir-with-fns (cadr ir) env fenv)
          (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'cmp-eq)
     (if (= (nc-eval-ir-with-fns (cadr ir) env fenv)
            (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-lt)
     (if (< (nc-eval-ir-with-fns (cadr ir) env fenv)
            (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-gt)
     (if (> (nc-eval-ir-with-fns (cadr ir) env fenv)
            (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-le)
     (if (<= (nc-eval-ir-with-fns (cadr ir) env fenv)
             (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cmp-ge)
     (if (>= (nc-eval-ir-with-fns (cadr ir) env fenv)
             (nc-eval-ir-with-fns (caddr ir) env fenv)) 1 0))
    ((nc-has-tag ir 'cons-ir)
     (cons (nc-eval-ir-with-fns (cadr ir) env fenv)
           (nc-eval-ir-with-fns (caddr ir) env fenv)))
    ((nc-has-tag ir 'car-ir)
     (car (nc-eval-ir-with-fns (cadr ir) env fenv)))
    ((nc-has-tag ir 'cdr-ir)
     (cdr (nc-eval-ir-with-fns (cadr ir) env fenv)))
    ((nc-has-tag ir 'if-ir)
     (if (not (= (nc-eval-ir-with-fns (cadr ir) env fenv) 0))
         (nc-eval-ir-with-fns (caddr ir) env fenv)
         (nc-eval-ir-with-fns (cadddr ir) env fenv)))
    ((nc-has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir))))
       (labels ((bind (vs os e)
                  (if (null vs) e
                      (let ((v (nc-eval-ir-with-fns (car vs) env fenv)))
                        (bind (cdr vs) (cdr os)
                              (append e (list v)))))))
         (nc-eval-ir-with-fns bir (bind vals offs env) fenv))))
    ((nc-has-tag ir 'progn-ir)
     ;; progn-ir = (progn-ir (ir1 ir2 ... irn))
     (let ((forms-ir (cadr ir)))
       (labels ((eval-seq (fs)
                  (if (null fs)
                      0
                      (let ((v (nc-eval-ir-with-fns (car fs) env fenv)))
                        (if (null (cdr fs))
                            v
                            (eval-seq (cdr fs)))))))
         (eval-seq forms-ir))))
    ((nc-has-tag ir 'call-fn)
     ;; call-fn = (call-fn name args-ir-list)
     (let* ((fnm (cadr ir))
            (args-ir (caddr ir))
            (fn-def (cdr (assoc fnm fenv))))
       (if fn-def
           ;; fn-def = (name params body-ir param-base)
           (let* ((params (cadr fn-def))
                  (body-ir (caddr fn-def)))
             ;; Evaluate arguments
             (labels ((eval-args (airs acc)
                        (if (null airs) (reverse acc)
                            (eval-args (cdr airs)
                                       (cons (nc-eval-ir-with-fns (car airs) env fenv) acc)))))
               (let ((arg-vals (eval-args args-ir nil)))
                 ;; Call with new env containing args
                 (nc-eval-ir-with-fns body-ir arg-vals fenv))))
           0)))
    ((nc-has-tag ir 'funcall-ir)
     ;; funcall-ir = (funcall-ir fn-ir args-ir-list)
     ;; fn-ir evaluates to a function name (symbol) or closure
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (fn-val (nc-eval-ir-with-fns fn-ir env fenv)))
       ;; Check if fn-val is a closure (list starting with :closure)
       (if (and (consp fn-val) (eq (car fn-val) :closure))
           ;; Closure: (:closure params body free-vars captured-vals)
           (let* ((params (cadr fn-val))
                  (body (caddr fn-val))
                  (free-vars (cadddr fn-val))
                  (captured-vals (nth 4 fn-val)))
             (labels ((eval-args (airs acc)
                        (if (null airs) (reverse acc)
                            (eval-args (cdr airs)
                                       (cons (nc-eval-ir-with-fns (car airs) env fenv) acc)))))
               (let* ((arg-vals (eval-args args-ir nil))
                      ;; Build env with params and free vars
                      (param-bindings (mapcar #'list params))
                      (free-bindings (mapcar #'list free-vars))
                      (param-env (nc-env-extend param-bindings nil))
                      (full-env (nc-env-extend free-bindings param-env))
                      ;; Build value list: args then captured
                      (all-vals (append arg-vals captured-vals))
                      ;; Compile body with this env
                      (body-ir (nc-compile body full-env fenv)))
                 (nc-eval-ir-with-fns body-ir all-vals fenv))))
           ;; Named function: look up in fenv
           (let ((fn-def (cdr (assoc fn-val fenv))))
             (if fn-def
                 (let* ((params (cadr fn-def))
                        (body-ir (caddr fn-def)))
                   (labels ((eval-args (airs acc)
                              (if (null airs) (reverse acc)
                                  (eval-args (cdr airs)
                                             (cons (nc-eval-ir-with-fns (car airs) env fenv) acc)))))
                     (let ((arg-vals (eval-args args-ir nil)))
                       (nc-eval-ir-with-fns body-ir arg-vals fenv))))
                 0)))))
    ((nc-has-tag ir 'lambda-ir)
     ;; lambda-ir = (lambda-ir params body free-vars free-var-offsets)
     ;; Create a closure: capture the values of free variables using offsets
     (let* ((params (cadr ir))
            (body (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth 4 ir)))  ; The offsets computed at compile time
       ;; Capture current values using the pre-computed offsets
       (labels ((capture (offs acc)
                  (if (null offs)
                      (reverse acc)
                      (let ((val (nth (car offs) env)))
                        (capture (cdr offs) (cons val acc))))))
         ;; Return: (:closure params body free-vars captured-vals)
         (list :closure params body free-vars (capture free-offsets nil)))))
    ((nc-has-tag ir 'dotimes-ir)
     ;; dotimes-ir = (dotimes-ir var count-ir body result-form compile-env)
     (let* ((var (cadr ir))
            (count-ir (caddr ir))
            (body (cadddr ir))
            (result-form (nth 4 ir))
            (compile-env (nth 5 ir))
            (count (nc-eval-ir-with-fns count-ir env fenv)))
       ;; Iterative loop
       (labels ((iter (i)
                  (if (>= i count)
                      (if result-form
                          (let* ((new-env (nc-env-extend (list (list var)) compile-env))
                                 (result-ir (nc-compile result-form new-env fenv)))
                            (nc-eval-ir-with-fns result-ir (append env (list i)) fenv))
                          0)
                      (let* ((new-env (nc-env-extend (list (list var)) compile-env))
                             (body-ir (if (null (cdr body))
                                          (nc-compile (car body) new-env fenv)
                                          (nc-compile (cons 'progn body) new-env fenv))))
                        (nc-eval-ir-with-fns body-ir (append env (list i)) fenv)
                        (iter (+ i 1))))))
         (iter 0))))
    ((nc-has-tag ir 'dolist-ir)
     ;; dolist-ir = (dolist-ir var list-ir body result-form compile-env)
     (let* ((var (cadr ir))
            (list-ir (caddr ir))
            (body (cadddr ir))
            (result-form (nth 4 ir))
            (compile-env (nth 5 ir))
            (lst (nc-eval-ir-with-fns list-ir env fenv)))
       ;; Iterative loop over list
       (labels ((iter (remaining)
                  (if (null remaining)
                      (if result-form
                          (let* ((new-env (nc-env-extend (list (list var)) compile-env))
                                 (result-ir (nc-compile result-form new-env fenv)))
                            (nc-eval-ir-with-fns result-ir (append env (list nil)) fenv))
                          0)
                      (let* ((elem (car remaining))
                             (new-env (nc-env-extend (list (list var)) compile-env))
                             (body-ir (if (null (cdr body))
                                          (nc-compile (car body) new-env fenv)
                                          (nc-compile (cons 'progn body) new-env fenv))))
                        (nc-eval-ir-with-fns body-ir (append env (list elem)) fenv)
                        (iter (cdr remaining))))))
         (iter lst))))
    (t 0)))

;;; ============================================================
;;; Part 7: Code Generator (nc-codegen-*)
;;; ============================================================

(defun nc-codegen (ir rtaddrs fnoffs td)
  (cond
    ((nc-has-tag ir 'lit)
     (let* ((v (cadr ir))
            (tg (ash v 4)))
       (if (and (>= tg 0) (< tg #x10000))
           (nc-movz 0 tg)
           (nc-load-addr 0 tg))))
    ((nc-has-tag ir 'nil-ir)
     ;; nil is represented as tagged 0 (fixnum 0) in native code
     (nc-movz 0 0))
    ((nc-has-tag ir 'sym-lit)
     ;; Symbol literal: build string from chars, then call make-symbol-from-string
     ;; Runtime table index 11 = make_symbol_from_string at offset 88
     (let* ((name (cadr ir))
            (chars (nc-string-to-char-codes name))
            (str-code (nc-codegen-string-from-chars chars td)))
       (nc-append-all
        (list str-code
              (nc-ldr-offset 9 19 88)
              (nc-blr 9)))))
    ((nc-has-tag ir 'var)
     (let* ((off (cadr ir))
            (off8 (* off 8))
            (i1 (nc-sub-imm 1 20 off8))
            (i2 (nc-ldr-offset 0 1 0)))
       (nc-append-all (list i1 i2))))
    ((nc-has-tag ir 'get-tag)
     (let* ((ac (nc-codegen (cadr ir) rtaddrs fnoffs td))
            (i1 (nc-movz 1 #xF))
            (i2 (nc-and-reg 0 0 1))
            (i3 (nc-lsl-imm 0 0 4)))
       (nc-append-all (list ac i1 i2 i3))))
    ((nc-has-tag ir 'add)
     ;; Use nc-append-all to avoid deeply nested let*
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-add-reg 0 0 1)))))))))))
    ((nc-has-tag ir 'sub)
     ;; Use nc-append-all to avoid deeply nested let*
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-sub-reg 0 0 1)))))))))))
    ((nc-has-tag ir 'mul)
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-lsr-imm 0 0 4)
                          (nc-lsr-imm 1 1 4)
                          (nc-mul-reg 0 0 1)
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'cmp-eq)
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-cmp-reg 0 1)
                          (nc-cset 0 (nc-cond-eq))
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'cmp-lt)
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-cmp-reg 0 1)
                          (nc-cset 0 (nc-cond-lt))
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'cmp-gt)
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-cmp-reg 0 1)
                          (nc-cset 0 (nc-cond-gt))
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'cmp-le)
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-cmp-reg 0 1)
                          (nc-cset 0 (nc-cond-le))
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'cmp-ge)
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-cmp-reg 0 1)
                          (nc-cset 0 (nc-cond-ge))
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'cons-ir)
     (let ((car-ir (cadr ir)))
       (let ((cdr-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((cs (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((cc (nc-codegen car-ir rtaddrs fnoffs nd)))
                 (let ((dc (nc-codegen cdr-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          cc
                          (nc-str-offset 0 31 cs)
                          (nc-ldr-offset 24 31 xs)
                          dc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 cs)
                          (nc-ldr-offset 9 19 0)
                          (nc-blr 9)))))))))))
    ((nc-has-tag ir 'car-ir)
     (let ((arg-ir (cadr ir)))
       (let ((ac (nc-codegen arg-ir rtaddrs fnoffs td)))
         (nc-append-all
          (list ac
                (nc-ldr-offset 9 19 8)
                (nc-blr 9))))))
    ((nc-has-tag ir 'cdr-ir)
     (let ((arg-ir (cadr ir)))
       (let ((ac (nc-codegen arg-ir rtaddrs fnoffs td)))
         (nc-append-all
          (list ac
                (nc-ldr-offset 9 19 16)
                (nc-blr 9))))))
    ((nc-has-tag ir 'if-ir)
     (let ((test-ir (cadr ir)))
       (let ((then-ir (caddr ir)))
         (let ((else-ir (cadddr ir)))
           (let ((tc (nc-codegen test-ir rtaddrs fnoffs td)))
             (let ((thc (nc-codegen then-ir rtaddrs fnoffs td)))
               (let ((elc (nc-codegen else-ir rtaddrs fnoffs td)))
                 (let ((thl (nc-count-instrs thc)))
                   (let ((ell (nc-count-instrs elc)))
                     (nc-append-all
                      (list tc
                            (nc-movz 1 0)
                            (nc-cmp-reg 0 1)
                            (nc-b-cond (nc-cond-eq) (* (+ thl 1) 4))
                            thc
                            (nc-b-offset (* ell 4))
                            elc)))))))))))
    ((nc-has-tag ir 'let-ir)
     ;; let-ir = (let-ir vals bir count offs)
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 3 (cdr ir)))  ;; offs is at index 3
            (xs (nc-temp-slot td))
            (nd (+ td 1))
            (acc (nc-str-offset 24 31 xs)))
       (labels ((gb (vs os a)
                  (if (null vs) a
                      (let* ((vc (nc-codegen (car vs) rtaddrs fnoffs nd))
                             (s1 (nc-sub-imm 1 20 (* (car os) 8)))
                             (s2 (nc-str-offset 0 1 0))
                             (st (append s1 s2))
                             (ld (nc-ldr-offset 24 31 xs))
                             (t1 (append a ld))
                             (t2 (append t1 vc))
                             (t3 (append t2 st)))
                        (gb (cdr vs) (cdr os) t3)))))
         (let* ((body-code (gb vals offs nil))
                (final-ld (nc-ldr-offset 24 31 xs))
                (bc (nc-codegen bir rtaddrs fnoffs nd))
                (r1 (append acc body-code))
                (r2 (append r1 final-ld)))
           (append r2 bc)))))
    ((nc-has-tag ir 'call-fn)
     (let* ((fnm (cadr ir))
            (airs (caddr ir))
            (na (length airs))
            (xs (nc-temp-slot td))
            (nd (+ td 1)))
       (labels ((ga (as i a)
                  (if (null as) a
                      (let* ((rs (if (> i 0) (nc-ldr-offset 24 31 xs) nil))
                             (ac (nc-codegen (car as) rtaddrs fnoffs nd))
                             (st (nc-str-offset 0 31 (nc-spill-slot i)))
                             (t1 (append a rs))
                             (t2 (append t1 ac))
                             (t3 (append t2 st)))
                        (ga (cdr as) (+ i 1) t3))))
                (gl (i a)
                  (if (>= i na) a
                      (let* ((ld (nc-ldr-offset i 31 (nc-spill-slot i)))
                             (t1 (append a ld)))
                        (gl (+ i 1) t1)))))
         (let* ((save-x24 (nc-str-offset 24 31 xs))
                (args-code (ga airs 0 nil))
                (restore-x24 (nc-ldr-offset 24 31 xs))
                (load-args (gl 0 nil))
                (set-argc (nc-movz 23 na))
                (call-fn (nc-bl-offset 0))
                (r1 (append save-x24 args-code))
                (r2 (append r1 restore-x24))
                (r3 (append r2 load-args))
                (r4 (append r3 set-argc)))
           (append r4 call-fn)))))
    ((nc-has-tag ir 'progn-ir)
     ;; progn-ir = (progn-ir (ir1 ir2 ... irn))
     ;; Generate code for each form, keep result of last
     (let ((forms-ir (cadr ir)))
       (labels ((gen-seq (fs acc)
                  (if (null fs)
                      acc
                      (let ((fc (nc-codegen (car fs) rtaddrs fnoffs td)))
                        (gen-seq (cdr fs) (append acc fc))))))
         (gen-seq forms-ir nil))))
    ((nc-has-tag ir 'div)
     ;; Division: both operands untagged, divide, re-tag
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-lsr-imm 0 0 4)
                          (nc-lsr-imm 1 1 4)
                          (nc-sdiv-reg 0 0 1)
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'mod)
     ;; Modulo: a mod b = a - (a / b) * b
     (let ((left-ir (cadr ir)))
       (let ((right-ir (caddr ir)))
         (let ((xs (nc-temp-slot td)))
           (let ((ls (nc-temp-slot (+ td 1))))
             (let ((nd (+ td 2)))
               (let ((lc (nc-codegen left-ir rtaddrs fnoffs nd)))
                 (let ((rc (nc-codegen right-ir rtaddrs fnoffs nd)))
                   (nc-append-all
                    (list (nc-str-offset 24 31 xs)
                          lc
                          (nc-str-offset 0 31 ls)
                          (nc-ldr-offset 24 31 xs)
                          rc
                          (nc-mov-reg 1 0)
                          (nc-ldr-offset 0 31 ls)
                          (nc-lsr-imm 0 0 4)
                          (nc-lsr-imm 1 1 4)
                          (nc-sdiv-reg 2 0 1)
                          (nc-mul-reg 2 2 1)
                          (nc-sub-reg 0 0 2)
                          (nc-lsl-imm 0 0 4)))))))))))
    ((nc-has-tag ir 'lambda-ir)
     ;; lambda-ir should be lifted to lambda-ref before codegen
     ;; If we encounter it directly, it's an error - return 0
     (nc-movz 0 0))
    ((nc-has-tag ir 'lambda-ref)
     ;; lambda-ref = (lambda-ref name free-var-offsets)
     ;; Create a closure:
     ;; 1. Get code pointer from code base + lambda offset (from fnoffs)
     ;; 2. Create env vector with captured values
     ;; 3. Call make-closure
     ;; Runtime table: [3] make-closure at offset 24, [6] code-base at offset 48
     ;;                [7] make-vector at offset 56, [8] vector-set at offset 64
     (let* ((name (cadr ir))
            (free-offsets (caddr ir))
            (capture-count (length free-offsets))
            (fn-entry (assoc name fnoffs))
            (fn-offset (if fn-entry (cdr fn-entry) 0))
            (offset-bytes (* fn-offset 4))
            (code-slot (nc-temp-slot td))
            (env-slot (nc-temp-slot (+ td 1))))
       (if (= capture-count 0)
           ;; No captures - simple closure with nil env
           (nc-append-all
            (list
             ;; Get code pointer
             (nc-ldr-offset 9 19 48)            ; x9 = code base
             (nc-load-addr 10 offset-bytes)    ; x10 = offset
             (nc-add-reg 0 9 10)               ; x0 = code ptr
             (nc-movz 1 0)                     ; x1 = nil (no env)
             (nc-ldr-offset 11 19 24)          ; make-closure
             (nc-blr 11)))
           ;; Has captures - build env vector
           (let ((capture-stores
                  (labels ((store-caps (offs idx acc)
                             (if (null offs)
                                 acc
                                 (let* ((off (car offs))
                                        (store
                                         (nc-append-all
                                          (list
                                           (nc-ldr-offset 0 31 env-slot)        ; x0 = vector
                                           (nc-movz 1 idx)                      ; x1 = index
                                           (nc-sub-imm 2 20 (* off 8))          ; x2 = x20 - off*8
                                           (nc-ldr-offset 2 2 0)                ; x2 = captured value
                                           (nc-ldr-offset 11 19 64)             ; vector-set
                                           (nc-blr 11)))))
                                   (store-caps (cdr offs) (+ idx 1) (append acc store))))))
                    (store-caps free-offsets 0 nil))))
             (nc-append-all
              (list
               ;; Get code pointer and save
               (nc-ldr-offset 9 19 48)          ; x9 = code base
               (nc-load-addr 10 offset-bytes)   ; x10 = offset
               (nc-add-reg 0 9 10)              ; x0 = code ptr
               (nc-str-offset 0 31 code-slot)   ; save code ptr
               ;; Allocate env vector
               (nc-movz 0 capture-count)        ; x0 = length
               (nc-ldr-offset 11 19 56)         ; make-vector
               (nc-blr 11)                      ; x0 = vector
               (nc-str-offset 0 31 env-slot)    ; save vector
               ;; Store captures
               capture-stores
               ;; Make closure
               (nc-ldr-offset 0 31 code-slot)   ; x0 = code ptr
               (nc-ldr-offset 1 31 env-slot)    ; x1 = env vector
               (nc-ldr-offset 11 19 24)         ; make-closure
               (nc-blr 11)))))))
    ((nc-has-tag ir 'funcall-ir)
     ;; funcall-ir = (funcall-ir fn-ir args-ir-list)
     ;; 1. Evaluate fn-ir to get closure
     ;; 2. Extract code pointer and env from closure
     ;; 3. Set up args and call
     ;; Runtime table: [4] closure-code at offset 32, [5] closure-env at offset 40
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (num-args (length args-ir))
            ;; Temp slots: 0=x24-save, 1=closure, 2=code, 3=env, 4..4+n-1=args
            (x24-slot (nc-temp-slot td))
            (closure-slot (nc-temp-slot (+ td 1)))
            (code-slot (nc-temp-slot (+ td 2)))
            (env-slot (nc-temp-slot (+ td 3)))
            (arg-base (+ td 4))
            (nested-td (+ arg-base num-args))
            ;; Evaluate function
            (fn-code (nc-codegen fn-ir rtaddrs fnoffs nested-td)))
       (labels ((gen-args (airs idx acc)
                  (if (null airs)
                      acc
                      (let* ((rs (if (> idx 0) (nc-ldr-offset 24 31 x24-slot) nil))
                             (ac (nc-codegen (car airs) rtaddrs fnoffs nested-td))
                             (st (nc-str-offset 0 31 (nc-temp-slot (+ arg-base idx)))))
                        (gen-args (cdr airs) (+ idx 1)
                                  (nc-append-all (list acc rs ac st))))))
                (load-args (idx acc)
                  (if (>= idx num-args)
                      acc
                      (let ((ld (nc-ldr-offset idx 31 (nc-temp-slot (+ arg-base idx)))))
                        (load-args (+ idx 1) (append acc ld))))))
         (nc-append-all
          (list
           ;; Save x24
           (nc-str-offset 24 31 x24-slot)
           ;; Evaluate and save closure
           fn-code
           (nc-str-offset 0 31 closure-slot)
           ;; Get code pointer
           (nc-ldr-offset 9 19 32)              ; closure-code
           (nc-blr 9)                           ; x0 = code ptr
           (nc-str-offset 0 31 code-slot)
           ;; Get closure env and save
           (nc-ldr-offset 0 31 closure-slot)
           (nc-ldr-offset 9 19 40)              ; closure-env
           (nc-blr 9)                           ; x0 = env
           (nc-str-offset 0 31 env-slot)
           ;; Restore x24 for arg evaluation
           (nc-ldr-offset 24 31 x24-slot)
           ;; Evaluate args
           (gen-args args-ir 0 nil)
           ;; Load args into registers
           (load-args 0 nil)
           ;; Set x24 to callee's env
           (nc-ldr-offset 24 31 env-slot)
           ;; Set argc
           (nc-movz 23 num-args)
           ;; Load code pointer and call
           (nc-ldr-offset 9 31 code-slot)
           (nc-blr 9)
           ;; Restore x24
           (nc-ldr-offset 24 31 x24-slot))))))
    ((nc-has-tag ir 'dotimes-ir)
     ;; dotimes-ir = (dotimes-ir var count-ir body result-form compile-env)
     ;; Generate counted loop:
     ;; 1. Evaluate count, save to slot
     ;; 2. Initialize counter to 0
     ;; 3. Loop: compare counter to count, branch if >=
     ;; 4. Store counter as var, execute body
     ;; 5. Increment counter, branch back
     ;; 6. Evaluate result with final counter value
     (let* ((var (cadr ir))
            (count-ir (caddr ir))
            (body (cadddr ir))
            (result-form (nth 4 ir))
            (compile-env (nth 5 ir))
            ;; Temp slots: 0=count, 1=counter, 2=x24-save
            (count-slot (nc-temp-slot td))
            (counter-slot (nc-temp-slot (+ td 1)))
            (x24-slot (nc-temp-slot (+ td 2)))
            (body-td (+ td 3))
            ;; Compile count expression
            (count-code (nc-codegen count-ir rtaddrs fnoffs body-td))
            ;; Compile body with var at offset 0 in extended env
            (new-env (nc-env-extend (list (list var)) compile-env))
            (body-ir (if (null (cdr body))
                         (nc-compile (car body) new-env nil)
                         (nc-compile (cons 'progn body) new-env nil)))
            (body-code (nc-codegen body-ir rtaddrs fnoffs body-td))
            (body-instrs (nc-count-instrs body-code))
            ;; Result compilation
            (result-ir (if result-form
                           (nc-compile result-form new-env nil)
                           (list 'lit 0)))
            (result-code (nc-codegen result-ir rtaddrs fnoffs body-td)))
       (nc-append-all
        (list
         ;; Save x24
         (nc-str-offset 24 31 x24-slot)
         ;; Evaluate and save count
         count-code
         (nc-str-offset 0 31 count-slot)
         ;; Initialize counter to 0
         (nc-movz 0 0)
         (nc-str-offset 0 31 counter-slot)
         ;; Loop start: load counter and count, compare
         ;; Loop test: 4 instrs (ldr counter, ldr count, cmp, b.ge)
         (nc-ldr-offset 0 31 counter-slot)
         (nc-ldr-offset 1 31 count-slot)
         (nc-cmp-reg 0 1)
         ;; Branch past body + incr + loop-back if counter >= count
         ;; Body instrs + store var (4) + incr (4) + branch back (1) = body-instrs + 9
         (nc-b-cond (nc-cond-ge) (* (+ body-instrs 9) 4))
         ;; Store counter as var (at offset 0 from x20)
         (nc-ldr-offset 0 31 counter-slot)
         (nc-sub-imm 1 20 0)  ; x1 = x20 - 0
         (nc-str-offset 0 1 0)
         ;; Restore x24 for body
         (nc-ldr-offset 24 31 x24-slot)
         ;; Execute body
         body-code
         ;; Increment counter
         (nc-ldr-offset 0 31 counter-slot)
         (nc-add-imm 0 0 #x10)  ; add tagged 1
         (nc-str-offset 0 31 counter-slot)
         ;; Branch back to loop start
         ;; Distance: -(loop test (4) + store var (4) + body + incr (3))
         (nc-b-offset (- (* (+ body-instrs 11) 4)))
         ;; After loop: evaluate result with final counter
         (nc-ldr-offset 0 31 counter-slot)
         (nc-sub-imm 1 20 0)
         (nc-str-offset 0 1 0)
         (nc-ldr-offset 24 31 x24-slot)
         result-code))))
    ((nc-has-tag ir 'dolist-ir)
     ;; dolist-ir = (dolist-ir var list-ir body result-form compile-env)
     ;; Generate list iteration loop:
     ;; 1. Evaluate list, save to slot
     ;; 2. Loop: check if null, branch if yes
     ;; 3. Get car, store as var, execute body
     ;; 4. Get cdr, save, branch back
     ;; 5. Evaluate result
     (let* ((var (cadr ir))
            (list-ir (caddr ir))
            (body (cadddr ir))
            (result-form (nth 4 ir))
            (compile-env (nth 5 ir))
            ;; Temp slots: 0=list-ptr, 1=x24-save
            (list-slot (nc-temp-slot td))
            (x24-slot (nc-temp-slot (+ td 1)))
            (body-td (+ td 2))
            ;; Compile list expression
            (list-code (nc-codegen list-ir rtaddrs fnoffs body-td))
            ;; Compile body with var at offset 0 in extended env
            (new-env (nc-env-extend (list (list var)) compile-env))
            (body-ir (if (null (cdr body))
                         (nc-compile (car body) new-env nil)
                         (nc-compile (cons 'progn body) new-env nil)))
            (body-code (nc-codegen body-ir rtaddrs fnoffs body-td))
            (body-instrs (nc-count-instrs body-code))
            ;; Result compilation
            (result-ir (if result-form
                           (nc-compile result-form new-env nil)
                           (list 'lit 0)))
            (result-code (nc-codegen result-ir rtaddrs fnoffs body-td)))
       (nc-append-all
        (list
         ;; Save x24
         (nc-str-offset 24 31 x24-slot)
         ;; Evaluate and save list
         list-code
         (nc-str-offset 0 31 list-slot)
         ;; Loop start: check if list is nil (tag 0)
         (nc-ldr-offset 0 31 list-slot)
         (nc-movz 1 0)  ; nil = 0
         (nc-cmp-reg 0 1)
         ;; Branch past body if list is nil
         ;; Body: store var (4) + body + get cdr (4) + branch (1) = body-instrs + 9
         (nc-b-cond (nc-cond-eq) (* (+ body-instrs 9) 4))
         ;; Get car of list -> var
         (nc-ldr-offset 0 31 list-slot)
         (nc-ldr-offset 9 19 8)  ; car function at offset 8
         (nc-blr 9)
         (nc-sub-imm 1 20 0)
         (nc-str-offset 0 1 0)
         ;; Restore x24 for body
         (nc-ldr-offset 24 31 x24-slot)
         ;; Execute body
         body-code
         ;; Get cdr, save as new list
         (nc-ldr-offset 0 31 list-slot)
         (nc-ldr-offset 9 19 16)  ; cdr function at offset 16
         (nc-blr 9)
         (nc-str-offset 0 31 list-slot)
         ;; Branch back to loop start
         ;; Distance: -(null check (3) + get car (5) + body + get cdr (4))
         (nc-b-offset (- (* (+ body-instrs 12) 4)))
         ;; After loop: evaluate result (var is nil at this point)
         (nc-movz 0 0)  ; nil
         (nc-sub-imm 1 20 0)
         (nc-str-offset 0 1 0)
         (nc-ldr-offset 24 31 x24-slot)
         result-code))))
    (t (nc-movz 0 0))))

;;; ============================================================
;;; Part 8: Multi-Function Compiler
;;; ============================================================

(defun nc-compile-defun (name params body env fenv)
  (let* ((bs (mapcar (lambda (p) (list p)) params))
         (penv (nc-env-extend bs env))
         (pb (if params (nc-env-lookup (car params) penv) 0))
         (rfenv (cons (cons name nil) fenv))
         (bir (nc-compile body penv rfenv)))
    (list name params bir pb)))

(defun nc-compile-forms-h (forms env fenv)
  (if (consp forms)
      (let ((f (car forms)))
        (if (and (consp f) (eq (car f) 'defun))
            (let* ((nm (cadr f))
                   (ps (caddr f))
                   (bd (cadddr f))
                   (cf (nc-compile-defun nm ps bd env fenv))
                   (nfenv (cons (cons nm cf) fenv))
                   (rr (nc-compile-forms-h (cdr forms) env nfenv)))
              (list (cons cf (car rr)) (cadr rr)))
            (list nil (nc-compile f env fenv))))
      (list nil (list 'lit 0))))

(defun nc-compile-forms (forms)
  (nc-compile-forms-h forms nil nil))

(defun nc-gen-param-stores (params base idx acc)
  (if (null params)
      acc
      (let ((st (append (nc-mov-reg 22 idx)
                        (nc-sub-imm 21 20 (* (+ base idx) 8))
                        (nc-str-offset 22 21 0))))
        (nc-gen-param-stores (cdr params) base (+ idx 1) (append acc st)))))

(defun nc-codegen-fn (fn rtaddrs fnoffs)
  (let* ((ps (cadr fn))
         (bir (caddr fn))
         (pb (cadddr fn))
         (pc (nc-gen-param-stores ps pb 0 nil))
         (bc (nc-codegen bir rtaddrs fnoffs 0)))
    (append pc bc (nc-ret))))

(defun nc-codegen-main (mir rtaddrs)
  (append (nc-prologue)
          (nc-codegen mir rtaddrs nil 0)
          (nc-epilogue)))

(defparameter *lambda-counter* 0)

(defun nc-gensym-lambda ()
  "Generate unique lambda name"
  (incf *lambda-counter*)
  (intern (format nil "LAMBDA-~A" *lambda-counter*)))

(defun nc-lift-lambdas (ir)
  "Extract all lambda-ir nodes from IR, replacing them with lambda-ref nodes.
   Returns (values transformed-ir lambdas) where lambdas is alist of (name . lambda-ir)"
  (labels ((lift (ir lambdas)
             (cond
               ((null ir) (values ir lambdas))
               ((not (consp ir)) (values ir lambdas))
               ((nc-has-tag ir 'lambda-ir)
                ;; Found a lambda - give it a name, store it, return reference
                (let* ((name (nc-gensym-lambda))
                       (params (cadr ir))
                       (body (caddr ir))
                       (free-vars (cadddr ir))
                       (free-offsets (nth 4 ir)))
                  ;; Recursively lift lambdas from the body
                  (multiple-value-bind (new-body more-lambdas)
                      (lift body lambdas)
                    (let ((lambda-entry (list name params new-body free-vars free-offsets)))
                      (values (list 'lambda-ref name free-offsets)
                              (cons lambda-entry more-lambdas))))))
               ((nc-has-tag ir 'let-ir)
                ;; let-ir = (let-ir vals bir count offs)
                (let ((vals (cadr ir))
                      (bir (caddr ir))
                      (count (cadddr ir))
                      (offs (nth 4 ir)))
                  (multiple-value-bind (new-vals lambdas1)
                      (lift-list vals lambdas)
                    (multiple-value-bind (new-bir lambdas2)
                        (lift bir lambdas1)
                      (values (list 'let-ir new-vals new-bir count offs) lambdas2)))))
               ((nc-has-tag ir 'if-ir)
                (let ((test (cadr ir))
                      (then (caddr ir))
                      (else (cadddr ir)))
                  (multiple-value-bind (new-test l1) (lift test lambdas)
                    (multiple-value-bind (new-then l2) (lift then l1)
                      (multiple-value-bind (new-else l3) (lift else l2)
                        (values (list 'if-ir new-test new-then new-else) l3))))))
               ((nc-has-tag ir 'progn-ir)
                (multiple-value-bind (new-forms new-lambdas)
                    (lift-list (cadr ir) lambdas)
                  (values (list 'progn-ir new-forms) new-lambdas)))
               ((nc-has-tag ir 'funcall-ir)
                (let ((fn-ir (cadr ir))
                      (args-ir (caddr ir)))
                  (multiple-value-bind (new-fn l1) (lift fn-ir lambdas)
                    (multiple-value-bind (new-args l2) (lift-list args-ir l1)
                      (values (list 'funcall-ir new-fn new-args) l2)))))
               ((nc-has-tag ir 'call-fn)
                (let ((name (cadr ir))
                      (args-ir (caddr ir)))
                  (multiple-value-bind (new-args new-lambdas)
                      (lift-list args-ir lambdas)
                    (values (list 'call-fn name new-args) new-lambdas))))
               ((or (nc-has-tag ir 'add) (nc-has-tag ir 'sub)
                    (nc-has-tag ir 'mul) (nc-has-tag ir 'div)
                    (nc-has-tag ir 'mod) (nc-has-tag ir 'cmp-eq)
                    (nc-has-tag ir 'cmp-lt) (nc-has-tag ir 'cmp-gt)
                    (nc-has-tag ir 'cmp-le) (nc-has-tag ir 'cmp-ge)
                    (nc-has-tag ir 'cons-ir))
                (let ((left (cadr ir))
                      (right (caddr ir)))
                  (multiple-value-bind (new-left l1) (lift left lambdas)
                    (multiple-value-bind (new-right l2) (lift right l1)
                      (values (list (car ir) new-left new-right) l2)))))
               ((or (nc-has-tag ir 'car-ir) (nc-has-tag ir 'cdr-ir))
                (multiple-value-bind (new-arg new-lambdas)
                    (lift (cadr ir) lambdas)
                  (values (list (car ir) new-arg) new-lambdas)))
               (t (values ir lambdas))))
           (lift-list (irs lambdas)
             (if (null irs)
                 (values nil lambdas)
                 (multiple-value-bind (new-first l1) (lift (car irs) lambdas)
                   (multiple-value-bind (new-rest l2) (lift-list (cdr irs) l1)
                     (values (cons new-first new-rest) l2))))))
    (lift ir nil)))

(defun nc-codegen-lambda (lambda-entry rtaddrs fnoffs)
  "Generate code for a lifted lambda.
   lambda-entry = (name params body free-vars free-offsets)"
  (let* ((params (cadr lambda-entry))
         (body (caddr lambda-entry))
         ;; Lambda params start at offset 0
         (pb 0)
         (pc (nc-gen-param-stores params pb 0 nil))
         (bc (nc-codegen body rtaddrs fnoffs 0)))
    (append pc bc (nc-ret))))

(defun nc-compile-program (forms rtaddrs)
  (let* ((r (nc-compile-forms forms))
         (fns (car r))
         (mir (cadr r))
         (mc (nc-codegen-main mir rtaddrs)))
    mc))

;;; ============================================================
;;; Part 9: Entry Point
;;; ============================================================

(defun nc-eval-forms (forms)
  "Compile and evaluate multiple forms, including defun.
   Uses two-pass approach to support mutual recursion:
   1. First pass: collect all defun names into fenv with placeholders
   2. Second pass: compile bodies with complete fenv, then evaluate non-defun forms"
  ;; Pass 1: Collect all defun names
  (labels ((collect-defuns (fs acc)
             (if (null fs)
                 (reverse acc)
                 (let ((f (car fs)))
                   (if (and (consp f) (eq (car f) 'defun))
                       (collect-defuns (cdr fs) (cons (cadr f) acc))
                       (collect-defuns (cdr fs) acc)))))
           ;; Build initial fenv with placeholders
           (build-fenv (names acc)
             (if (null names)
                 acc
                 (build-fenv (cdr names) (cons (cons (car names) nil) acc))))
           ;; Compile all defuns with complete fenv
           (compile-defuns (fs fenv acc)
             (if (null fs)
                 (values fenv (reverse acc))
                 (let ((f (car fs)))
                   (if (and (consp f) (eq (car f) 'defun))
                       (let* ((nm (cadr f))
                              (ps (caddr f))
                              (bd (cadddr f))
                              (cf (nc-compile-defun nm ps bd nil fenv))
                              (entry (assoc nm fenv)))
                         ;; Update existing entry with compiled function
                         (setf (cdr entry) cf)
                         (compile-defuns (cdr fs) fenv acc))
                       ;; Non-defun form - save for later evaluation
                       (compile-defuns (cdr fs) fenv (cons f acc))))))
           ;; Evaluate non-defun forms
           (eval-forms (fs fenv)
             (if (null fs)
                 0
                 (let* ((ir (nc-compile (car fs) nil fenv))
                        (result (nc-eval-ir-with-fns ir nil fenv)))
                   (if (null (cdr fs))
                       result
                       (eval-forms (cdr fs) fenv))))))
    ;; Execute two-pass compilation
    (let* ((defun-names (collect-defuns forms nil))
           (initial-fenv (build-fenv defun-names nil)))
      (multiple-value-bind (final-fenv other-forms)
          (compile-defuns forms initial-fenv nil)
        (eval-forms other-forms final-fenv)))))

(defun main ()
  ;; Full pipeline: parse -> compile to IR -> evaluate IR
  (let* ((src "(+ (* 3 4) 5)")
         (forms (nc-read-all src)))
    (if (consp forms)
        (nc-eval-forms forms)
        0)))

(main)
