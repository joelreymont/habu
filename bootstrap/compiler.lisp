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
;;; Part 6: IR Compiler (nc-compile-*)
;;; ============================================================

(defun nc-quote-ir (obj)
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'lit 0))
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
     ;; fn-ir evaluates to a function name (symbol)
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir))
            (fn-val (nc-eval-ir-with-fns fn-ir env fenv)))
       ;; fn-val should be a function name (symbol) - look it up in fenv
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
             0))))
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
     (let* ((vals (cadr ir))
            (bir (caddr ir))
            (offs (nth 4 (cdr ir)))
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
