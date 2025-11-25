;;;; ARM64 Instruction Encoders
;;;; Pure Habu - no SBCL dependencies

;;; Helper to encode 32-bit word as little-endian byte list
(defun encode-word-le (word)
  (list (logand word #xFF)
        (logand (ash word -8) #xFF)
        (logand (ash word -16) #xFF)
        (logand (ash word -24) #xFF)))

;;; MOVZ - move wide with zero
(defun movz (rd imm)
  (let ((instr (logior #xD2800000
                       (ash (logand imm #xFFFF) 5)
                       rd)))
    (encode-word-le instr)))

;;; ADD (shifted register)
(defun add-reg (rd rn rm)
  (let ((instr (logior #x8B000000
                       (ash rm 16)
                       (ash rn 5)
                       rd)))
    (encode-word-le instr)))

;;; SUB (shifted register)
(defun sub-reg (rd rn rm)
  (let ((instr (logior #xCB000000
                       (ash rm 16)
                       (ash rn 5)
                       rd)))
    (encode-word-le instr)))

;;; MUL
(defun mul-reg (rd rn rm)
  (let ((instr (logior #x9B007C00
                       (ash rm 16)
                       (ash rn 5)
                       rd)))
    (encode-word-le instr)))

;;; SDIV (signed divide)
(defun sdiv-reg (rd rn rm)
  (let ((instr (logior #x9AC00C00
                       (ash rm 16)
                       (ash rn 5)
                       rd)))
    (encode-word-le instr)))

;;; LSL (logical shift left) - immediate
(defun lsl-imm (rd rn shift)
  (let* ((immr (logand (- #x40 shift) #x3F))
         (imms (- #x3F shift))
         (instr (logior #xD3400000
                        (ash immr 16)
                        (ash imms 10)
                        (ash rn 5)
                        rd)))
    (encode-word-le instr)))

;;; LSR (logical shift right) - immediate
(defun lsr-imm (rd rn shift)
  (let* ((immr shift)
         (imms #x3F)
         (instr (logior #xD340FC00
                        (ash immr 16)
                        (ash rn 5)
                        rd)))
    (encode-word-le instr)))

;;; MOV (register) - alias for ORR with xzr
(defun mov-reg (rd rm)
  (let ((instr (logior #xAA0003E0
                       (ash rm 16)
                       rd)))
    (encode-word-le instr)))

;;; AND (shifted register)
(defun and-reg (rd rn rm)
  (let ((instr (logior #x8A000000
                       (ash rm 16)
                       (ash rn 5)
                       rd)))
    (encode-word-le instr)))

;;; ORR (shifted register)
(defun orr-reg (rd rn rm)
  (let ((instr (logior #xAA000000
                       (ash rm 16)
                       (ash rn 5)
                       rd)))
    (encode-word-le instr)))

;;; LDR (unsigned offset) - 64-bit
(defun ldr-offset (rt rn offset)
  (let* ((imm12 (ash offset -3))
         (instr (logior #xF9400000
                        (ash imm12 10)
                        (ash rn 5)
                        rt)))
    (encode-word-le instr)))

;;; STR (unsigned offset) - 64-bit
(defun str-offset (rt rn offset)
  (let* ((imm12 (ash offset -3))
         (instr (logior #xF9000000
                        (ash imm12 10)
                        (ash rn 5)
                        rt)))
    (encode-word-le instr)))

;;; ADD (immediate)
(defun add-imm (rd rn imm)
  (let* ((rn-enc (if (= rn 31) 31 rn))
         (rd-enc (if (= rd 31) 31 rd))
         (instr (logior #x91000000
                        (ash (logand imm #xFFF) 10)
                        (ash rn-enc 5)
                        rd-enc)))
    (encode-word-le instr)))

;;; SUB (immediate)
(defun sub-imm (rd rn imm)
  (let* ((rn-enc (if (= rn 31) 31 rn))
         (rd-enc (if (= rd 31) 31 rd))
         (instr (logior #xD1000000
                        (ash (logand imm #xFFF) 10)
                        (ash rn-enc 5)
                        rd-enc)))
    (encode-word-le instr)))

;;; STP (store pair) - signed offset
(defun stp-offset (rt1 rt2 rn imm)
  (let* ((imm7 (logand (ash imm -3) #x7F))
         (instr (logior #xA9000000
                        (ash imm7 15)
                        (ash rt2 10)
                        (ash rn 5)
                        rt1)))
    (encode-word-le instr)))

;;; LDP (load pair) - signed offset
(defun ldp-offset (rt1 rt2 rn imm)
  (let* ((imm7 (logand (ash imm -3) #x7F))
         (instr (logior #xA9400000
                        (ash imm7 15)
                        (ash rt2 10)
                        (ash rn 5)
                        rt1)))
    (encode-word-le instr)))

;;; CMP (shifted register) - alias for SUBS with xzr dest
(defun cmp-reg (rn rm)
  (let ((instr (logior #xEB00001F
                       (ash rm 16)
                       (ash rn 5))))
    (encode-word-le instr)))

;;; CSET - conditional set
(defun cset (rd cond-code)
  (let* ((inv-cond (logxor cond-code 1))
         (instr (logior #x9A9F07E0
                        (ash inv-cond 12)
                        rd)))
    (encode-word-le instr)))

;;; B - unconditional branch
(defun b-offset (offset)
  (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
         (instr (logior #x14000000 imm26)))
    (encode-word-le instr)))

;;; BL - branch with link
(defun bl-offset (offset)
  (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
         (instr (logior #x94000000 imm26)))
    (encode-word-le instr)))

;;; B.cond - conditional branch
(defun b-cond (cond-code offset)
  (let* ((imm19 (logand (ash offset -2) #x7FFFF))
         (instr (logior #x54000000
                        (ash imm19 5)
                        cond-code)))
    (encode-word-le instr)))

;;; RET - return from subroutine
(defun ret ()
  (encode-word-le #xD65F03C0))

;;; MOVK - move wide with keep
(defun movk (rd imm shift)
  (let* ((hw (ash shift -4))
         (instr (logior #xF2800000
                        (ash hw 21)
                        (ash (logand imm #xFFFF) 5)
                        rd)))
    (encode-word-le instr)))

;;; BLR - branch with link to register
(defun blr (rn)
  (let ((instr (logior #xD63F0000
                       (ash rn 5))))
    (encode-word-le instr)))

;;; STR (pre-index)
(defun str-pre (rt rn offset)
  (let* ((simm9 (logand offset #x1FF))
         (instr (logior #xF8000C00
                        (ash simm9 12)
                        (ash rn 5)
                        rt)))
    (encode-word-le instr)))

;;; LDR (post-index)
(defun ldr-post (rt rn offset)
  (let* ((simm9 (logand offset #x1FF))
         (instr (logior #xF8400400
                        (ash simm9 12)
                        (ash rn 5)
                        rt)))
    (encode-word-le instr)))

;;; PUSH - pseudo-instruction using STR pre-index
(defun push-reg (rt)
  (str-pre rt 31 -8))

;;; POP - pseudo-instruction using LDR post-index
(defun pop-reg (rt)
  (ldr-post rt 31 8))

;;; Load 64-bit address into register (up to 4 instructions)
(defun load-addr (rd addr)
  (let ((lo16 (logand addr #xFFFF))
        (hi16 (logand (ash addr -16) #xFFFF))
        (hi32 (logand (ash addr -32) #xFFFF))
        (hi48 (logand (ash addr -48) #xFFFF)))
    (append (movz rd lo16)
            (if (> hi16 0) (movk rd hi16 16) nil)
            (if (> hi32 0) (movk rd hi32 32) nil)
            (if (> hi48 0) (movk rd hi48 48) nil))))

;;; Condition codes
(defun cond-eq () 0)   ; equal
(defun cond-ne () 1)   ; not equal
(defun cond-lt () 11)  ; signed less than (b.lt)
(defun cond-le () 13)  ; signed less than or equal (b.le)
(defun cond-gt () 12)  ; signed greater than (b.gt)
(defun cond-ge () 10)  ; signed greater than or equal (b.ge)
