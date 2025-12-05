;;;; test-arm64-props.lisp - ARM64 property tests
;;;;
;;;; Property-based tests for ARM64 instruction encoding.

(in-package :habu)

;;; ============================================================
;;; ARM64-Specific Generators
;;; ============================================================

(defvar *register-keywords*
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7 :x8 :x9 :x10 :x11 :x12 :x13 :x14
    :x15 :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23 :x24 :x25 :x26 :x27
    :x28 :x29 :x30))

(defun gen-register ()
  "Generator for ARM64 registers :x0 to :x30, shrinks toward :x0."
  (make-gen
   (lambda () (nth (random 31) *register-keywords*))
   (lambda (reg)
     (let ((pos (position reg *register-keywords*)))
       (cond ((null pos) nil)
             ((= pos 0) nil)  ; Already :x0
             ((< pos 4) (list :x0))
             (t (list :x0 (nth (truncate pos 2) *register-keywords*))))))))

(defun gen-register-or-sp ()
  "Generator for ARM64 registers :x0 to :x30 or :sp, shrinks toward :x0."
  (make-gen
   (lambda ()
     (let ((n (random 32)))
       (if (= n 31) :sp (nth n *register-keywords*))))
   (lambda (reg)
     (if (eq reg :sp)
         (list :x0)
         (shrink-value (gen-register) reg)))))

(defun gen-imm12 ()
  "Generator for 12-bit immediates (0-4095), shrinks toward 0."
  (gen-int 0 4095))

(defun gen-imm16 ()
  "Generator for 16-bit immediates (0-65535), shrinks toward 0."
  (gen-int 0 65535))

(defun gen-branch-offset ()
  "Generator for branch offsets (instruction count), shrinks toward 0."
  (gen-int -8388608 8388607))

(defun gen-mem-offset ()
  "Generator for memory offsets (multiples of 8), shrinks toward 0."
  (make-gen
   (lambda () (* 8 (- (random 64) 32)))
   (lambda (off)
     (cond ((= off 0) nil)
           ((> off 0) (list 0 (* 8 (truncate (/ off 8) 2))))
           (t (list 0 (* 8 (truncate (/ off 8) 2))))))))

(defun gen-cond-code ()
  "Generator for condition codes, shrinks toward :eq."
  (gen-one-of '(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al :nv)))

;;; ============================================================
;;; ARM64 Instruction Decoders (for roundtrip testing)
;;; ============================================================

(defun bytes-to-word (bytes)
  "Convert 4 bytes (little-endian) to 32-bit word."
  (logior (nth 0 bytes)
          (ash (nth 1 bytes) 8)
          (ash (nth 2 bytes) 16)
          (ash (nth 3 bytes) 24)))

(defun decode-reg (bits)
  "Decode 5-bit register field to keyword."
  (if (= bits 31)
      :sp
      (nth bits *register-keywords*)))

(defun decode-add-sub-imm (bytes)
  "Decode ADD/SUB immediate. Returns (op rd rn imm) or nil."
  (when (= (length bytes) 4)
    (let* ((word (bytes-to-word bytes))
           (rd (logand word #x1F))
           (rn (logand (ash word -5) #x1F))
           (imm12 (logand (ash word -10) #xFFF))
           (shift (logand (ash word -22) #x1))
           (op-bit (logand (ash word -30) #x1))
           (sf (logand (ash word -31) #x1)))
      (when (and (= sf 1)
                 (= (logand (ash word -23) #x7F) #x22))
        (list (if (= op-bit 0) :add :sub)
              (decode-reg rd)
              (decode-reg rn)
              (if (= shift 1) (ash imm12 12) imm12))))))

(defun decode-movz (bytes)
  "Decode MOVZ. Returns (rd imm16 shift) or nil."
  (when (= (length bytes) 4)
    (let* ((word (bytes-to-word bytes))
           (rd (logand word #x1F))
           (imm16 (logand (ash word -5) #xFFFF))
           (hw (logand (ash word -21) #x3))
           (opc (logand (ash word -29) #x3))  ; 2 bits for opc
           (fixed (logand (ash word -23) #x3F))  ; should be 100101 = 37
           (sf (logand (ash word -31) #x1)))
      (when (and (= sf 1) (= opc 2) (= fixed 37))  ; opc=10 for MOVZ
        (list (decode-reg rd) imm16 (* hw 16))))))

(defun decode-logical-reg (bytes)
  "Decode ORR/AND/EOR register. Returns (op rd rn rm) or nil."
  (when (= (length bytes) 4)
    (let* ((word (bytes-to-word bytes))
           (rd (logand word #x1F))
           (rn (logand (ash word -5) #x1F))
           (imm6 (logand (ash word -10) #x3F))
           (rm (logand (ash word -16) #x1F))
           (n-bit (logand (ash word -21) #x1))
           (shift-type (logand (ash word -22) #x3))
           (opc (logand (ash word -29) #x3))
           (sf (logand (ash word -31) #x1)))
      (when (and (= sf 1) (= imm6 0) (= shift-type 0) (= n-bit 0))
        (list (case opc (0 :and) (1 :orr) (2 :eor) (3 :ands))
              (decode-reg rd)
              (decode-reg rn)
              (decode-reg rm))))))

;;; ============================================================
;;; ARM64 Encoding Properties - Size Invariants
;;; ============================================================

(defproperty prop-add-imm-size ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (= 4 (length (arm64:add rd rn imm :imm t))))

(defproperty prop-sub-imm-size ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (= 4 (length (arm64:sub rd rn imm :imm t))))

(defproperty prop-movz-size ((rd (gen-register)) (imm (gen-imm16)))
  (= 4 (length (arm64:movz rd imm))))

(defproperty prop-movk-size ((rd (gen-register)) (imm (gen-imm16)))
  (= 4 (length (arm64:movk rd imm))))

(defproperty prop-orr-reg-size ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (= 4 (length (arm64:orr rd rn rm))))

(defproperty prop-and-reg-size ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (= 4 (length (arm64:and* rd rn rm))))

(defproperty prop-eor-reg-size ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (= 4 (length (arm64:eor rd rn rm))))

(defproperty prop-ldr-size ((rt (gen-register)) (rn (gen-register-or-sp)))
  (= 4 (length (arm64:ldr rt rn :offset 0))))

(defproperty prop-str-size ((rt (gen-register)) (rn (gen-register-or-sp)))
  (= 4 (length (arm64:str rt rn :offset 0))))

(defproperty prop-bl-size ((offset (gen-branch-offset)))
  (= 4 (length (arm64:bl offset))))

(defproperty prop-b-size ((offset (gen-branch-offset)))
  (= 4 (length (arm64:b offset))))

(defproperty prop-ret-size ((dummy (gen-int 0 0)))
  (declare (ignore dummy))
  (= 4 (length (arm64:ret))))

;;; ============================================================
;;; ARM64 Encoding Properties - Roundtrips
;;; ============================================================

(defproperty prop-add-imm-roundtrip ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (let* ((encoded (arm64:add rd rn imm :imm t))
         (decoded (decode-add-sub-imm encoded)))
    (and decoded
         (eq (first decoded) :add)
         (eq (second decoded) rd)
         (eq (third decoded) rn)
         (= (fourth decoded) imm))))

(defproperty prop-sub-imm-roundtrip ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (let* ((encoded (arm64:sub rd rn imm :imm t))
         (decoded (decode-add-sub-imm encoded)))
    (and decoded
         (eq (first decoded) :sub)
         (eq (second decoded) rd)
         (eq (third decoded) rn)
         (= (fourth decoded) imm))))

(defproperty prop-movz-roundtrip ((rd (gen-register)) (imm (gen-imm16)))
  (let* ((encoded (arm64:movz rd imm))
         (decoded (decode-movz encoded)))
    (and decoded
         (eq (first decoded) rd)
         (= (second decoded) imm)
         (= (third decoded) 0))))

(defproperty prop-orr-reg-roundtrip ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (let* ((encoded (arm64:orr rd rn rm))
         (decoded (decode-logical-reg encoded)))
    (and decoded
         (eq (first decoded) :orr)
         (eq (second decoded) rd)
         (eq (third decoded) rn)
         (eq (fourth decoded) rm))))

;;; ============================================================
;;; Tagged Value Properties
;;; ============================================================

(defproperty prop-fixnum-tag-roundtrip ((n (gen-int -1000000 1000000)))
  (let ((tagged (ash n 4)))
    (= (ash tagged -4) n)))

(defproperty prop-fixnum-tag-is-zero ((n (gen-int -1000000 1000000)))
  (let ((tagged (ash n 4)))
    (= (logand tagged #xF) 0)))

(defproperty prop-large-fixnum-roundtrip ((n (gen-int -2305843009213693952 2305843009213693951)))
  (let ((tagged (ash n 4)))
    (= (ash tagged -4) n)))

;;; ============================================================
;;; Code Size Properties
;;; ============================================================

(defproperty prop-instruction-size-nop ((dummy (gen-int 0 0)))
  (declare (ignore dummy))
  (= 4 (length (arm64:nop))))

(defproperty prop-instruction-size-add ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (= 4 (length (arm64:add rd rn imm :imm t))))

(defproperty prop-flatten-preserves-bytes ((n (gen-int 1 10)))
  (let* ((instrs (loop repeat n collect (arm64:nop)))
         (flat (flatten-code-keep-markers-and-calls instrs)))
    ;; Each NOP is 4 bytes, so n NOPs should be 4*n bytes
    (= (length flat) (* n 4))))

(defproperty prop-flatten-nested ((n (gen-int 1 5)))
  (let* ((inner (loop repeat n collect (arm64:ret)))
         (outer (list inner inner))
         (flat (flatten-code-keep-markers-and-calls outer)))
    ;; 2 groups of n RETs, each 4 bytes
    (= (length flat) (* 2 n 4))))

;;; ============================================================
;;; Decoder Unit Tests
;;; ============================================================

(defun test-bytes-to-word ()
  "Test bytes-to-word conversion."
  (if (and (= (bytes-to-word '(#x01 #x02 #x03 #x04)) #x04030201)
           (= (bytes-to-word '(#xFF #x00 #x00 #x00)) #x000000FF)
           (= (bytes-to-word '(#x00 #xFF #x00 #x00)) #x0000FF00))
      (progn (format t "  [PASS] bytes-to-word works~%") t)
      (progn (format t "  [FAIL] bytes-to-word incorrect~%") nil)))

(defun test-decode-reg ()
  "Test register decoding."
  (if (and (eq (decode-reg 0) :x0)
           (eq (decode-reg 15) :x15)
           (eq (decode-reg 30) :x30)
           (eq (decode-reg 31) :sp))
      (progn (format t "  [PASS] decode-reg works~%") t)
      (progn (format t "  [FAIL] decode-reg incorrect~%") nil)))

(defun test-gen-register ()
  "Test that gen-register produces valid register keywords."
  (let ((g (gen-register))
        (all-valid t))
    (dotimes (i 100)
      (let ((v (gen-value g)))
        (unless (member v *register-keywords*)
          (setf all-valid nil))))
    (if all-valid
        (progn (format t "  [PASS] gen-register produces valid registers~%") t)
        (progn (format t "  [FAIL] gen-register produced invalid register~%") nil))))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-arm64-prop-tests (&optional (trials *quickcheck-trials*))
  "Run all ARM64 property tests."
  (format t "~%=== ARM64 Property Tests (~D trials each) ===~%~%" trials)
  (reset-property-stats)

  (let ((unit-pass 0) (unit-fail 0))
    ;; Unit tests
    (format t "Decoder unit tests:~%")
    (if (test-bytes-to-word) (incf unit-pass) (incf unit-fail))
    (if (test-decode-reg) (incf unit-pass) (incf unit-fail))
    (if (test-gen-register) (incf unit-pass) (incf unit-fail))

    ;; Size properties
    (format t "~%ARM64 instruction size invariants:~%")
    (run-property 'prop-add-imm-size trials)
    (run-property 'prop-sub-imm-size trials)
    (run-property 'prop-movz-size trials)
    (run-property 'prop-movk-size trials)
    (run-property 'prop-orr-reg-size trials)
    (run-property 'prop-and-reg-size trials)
    (run-property 'prop-eor-reg-size trials)
    (run-property 'prop-ldr-size trials)
    (run-property 'prop-str-size trials)
    (run-property 'prop-bl-size trials)
    (run-property 'prop-b-size trials)
    (run-property 'prop-ret-size trials)

    ;; Roundtrip properties
    (format t "~%ARM64 encoding roundtrips:~%")
    (run-property 'prop-add-imm-roundtrip trials)
    (run-property 'prop-sub-imm-roundtrip trials)
    (run-property 'prop-movz-roundtrip trials)
    (run-property 'prop-orr-reg-roundtrip trials)

    ;; Tagged value properties
    (format t "~%Tagged value invariants:~%")
    (run-property 'prop-fixnum-tag-roundtrip trials)
    (run-property 'prop-fixnum-tag-is-zero trials)
    (run-property 'prop-large-fixnum-roundtrip trials)

    ;; Code generation properties
    (format t "~%Code generation:~%")
    (run-property 'prop-instruction-size-nop trials)
    (run-property 'prop-instruction-size-add trials)
    (run-property 'prop-flatten-preserves-bytes trials)
    (run-property 'prop-flatten-nested trials)

    ;; Summary
    (format t "~%ARM64 Tests: ~D unit + ~D property = ~D passed, ~D failed~%"
            unit-pass *property-pass-count*
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))

    (values (and (= unit-fail 0) (= *property-fail-count* 0))
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))))
