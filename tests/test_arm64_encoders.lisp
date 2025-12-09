;; Test compiling real ARM64 encoder functions to native executables
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

;; Use test-specific package to avoid polluting :habu namespace
(defpackage :habu-test-arm64-encoders
  (:use :cl))
(in-package :habu-test-arm64-encoders)

(format t "~%=== Test compiling ARM64 encoder functions ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/arm_~A" name)))
    (handler-case
        (progn
          (habu:deliver source path)
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" path)
                              :output nil :error nil :wait t)
          (let* ((proc (sb-ext:run-program path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn
                  (format t "[PASS] ~A = ~A~%" name code)
                  (incf *tests-passed*))
                (progn
                  (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code)
                  (incf *tests-failed*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *tests-failed*)))))

;; Test 1: nc-encode-word - bytes from 32-bit word
(test-native "encode-word"
  "(defun encode-word (word)
     (let* ((b0 (logand word #xFF))
            (s1 (ash word -8))
            (b1 (logand s1 #xFF))
            (s2 (ash word -16))
            (b2 (logand s2 #xFF))
            (s3 (ash word -24))
            (b3 (logand s3 #xFF)))
       (list b0 b1 b2 b3)))
   (car (encode-word #xD65F03C0))"  ; RET instruction
  #xC0)

;; Test 2: nc-movz - move immediate
(test-native "movz-enc"
  "(defun encode-word (word)
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
   ;; MOVZ x0, #42 = #xD2800540
   (car (movz 0 42))"
  #x40)

;; Test 3: nc-add-reg - add two registers
(test-native "add-reg"
  "(defun encode-word (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)
           (logand (ash word -16) #xFF)
           (logand (ash word -24) #xFF)))
   (defun add-reg (rd rn rm)
     (let* ((rm-shift (ash rm 16))
            (rn-shift (ash rn 5))
            (or1 (logior #x8B000000 rm-shift))
            (or2 (logior or1 rn-shift))
            (word (logior or2 rd)))
       (encode-word word)))
   ;; ADD x0, x1, x2 = #x8B020020
   (car (add-reg 0 1 2))"
  #x20)

;; Test 4: nc-ldr-offset - load from memory
(test-native "ldr-offset"
  "(defun encode-word (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)
           (logand (ash word -16) #xFF)
           (logand (ash word -24) #xFF)))
   (defun ldr-offset (rt rn offset)
     (let* ((off-s (ash offset -3))
            (off-ss (ash off-s 10))
            (rn-s (ash rn 5))
            (or1 (logior #xF9400000 off-ss))
            (or2 (logior or1 rn-s))
            (word (logior or2 rt)))
       (encode-word word)))
   ;; LDR x0, [x20, #8] - simpler case
   (car (ldr-offset 0 20 8))"
  #x80)  ; First byte: (off/8)<<10 | (20<<5) | 0 = #x00000280, byte0=#x80

;; Test 5: nc-str-offset - store to memory
(test-native "str-offset"
  "(defun encode-word (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)
           (logand (ash word -16) #xFF)
           (logand (ash word -24) #xFF)))
   (defun str-offset (rt rn offset)
     (let* ((off-s (ash offset -3))
            (off-ss (ash off-s 10))
            (rn-s (ash rn 5))
            (or1 (logior #xF9000000 off-ss))
            (or2 (logior or1 rn-s))
            (word (logior or2 rt)))
       (encode-word word)))
   (cadddr (str-offset 0 31 16))"
  #xF9)  ; Fourth byte (high byte of base)

;; Test 6: Combined encoding - length of generated code
(test-native "multi-instr"
  "(defun encode-word (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)))
   (defun movz (rd imm)
     (let* ((word (logior #xD2800000 (ash imm 5) rd)))
       (encode-word word)))
   (defun ret ()
     (encode-word #xD65F03C0))
   (let* ((mov-bytes (movz 0 42))
          (ret-bytes (ret)))
     (+ (length mov-bytes) (length ret-bytes)))"
  4)  ; 2 bytes + 2 bytes = 4

;; Test 7: Branch encoding
(test-native "branch-imm"
  "(defun encode-word (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)
           (logand (ash word -16) #xFF)
           (logand (ash word -24) #xFF)))
   (defun b-imm (offset)
     (let* ((imm26 (logand (ash offset -2) #x3FFFFFF))
            (word (logior #x14000000 imm26)))
       (encode-word word)))
   ;; B +8 (2 instructions forward)
   (car (b-imm 8))"
  #x02)  ; Low byte of 14000002

;; Test 8: Compare and branch
(test-native "cmp-br"
  "(defun encode-word (word)
     (list (logand word #xFF)
           (logand (ash word -8) #xFF)
           (logand (ash word -16) #xFF)
           (logand (ash word -24) #xFF)))
   (defun cbz (rt offset)
     (let* ((imm19 (logand (ash offset -2) #x7FFFF))
            (imm-s (ash imm19 5))
            (word (logior #xB4000000 imm-s rt)))
       (encode-word word)))
   ;; CBZ x0, +16
   (cadddr (cbz 0 16))"
  #xB4)  ; High byte

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
