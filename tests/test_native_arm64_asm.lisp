;; Test ARM64 assembler functions in native code
;; Verifies instruction encoding works correctly when compiled to native executable
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test native ARM64 assembler ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

;; Core ARM64 encoder functions inlined for native testing
(defparameter *asm-core* "
(defun arm64-encode (word)
  (list (logand word #xFF)
        (logand (ash word -8) #xFF)
        (logand (ash word -16) #xFF)
        (logand (ash word -24) #xFF)))

(defun arm64-movz (rd imm lsl)
  (arm64-encode (logior #xD2800000
                        (ash (ash lsl -4) 21)
                        (ash (logand imm #xFFFF) 5)
                        rd)))

(defun arm64-add-reg (rd rn rm)
  (arm64-encode (logior #x8B000000
                        (ash rm 16)
                        (ash rn 5)
                        rd)))

(defun arm64-sub-imm (rd rn imm)
  (arm64-encode (logior #xD1000000
                        (ash (logand imm #xFFF) 10)
                        (ash rn 5)
                        rd)))

(defun arm64-ret ()
  (arm64-encode #xD65F03C0))

(defun arm64-b (offset)
  (arm64-encode (logior #x14000000
                        (logand offset #x03FFFFFF))))

(defun arm64-cmp-imm (rn imm)
  (arm64-encode (logior #xF100001F
                        (ash (logand imm #xFFF) 10)
                        (ash rn 5))))

(defun arm64-ldr (rt rn offset)
  (arm64-encode (logior #xF9400000
                        (ash (ash offset -3) 10)
                        (ash rn 5)
                        rt)))
")

(defun test-native (name source expected)
  (let ((path (format nil "/tmp/nasm_~A" name)))
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

;; Test 1: Basic encode function - returns 4-element list
(test-native "encode-len"
  (concatenate 'string *asm-core*
    "(length (arm64-encode #x12345678))")
  4)

;; Test 2: Encode first byte (little-endian)
(test-native "encode-byte0"
  (concatenate 'string *asm-core*
    "(car (arm64-encode #x12345678))")
  #x78)

;; Test 3: Encode second byte
(test-native "encode-byte1"
  (concatenate 'string *asm-core*
    "(cadr (arm64-encode #x12345678))")
  #x56)

;; Test 4: MOVZ x0, #42 encoding
(test-native "movz-encoding"
  (concatenate 'string *asm-core*
    "(let ((bytes (arm64-movz 0 42 0)))
       (car bytes))")
  #x40)  ; x0 in bits 0-4, imm 42 shifted

;; Test 5: ADD register encoding
(test-native "add-reg"
  (concatenate 'string *asm-core*
    "(let ((bytes (arm64-add-reg 0 1 2)))
       (+ (car bytes) (cadr bytes)))")
  #x20)  ; rd=0, rn=1, rm=2

;; Test 6: SUB immediate encoding
(test-native "sub-imm"
  (concatenate 'string *asm-core*
    "(let ((bytes (arm64-sub-imm 0 1 16)))
       (car bytes))")
  #x20)  ; rd=0, rn=1

;; Test 7: RET encoding - should be D65F03C0
(test-native "ret-encoding"
  (concatenate 'string *asm-core*
    "(let ((bytes (arm64-ret)))
       (car bytes))")
  #xC0)

;; Test 8: Branch encoding
(test-native "branch-offset"
  (concatenate 'string *asm-core*
    "(let ((bytes (arm64-b 5)))
       (car bytes))")
  #x05)

;; Test 9: CMP immediate
(test-native "cmp-imm"
  (concatenate 'string *asm-core*
    "(let ((bytes (arm64-cmp-imm 0 42)))
       (cadddr bytes))")
  #xF1)  ; CMP immediate opcode high byte

;; Test 10: Load with offset - LDR x0, [sp, #16]
;; Encoding: #xF9400BE0 -> little-endian byte 0 = #xE0 = 224
(test-native "ldr-offset"
  (concatenate 'string *asm-core*
    "(let ((bytes (arm64-ldr 0 31 16)))
       (car bytes))")
  224)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
