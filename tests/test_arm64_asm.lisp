;;; ARM64 assembler tests - SVC, BR, and syscall support
(load "arm64/asm.lisp")
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(defpackage :habu-test-arm64-asm
  (:use :cl))

(in-package :habu-test-arm64-asm)

(format t "~%=== ARM64 Assembler Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-exec (name code expected)
  "Create executable from ARM64 code, run it, check exit code"
  (handler-case
    (let ((output-path (format nil "/tmp/asm_test_~A" name)))
      (write-minimal-macho-executable output-path code)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (result (sb-ext:process-exit-code proc)))
        (if (= result expected)
            (progn (format t "[PASS] ~A = ~A~%" name result)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;;; BR instruction - branch register without link
(test-exec "br-basic"
  ;; Load address of target, branch to it, target returns 42
  (append
   (arm64:adr 9 8)               ; x9 = PC + 8 (addr of mov x0, #42)
   (arm64:br 9)                  ; branch to x9
   (arm64:movz 0 99)             ; skipped - would return 99
   (arm64:movz 0 42)             ; target: return 42
   (arm64:ret))
  42)

;;; SVC instruction - supervisor call
(test-exec "svc-exit"
  ;; Call exit(42) syscall directly
  (append
   (arm64:movz 16 arm64:+sys-exit+)  ; x16 = syscall number
   (arm64:movz 0 42)                  ; x0 = exit code
   (arm64:svc #x80))                  ; invoke syscall
  42)

;;; SVC with write syscall
(test-exec "svc-write"
  ;; write(1, "!\n", 2) then return 42
  (append
   ;; Prologue
   (arm64:sub arm64:+sp+ arm64:+sp+ #x10 :imm t)
   (arm64:str arm64:+lr+ arm64:+sp+)
   ;; write syscall
   (arm64:movz 16 arm64:+sys-write+)  ; x16 = SYS_write
   (arm64:movz 0 1)                    ; x0 = 1 (stdout)
   (arm64:adr 1 28)                    ; x1 = address of "!\n" (7 instrs * 4)
   (arm64:movz 2 2)                    ; x2 = 2 (length)
   (arm64:svc #x80)                    ; syscall
   ;; Return 42
   (arm64:movz 0 42)
   ;; Epilogue
   (arm64:ldr arm64:+lr+ arm64:+sp+)
   (arm64:add arm64:+sp+ arm64:+sp+ #x10 :imm t)
   (arm64:ret)
   ;; Data: "!\n"
   '(#x21 #x0A #x00 #x00))
  42)

;;; BRK instruction - breakpoint trap (used for undefined function calls)
;;; Note: BRK causes SIGTRAP which has exit code 133 (128 + 5)
(format t "~%Testing BRK instruction encoding...~%")
(let ((brk-bytes (arm64:brk #xF01)))
  (if (equal brk-bytes '(#x20 #xE0 #x21 #xD4))
      (progn (format t "[PASS] BRK #xF01 encodes correctly~%")
             (incf *pass-count*))
      (progn (format t "[FAIL] BRK #xF01: expected (20 E0 21 D4), got ~A~%" brk-bytes)
             (incf *fail-count*))))

;;; Summary
(format t "~%~A/~A tests passed~%" *pass-count* (+ *pass-count* *fail-count*))
(when (> *fail-count* 0)
  (sb-ext:exit :code 1))
