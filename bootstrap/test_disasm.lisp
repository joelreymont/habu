;;;; Disassembler Integration Tests
;;;; Compares habu disassembler output against lldb for correctness

(in-package :habu)

;;; Test data: known ARM64 instructions with expected disassembly
;;; Format: (bytes-little-endian expected-mnemonic expected-operands)
;;; NOTE: Bytes must be in little-endian file order (LSB first)
(defparameter *disasm-test-cases*
  '(;; NEG (SUB from XZR): sub x11, xzr, x11 = neg x11, x11
    ;; lldb shows: neg x11, x11
    ;; File bytes: eb 03 0b cb -> instruction 0xCB0B03EB
    ((#xEB #x03 #x0B #xCB) "neg" "x11, x11")

    ;; MOV immediate: mov x10, #0x10
    ((#x0A #x02 #x80 #xD2) "mov" "x10, #0x10")

    ;; MOV immediate: mov x14, #0x6 (nil tag)
    ((#xCE #x00 #x80 #xD2) "mov" "x14, #0x6")

    ;; CMP registers: cmp x9, x10
    ((#x3F #x01 #x0A #xEB) "cmp" "x9, x10")

    ;; CSET: cset x11, ge
    ((#xEB #xB7 #x9F #x9A) "cset" "x11, ge")

    ;; CSET: cset x11, eq
    ((#xEB #x17 #x9F #x9A) "cset" "x11, eq")

    ;; ADD immediate: add x11, x11, #0x6
    ((#x6B #x19 #x00 #x91) "add" "x11, x11, #0x6")

    ;; RET
    ((#xC0 #x03 #x5F #xD6) "ret" "")

    ;; STR: str x0, [x20]
    ((#x80 #x02 #x00 #xF9) "str" "x0, [x20")

    ;; LDR: ldr x10, [x10]
    ((#x4A #x01 #x40 #xF9) "ldr" "x10, [x10")

    ;; B.NE: conditional branch
    ((#x41 #x00 #x00 #x54) "b.ne" "")))

(defun run-disasm-test (test-case)
  "Run a single disassembler test case.
   Returns (passed test-case actual-output) or (failed test-case actual-output expected)."
  (destructuring-bind (bytes expected-mnemonic expected-operands) test-case
    (let* ((output (with-output-to-string (s)
                     (disassemble-bytes bytes s t)))  ; little-endian = t
           ;; Extract the instruction line (skip header)
           (lines (remove-if #'(lambda (l) (or (string= l "")
                                                (search "ARM64" l)
                                                (search "OFF" l)
                                                (search "---" l)))
                             (split-string output #\Newline)))
           (instr-line (first lines))
           ;; Check if mnemonic and operands are present
           (mnemonic-found (and instr-line (search expected-mnemonic instr-line)))
           (operands-found (or (string= expected-operands "")
                               (and instr-line (search expected-operands instr-line)))))
      (if (and mnemonic-found operands-found)
          (list 'passed test-case instr-line)
          (list 'failed test-case instr-line
                (format nil "~A ~A" expected-mnemonic expected-operands))))))

(defun split-string (string delimiter)
  "Split STRING by DELIMITER character."
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun run-disasm-tests ()
  "Run all disassembler tests and report results."
  (format t "~%=== Disassembler Tests ===~%")
  (let ((passed 0)
        (failed 0)
        (failures nil))
    (dolist (tc *disasm-test-cases*)
      (let ((result (run-disasm-test tc)))
        (if (eq (first result) 'passed)
            (progn
              (incf passed)
              (format t "."))
            (progn
              (incf failed)
              (format t "F")
              (push result failures)))))
    (format t "~%~%Results: ~D passed, ~D failed~%" passed failed)
    (when failures
      (format t "~%Failures:~%")
      (dolist (f (reverse failures))
        (destructuring-bind (status test-case actual expected) f
          (declare (ignore status))
          (format t "  Bytes: ~{~2,'0X ~}~%" (first test-case))
          (format t "  Expected: ~A~%" expected)
          (format t "  Actual: ~A~%~%" actual))))
    (values passed failed)))

;;; Test against lldb for specific binary
(defun compare-with-lldb (binary-path addr count)
  "Compare habu disassembly with lldb for BINARY-PATH at ADDR for COUNT instructions.
   Returns list of differences."
  (let* (;; Get lldb output
         (lldb-cmd (format nil "echo 'disassemble -s ~A -c ~D' | lldb ~A 2>/dev/null | grep -E '^[a-z]'"
                           addr count binary-path))
         (lldb-output (with-output-to-string (s)
                        (sb-ext:run-program "/bin/sh" (list "-c" lldb-cmd) :output s)))
         ;; Read bytes from binary
         (file-offset (- addr #x100000000))  ; Adjust for VM base
         (bytes (with-open-file (f binary-path :element-type '(unsigned-byte 8))
                  (file-position f file-offset)
                  (loop repeat (* count 4)
                        collect (read-byte f))))
         ;; Get habu output
         (habu-output (with-output-to-string (s)
                        (disassemble-bytes bytes s t))))
    (format t "~%LLDB output:~%~A~%" lldb-output)
    (format t "~%Habu output:~%~A~%" habu-output)
    ;; Return both for manual comparison
    (list lldb-output habu-output)))

;;; Quick test runner
(defun test-disasm ()
  "Quick test of disassembler fixes."
  (run-disasm-tests))
