;; Test micro-compiler: minimal compiler that generates ARM64-like code patterns
;; This tests the foundation for self-hosting: code generation patterns in native code
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Test micro-compiler ===~%~%")

(defvar *tests-passed* 0)
(defvar *tests-failed* 0)

(defun test-native (name source expected)
  "Compile source to native executable and verify exit code"
  (let ((path (format nil "/tmp/mc_~A" name)))
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

;; Test 1: Bytecode-like instruction representation
;; Generate a list representing ADD instruction and extract opcode
(test-native "instr-repr"
  "(let* ((add-instr (list 'ADD 0 1 2))
          (opcode (car add-instr))
          (rd (cadr add-instr))
          (rn (caddr add-instr))
          (rm (cadddr add-instr)))
     (if (eq opcode 'ADD)
         (+ rd rn rm)  ; 0 + 1 + 2 = 3
         0))"
  3)

;; Test 2: Instruction encoding function
;; Encode an ADD instruction as a 32-bit value (simplified)
(test-native "encode-add"
  "(defun encode-add (rd rn rm)
     ;; Simplified encoding: rd in bits 0-4, rn in bits 5-9, rm in bits 16-20
     ;; opcode 0x8B in upper byte
     (+ rd (* rn 32) (* rm 65536) (* #x8B 16777216)))
   (let ((encoded (encode-add 0 1 2)))
     ;; Check low byte contains rd=0
     (mod encoded 32))"
  0)

;; Test 3: Register allocation simulation
;; Allocate registers for an expression tree
(test-native "reg-alloc"
  "(defun alloc-reg (used)
     ;; Find first unused register (0-7)
     (labels ((find-free (n)
                (if (> n 7)
                    0  ; fallback
                    (if (member n used)
                        (find-free (+ n 1))
                        n))))
       (find-free 0)))
   ;; With r0, r1 used, allocate returns r2
   (alloc-reg (list 0 1))"
  2)

;; Test 4: Stack offset calculation
;; Calculate stack offsets for local variables
(test-native "stack-offset"
  "(defun calc-offset (var-idx)
     (* var-idx 8))  ; 8 bytes per slot
   (defun lookup-var (name env)
     (labels ((find-idx (e idx)
                (if (null e)
                    0
                    (if (eq (car e) name)
                        idx
                        (find-idx (cdr e) (+ idx 1))))))
       (find-idx env 0)))
   (let* ((env (list 'x 'y 'z))
          (idx (lookup-var 'z env))
          (offset (calc-offset idx)))
     offset)"
  16)  ; z is at index 2, offset = 2 * 8 = 16

;; Test 5: Instruction list builder
;; Build a list of instructions for (+ x 1)
(test-native "instr-builder"
  "(defun build-add-const (var-offset const)
     (list
       (list 'LDR 0 'SP var-offset)   ; Load var into r0
       (list 'ADD 0 0 const)))        ; Add const
   (let* ((instrs (build-add-const 8 5))
          (load-instr (car instrs))
          (add-instr (cadr instrs)))
     ;; Return the constant from the add instruction
     (cadddr add-instr))"
  5)

;; Test 6: Code size calculation
;; Calculate total code size from instruction list
(test-native "code-size"
  "(defun instr-size (instr)
     (let ((op (car instr)))
       (cond
         ((eq op 'LDR) 4)
         ((eq op 'STR) 4)
         ((eq op 'ADD) 4)
         ((eq op 'MUL) 4)
         ((eq op 'B) 4)
         ((eq op 'BL) 4)
         (t 4))))
   (defun total-size (instrs)
     (if (null instrs)
         0
         (+ (instr-size (car instrs)) (total-size (cdr instrs)))))
   (total-size (list
     (list 'LDR 0 'SP 8)
     (list 'ADD 0 0 1)
     (list 'STR 0 'SP 16)
     (list 'B 'end)))"
  16)  ; 4 instructions * 4 bytes = 16

;; Test 7: Branch offset calculation
;; Calculate relative branch offset
(test-native "branch-offset"
  "(defun calc-branch-offset (from-addr to-addr)
     ;; Branch offset in ARM64 is (target - current) / 4
     (/ (- to-addr from-addr) 4))
   (let* ((loop-start 100)
          (branch-addr 120)
          (offset (calc-branch-offset branch-addr loop-start)))
     (+ offset 50))"  ; -5 + 50 = 45
  45)

;; Test 8: Function prologue size
;; Calculate prologue size for given number of saved registers
(test-native "prologue-size"
  "(defun prologue-size (num-saved-regs)
     ;; STP for each pair + SUB SP
     (let ((stp-count (/ (+ num-saved-regs 1) 2)))  ; round up
       (* stp-count 4)))
   (prologue-size 4)"  ; 4 regs = 2 STP = 8 bytes, but we floor divide so 2*4=8
  8)

;; Test 9: Fixup table building
;; Build a table of (offset . target) pairs for later patching
(test-native "fixup-table"
  "(defun add-fixup (table offset target)
     (cons (cons offset target) table))
   (let* ((table nil)
          (table (add-fixup table 100 'foo))
          (table (add-fixup table 200 'bar))
          (first-fixup (car table))
          (first-offset (car first-fixup)))
     first-offset)"
  200)  ; Most recent fixup first

;; Test 10: Label resolution
;; Directly access alist entries without recursion (keep result < 256 for exit code)
(test-native "label-resolve"
  "(let* ((labels (list (cons 1 20) (cons 2 22)))
          (entry1 (car labels))
          (entry2 (cadr labels))
          (addr1 (cdr entry1))
          (addr2 (cdr entry2)))
     (+ addr1 addr2))"
  42)

(format t "~%Results: ~A passed, ~A failed~%~%" *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:quit :unix-status 1)
    (sb-ext:quit :unix-status 0))
