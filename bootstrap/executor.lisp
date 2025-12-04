;;;; executor.lisp - Execute compiled Habu machine code
;;;;
;;;; Provides execution of compiled ARM64 code on macOS.
;;;;
;;;; NOTE: In-process JIT on ARM64 macOS is not possible when running under
;;;; SBCL because pthread_jit_write_protect_np is global per-thread and
;;;; affects SBCL's own code pages. Instead, we use subprocess execution.
;;;;
;;;; The subprocess approach:
;;;; 1. Compile expression wrapped in sys-exit
;;;; 2. Create a Mach-O executable via deliver
;;;; 3. Run it and capture exit code (for fixnum results 0-255)
;;;;
;;;; For complex return values, use the test harness directly.

(in-package :habu)

;;; ============================================================
;;; Subprocess Execution
;;; ============================================================

(defvar *jit-temp-counter* 0
  "Counter for unique temp file names")

#+sbcl
(defun jit-eval (expr)
  "Compile and execute an expression via subprocess.
   Returns the untagged result for small fixnums (0-255).
   Larger results or non-fixnum values require different approach."
  (let* ((temp-name (format nil "/tmp/habu_jit_~D_~D"
                            (sb-posix:getpid)
                            (incf *jit-temp-counter*)))
         ;; Handle progn specially to keep defuns at top level
         (wrapped-source
          (if (and (consp expr) (eq (car expr) 'progn))
              ;; Extract forms, wrap only last one in sys-exit
              (let ((forms (cdr expr)))
                (if (null forms)
                    "(sys-exit 0)"
                    (with-output-to-string (s)
                      (dolist (f (butlast forms))
                        (format s "~S~%" f))
                      (format s "(sys-exit ~S)" (car (last forms))))))
              ;; Simple case: wrap whole expression
              (format nil "(sys-exit ~S)" expr))))
    (unwind-protect
        (progn
          ;; Compile to executable using deliver
          (deliver wrapped-source temp-name)
          ;; Run and get exit code
          (let* ((proc (sb-ext:run-program temp-name nil
                                           :output nil
                                           :error nil
                                           :wait t))
                 (exit-code (sb-ext:process-exit-code proc)))
            exit-code))
      ;; Cleanup temp files
      (ignore-errors (delete-file temp-name))
      (ignore-errors (delete-file (format nil "~A.map" temp-name))))))

#+sbcl
(defun jit-compile-expression (expr)
  "Compile an expression to ARM64 bytecode.
   Returns a byte vector (for inspection/debugging).
   Use jit-eval to actually execute."
  (let* ((compiled (compile-forms (list expr)))
         (main-ir (cadr compiled))
         (code (codegen-main main-ir nil))
         (bytes (resolve-calls-simple code)))
    (coerce bytes '(vector (unsigned-byte 8)))))

#+sbcl
(defun jit-disasm (expr)
  "Compile and disassemble an expression.
   Returns a string with the disassembly."
  (let* ((bytes (jit-compile-expression expr))
         (temp-bin "/tmp/habu_jit_disasm.bin"))
    (with-open-file (out temp-bin :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
      (write-sequence bytes out))
    (with-output-to-string (s)
      (format s "=== ~S ===~%" expr)
      (format s "~D bytes~%~%" (length bytes))
      ;; Use llvm-objdump for disassembly
      (let ((proc (sb-ext:run-program
                   "/usr/bin/llvm-objdump"
                   (list "-d" "-triple=aarch64" temp-bin)
                   :output s :error :output :wait t)))
        (declare (ignore proc)))
      (delete-file temp-bin))))

;;; ============================================================
;;; Batch Execution (for test runner)
;;; ============================================================

#+sbcl
(defun jit-test (name expr expected)
  "Test that expr evaluates to expected (via exit code).
   Returns T on success, NIL on failure."
  (let ((result (jit-eval expr)))
    (if (= result expected)
        (progn
          (format t "[PASS] ~A = ~A~%" name result)
          t)
        (progn
          (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected result)
          nil))))

#+sbcl
(defun jit-run-tests (tests)
  "Run a list of tests. Each test is (name expr expected).
   Returns number of failures."
  (let ((passed 0)
        (failed 0))
    (dolist (test tests)
      (let ((name (first test))
            (expr (second test))
            (expected (third test)))
        (if (jit-test name expr expected)
            (incf passed)
            (incf failed))))
    (format t "~%Results: ~A passed, ~A failed~%" passed failed)
    failed))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

(defun untag-fixnum (tagged-value)
  "Convert tagged fixnum to regular integer"
  (ash tagged-value -4))

(defun tag-fixnum (value)
  "Convert regular integer to tagged fixnum"
  (ash value 4))

;;; ============================================================
;;; Exports
;;; ============================================================

(export '(jit-eval
          jit-compile-expression
          jit-disasm
          jit-test
          jit-run-tests
          untag-fixnum
          tag-fixnum))
