;;;; Benchmark Habu compiler vs SBCL

(load "../bootstrap/compiler.lisp")

(in-package :habu-compiler)

(defun time-compilation (expr iterations)
  "Time compilation of expression"
  (let ((start (get-internal-real-time)))
    (dotimes (i iterations)
      (compile-expression expr :arch :x86_64))
    (let* ((end (get-internal-real-time))
           (elapsed (/ (- end start) internal-time-units-per-second))
           (per-compile (/ elapsed iterations)))
      (values elapsed per-compile))))

(defun benchmark-compilation-speed ()
  "Compare Habu vs SBCL compilation speed"
  (format t "~%Compilation Speed Benchmark~%")
  (format t "===========================~%~%")

  (let ((iterations 10000))
    ;; Habu: Simple fixnum
    (format t "Habu - Fixnum (42):~%")
    (multiple-value-bind (total per) (time-compilation 42 iterations)
      (format t "  Total: ~,6F seconds~%" total)
      (format t "  Per compilation: ~,2F us~%" (* per 1000000))
      (format t "  Throughput: ~,0F compilations/sec~%~%" (/ iterations total)))

    ;; Habu: Addition
    (format t "Habu - Addition (+ 10 20):~%")
    (multiple-value-bind (total per) (time-compilation '(+ 10 20) iterations)
      (format t "  Total: ~,6F seconds~%" total)
      (format t "  Per compilation: ~,2F us~%" (* per 1000000))
      (format t "  Throughput: ~,0F compilations/sec~%~%" (/ iterations total)))

    ;; Habu: Nested
    (format t "Habu - Nested (+ (+ 1 2) (+ 3 4)):~%")
    (multiple-value-bind (total per) (time-compilation '(+ (+ 1 2) (+ 3 4)) (/ iterations 10))
      (format t "  Total: ~,6F seconds~%" total)
      (format t "  Per compilation: ~,2F us~%" (* per 1000000))
      (format t "  Throughput: ~,0F compilations/sec~%~%" (/ (/ iterations 10) total)))

    ;; SBCL comparison
    (format t "~%SBCL - Simple function:~%")
    (let ((start (get-internal-real-time)))
      (dotimes (i (/ iterations 10))
        (compile nil '(lambda () 42)))
      (let* ((end (get-internal-real-time))
             (total (/ (- end start) internal-time-units-per-second))
             (per (/ total (/ iterations 10))))
        (format t "  Total: ~,6F seconds~%" total)
        (format t "  Per compilation: ~,2F us~%" (* per 1000000))
        (format t "  Throughput: ~,0F compilations/sec~%~%" (/ (/ iterations 10) total))))))

(defun benchmark-code-size ()
  "Compare generated code sizes"
  (format t "~%Code Size Benchmark~%")
  (format t "===================~%~%")

  (let ((tests '((42 "Fixnum literal")
                 ((+ 1 2) "Simple addition")
                 ((+ 10 20) "Larger addition")
                 ((- 100 50) "Subtraction")
                 ((+ (+ 1 2) 3) "Nested (depth 2)")
                 ((+ (+ 1 2) (+ 3 4)) "Nested (depth 2, binary)")
                 ((+ (+ (+ 1 2) 3) 4) "Nested (depth 3)"))))

    (dolist (test tests)
      (let* ((expr (first test))
             (desc (second test))
             (x86-code (compile-expression expr :arch :x86_64))
             (arm-code (compile-expression expr :arch :arm64)))
        (format t "~A:~%" desc)
        (format t "  Expression: ~S~%" expr)
        (format t "  x86_64: ~3D bytes~%" (length x86-code))
        (format t "  ARM64:  ~3D bytes~%" (length arm-code))
        (format t "~%")))

    ;; Compare to SBCL disassembly
    (format t "SBCL equivalent (lambda () 42):~%")
    (let ((fn (compile nil '(lambda () 42))))
      (format t "  Disassembly:~%")
      (with-output-to-string (*standard-output*)
        (disassemble fn))
      (format t "  (Use (disassemble fn) to see full output)~%"))))

(defun benchmark-memory-usage ()
  "Measure memory usage during compilation"
  (format t "~%Memory Usage Benchmark~%")
  (format t "======================~%~%")

  ;; GC before measurement
  (sb-ext:gc :full t)

  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i 1000)
      (compile-expression '(+ (+ 1 2) (+ 3 4)) :arch :x86_64))
    (let* ((after (sb-ext:get-bytes-consed))
           (consed (- after before))
           (per-compile (/ consed 1000)))
      (format t "Habu - 1000 compilations of nested expression:~%")
      (format t "  Total memory: ~,2F KB~%" (/ consed 1024))
      (format t "  Per compilation: ~,2F bytes~%~%" per-compile)))

  (sb-ext:gc :full t)

  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i 1000)
      (compile nil '(lambda () (+ (+ 1 2) (+ 3 4)))))
    (let* ((after (sb-ext:get-bytes-consed))
           (consed (- after before))
           (per-compile (/ consed 1000)))
      (format t "SBCL - 1000 compilations of equivalent lambda:~%")
      (format t "  Total memory: ~,2F KB~%" (/ consed 1024))
      (format t "  Per compilation: ~,2F bytes~%~%" per-compile))))

(defun analyze-code-quality ()
  "Analyze generated code quality"
  (format t "~%Code Quality Analysis~%")
  (format t "=====================~%~%")

  (format t "Fixnum encoding (42):~%")
  (let ((code (compile-expression 42 :arch :x86_64)))
    (format t "  Bytes: ~{~2,'0X ~}~%" (coerce code 'list))
    (format t "  x86_64 optimal: 48 B8 [8 bytes imm]~%")
    (format t "  Size: ~A bytes~%" (length code))
    (format t "  Optimal: 10 bytes (mov rax, imm64; includes encoding)~%~%"))

  (format t "Addition (+ 10 20):~%")
  (let ((code (compile-expression '(+ 10 20) :arch :x86_64)))
    (format t "  Size: ~A bytes~%" (length code))
    (format t "  Breakdown:~%")
    (format t "    - Load first operand: 10 bytes~%")
    (format t "    - Push: 1 byte~%")
    (format t "    - Load second operand: 10 bytes~%")
    (format t "    - Pop and add: 11 bytes~%")
    (format t "  Analysis: Uses stack for intermediate values~%")
    (format t "  Optimization opportunity: Use registers directly~%~%"))

  (format t "Potential Improvements:~%")
  (format t "  1. Register allocation instead of stack~%")
  (format t "  2. Constant folding (+ 10 20) -> 30~%")
  (format t "  3. Smaller immediate encodings for small values~%")
  (format t "  4. Peephole optimization~%")
  (format t "  5. Common subexpression elimination~%"))

(defun benchmark-architectures ()
  "Compare x86_64 vs ARM64 code generation"
  (format t "~%Architecture Comparison~%")
  (format t "=======================~%~%")

  (let ((exprs '(42
                 (+ 1 2)
                 (+ 10 20)
                 (- 50 25)
                 (+ (+ 1 2) 3))))

    (format t "~40A ~10A ~10A ~10A~%" "Expression" "x86_64" "ARM64" "Difference")
    (format t "~80@{~A~:*~}~%" "-")

    (dolist (expr exprs)
      (let* ((x86-code (compile-expression expr :arch :x86_64))
             (arm-code (compile-expression expr :arch :arm64))
             (x86-size (length x86-code))
             (arm-size (length arm-code))
             (diff (- x86-size arm-size)))
        (format t "~40S ~10D ~10D ~10@D~%" expr x86-size arm-size diff)))))

(defun run-all-benchmarks ()
  "Run all compiler benchmarks"
  (format t "~%Habu Compiler Benchmarks~%")
  (format t "========================~%")

  (benchmark-compilation-speed)
  (benchmark-code-size)
  (benchmark-memory-usage)
  (benchmark-architectures)
  (analyze-code-quality)

  (format t "~%Benchmark complete!~%"))

;; Run benchmarks
(run-all-benchmarks)
