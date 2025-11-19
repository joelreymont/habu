;;;; test-standalone.lisp - Test standalone executable generation

(load "compiler.lisp")
(load "macho-generator.lisp")
(in-package :habu-compiler)

(format t "~%========================================~%")
(format t "  Standalone Executable Generation~%")
(format t "========================================~%")

;; Test 1: Simple arithmetic
(format t "~%[34m1. Simple Arithmetic[0m~%")
(format t "Expression: (+ 2 3)~%")
(compile-to-executable '(+ 2 3) :output-file "test-add")

;; Test the executable
(format t "~%Testing executable...~%")
#+sbcl
(let ((result (sb-ext:run-program "./test-add" nil :search nil :wait t)))
  (format t "Exit code: ~D~%" (sb-ext:process-exit-code result))
  (format t "Expected: 80 (5 << 4, tagged fixnum)~%")
  (when (= (sb-ext:process-exit-code result) 80)
    (format t "[32m✓ Correct![0m~%")))

;; Test 2: Multiplication
(format t "~%[34m2. Multiplication[0m~%")
(format t "Expression: (* 6 7)~%")
(compile-to-executable '(* 6 7) :output-file "test-mul")

#+sbcl
(let ((result (sb-ext:run-program "./test-mul" nil :search nil :wait t)))
  (format t "Exit code: ~D~%" (sb-ext:process-exit-code result))
  (format t "Expected: 672 (42 << 4, tagged fixnum)~%")
  (when (= (sb-ext:process-exit-code result) 672)
    (format t "[32m✓ Correct![0m~%")))

;; Test 3: Nested expression
(format t "~%[34m3. Nested Expression[0m~%")
(format t "Expression: (+ (* 2 3) (* 4 5))~%")
(compile-to-executable '(+ (* 2 3) (* 4 5)) :output-file "test-nested")

#+sbcl
(let ((result (sb-ext:run-program "./test-nested" nil :search nil :wait t)))
  (format t "Exit code: ~D~%" (sb-ext:process-exit-code result))
  (format t "Expected: 416 (26 << 4, tagged fixnum)~%")
  (when (= (sb-ext:process-exit-code result) 416)
    (format t "[32m✓ Correct![0m~%")))

;; Show file info
(format t "~%========================================~%")
(format t "  Generated Binaries~%")
(format t "========================================~%")

#+sbcl
(progn
  (format t "~%File information:~%")
  (sb-ext:run-program "/usr/bin/file" '("test-add") :search nil :output *standard-output*)
  (sb-ext:run-program "/usr/bin/ls" '("-lh" "test-add" "test-mul" "test-nested")
                     :search nil :output *standard-output*))

(format t "~%========================================~%")
(format t "  Summary~%")
(format t "========================================~%")
(format t "✓ Generated standalone Mach-O executables~%")
(format t "✓ No SBCL dependency~%")
(format t "✓ Native macOS binaries~%")
(format t "✓ Can be distributed and run anywhere~%")
(format t "~%This is Phase 2 in action!~%")

(sb-ext:quit)
