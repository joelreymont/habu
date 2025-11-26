#!/usr/bin/env sbcl --script
;;; Test bootstrap compiler execution
;;; Compiles Lisp source to ARM64 bytecode and executes via run-bytecode

(load "bootstrap/compiler.lisp")
(in-package :habu)

(defparameter *tests-passed* 0)
(defparameter *tests-failed* 0)

(defun write-bytecode (code filename)
  "Write bytecode list to binary file"
  (with-open-file (out filename :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
    (dolist (byte code)
      (write-byte byte out))))

(defun parse-hex-result (output)
  "Parse hex result from run-bytecode output"
  ;; Output format: "Raw result: 0x2a0 (672)"
  (let ((result-pos (search "Raw result:" output)))
    (when result-pos
      (let* ((start (+ result-pos 12))  ; Skip "Raw result: "
             (end (or (position #\Space output :start start) (length output)))
             (hex-str (string-trim " " (subseq output start end))))
        ;; Strip 0x prefix if present
        (when (and (> (length hex-str) 2)
                   (string-equal (subseq hex-str 0 2) "0x"))
          (setf hex-str (subseq hex-str 2)))
        ;; Handle negative values (sign-extend from 64-bit)
        (let ((val (parse-integer hex-str :radix 16 :junk-allowed t)))
          (when val
            ;; Untag: arithmetic shift right by 4
            (ash val -4)))))))

(defun run-bytecode-file (filename)
  "Execute bytecode via run-bytecode and return result"
  (let* ((output (with-output-to-string (s)
                   (sb-ext:run-program "./run-bytecode" (list filename)
                                       :output s :error :output
                                       :search nil))))
    (parse-hex-result output)))

(defun compile-and-run (source)
  "Compile source to bytecode, execute, return result"
  (let* ((forms (nc-read-all source))
         (compiled (nc-compile-forms forms))
         (mir (cadr compiled))
         (main-code (nc-codegen-main mir nil))
         (tmpfile "/tmp/bootstrap-test.bin"))
    (write-bytecode main-code tmpfile)
    (run-bytecode-file tmpfile)))

(defun run-exec-test (name source expected)
  "Compile, execute, verify result"
  (handler-case
      (let ((result (compile-and-run source)))
        (if (and result (= result expected))
            (progn
              (format t "[PASS] ~A: ~A = ~A~%" name source expected)
              (incf *tests-passed*))
            (progn
              (format t "[FAIL] ~A: expected ~A got ~A~%" name expected result)
              (incf *tests-failed*))))
    (error (e)
      (format t "[FAIL] ~A: ~A~%" name e)
      (incf *tests-failed*))))

;; Check if run-bytecode exists
(unless (probe-file "run-bytecode")
  (format t "run-bytecode not found, skipping execution tests~%")
  (sb-ext:exit :code 0))

(format t "~%=== Bootstrap Execution Tests ===~%~%")

;; Test 1: Simple literal
(run-exec-test "literal" "42" 42)

;; Test 2: Addition
(run-exec-test "add" "(+ 10 20)" 30)

;; Test 3: Nested arithmetic
(run-exec-test "nested" "(+ (* 3 4) 5)" 17)

;; Test 4: Subtraction
(run-exec-test "sub" "(- 100 42)" 58)

;; Test 5: Multiplication
(run-exec-test "mul" "(* 7 8)" 56)

;; Test 6: Division
(run-exec-test "div" "(/ 100 4)" 25)

;; Test 7: Comparison
(run-exec-test "cmp-eq" "(if (= 5 5) 1 0)" 1)

;; Test 8: Let binding
(run-exec-test "let" "(let ((x 10)) (+ x 5))" 15)

;; Summary
(format t "~%=== Results: ~A passed, ~A failed ===~%"
        *tests-passed* *tests-failed*)

(if (> *tests-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
