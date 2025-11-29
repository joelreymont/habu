;;; Test bitwise operations (logand, logior, logxor, ash) in bootstrap compiler
(load "bootstrap/compiler.lisp")
(in-package :habu)

(defvar *passed* 0)
(defvar *failed* 0)

(defun parse-run-bytecode-output (output)
  "Parse raw result from run-bytecode output"
  (let ((pos (search "Raw result:" output)))
    (when pos
      (let* ((start (+ pos 12))
             (end (or (position #\Space output :start start) (length output)))
             (hex (subseq output start end)))
        (when (and (> (length hex) 2)
                   (string-equal (subseq hex 0 2) "0x"))
          (setf hex (subseq hex 2)))
        (let ((raw (parse-integer hex :radix 16 :junk-allowed t)))
          (when raw
            (if (>= raw (ash 1 63))
                (ash (- raw (ash 1 64)) -4)
                (ash raw -4))))))))

(defun test-case (name source expected)
  (format t "~A: " name)
  (handler-case
    (let* ((code (compile-program (read-all source) nil)))
      (with-open-file (out "/tmp/bitwise-test.bin" :direction :output
                           :if-exists :supersede
                           :element-type '(unsigned-byte 8))
        (dolist (byte code) (write-byte byte out)))
      (let* ((output (with-output-to-string (s)
                       (sb-ext:run-program "./run-bytecode" '("/tmp/bitwise-test.bin")
                                           :output s :error :output :search nil)))
             (result (parse-run-bytecode-output output)))
        (if (eql result expected)
            (progn
              (format t "PASS (~A)~%" result)
              (incf *passed*))
            (progn
              (format t "FAIL (expected ~A, got ~A)~%" expected result)
              (incf *failed*)))))
    (error (e)
      (format t "ERROR: ~A~%" e)
      (incf *failed*))))

(format t "~%=== Bootstrap Bitwise Operations Tests ===~%~%")

;; Basic logand
(test-case "logand-basic" "(logand 15 7)" 7)
(test-case "logand-zero" "(logand 255 0)" 0)
(test-case "logand-identity" "(logand 42 -1)" 42)

;; Basic logior
(test-case "logior-basic" "(logior 8 4)" 12)
(test-case "logior-zero" "(logior 42 0)" 42)
(test-case "logior-overlap" "(logior 15 7)" 15)

;; Basic logxor
(test-case "logxor-basic" "(logxor 15 7)" 8)
(test-case "logxor-same" "(logxor 42 42)" 0)
(test-case "logxor-zero" "(logxor 42 0)" 42)

;; Basic ash (left shift)
(test-case "ash-left-1" "(ash 1 4)" 16)
(test-case "ash-left-2" "(ash 3 2)" 12)
(test-case "ash-left-0" "(ash 42 0)" 42)

;; Basic ash (right shift)
(test-case "ash-right-1" "(ash 16 -4)" 1)
(test-case "ash-right-2" "(ash 12 -2)" 3)

;; Combined operations
(test-case "and-or" "(logior (logand 15 3) 8)" 11)
(test-case "xor-and" "(logand (logxor 255 15) 255)" 240)

;; With functions
(test-case "fn-logand" "(defun mask (x) (logand x 255)) (mask 1000)" 232)
(test-case "fn-ash" "(defun shl (x n) (ash x n)) (shl 1 8)" 256)

(format t "~%=== Results: ~A passed, ~A failed ===~%" *passed* *failed*)
