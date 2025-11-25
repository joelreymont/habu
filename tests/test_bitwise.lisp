#!/usr/bin/env sbcl --script
;;; Tests for bitwise operations: logand, logior, logxor, ash

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== Bitwise Operations Tests ===~%~%")

;; Test 1: logand - basic
(run-test "logand-basic"
          '((logand #xFF #x0F))
          #x0F)

;; Test 2: logand - identity with -1
(run-test "logand-identity"
          '((logand #x5A -1))
          #x5A)

;; Test 3: logand - variadic
(run-test "logand-variadic"
          '((logand #xFF #x0F #x03))
          #x03)

;; Test 4: logand - single arg returns itself
(run-test "logand-single"
          '((logand #x5A))
          #x5A)

;; Test 5: logior - basic
(run-test "logior-basic"
          '((logior #xF0 #x0F))
          #xFF)

;; Test 6: logior - with zero
(run-test "logior-zero"
          '((logior #x5A #x00))
          #x5A)

;; Test 7: logior - variadic
(run-test "logior-variadic"
          '((logior #x01 #x02 #x04 #x08))
          #x0F)

;; Test 8: logior - single arg returns itself
(run-test "logior-single"
          '((logior #x5A))
          #x5A)

;; Test 9: logxor - basic
(run-test "logxor-basic"
          '((logxor #xFF #x0F))
          #xF0)

;; Test 10: logxor - toggle bits
(run-test "logxor-toggle"
          '((logxor #x55 #xFF))
          #xAA)

;; Test 11: logxor - same value returns 0
(run-test "logxor-same"
          '((logxor #x5A #x5A))
          #x0)

;; Test 12: ash - left shift
(run-test "ash-left"
          '((ash #x01 #x4))
          #x10)

;; Test 13: ash - right shift
(run-test "ash-right"
          '((ash #x10 (- 0 4)))
          #x01)

;; Test 14: ash - shift by 0
(run-test "ash-zero"
          '((ash #x5A #x0))
          #x5A)

;; Test 15: Combined bitwise in expression
(run-test "combined-bitwise"
          '((logand (logior #xF0 #x0A) #xFA))
          #xFA)

;; Test 16: Extract byte with logand
(run-test "extract-byte"
          '((logand (ash #x1234 (- 0 8)) #xFF))
          #x12)

(format t "~%=== All Bitwise Operations Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
