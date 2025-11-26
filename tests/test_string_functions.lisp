#!/usr/bin/env sbcl --script
;;; Tests for string functions (length, ref, compare, case, concat)

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== String Function Tests ===~%~%")

;; Test 1: string-length on literal
(run-test "string-length-literal"
          '((string-length "hello"))
          #x5)

;; Test 2: string-length on empty string
(run-test "string-length-empty"
          '((string-length ""))
          #x0)

;; Test 3: string-ref first char
(run-test "string-ref-first"
          '((string-ref "hello" 0))
          #x68)  ; 'h' = 104 = #x68

;; Test 4: string-ref last char
(run-test "string-ref-last"
          '((string-ref "hello" 4))
          #x6F)  ; 'o' = 111 = #x6F

;; Test 5: string= equal strings
(run-test "string=-equal"
          '((if (string= "foo" "foo") #x1 #x0))
          #x1)

;; Test 6: string= different strings
(run-test "string=-different"
          '((if (string= "foo" "bar") #x1 #x0))
          #x0)

;; Test 7: string= different lengths
(run-test "string=-diff-length"
          '((if (string= "foo" "foobar") #x1 #x0))
          #x0)

;; Test 8: string-upcase
(run-test "string-upcase-lower"
          '((string-ref (string-upcase "abc") 0))
          #x41)  ; 'A' = 65 = #x41

;; Test 9: string-downcase
(run-test "string-downcase-upper"
          '((string-ref (string-downcase "ABC") 0))
          #x61)  ; 'a' = 97 = #x61

;; Test 10: string-concat two strings
(run-test "string-concat-two"
          '((string-length (string-concat "foo" "bar")))
          #x6)

;; Test 11: string-concat content check
(run-test "string-concat-content"
          '((string-ref (string-concat "ab" "cd") 2))
          #x63)  ; 'c' = 99 = #x63

;; Test 12: subseq string
(run-test "subseq-string"
          '((string-length (subseq "hello" 1 4)))
          #x3)  ; "ell"

;; Test 13: subseq content
(run-test "subseq-content"
          '((string-ref (subseq "hello" 1 4) 0))
          #x65)  ; 'e' = 101 = #x65

;; Test 14: write-to-string fixnum
(run-test "write-to-string-fixnum"
          '((string-length (write-to-string #x7B)))  ; 123 -> "123"
          #x3)

;; Test 15: stringp predicate true
(run-test "stringp-true"
          '((if (stringp "hello") #x1 #x0))
          #x1)

;; Test 16: stringp predicate false
(run-test "stringp-false"
          '((if (stringp #x42) #x1 #x0))
          #x0)

(format t "~%=== All String Function Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
