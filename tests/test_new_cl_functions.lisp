#!/usr/bin/env sbcl --script
;;; Tests for newly added CL functions: integerp, characterp, nreverse, nconc, butlast, position, equal, truncate

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== New CL Functions Tests ===~%~%")

;; Test 1: integerp - true for fixnum
(run-test "integerp-true"
          '((integerp #x42))
          #x1)

;; Test 2: integerp - false for cons
(run-test "integerp-false"
          '((integerp (cons #x1 #x2)))
          #x0)

;; Test 3: characterp - true for fixnum (chars are fixnums in Habu)
(run-test "characterp-fixnum"
          '((characterp #x41))
          #x1)

;; Test 4: nreverse - destructive reverse
(run-test "nreverse-basic"
          '((let ((lst (cons #x1 (cons #x2 (cons #x3 #x0)))))
              (car (nreverse lst))))
          #x3)

;; Test 5: nconc - destructive append
(run-test "nconc-basic"
          '((let ((a (cons #x1 (cons #x2 #x0)))
                  (b (cons #x3 (cons #x4 #x0))))
              (length (nconc a b))))
          #x4)

;; Test 6: nconc - with nil first
(run-test "nconc-nil-first"
          '((car (nconc #x0 (cons #x5 #x0))))
          #x5)

;; Test 7: butlast - remove last element
(run-test "butlast-basic"
          '((length (butlast (cons #x1 (cons #x2 (cons #x3 #x0))))))
          #x2)

;; Test 8: position - find element
(run-test "position-found"
          '((position #x2 (cons #x1 (cons #x2 (cons #x3 #x0)))))
          #x1)

;; Test 9: position - not found returns nil (0)
(run-test "position-not-found"
          '((position #x5 (cons #x1 (cons #x2 #x0))))
          #x0)

;; Test 10: equal - same fixnums
(run-test "equal-fixnums"
          '((equal #x5 #x5))
          #x1)

;; Test 11: equal - different fixnums
(run-test "equal-diff-fixnums"
          '((equal #x5 #x6))
          #x0)

;; Test 12: equal - lists
(run-test "equal-lists"
          '((equal (cons #x1 (cons #x2 #x0))
                   (cons #x1 (cons #x2 #x0))))
          #x1)

;; Test 13: truncate - basic division
(run-test "truncate-basic"
          '((truncate #x14 #x5))
          #x4)  ; 20 / 5 = 4

;; Test 14: truncate - with remainder
(run-test "truncate-remainder"
          '((truncate #x15 #x5))
          #x4)  ; 21 / 5 = 4

(format t "~%=== All New CL Functions Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
