#!/usr/bin/env sbcl --script
;;; Tests for IEEE 754 float support

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(defun run-float-test (name forms)
  "Test that result has float tag (0x7)"
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= (logand result #xF) #x7))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected float tag 0x7, got tag ~X)~%" name (logand result #xF))
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== IEEE 754 Float Tests ===~%~%")

;; Test 1: floatp returns true for floats
(run-test "floatp-true"
          '((floatp (float #x1)))
          #x1)

;; Test 2: floatp returns false for fixnums
(run-test "floatp-false-fixnum"
          '((floatp #x5))
          #x0)

;; Test 3: float converts fixnum to float (check tag)
(run-float-test "float-conversion"
                '((float #x5)))

;; Test 4: float+ adds two floats
(run-float-test "float-add"
                '((float+ (float #x2) (float #x3))))

;; Test 5: float- subtracts floats
(run-float-test "float-subtract"
                '((float- (float #x5) (float #x2))))

;; Test 6: float* multiplies floats
(run-float-test "float-multiply"
                '((float* (float #x3) (float #x4))))

;; Test 7: float/ divides floats
(run-float-test "float-divide"
                '((float/ (float #xA) (float #x2))))

;; Test 8: float< in conditional (true case)
(run-test "float-lt-true"
          '((if (float< (float #x2) (float #x5)) #x1 #x0))
          #x1)

;; Test 9: float< in conditional (false case)
(run-test "float-lt-false"
          '((if (float< (float #x5) (float #x2)) #x1 #x0))
          #x0)

;; Test 10: float> in conditional (true case)
(run-test "float-gt-true"
          '((if (float> (float #x5) (float #x2)) #x1 #x0))
          #x1)

;; Test 11: float> in conditional (false case)
(run-test "float-gt-false"
          '((if (float> (float #x2) (float #x5)) #x1 #x0))
          #x0)

;; Test 12: float= in conditional (true case)
(run-test "float-eq-true"
          '((if (float= (float #x5) (float #x5)) #x1 #x0))
          #x1)

;; Test 13: float= in conditional (false case)
(run-test "float-eq-false"
          '((if (float= (float #x5) (float #x3)) #x1 #x0))
          #x0)

;; Test 14: float-truncate converts float back to fixnum
(run-test "float-truncate"
          '((float-truncate (float #x7)))
          #x7) ; untagged 7

;; Test 15: float<= in conditional
(run-test "float-le-true"
          '((if (float<= (float #x3) (float #x5)) #x1 #x0))
          #x1)

;; Test 16: float>= in conditional
(run-test "float-ge-true"
          '((if (float>= (float #x5) (float #x3)) #x1 #x0))
          #x1)

;; Test 17: Chained float operations
(run-float-test "float-chained"
                '((float+ (float* (float #x2) (float #x3))
                          (float #x4))))

;; Test 18: Float with let binding
(run-float-test "float-let-binding"
                '((let ((x (float #x3))
                        (y (float #x4)))
                    (float+ x y))))

;; Test 19: Float in nested conditional
(run-test "float-in-conditional"
          '((if (float< (float #x2) (float #x5))
                (if (float> (float #x5) (float #x3))
                    #x1
                    #x0)
                #x0))
          #x1)

;; Test 20: Float? predicate (Habu style)
(run-test "float?-true"
          '((float? (float #xA)))
          #x1)

(format t "~%=== All Float Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
