;;;; Test Suite for Habu Standard Library
;;;; Tests all stdlib functions with expected results

;;; Test runner helpers
(defun test (name expected actual)
  "Report test result"
  (if (= expected actual)
      (progn
        (print "✓ PASS: ")
        (print name))
      (progn
        (print "✗ FAIL: ")
        (print name)
        (print " Expected: ")
        (print expected)
        (print " Got: ")
        (print actual))))

;;; ============================================
;;; Boolean/Logic Tests
;;; ============================================

(test "not-true" nil (not 1))
(test "not-false" 1 (not nil))
(test "null?-nil" 1 (null? nil))
(test "null?-nonnil" nil (null? 1))

;;; ============================================
;;; Numeric Predicate Tests
;;; ============================================

(test "zero?-zero" 1 (zero? 0))
(test "zero?-nonzero" nil (zero? 5))
(test "positive?-pos" 1 (positive? 5))
(test "positive?-neg" nil (positive? -5))
(test "negative?-neg" 1 (negative? -5))
(test "negative?-pos" nil (negative? 5))
(test "even?-2" 1 (even? 2))
(test "even?-3" nil (even? 3))
(test "odd?-3" 1 (odd? 3))
(test "odd?-2" nil (odd? 2))

;;; ============================================
;;; Numeric Utility Tests
;;; ============================================

(test "abs-pos" 5 (abs 5))
(test "abs-neg" 5 (abs -5))
(test "min-1-2" 1 (min 1 2))
(test "min-2-1" 1 (min 2 1))
(test "max-1-2" 2 (max 1 2))
(test "max-2-1" 2 (max 2 1))
(test "square-3" 9 (square 3))
(test "cube-3" 27 (cube 3))

;;; ============================================
;;; List Utility Tests
;;; ============================================

(test "length-empty" 0 (length nil))
(test "length-1" 1 (length '(1)))
(test "length-3" 3 (length '(1 2 3)))

(test "append-empty" '(1 2) (append nil '(1 2)))
(test "append-nonempty" '(1 2 3 4) (append '(1 2) '(3 4)))

(test "reverse-empty" nil (reverse nil))
(test "reverse-1" '(1) (reverse '(1)))
(test "reverse-3" '(3 2 1) (reverse '(1 2 3)))

(test "nth-0" 1 (nth 0 '(1 2 3)))
(test "nth-1" 2 (nth 1 '(1 2 3)))
(test "nth-2" 3 (nth 2 '(1 2 3)))

(test "last-1" 1 (last '(1)))
(test "last-3" 3 (last '(1 2 3)))

(test "take-0" nil (take 0 '(1 2 3)))
(test "take-2" '(1 2) (take 2 '(1 2 3 4)))
(test "take-too-many" '(1 2) (take 5 '(1 2)))

(test "drop-0" '(1 2 3) (drop 0 '(1 2 3)))
(test "drop-2" '(3 4) (drop 2 '(1 2 3 4)))
(test "drop-too-many" nil (drop 5 '(1 2)))

;;; ============================================
;;; Higher-Order Function Tests
;;; ============================================

(defun double (x) (* 2 x))
(test "map-double" '(2 4 6) (map double '(1 2 3)))

(test "filter-positive" '(1 2) (filter positive? '(-1 1 -2 2)))

(test "fold-sum" 10 (fold + 0 '(1 2 3 4)))
(test "fold-product" 24 (fold * 1 '(1 2 3 4)))

(test "reduce-sum" 10 (reduce + '(1 2 3 4)))
(test "reduce-max" 5 (reduce max '(1 5 3 2)))

;;; ============================================
;;; List Predicate Tests
;;; ============================================

(test "member?-found" 1 (member? 2 '(1 2 3)))
(test "member?-notfound" nil (member? 4 '(1 2 3)))

(test "all?-positive-all" 1 (all? positive? '(1 2 3)))
(test "all?-positive-some" nil (all? positive? '(1 -2 3)))

(test "any?-positive-some" 1 (any? positive? '(-1 2 -3)))
(test "any?-positive-none" nil (any? positive? '(-1 -2 -3)))

;;; ============================================
;;; Numeric Algorithm Tests
;;; ============================================

(test "factorial-0" 1 (factorial 0))
(test "factorial-1" 1 (factorial 1))
(test "factorial-5" 120 (factorial 5))

(test "fibonacci-0" 0 (fibonacci 0))
(test "fibonacci-1" 1 (fibonacci 1))
(test "fibonacci-5" 5 (fibonacci 5))
(test "fibonacci-10" 55 (fibonacci 10))

(test "gcd-12-8" 4 (gcd 12 8))
(test "gcd-15-25" 5 (gcd 15 25))

(test "power-2-0" 1 (power 2 0))
(test "power-2-3" 8 (power 2 3))
(test "power-3-4" 81 (power 3 4))

;;; ============================================
;;; List Construction Tests
;;; ============================================

(test "range-1-3" '(1 2 3) (range 1 3))
(test "range-0-5" '(0 1 2 3 4 5) (range 0 5))

(test "repeat-3-x" '(x x x) (repeat 3 'x))
(test "repeat-0-x" nil (repeat 0 'x))

;;; ============================================
;;; List Processing Tests
;;; ============================================

(test "sum-empty" 0 (sum nil))
(test "sum-1-2-3" 6 (sum '(1 2 3)))

(test "product-empty" 1 (product nil))
(test "product-1-2-3" 6 (product '(1 2 3)))

(defun is-even (x) (even? x))
(test "count-evens" 2 (count is-even '(1 2 3 4)))

(test "zip-two-lists" '((1 . a) (2 . b)) (zip '(1 2) '(a b)))
(test "zip-unequal" '((1 . a)) (zip '(1 2 3) '(a)))

;;; ============================================
;;; Sorting Tests
;;; ============================================

(test "insert-empty" '(5) (insert 5 nil))
(test "insert-middle" '(1 2 5 10) (insert 5 '(1 2 10)))

(test "sort-empty" nil (sort nil))
(test "sort-sorted" '(1 2 3) (sort '(1 2 3)))
(test "sort-reverse" '(1 2 3) (sort '(3 2 1)))
(test "sort-random" '(1 2 3 4 5) (sort '(3 1 4 5 2)))

;;; ============================================
;;; Utility Tests
;;; ============================================

(test "identity-5" 5 (identity 5))
(test "identity-nil" nil (identity nil))

;;; ============================================
;;; Summary
;;; ============================================

(print "")
(print "=== All Standard Library Tests Complete ===")
