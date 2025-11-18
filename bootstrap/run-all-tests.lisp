;;;; Comprehensive Test Suite for Habu Compiler
;;;; Uses test harness to run all compiler tests

(load "test-harness.lisp")
(in-package :habu-compiler)

(reset-test-stats)

(format t "~%")
(format t "~A~%" (color-blue "========================================="))
(format t "~A~%" (color-blue "  Habu Compiler Test Suite"))
(format t "~A~%" (color-blue "========================================="))

;;; Test literals
(test-group "Literals"
  (test-case fixnum-small
    (assert-compiles-both '42))

  (test-case fixnum-large
    (assert-compiles-both '1000))

  (test-case fixnum-negative
    (assert-compiles-both '-100))

  (test-case fixnum-zero
    (assert-compiles-both '0)))

;;; Test arithmetic operators
(test-group "Arithmetic"
  (test-case addition
    (assert-compiles-both '(+ 10 20)))

  (test-case subtraction
    (assert-compiles-both '(- 100 50)))

  (test-case multiplication
    (assert-compiles-both '(* 6 7)))

  (test-case division
    (assert-compiles-both '(/ 100 5)))

  (test-case modulo
    (assert-compiles-both '(mod 17 5)))

  (test-case remainder
    (assert-compiles-both '(rem 17 5)))

  (test-case nested-arithmetic
    (assert-compiles-both '(+ (* 3 4) (/ 20 2)))))

;;; Test comparison operators
(test-group "Comparison"
  (test-case less-than
    (assert-compiles-both '(< 5 10)))

  (test-case greater-than
    (assert-compiles-both '(> 20 15)))

  (test-case equal
    (assert-compiles-both '(= 42 42)))

  (test-case less-or-equal
    (assert-compiles-both '(<= 10 10)))

  (test-case greater-or-equal
    (assert-compiles-both '(>= 15 10))))

;;; Test boolean operators
(test-group "Boolean Operators"
  (test-case not-operator
    (assert-compiles-both '(not 0)))

  (test-case and-operator
    (assert-compiles-both '(and 1 2 3)))

  (test-case or-operator
    (assert-compiles-both '(or 0 0 5)))

  (test-case and-short-circuit
    (assert-compiles-both '(and (< 5 10) (> 20 15))))

  (test-case or-short-circuit
    (assert-compiles-both '(or (< 10 5) (= 3 3))))

  (test-case nested-boolean
    (assert-compiles-both '(and (or 0 1) (not 0)))))

;;; Test conditionals
(test-group "Conditionals"
  (test-case if-simple
    (assert-compiles-both '(if 1 100 200)))

  (test-case if-with-test
    (assert-compiles-both '(if (< 5 10) 42 0)))

  (test-case if-nested
    (assert-compiles-both '(if (> 20 30) 1 (+ 10 20))))

  (test-case cond-simple
    (assert-compiles-both '(cond ((< 5 10) 100) (t 200))))

  (test-case cond-multiple
    (assert-compiles-both '(cond ((< 5 3) 100) ((< 5 7) 200) (t 300))))

  (test-case when-simple
    (assert-compiles-both '(when (< 5 10) 42)))

  (test-case unless-simple
    (assert-compiles-both '(unless (> 5 10) 42))))

;;; Test variables and let
(test-group "Variables and Let"
  (test-case let-single
    (assert-compiles-both '(let ((x 42)) x)))

  (test-case let-with-arithmetic
    (assert-compiles-both '(let ((x 10)) (+ x 20))))

  (test-case let-multiple-bindings
    (assert-compiles-both '(let ((x 5) (y 10)) (+ x y))))

  (test-case let-nested
    (assert-compiles-both '(let ((x 10)) (let ((y 20)) (+ x y)))))

  (test-case let-with-if
    (assert-compiles-both '(let ((x 5)) (if (< x 10) 100 200))))

  (test-case let-complex
    (assert-compiles-both '(let ((a 3) (b 4)) (* (+ a b) 2)))))

;;; Test let* (sequential bindings)
(test-group "Let* (Sequential Bindings)"
  (test-case let*-simple
    (assert-compiles-both '(let* ((x 1) (y 2)) (+ x y))))

  (test-case let*-dependent
    (assert-compiles-both '(let* ((x 5) (y (+ x 1))) y)))

  (test-case let*-multiple-dependent
    (assert-compiles-both '(let* ((x 1) (y (+ x 1)) (z (+ y 1))) z)))

  (test-case let*-empty
    (assert-compiles-both '(let* () 42)))

  (test-case let*-nested
    (assert-compiles-both '(let* ((a 1) (b 2)) (let* ((c 3) (d (+ a b c))) d)))))

;;; Test lambda and functions
(test-group "Lambda and Functions"
  (test-case lambda-identity
    (assert-compiles-both '((lambda (x) x) 42)))

  (test-case lambda-arithmetic
    (assert-compiles-both '((lambda (x) (* x 2)) 21)))

  (test-case lambda-multiple-params
    (assert-compiles-both '((lambda (x y) (+ x y)) 10 20)))

  (test-case lambda-complex
    (assert-compiles-both '((lambda (a b) (+ (* a a) (* b b))) 3 4)))

  (test-case lambda-nested
    (assert-compiles-both '((lambda (x) ((lambda (y) (+ x y)) 20)) 10)))

  (test-case lambda-with-if
    (assert-compiles-both '((lambda (n) (if (< n 2) n (+ n n))) 10))))

;;; Test progn and begin
(test-group "Progn (Sequential Evaluation)"
  (test-case progn-simple
    (assert-compiles-both '(progn 1 2 3)))

  (test-case progn-with-arithmetic
    (assert-compiles-both '(progn (+ 1 2) (* 3 4) (- 10 5))))

  (test-case progn-in-lambda
    (assert-compiles-both '((lambda (x) (progn (+ x 1) (* x 2))) 10)))

  (test-case begin-alias
    (assert-compiles-both '(begin 1 2 3)))

  (test-case begin-with-expressions
    (assert-compiles-both '(begin (+ 1 2) (* 3 4)))))

;;; Test quote
(test-group "Quote"
  (test-case quote-integer
    (assert-compiles-both ''42))

  (test-case quote-zero
    (assert-compiles-both ''0))

  (test-case quote-in-expression
    (assert-compiles-both '(+ '10 '20)))

  (test-case quote-in-let
    (assert-compiles-both '(let ((x '5)) (+ x '10)))))

;;; Note: car and cdr compile but require runtime cons cells to be useful
;;; They are tested separately in test_car_cdr.lisp

;;; Test bitwise operators
(test-group "Bitwise Operators"
  (test-case logand-simple
    (assert-compiles-both '(logand 15 7)))

  (test-case logior-simple
    (assert-compiles-both '(logior 8 4)))

  (test-case logxor-simple
    (assert-compiles-both '(logxor 15 9)))

  (test-case lognot-simple
    (assert-compiles-both '(lognot 0)))

  (test-case ash-left-shift
    (assert-compiles-both '(ash 5 2)))

  (test-case ash-right-shift
    (assert-compiles-both '(ash 20 -2)))

  (test-case bitwise-nested
    (assert-compiles-both '(logand (logior 8 4) 15)))

  (test-case logcount-simple
    (assert-compiles-both '(logcount 7)))

  (test-case logcount-zero
    (assert-compiles-both '(logcount 0)))

  (test-case logcount-power-of-two
    (assert-compiles-both '(logcount 16)))

  (test-case logtest-true
    (assert-compiles-both '(logtest 7 3)))

  (test-case logtest-false
    (assert-compiles-both '(logtest 8 4)))

  (test-case logbitp-set
    (assert-compiles-both '(logbitp 2 7)))

  (test-case logbitp-unset
    (assert-compiles-both '(logbitp 3 7)))

  (test-case lognand-simple
    (assert-compiles-both '(lognand 15 7)))

  (test-case lognor-simple
    (assert-compiles-both '(lognor 8 4)))

  (test-case logeqv-simple
    (assert-compiles-both '(logeqv 15 7)))

  (test-case logandc1-simple
    ; (logandc1 x y) => (logand (lognot x) y) = ~x & y
    (assert-compiles-both '(logandc1 15 7)))

  (test-case logandc2-simple
    ; (logandc2 x y) => (logand x (lognot y)) = x & ~y
    (assert-compiles-both '(logandc2 15 7)))

  (test-case logorc1-simple
    ; (logorc1 x y) => (logior (lognot x) y) = ~x | y
    (assert-compiles-both '(logorc1 8 4)))

  (test-case logorc2-simple
    ; (logorc2 x y) => (logior x (lognot y)) = x | ~y
    (assert-compiles-both '(logorc2 8 4))))

;;; Test numeric operators
(test-group "Numeric Operators"
  (test-case min-simple
    (assert-compiles-both '(min 5 10)))

  (test-case max-simple
    (assert-compiles-both '(max 5 10)))

  (test-case abs-negative
    (assert-compiles-both '(abs -10)))

  (test-case abs-positive
    (assert-compiles-both '(abs 10)))

  (test-case 1+-simple
    (assert-compiles-both '(1+ 5)))

  (test-case 1--simple
    (assert-compiles-both '(1- 5)))

  (test-case numeric-nested
    (assert-compiles-both '(max (min 10 20) (abs -15))))

  (test-case gcd-simple
    (assert-compiles-both '(gcd 12 8)))

  (test-case gcd-coprime
    (assert-compiles-both '(gcd 17 19)))

  (test-case gcd-with-zero
    (assert-compiles-both '(gcd 42 0)))

  (test-case gcd-negative
    (assert-compiles-both '(gcd -12 8)))

  (test-case gcd-both-negative
    (assert-compiles-both '(gcd -24 -36)))

  (test-case isqrt-perfect-square
    (assert-compiles-both '(isqrt 16)))

  (test-case isqrt-non-perfect
    (assert-compiles-both '(isqrt 17)))

  (test-case isqrt-zero
    (assert-compiles-both '(isqrt 0)))

  (test-case isqrt-one
    (assert-compiles-both '(isqrt 1)))

  (test-case isqrt-large
    (assert-compiles-both '(isqrt 100)))

  (test-case integer-length-zero
    (assert-compiles-both '(integer-length 0)))

  (test-case integer-length-one
    (assert-compiles-both '(integer-length 1)))

  (test-case integer-length-seven
    (assert-compiles-both '(integer-length 7)))

  (test-case integer-length-eight
    (assert-compiles-both '(integer-length 8)))

  (test-case integer-length-negative
    (assert-compiles-both '(integer-length -8)))

  (test-case lcm-simple
    (assert-compiles-both '(lcm 4 6)))

  (test-case lcm-coprime
    (assert-compiles-both '(lcm 5 7)))

  (test-case lcm-with-zero
    (assert-compiles-both '(lcm 12 0)))

  (test-case lcm-negative
    (assert-compiles-both '(lcm -4 6)))

  (test-case lcm-both-negative
    (assert-compiles-both '(lcm -6 -8)))

  (test-case expt-simple
    (assert-compiles-both '(expt 2 3)))

  (test-case expt-zero-exponent
    (assert-compiles-both '(expt 5 0)))

  (test-case expt-one-exponent
    (assert-compiles-both '(expt 7 1)))

  (test-case expt-larger
    (assert-compiles-both '(expt 3 4)))

  (test-case expt-base-one
    (assert-compiles-both '(expt 1 10)))

  (test-case floor-positive
    (assert-compiles-both '(floor 42)))

  (test-case floor-negative
    (assert-compiles-both '(floor -17)))

  (test-case floor-zero
    (assert-compiles-both '(floor 0)))

  (test-case ceiling-positive
    (assert-compiles-both '(ceiling 42)))

  (test-case ceiling-negative
    (assert-compiles-both '(ceiling -17)))

  (test-case ceiling-zero
    (assert-compiles-both '(ceiling 0)))

  (test-case truncate-positive
    (assert-compiles-both '(truncate 42)))

  (test-case truncate-negative
    (assert-compiles-both '(truncate -17)))

  (test-case truncate-zero
    (assert-compiles-both '(truncate 0)))

  (test-case round-positive
    (assert-compiles-both '(round 42)))

  (test-case round-negative
    (assert-compiles-both '(round -17)))

  (test-case round-zero
    (assert-compiles-both '(round 0)))

  (test-case ffloor-positive
    (assert-compiles-both '(ffloor 7 3)))

  (test-case ffloor-negative-dividend
    (assert-compiles-both '(ffloor -7 3)))

  (test-case ffloor-negative-divisor
    (assert-compiles-both '(ffloor 7 -3)))

  (test-case fceiling-positive
    (assert-compiles-both '(fceiling 7 3)))

  (test-case fceiling-negative-dividend
    (assert-compiles-both '(fceiling -7 3)))

  (test-case fceiling-negative-divisor
    (assert-compiles-both '(fceiling 7 -3)))

  (test-case ftruncate-positive
    (assert-compiles-both '(ftruncate 7 3)))

  (test-case ftruncate-negative-dividend
    (assert-compiles-both '(ftruncate -7 3)))

  (test-case ftruncate-negative-divisor
    (assert-compiles-both '(ftruncate 7 -3)))

  (test-case fround-positive
    (assert-compiles-both '(fround 7 3)))

  (test-case fround-negative-dividend
    (assert-compiles-both '(fround -7 3)))

  (test-case fround-negative-divisor
    (assert-compiles-both '(fround 7 -3))))

;;; Test predicates
(test-group "Predicates"
  (test-case zerop-zero
    (assert-compiles-both '(zerop 0)))

  (test-case zerop-nonzero
    (assert-compiles-both '(zerop 5)))

  (test-case plusp-positive
    (assert-compiles-both '(plusp 5)))

  (test-case plusp-negative
    (assert-compiles-both '(plusp -5)))

  (test-case minusp-negative
    (assert-compiles-both '(minusp -5)))

  (test-case minusp-positive
    (assert-compiles-both '(minusp 5)))

  (test-case evenp-even
    (assert-compiles-both '(evenp 4)))

  (test-case evenp-odd
    (assert-compiles-both '(evenp 5)))

  (test-case oddp-odd
    (assert-compiles-both '(oddp 5)))

  (test-case oddp-even
    (assert-compiles-both '(oddp 4)))

  (test-case signum-negative
    (assert-compiles-both '(signum -5)))

  (test-case signum-zero
    (assert-compiles-both '(signum 0)))

  (test-case signum-positive
    (assert-compiles-both '(signum 5)))

  (test-case predicate-in-conditional
    (assert-compiles-both '(if (zerop (mod 10 2)) (evenp 10) (oddp 10))))

  (test-case numberp-fixnum
    (assert-compiles-both '(numberp 42)))

  (test-case integerp-fixnum
    (assert-compiles-both '(integerp 42)))

  (test-case atom-fixnum
    (assert-compiles-both '(atom 42)))

  (test-case listp-fixnum
    (assert-compiles-both '(listp 42)))

  (test-case consp-fixnum
    (assert-compiles-both '(consp 42)))

  (test-case symbolp-fixnum
    (assert-compiles-both '(symbolp 42))))

;;; Test utility functions
(test-group "Utility Functions"
  (test-case null-zero
    (assert-compiles-both '(null 0)))

  (test-case null-nonzero
    (assert-compiles-both '(null 5)))

  (test-case identity-simple
    (assert-compiles-both '(identity 42)))

  (test-case identity-expression
    (assert-compiles-both '(identity (+ 10 20))))

  (test-case null-in-conditional
    (assert-compiles-both '(if (null 0) 100 200))))

;;; Test case pattern matching
(test-group "Case Pattern Matching"
  (test-case case-simple
    (assert-compiles-both '(case 2 (1 100) (2 200) (t 300))))

  (test-case case-multiple-keys
    (assert-compiles-both '(case 3 ((1 2) 100) ((3 4) 200) (t 300))))

  (test-case case-with-expression
    (assert-compiles-both '(case (+ 1 1) (1 100) (2 200) (t 300)))))

;;; Test defun (global function definitions)
(test-group "Defun (Global Functions)"
  (test-case defun-simple
    ;; Clear function table first
    (clrhash *function-table*)
    (assert-compiles-both '(defun square (x) (* x x))))

  (test-case defun-call
    (assert-compiles-both '(square 5)))

  (test-case defun-multiple-params
    (assert-compiles-both '(defun add (a b) (+ a b))))

  (test-case defun-call-multiple
    (assert-compiles-both '(add 10 20)))

  (test-case defun-with-if
    (assert-compiles-both '(defun abs-val (n) (if (< n 0) (- 0 n) n))))

  (test-case defun-nested-call
    (assert-compiles-both '(defun double (x) (+ x x)))
    (assert-compiles-both '(defun quadruple (x) (double (double x))))
    (assert-compiles-both '(quadruple 3)))

  (test-case defun-with-let
    (assert-compiles-both '(defun pythag (a b) (let ((a2 (* a a)) (b2 (* b b))) (+ a2 b2)))))

  (test-case defun-zero-params
    (assert-compiles-both '(defun answer () 42))
    (assert-compiles-both '(answer))))

;;; Test setq (variable mutation)
(test-group "Setq (Variable Mutation)"
  (test-case setq-simple
    (assert-compiles-both '(let ((x 5)) (setq x 10) x)))

  (test-case setq-with-expression
    (assert-compiles-both '(let ((x 5)) (setq x (+ x 1)) x)))

  (test-case setq-multiple
    (assert-compiles-both '(let ((x 1) (y 2)) (setq x 10) (setq y 20) (+ x y))))

  (test-case setq-in-progn
    (assert-compiles-both '(let ((x 5)) (progn (setq x 10) (setq x (* x 2)) x))))

  (test-case setq-in-conditional
    (assert-compiles-both '(let ((x 5)) (if (< x 10) (setq x 100) (setq x 200)) x)))

  (test-case setq-nested-let
    (assert-compiles-both '(let ((x 1)) (let ((y 2)) (setq x (+ x y)) x))))

  (test-case setq-in-lambda
    (assert-compiles-both '((lambda (x) (setq x (* x x)) x) 5)))

  (test-case setq-accumulator
    (assert-compiles-both '(let ((sum 0)) (setq sum (+ sum 1)) (setq sum (+ sum 2)) (setq sum (+ sum 3)) sum))))

;;; Test incf and decf (increment/decrement macros)
(test-group "Incf/Decf (Increment/Decrement)"
  (test-case incf-simple
    (assert-compiles-both '(let ((x 5)) (incf x) x)))

  (test-case incf-with-delta
    (assert-compiles-both '(let ((x 10)) (incf x 5) x)))

  (test-case decf-simple
    (assert-compiles-both '(let ((x 10)) (decf x) x)))

  (test-case decf-with-delta
    (assert-compiles-both '(let ((x 20)) (decf x 7) x)))

  (test-case incf-decf-combined
    (assert-compiles-both '(let ((x 10)) (incf x 5) (decf x 3) x))))

;;; Test additional comparison operators
(test-group "Additional Comparison Operators"
  (test-case not-equal-true
    (assert-compiles-both '(/= 5 10)))

  (test-case not-equal-false
    (assert-compiles-both '(/= 5 5)))

  (test-case equal-alias
    (assert-compiles-both '(equal 10 10)))

  (test-case not-equal-in-conditional
    (assert-compiles-both '(if (/= 5 10) 100 200)))

  (test-case eql-equal
    (assert-compiles-both '(eql 42 42)))

  (test-case eql-not-equal
    (assert-compiles-both '(eql 10 20)))

  (test-case eq-equal
    (assert-compiles-both '(eq 42 42)))

  (test-case eq-not-equal
    (assert-compiles-both '(eq 10 20))))

;;; Test complex expressions
(test-group "Complex Expressions"
  (test-case arithmetic-nested
    (assert-compiles-both '(+ (* 3 4) (/ 20 2))))

  (test-case if-with-nested-arithmetic
    (assert-compiles-both '(if (< (* 2 3) (+ 5 2)) 100 200)))

  (test-case let-with-complex-body
    (assert-compiles-both '(let ((a 3) (b 4)) (* (+ a b) 2))))

  (test-case nested-let-if
    (assert-compiles-both '(let ((x 10))
                             (if (< x 20)
                                 (+ x 5)
                                 (* x 2)))))

  (test-case let-if-branches
    (assert-compiles-both '(let ((a 5) (b 3))
                             (if (> a b)
                                 (- a b)
                                 (- b a)))))

  (test-case lambda-with-conditional
    (assert-compiles-both '((lambda (n)
                              (if (< n 2)
                                  n
                                  (+ n n)))
                            10))))

;;; Test macros
(test-group "Macros"
  (test-case defmacro-simple
    (clrhash *macro-table*)
    (assert-compiles-both '(defmacro square (x) (* x x))))

  (test-case macro-expansion
    (clrhash *macro-table*)
    (compile-expression '(defmacro double (x) (+ x x)))
    (assert-compiles-both '(double 5)))

  (test-case macro-multiple-params
    (clrhash *macro-table*)
    (compile-expression '(defmacro add-and-double (a b) (* (+ a b) 2)))
    (assert-compiles-both '(add-and-double 3 4)))

  (test-case macro-conditional
    (clrhash *macro-table*)
    (compile-expression '(defmacro abs-diff (a b) (if (> a b) (- a b) (- b a))))
    (assert-compiles-both '(abs-diff 10 5)))

  (test-case macro-nested
    (clrhash *macro-table*)
    (compile-expression '(defmacro square (x) (* x x)))
    (compile-expression '(defmacro quad (x) (square (square x))))
    (assert-compiles-both '(quad 2))))

;;; Test constant folding optimization
(test-group "Constant Folding"
  (test-case constant-arithmetic
    ; (+ 2 3) should compile to the constant 5
    (assert-compiles-both '(+ 2 3)))

  (test-case constant-nested
    ; Nested constants should all be folded
    (assert-compiles-both '(* (+ 2 3) (- 10 4))))

  (test-case constant-comparison
    ; Constant comparisons should be folded
    (assert-compiles-both '(< 3 5)))

  (test-case constant-if-true
    ; (if 1 x y) with constant condition should optimize
    (assert-compiles-both '(if 1 42 99)))

  (test-case constant-if-false
    ; (if 0 x y) should eliminate dead branch
    (assert-compiles-both '(if 0 42 99)))

  (test-case mixed-constant-variable
    ; Mix of constants and variables - only constants folded
    (assert-compiles-both '(let ((x 10)) (+ (* 2 3) x)))))

;;; Test edge cases
(test-group "Edge Cases"
  (test-case large-fixnum
    ; Test large fixnum values
    (assert-compiles-both '1000000))

  (test-case negative-large
    ; Test large negative values
    (assert-compiles-both '-1000000))

  (test-case division-truncation
    ; Test that integer division truncates
    (assert-compiles-both '(/ 7 3)))

  (test-case modulo-negative-dividend
    ; Test modulo with negative dividend
    (assert-compiles-both '(mod -10 3)))

  (test-case modulo-negative-divisor
    ; Test modulo with negative divisor
    (assert-compiles-both '(mod 10 -3)))

  (test-case comparison-equal-values
    ; Test comparison with equal values
    (assert-compiles-both '(< 5 5)))

  (test-case nested-arithmetic-deep
    ; Test deeply nested arithmetic
    (assert-compiles-both '(+ (+ (+ 1 2) (+ 3 4)) (+ (+ 5 6) (+ 7 8)))))

  (test-case bitwise-zero
    ; Test bitwise operations with zero
    (assert-compiles-both '(logand 0 -1)))

  (test-case bitwise-all-ones
    ; Test bitwise operations with all bits set
    (assert-compiles-both '(logior 0 -1)))

  (test-case shift-by-zero
    ; Test shifting by zero
    (assert-compiles-both '(ash 42 0)))

  (test-case shift-negative
    ; Test right shift (negative amount)
    (assert-compiles-both '(ash 100 -2)))

  (test-case gcd-coprime
    ; Test GCD of coprime numbers
    (assert-compiles-both '(gcd 17 19)))

  (test-case gcd-with-zero
    ; Test GCD with zero
    (assert-compiles-both '(gcd 42 0)))

  (test-case lcm-one
    ; Test LCM with 1
    (assert-compiles-both '(lcm 42 1)))

  (test-case isqrt-perfect-square
    ; Test integer square root of perfect square
    (assert-compiles-both '(isqrt 144)))

  (test-case isqrt-non-perfect
    ; Test integer square root rounds down
    (assert-compiles-both '(isqrt 145)))

  (test-case let-shadowing
    ; Test variable shadowing in nested let
    (assert-compiles-both '(let ((x 5)) (let ((x 10)) x))))

  (test-case if-nested-deep
    ; Test deeply nested conditionals
    (assert-compiles-both '(if (< 1 2) (if (< 2 3) (if (< 3 4) 100 200) 300) 400))))

;;; Test algebraic simplifications
(test-group "Algebraic Simplifications"
  (test-case multiply-by-zero
    ; (* x 0) should simplify to 0
    (assert-compiles-both '(let ((x 5)) (* x 0))))

  (test-case multiply-by-one
    ; (* x 1) should simplify to x
    (assert-compiles-both '(let ((x 5)) (* x 1))))

  (test-case add-zero
    ; (+ x 0) should simplify to x
    (assert-compiles-both '(let ((x 5)) (+ x 0))))

  (test-case subtract-zero
    ; (- x 0) should simplify to x
    (assert-compiles-both '(let ((x 5)) (- x 0))))

  (test-case divide-by-one
    ; (/ x 1) should simplify to x
    (assert-compiles-both '(let ((x 5)) (/ x 1))))

  (test-case logand-with-zero
    ; (logand x 0) should simplify to 0
    (assert-compiles-both '(let ((x 5)) (logand x 0))))

  (test-case logior-with-zero
    ; (logior x 0) should simplify to x
    (assert-compiles-both '(let ((x 5)) (logior x 0))))

  (test-case logxor-with-zero
    ; (logxor x 0) should simplify to x
    (assert-compiles-both '(let ((x 5)) (logxor x 0)))))

;;; C[ad]r combinations (cadr, caddr, etc.) are implemented via expansion
;;; to nested car/cdr calls. Tests are deferred until runtime heap integration
;;; is complete, as car/cdr/cons require runtime support in compiled code.
;;; The expansion mechanism is in place and ready to use.

;;; Test utility functions
(test-group "Utility Functions"
  (test-case square-simple
    ; (square x) => (* x x)
    (assert-compiles-both '(square 5)))

  (test-case square-expression
    ; Should bind to temp var to avoid double evaluation
    (assert-compiles-both '(square (+ 3 2))))

  (test-case clamp-within-range
    ; (clamp x low high) => (max low (min high x))
    (assert-compiles-both '(clamp 5 1 10)))

  (test-case clamp-below-range
    (assert-compiles-both '(clamp -5 0 10)))

  (test-case clamp-above-range
    (assert-compiles-both '(clamp 15 0 10)))

  (test-case between-true
    ; (between x low high) => (and (>= x low) (<= x high))
    (assert-compiles-both '(between 5 1 10)))

  (test-case between-false-low
    (assert-compiles-both '(between 0 1 10)))

  (test-case between-false-high
    (assert-compiles-both '(between 15 1 10)))

  (test-case neg-simple
    ; (neg x) => (- x)
    (assert-compiles-both '(neg 5)))

  (test-case let1-simple
    ; (let1 var val body) => (let ((var val)) body)
    (assert-compiles-both '(let1 x 10 (+ x 5)))))

;;; Test Scheme-style aliases
(test-group "Scheme-style Aliases"
  (test-case zero?-true
    (assert-compiles-both '(zero? 0)))

  (test-case zero?-false
    (assert-compiles-both '(zero? 5)))

  (test-case positive?-true
    (assert-compiles-both '(positive? 5)))

  (test-case positive?-false
    (assert-compiles-both '(positive? -5)))

  (test-case negative?-true
    (assert-compiles-both '(negative? -5)))

  (test-case negative?-false
    (assert-compiles-both '(negative? 5)))

  (test-case even?-true
    (assert-compiles-both '(even? 4)))

  (test-case even?-false
    (assert-compiles-both '(even? 5)))

  (test-case odd?-true
    (assert-compiles-both '(odd? 5)))

  (test-case odd?-false
    (assert-compiles-both '(odd? 4)))

  (test-case number?-true
    (assert-compiles-both '(number? 42))))

;;; Test power-of-2 utilities
(test-group "Power-of-2 Utilities"
  (test-case power-of-2?-true-1
    (assert-compiles-both '(power-of-2? 1)))

  (test-case power-of-2?-true-2
    (assert-compiles-both '(power-of-2? 2)))

  (test-case power-of-2?-true-16
    (assert-compiles-both '(power-of-2? 16)))

  (test-case power-of-2?-true-256
    (assert-compiles-both '(power-of-2? 256)))

  (test-case power-of-2?-false-3
    (assert-compiles-both '(power-of-2? 3)))

  (test-case power-of-2?-false-7
    (assert-compiles-both '(power-of-2? 7)))

  (test-case power-of-2?-false-zero
    (assert-compiles-both '(power-of-2? 0)))

  (test-case power-of-2?-false-negative
    (assert-compiles-both '(power-of-2? -8)))

  (test-case power-of-2?-expression
    (assert-compiles-both '(power-of-2? (+ 8 8))))

  (test-case log2-1
    ; log2(1) = 0
    (assert-compiles-both '(log2 1)))

  (test-case log2-2
    ; log2(2) = 1
    (assert-compiles-both '(log2 2)))

  (test-case log2-16
    ; log2(16) = 4
    (assert-compiles-both '(log2 16)))

  (test-case log2-256
    ; log2(256) = 8
    (assert-compiles-both '(log2 256)))

  (test-case log2-expression
    (assert-compiles-both '(log2 (* 2 8)))))

;;; Test alignment utilities
(test-group "Alignment Utilities"
  (test-case align-up-already-aligned
    ; align-up(16, 8) = 16
    (assert-compiles-both '(align-up 16 8)))

  (test-case align-up-not-aligned
    ; align-up(17, 8) = 24
    (assert-compiles-both '(align-up 17 8)))

  (test-case align-up-zero
    ; align-up(0, 8) = 0
    (assert-compiles-both '(align-up 0 8)))

  (test-case align-up-boundary-16
    ; align-up(23, 16) = 32
    (assert-compiles-both '(align-up 23 16)))

  (test-case align-up-expression
    (assert-compiles-both '(align-up (+ 10 7) 8)))

  (test-case align-down-already-aligned
    ; align-down(16, 8) = 16
    (assert-compiles-both '(align-down 16 8)))

  (test-case align-down-not-aligned
    ; align-down(17, 8) = 16
    (assert-compiles-both '(align-down 17 8)))

  (test-case align-down-zero
    ; align-down(0, 8) = 0
    (assert-compiles-both '(align-down 0 8)))

  (test-case align-down-boundary-16
    ; align-down(23, 16) = 16
    (assert-compiles-both '(align-down 23 16)))

  (test-case align-down-expression
    (assert-compiles-both '(align-down (+ 10 7) 8)))

  (test-case aligned?-true
    ; aligned?(16, 8) = true
    (assert-compiles-both '(aligned? 16 8)))

  (test-case aligned?-false
    ; aligned?(17, 8) = false
    (assert-compiles-both '(aligned? 17 8)))

  (test-case aligned?-zero
    ; aligned?(0, 8) = true
    (assert-compiles-both '(aligned? 0 8)))

  (test-case aligned?-boundary-16
    ; aligned?(32, 16) = true
    (assert-compiles-both '(aligned? 32 16)))

  (test-case aligned?-expression
    (assert-compiles-both '(aligned? (* 4 8) 16))))

;;; Test additional utility functions
(test-group "Additional Utilities"
  (test-case cube-simple
    ; (cube x) => (* x x x)
    (assert-compiles-both '(cube 3)))

  (test-case cube-expression
    (assert-compiles-both '(cube (+ 1 2))))

  (test-case double-simple
    ; (double x) => (* x 2)
    (assert-compiles-both '(double 5)))

  (test-case double-expression
    (assert-compiles-both '(double (+ 3 4))))

  (test-case half-simple
    ; (half x) => (/ x 2)
    (assert-compiles-both '(half 10)))

  (test-case half-expression
    (assert-compiles-both '(half (* 3 4))))

  (test-case avg-simple
    ; (avg x y) => (/ (+ x y) 2)
    (assert-compiles-both '(avg 10 20)))

  (test-case avg-expression
    (assert-compiles-both '(avg (+ 5 5) (* 2 10))))

  (test-case range-simple
    ; (range x y) => (- y x)
    (assert-compiles-both '(range 5 15)))

  (test-case range-expression
    (assert-compiles-both '(range (* 2 3) (+ 10 5)))))

;;; Test bit manipulation utilities
(test-group "Bit Manipulation Utilities"
  (test-case set-bit-simple
    ; (set-bit x n) => (logior x (ash 1 n))
    ; set-bit(0, 2) = 4
    (assert-compiles-both '(set-bit 0 2)))

  (test-case set-bit-already-set
    ; set-bit(7, 1) = 7 (bit already set)
    (assert-compiles-both '(set-bit 7 1)))

  (test-case set-bit-expression
    (assert-compiles-both '(set-bit (+ 1 2) 3)))

  (test-case clear-bit-simple
    ; (clear-bit x n) => (logand x (lognot (ash 1 n)))
    ; clear-bit(7, 1) = 5
    (assert-compiles-both '(clear-bit 7 1)))

  (test-case clear-bit-already-clear
    ; clear-bit(5, 1) = 5 (bit already clear)
    (assert-compiles-both '(clear-bit 5 1)))

  (test-case clear-bit-expression
    (assert-compiles-both '(clear-bit (* 2 4) 2)))

  (test-case toggle-bit-simple
    ; (toggle-bit x n) => (logxor x (ash 1 n))
    ; toggle-bit(7, 3) = 15
    (assert-compiles-both '(toggle-bit 7 3)))

  (test-case toggle-bit-back
    ; toggle-bit(7, 1) = 5
    (assert-compiles-both '(toggle-bit 7 1)))

  (test-case toggle-bit-expression
    (assert-compiles-both '(toggle-bit (+ 4 3) 2)))

  (test-case mask-simple
    ; (mask n) => (1- (ash 1 n))
    ; mask(3) = 7
    (assert-compiles-both '(mask 3)))

  (test-case mask-8
    ; mask(8) = 255
    (assert-compiles-both '(mask 8)))

  (test-case mask-expression
    (assert-compiles-both '(mask (+ 2 2))))

  (test-case low-bits-simple
    ; (low-bits x n) => (logand x (mask n))
    ; low-bits(255, 4) = 15
    (assert-compiles-both '(low-bits 255 4)))

  (test-case low-bits-extract
    ; low-bits(23, 3) = 7
    (assert-compiles-both '(low-bits 23 3)))

  (test-case low-bits-expression
    (assert-compiles-both '(low-bits (* 8 3) 4)))

  (test-case high-bit?-positive
    ; high-bit? on positive number
    (assert-compiles-both '(high-bit? 10)))

  (test-case high-bit?-negative
    ; high-bit? on negative number
    (assert-compiles-both '(high-bit? -5))))

;;; Test error cases
(test-group "Error Handling"
  (test-case unbound-variable
    (assert-error 'undefined-var))

  (test-case wrong-arity-plus
    (assert-error '(+ 1))))

;;; Print final statistics
(report-test-stats)

;;; Exit with appropriate code
(if (> *test-failed* 0)
    (sb-ext:exit :code 1)
    (sb-ext:exit :code 0))
