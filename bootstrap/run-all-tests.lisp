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
    (assert-compiles-both '(logand (logior 8 4) 15))))

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
    (assert-compiles-both '(max (min 10 20) (abs -15)))))

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

  (test-case predicate-in-conditional
    (assert-compiles-both '(if (zerop (mod 10 2)) (evenp 10) (oddp 10)))))

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
    (assert-compiles-both '(if (/= 5 10) 100 200))))

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
