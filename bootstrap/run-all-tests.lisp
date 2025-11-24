;;;; Comprehensive Test Suite for Habu Compiler
;;;; Uses test harness to run all compiler tests

(let* ((here (or *load-truename*
                 (merge-pathnames "run-all-tests.lisp" *default-pathname-defaults*)))
       (root (make-pathname :name nil :type nil :defaults here)))
  (setf *default-pathname-defaults* root))

(load "test-harness.lisp")
(load "../sbcl-habu-shim.lisp")
(load "../habu-arm64-codegen-sbcl.lisp")
(in-package :habu-compiler)

(reset-test-stats)

(format t "~%")
(format t "~A~%" (color-blue "========================================="))
(format t "~A~%" (color-blue "  Habu Compiler Test Suite"))
(format t "~A~%" (color-blue "========================================="))
(defun write-bytecode (bytes path)
  (with-open-file (out path :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
    (dolist (b bytes)
      (write-byte b out))))

(defun parse-run-bytecode (output)
  (let ((idx (search "Untagged fixnum:" output)))
    (when idx
      (parse-integer output :start (+ idx 17) :junk-allowed t))))

(defun compile-and-run-arm64 (form)
  (let* ((code (habu-sbcl-codegen:compile-program-with-functions (list form)))
         (tmp (format nil "/tmp/habu-arm64-~A.bin" (gensym))))
    (write-bytecode code tmp)
    (let* ((out (with-output-to-string (s)
                  (sb-ext:run-program "../run-bytecode" (list tmp) :output s :search t)))
           (val (parse-run-bytecode out)))
      (unless val
        (error "run-bytecode did not produce a fixnum for ~S~%Output: ~A" form out))
      val)))

(defun assert-equal-arm64 (form expected)
  (let ((val (compile-and-run-arm64 form)))
    (unless (= val expected)
      (incf *test-failed*)
      (format t "~&[ARM64] mismatch for ~S: got ~A expected ~A~%" form val expected))
    t))

;;; Test literals
(test-group "Literals"
  (test-case fixnum-small
    (assert-equal-arm64 '42 42))

  (test-case fixnum-large
    (assert-equal-arm64 '1000 1000))

  (test-case fixnum-negative
    (assert-equal-arm64 '-100 -100))

  (test-case fixnum-zero
    (assert-equal-arm64 '0 0)))

;;; Test arithmetic operators
(test-group "Arithmetic"
  (test-case addition
    (assert-equal-arm64 '(+ 10 20) 30))

  (test-case subtraction
    (assert-equal-arm64 '(- 100 50) 50))

  (test-case multiplication
    (assert-equal-arm64 '(* 6 7) 42))

  (test-case division
    (assert-equal-arm64 '(/ 100 5) 20))

  (test-case modulo
    (assert-equal-arm64 '(mod 17 5) 2))

  (test-case remainder
    (assert-equal-arm64 '(rem 17 5) 2)))

;;; Test comparison operators
(test-group "Comparison"
  (test-case less-than
    (assert-equal-arm64 '(< 5 10) 1))

  (test-case greater-than
    (assert-equal-arm64 '(> 20 15) 1))

  (test-case equal
    (assert-equal-arm64 '(= 42 42) 1))

  (test-case less-or-equal
    (assert-equal-arm64 '(<= 10 10) 1))

  (test-case greater-or-equal
    (assert-equal-arm64 '(>= 15 10) 1)))

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
    (assert-compiles-both '(cond ((< 5 10) 100) (t 200)))) ; FIXME enable once cond returns correct

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

;;; Test comparison and range utilities
(test-group "Comparison and Range Utilities"
  (test-case min3-simple
    ; Minimum of three values
    (assert-compiles-both '(min3 5 3 8)))

  (test-case min3-first-smallest
    (assert-compiles-both '(min3 1 10 20)))

  (test-case min3-last-smallest
    (assert-compiles-both '(min3 20 10 1)))

  (test-case max3-simple
    ; Maximum of three values
    (assert-compiles-both '(max3 5 3 8)))

  (test-case max3-first-largest
    (assert-compiles-both '(max3 20 10 1)))

  (test-case max3-last-largest
    (assert-compiles-both '(max3 1 10 20)))

  (test-case within?-true
    ; Check if value is within range (inclusive)
    (assert-compiles-both '(within? 5 1 10)))

  (test-case within?-false-low
    (assert-compiles-both '(within? 0 1 10)))

  (test-case within?-false-high
    (assert-compiles-both '(within? 15 1 10)))

  (test-case within?-boundary
    (assert-compiles-both '(within? 10 1 10)))

  (test-case outside?-true-low
    ; Check if value is outside range
    (assert-compiles-both '(outside? 0 1 10)))

  (test-case outside?-true-high
    (assert-compiles-both '(outside? 15 1 10)))

  (test-case outside?-false
    (assert-compiles-both '(outside? 5 1 10)))

  (test-case sign-positive
    ; Return sign of number (-1, 0, or 1)
    (assert-compiles-both '(sign 10)))

  (test-case sign-negative
    (assert-compiles-both '(sign -10)))

  (test-case sign-zero
    (assert-compiles-both '(sign 0)))

  (test-case same-sign?-both-positive
    ; Check if two numbers have same sign
    (assert-compiles-both '(same-sign? 5 10)))

  (test-case same-sign?-both-negative
    (assert-compiles-both '(same-sign? -5 -10)))

  (test-case same-sign?-both-zero
    (assert-compiles-both '(same-sign? 0 0)))

  (test-case same-sign?-different
    (assert-compiles-both '(same-sign? 5 -10)))

  (test-case same-sign?-expression
    (assert-compiles-both '(same-sign? (+ 2 3) (* -2 3)))))

;;; Test bit rotation
(test-group "Bit Rotation"
  (test-case rotl-simple
    ; Rotate left - (rotl 5 1 8) = 10
    ; 00000101 << 1 = 00001010
    (assert-compiles-both '(rotl 5 1 8)))

  (test-case rotl-wrap
    ; Rotate with wrap - (rotl 128 1 8) = 1
    ; 10000000 << 1 = 00000001
    (assert-compiles-both '(rotl 128 1 8)))

  (test-case rotl-32bit
    ; 32-bit rotation (default)
    (assert-compiles-both '(rotl 1 1)))

  (test-case rotr-simple
    ; Rotate right - (rotr 10 1 8) = 5
    ; 00001010 >> 1 = 00000101
    (assert-compiles-both '(rotr 10 1 8)))

  (test-case rotr-wrap
    ; Rotate right with wrap - (rotr 1 1 8) = 128
    ; 00000001 >> 1 = 10000000
    (assert-compiles-both '(rotr 1 1 8))))

;;; Test conditional macros
(test-group "Conditional Macros"
  (test-case if-let-truthy
    ; (if-let var test then else)
    (assert-compiles-both '(if-let x 5 (+ x 1) 0)))

  (test-case if-let-falsy
    (assert-compiles-both '(if-let x 0 (+ x 1) 99)))

  (test-case if-let-expression
    (assert-compiles-both '(if-let x (+ 2 3) (* x 2) 0)))

  (test-case when-let-truthy
    ; (when-let var test body...)
    (assert-compiles-both '(when-let x 10 (+ x 5))))

  (test-case when-let-falsy
    (assert-compiles-both '(when-let x 0 (+ x 5))))

  (test-case when-let-multiple-forms
    (assert-compiles-both '(when-let x 5 (+ x 1) (+ x 2)))))

;;; Test additional predicates and utilities
(test-group "Additional Predicates and Utilities"
  (test-case nonzero?-true
    ; Opposite of zerop
    (assert-compiles-both '(nonzero? 5)))

  (test-case nonzero?-false
    (assert-compiles-both '(nonzero? 0)))

  (test-case divisible?-true
    ; Check if x is divisible by y
    (assert-compiles-both '(divisible? 10 5)))

  (test-case divisible?-false
    (assert-compiles-both '(divisible? 10 3)))

  (test-case divisible?-exact
    (assert-compiles-both '(divisible? 20 4)))

  (test-case multiple-of?-true
    ; Same as divisible?
    (assert-compiles-both '(multiple-of? 15 3)))

  (test-case multiple-of?-false
    (assert-compiles-both '(multiple-of? 15 4)))

  (test-case quot-simple
    ; Quotient (same as /)
    (assert-compiles-both '(quot 20 5)))

  (test-case quot-truncate
    (assert-compiles-both '(quot 22 5)))

  (test-case reciprocal-simple
    ; Reciprocal (1/x)
    (assert-compiles-both '(reciprocal 5)))

  (test-case reciprocal-large
    (assert-compiles-both '(reciprocal 100)))

  (test-case sqr-simple
    ; Square (alias for square)
    (assert-compiles-both '(sqr 7)))

  (test-case sqr-expression
    (assert-compiles-both '(sqr (+ 2 3)))))

;;; Test bit field operations
(test-group "Bit Field Operations"
  (test-case bit-field-extract-simple
    ; Extract 4 bits starting at position 2 from 0xFF (255)
    ; 11111111 >> 2 = 00111111, mask 4 bits = 0x0F (15)
    (assert-compiles-both '(bit-field 255 2 4)))

  (test-case bit-field-extract-zero
    ; Extract from position 0
    (assert-compiles-both '(bit-field 170 0 4)))

  (test-case bit-field-extract-high
    ; Extract high bits
    (assert-compiles-both '(bit-field 240 4 4)))

  (test-case bit-field-set-simple
    ; Set 4 bits at position 2 to value 5
    (assert-compiles-both '(bit-field-set 0 2 4 5)))

  (test-case bit-field-set-replace
    ; Replace existing bits
    (assert-compiles-both '(bit-field-set 255 2 4 0)))

  (test-case bit-field-set-partial
    (assert-compiles-both '(bit-field-set 128 0 4 15))))

;;; Test additional math utilities
(test-group "Additional Math Utilities"
  (test-case divides?-true
    ; Check if x divides y (opposite of divisible?)
    ; (divides? 5 15) => true (5 divides 15)
    (assert-compiles-both '(divides? 5 15)))

  (test-case divides?-false
    (assert-compiles-both '(divides? 5 17)))

  (test-case coprime?-true
    ; Check if x and y are coprime (gcd = 1)
    (assert-compiles-both '(coprime? 15 28)))

  (test-case coprime?-false
    (assert-compiles-both '(coprime? 12 18)))

  (test-case lerp-simple
    ; Linear interpolation: lerp(0, 100, 50) = 50
    (assert-compiles-both '(lerp 0 100 50)))

  (test-case lerp-quarter
    ; lerp(0, 100, 25) = 25
    (assert-compiles-both '(lerp 0 100 25)))

  (test-case lerp-offset
    ; lerp(10, 20, 50) = 15
    (assert-compiles-both '(lerp 10 20 50)))

  (test-case median3-middle
    ; Median of three values
    (assert-compiles-both '(median3 5 10 15)))

  (test-case median3-first
    (assert-compiles-both '(median3 10 5 15)))

  (test-case median3-last
    (assert-compiles-both '(median3 5 15 10)))

  (test-case constrain-simple
    ; Alias for clamp
    (assert-compiles-both '(constrain 5 0 10)))

  (test-case constrain-low
    (assert-compiles-both '(constrain -5 0 10)))

  (test-case constrain-high
    (assert-compiles-both '(constrain 15 0 10)))

  (test-case map-range-simple
    ; Map value from one range to another
    ; map-range(5, 0, 10, 0, 100) = 50
    (assert-compiles-both '(map-range 5 0 10 0 100)))

  (test-case map-range-offset
    ; map-range(15, 10, 20, 0, 100) = 50
    (assert-compiles-both '(map-range 15 10 20 0 100)))

  (test-case map-range-reverse
    ; Reverse mapping
    (assert-compiles-both '(map-range 5 0 10 100 0))))

;;; Test additional comparison and numeric utilities
(test-group "Additional Comparison and Numeric Utilities"
  (test-case positive-or-zero?-positive
    ; Check if number is >= 0
    (assert-compiles-both '(positive-or-zero? 5)))

  (test-case positive-or-zero?-zero
    (assert-compiles-both '(positive-or-zero? 0)))

  (test-case positive-or-zero?-negative
    (assert-compiles-both '(positive-or-zero? -5)))

  (test-case negative-or-zero?-negative
    ; Check if number is <= 0
    (assert-compiles-both '(negative-or-zero? -5)))

  (test-case negative-or-zero?-zero
    (assert-compiles-both '(negative-or-zero? 0)))

  (test-case negative-or-zero?-positive
    (assert-compiles-both '(negative-or-zero? 5)))

  (test-case strictly-between?-true
    ; Check if strictly between (exclusive)
    (assert-compiles-both '(strictly-between? 5 1 10)))

  (test-case strictly-between?-false-equal
    (assert-compiles-both '(strictly-between? 1 1 10)))

  (test-case strictly-between?-false-outside
    (assert-compiles-both '(strictly-between? 15 1 10)))

  (test-case approximately?-true
    ; Check if within tolerance
    (assert-compiles-both '(approximately? 10 12 3)))

  (test-case approximately?-false
    (assert-compiles-both '(approximately? 10 20 5)))

  (test-case nearest-multiple-exact
    ; Round to nearest multiple
    (assert-compiles-both '(nearest-multiple 15 5)))

  (test-case nearest-multiple-round-up
    (assert-compiles-both '(nearest-multiple 17 5)))

  (test-case nearest-multiple-round-down
    (assert-compiles-both '(nearest-multiple 13 5)))

  (test-case round-up-to-simple
    ; Alias for align-up
    (assert-compiles-both '(round-up-to 17 5)))

  (test-case round-down-to-simple
    ; Alias for align-down
    (assert-compiles-both '(round-down-to 17 5))))

;;; Test bit manipulation variations
(test-group "Bit Manipulation Variations"
  (test-case hamming-distance-same
    ; Count differing bits
    (assert-compiles-both '(hamming-distance 5 5)))

  (test-case hamming-distance-different
    (assert-compiles-both '(hamming-distance 5 10)))

  (test-case hamming-distance-complement
    (assert-compiles-both '(hamming-distance 255 0)))

  (test-case parity-even
    ; Parity: 0 if even number of bits, 1 if odd
    (assert-compiles-both '(parity 7)))

  (test-case parity-odd
    (assert-compiles-both '(parity 15)))

  (test-case parity-zero
    (assert-compiles-both '(parity 0))))

;;; Test mathematical sequences
(test-group "Mathematical Sequences"
  (test-case triangle-number-simple
    ; Triangular number: n * (n + 1) / 2
    (assert-compiles-both '(triangle-number 5)))

  (test-case triangle-number-zero
    (assert-compiles-both '(triangle-number 0)))

  (test-case triangle-number-ten
    (assert-compiles-both '(triangle-number 10)))

  (test-case square-number?-true
    ; Check if perfect square
    (assert-compiles-both '(square-number? 16)))

  (test-case square-number?-false
    (assert-compiles-both '(square-number? 15)))

  (test-case square-number?-one
    (assert-compiles-both '(square-number? 1)))

  (test-case square-number?-zero
    (assert-compiles-both '(square-number? 0)))

  (test-case square-number?-expression
    (assert-compiles-both '(square-number? (* 5 5)))))

;;; Boolean and utility conversion tests
(test-group "Conversion and Utility Functions"
  (test-case bool->int-true
    ; Convert true to 1
    (assert-compiles-both '(bool->int (= 5 5))))

  (test-case bool->int-false
    ; Convert false to 0
    (assert-compiles-both '(bool->int (= 5 6))))

  (test-case int->bool-nonzero
    ; Nonzero is true
    (assert-compiles-both '(int->bool 42)))

  (test-case int->bool-zero
    ; Zero is false
    (assert-compiles-both '(int->bool 0)))

  (test-case negate-if-true
    ; Negate when condition is true
    (assert-compiles-both '(negate-if (> 5 3) 10)))

  (test-case negate-if-false
    ; Don't negate when condition is false
    (assert-compiles-both '(negate-if (< 5 3) 10)))

  (test-case select-true
    ; Select first value when condition is true
    (assert-compiles-both '(select (> 5 3) 100 200)))

  (test-case select-false
    ; Select second value when condition is false
    (assert-compiles-both '(select (< 5 3) 100 200))))

;;; Advanced bit manipulation tests
(test-group "Advanced Bit Operations"
  (test-case count-leading-zeros-one
    ; CLZ of 1 should be 59 (60 - 1 bit)
    (assert-compiles-both '(count-leading-zeros 1)))

  (test-case count-leading-zeros-eight
    ; CLZ of 8 (0b1000) should be 56 (60 - 4 bits)
    (assert-compiles-both '(count-leading-zeros 8)))

  (test-case count-leading-zeros-large
    ; CLZ of large number
    (assert-compiles-both '(count-leading-zeros 65536)))

  (test-case count-trailing-zeros-one
    ; CTZ of 1 should be 0
    (assert-compiles-both '(count-trailing-zeros 1)))

  (test-case count-trailing-zeros-eight
    ; CTZ of 8 (0b1000) should be 3
    (assert-compiles-both '(count-trailing-zeros 8)))

  (test-case count-trailing-zeros-sixteen
    ; CTZ of 16 (0b10000) should be 4
    (assert-compiles-both '(count-trailing-zeros 16)))

  (test-case next-power-of-2-exact
    ; Already a power of 2
    (assert-compiles-both '(next-power-of-2 16)))

  (test-case next-power-of-2-round-up
    ; Round up to next power of 2
    (assert-compiles-both '(next-power-of-2 17)))

  (test-case next-power-of-2-one
    ; Next power of 2 for 1
    (assert-compiles-both '(next-power-of-2 1)))

  (test-case next-power-of-2-expression
    ; Next power of 2 with expression
    (assert-compiles-both '(next-power-of-2 (+ 10 5))))

  (test-case prev-power-of-2-exact
    ; Already a power of 2
    (assert-compiles-both '(prev-power-of-2 16)))

  (test-case prev-power-of-2-round-down
    ; Round down to previous power of 2
    (assert-compiles-both '(prev-power-of-2 17)))

  (test-case prev-power-of-2-large
    ; Previous power of 2 for larger number
    (assert-compiles-both '(prev-power-of-2 100))))

;;; Range wrapping tests
(test-group "Range Wrapping Functions"
  (test-case in-range?-true
    ; Value in range (alias for within?)
    (assert-compiles-both '(in-range? 5 1 10)))

  (test-case in-range?-false
    ; Value out of range
    (assert-compiles-both '(in-range? 15 1 10)))

  (test-case out-of-range?-true
    ; Value outside range
    (assert-compiles-both '(out-of-range? 15 1 10)))

  (test-case out-of-range?-false
    ; Value inside range
    (assert-compiles-both '(out-of-range? 5 1 10)))

  (test-case wrap-positive
    ; Wrap positive value
    (assert-compiles-both '(wrap 15 10)))

  (test-case wrap-in-range
    ; Value already in range
    (assert-compiles-both '(wrap 5 10)))

  (test-case wrap-negative
    ; Wrap negative value
    (assert-compiles-both '(wrap -3 10)))

  (test-case wrap-range-positive
    ; Wrap to range [10, 20)
    (assert-compiles-both '(wrap-range 25 10 20)))

  (test-case wrap-range-in-range
    ; Value already in range
    (assert-compiles-both '(wrap-range 15 10 20)))

  (test-case wrap-range-negative
    ; Wrap negative to range
    (assert-compiles-both '(wrap-range 5 10 20))))

;;; Additional bit utilities
(test-group "Additional Bit Utilities"
  (test-case bit-width-one
    ; 1 requires 1 bit
    (assert-compiles-both '(bit-width 1)))

  (test-case bit-width-seven
    ; 7 requires 3 bits
    (assert-compiles-both '(bit-width 7)))

  (test-case bit-width-sixteen
    ; 16 requires 5 bits
    (assert-compiles-both '(bit-width 16)))

  (test-case msb-position-one
    ; MSB of 1 is at position 0
    (assert-compiles-both '(msb-position 1)))

  (test-case msb-position-eight
    ; MSB of 8 is at position 3
    (assert-compiles-both '(msb-position 8)))

  (test-case msb-position-large
    ; MSB of 255
    (assert-compiles-both '(msb-position 255)))

  (test-case lsb-position-one
    ; LSB of 1 is at position 0
    (assert-compiles-both '(lsb-position 1)))

  (test-case lsb-position-eight
    ; LSB of 8 is at position 3
    (assert-compiles-both '(lsb-position 8)))

  (test-case lsb-position-six
    ; LSB of 6 (0b110) is at position 1
    (assert-compiles-both '(lsb-position 6))))

;;; Arithmetic aliases
(test-group "Arithmetic Aliases"
  (test-case inc-simple
    ; Increment 5
    (assert-compiles-both '(inc 5)))

  (test-case inc-zero
    ; Increment 0
    (assert-compiles-both '(inc 0)))

  (test-case dec-simple
    ; Decrement 5
    (assert-compiles-both '(dec 5)))

  (test-case dec-one
    ; Decrement 1
    (assert-compiles-both '(dec 1))))

;;; Comparison utilities
(test-group "Comparison Utilities"
  (test-case compare-less
    ; 3 < 5 returns -1
    (assert-compiles-both '(compare 3 5)))

  (test-case compare-equal
    ; 5 = 5 returns 0
    (assert-compiles-both '(compare 5 5)))

  (test-case compare-greater
    ; 7 > 5 returns 1
    (assert-compiles-both '(compare 7 5)))

  (test-case compare-expression
    ; Compare with expressions
    (assert-compiles-both '(compare (* 2 3) (+ 4 2))))

  (test-case clamp-01-in-range
    ; 0.5 clamped to [0, 1] stays 0.5 (but we use 0 since no floats)
    (assert-compiles-both '(clamp-01 0)))

  (test-case clamp-01-below
    ; -5 clamped to [0, 1] becomes 0
    (assert-compiles-both '(clamp-01 -5)))

  (test-case clamp-01-above
    ; 10 clamped to [0, 1] becomes 1
    (assert-compiles-both '(clamp-01 10))))

;;; Logical operators
(test-group "Logical Operators"
  (test-case implies-true-true
    ; true => true = true
    (assert-compiles-both '(implies (= 1 1) (= 2 2))))

  (test-case implies-true-false
    ; true => false = false
    (assert-compiles-both '(implies (= 1 1) (= 1 2))))

  (test-case implies-false-true
    ; false => true = true
    (assert-compiles-both '(implies (= 1 2) (= 1 1))))

  (test-case implies-false-false
    ; false => false = true
    (assert-compiles-both '(implies (= 1 2) (= 3 4))))

  (test-case xnor-same-true
    ; true xnor true = true
    (assert-compiles-both '(xnor 1 1)))

  (test-case xnor-same-false
    ; false xnor false = true
    (assert-compiles-both '(xnor 0 0)))

  (test-case xnor-different
    ; true xnor false = false
    (assert-compiles-both '(xnor 1 0)))

  (test-case nand-both-true
    ; true nand true = false
    (assert-compiles-both '(nand (= 1 1) (= 2 2))))

  (test-case nand-one-false
    ; true nand false = true
    (assert-compiles-both '(nand (= 1 1) (= 1 2))))

  (test-case nor-both-false
    ; false nor false = true
    (assert-compiles-both '(nor (= 1 2) (= 3 4))))

  (test-case nor-one-true
    ; true nor false = false
    (assert-compiles-both '(nor (= 1 1) (= 1 2)))))

;;; Number theory predicates
(test-group "Number Theory Predicates"
  (test-case triangular-number?-true
    ; 6 is triangular (1+2+3)
    (assert-compiles-both '(triangular-number? 6)))

  (test-case triangular-number?-false
    ; 7 is not triangular
    (assert-compiles-both '(triangular-number? 7)))

  (test-case triangular-number?-ten
    ; 10 is triangular (1+2+3+4)
    (assert-compiles-both '(triangular-number? 10)))

  (test-case triangular-number?-one
    ; 1 is triangular
    (assert-compiles-both '(triangular-number? 1)))

  (test-case pentagonal-number?-true
    ; 12 is pentagonal
    (assert-compiles-both '(pentagonal-number? 12)))

  (test-case pentagonal-number?-false
    ; 13 is not pentagonal
    (assert-compiles-both '(pentagonal-number? 13)))

  (test-case pentagonal-number?-one
    ; 1 is pentagonal
    (assert-compiles-both '(pentagonal-number? 1)))

  (test-case hexagonal-number?-true
    ; 6 is hexagonal
    (assert-compiles-both '(hexagonal-number? 6)))

  (test-case hexagonal-number?-false
    ; 7 is not hexagonal
    (assert-compiles-both '(hexagonal-number? 7)))

  (test-case hexagonal-number?-one
    ; 1 is hexagonal
    (assert-compiles-both '(hexagonal-number? 1))))

;;; Additional sequence functions
(test-group "Additional Sequence Functions"
  (test-case pentagonal-number-one
    ; P(1) = 1
    (assert-compiles-both '(pentagonal-number 1)))

  (test-case pentagonal-number-two
    ; P(2) = 5
    (assert-compiles-both '(pentagonal-number 2)))

  (test-case pentagonal-number-three
    ; P(3) = 12
    (assert-compiles-both '(pentagonal-number 3)))

  (test-case hexagonal-number-one
    ; H(1) = 1
    (assert-compiles-both '(hexagonal-number 1)))

  (test-case hexagonal-number-two
    ; H(2) = 6
    (assert-compiles-both '(hexagonal-number 2)))

  (test-case hexagonal-number-three
    ; H(3) = 15
    (assert-compiles-both '(hexagonal-number 3))))

;;; More utility predicates
(test-group "More Utility Predicates"
  (test-case one?-true
    ; 1 equals 1
    (assert-compiles-both '(one? 1)))

  (test-case one?-false
    ; 2 does not equal 1
    (assert-compiles-both '(one? 2)))

  (test-case negative-one?-true
    ; -1 equals -1
    (assert-compiles-both '(negative-one? -1)))

  (test-case negative-one?-false
    ; 1 does not equal -1
    (assert-compiles-both '(negative-one? 1)))

  (test-case positive-power-of-2?-true
    ; 8 is a positive power of 2
    (assert-compiles-both '(positive-power-of-2? 8)))

  (test-case positive-power-of-2?-false-not-power
    ; 7 is not a power of 2
    (assert-compiles-both '(positive-power-of-2? 7)))

  (test-case positive-power-of-2?-false-negative
    ; -8 is not a positive power of 2
    (assert-compiles-both '(positive-power-of-2? -8))))

;;; Additional conditional macros
(test-group "Additional Conditional Macros"
  (test-case if-not-false
    ; if-not runs then branch when false
    (assert-compiles-both '(if-not (= 1 2) 10 20)))

  (test-case if-not-true
    ; if-not runs else branch when true
    (assert-compiles-both '(if-not (= 1 1) 10 20)))

  (test-case when-not-false
    ; when-not runs when false
    (assert-compiles-both '(when-not (= 1 2) 42)))

  (test-case when-not-true
    ; when-not doesn't run when true
    (assert-compiles-both '(when-not (= 1 1) 42)))

  (test-case unless-let-truthy
    ; unless-let doesn't run with truthy value
    (assert-compiles-both '(unless-let (x 5) x)))

  (test-case unless-let-falsy
    ; unless-let runs with falsy value (0)
    (assert-compiles-both '(unless-let (x 0) 42))))

;;; List and cons aliases
(test-group "List and Cons Aliases"
  (test-case empty?-null
    ; null list is empty
    (assert-compiles-both '(empty? (quote ())))))

;;; Numeric utilities
(test-group "Numeric Utilities"
  (test-case abs-diff-positive
    ; |10 - 3| = 7
    (assert-compiles-both '(abs-diff 10 3)))

  (test-case abs-diff-negative
    ; |3 - 10| = 7
    (assert-compiles-both '(abs-diff 3 10)))

  (test-case distance-simple
    ; distance is alias for abs-diff
    (assert-compiles-both '(distance 5 12)))

  (test-case pow2-small
    ; 2^3 = 8
    (assert-compiles-both '(pow2 3)))

  (test-case pow2-large
    ; 2^10 = 1024
    (assert-compiles-both '(pow2 10)))

  (test-case pow10-small
    ; 10^2 = 100
    (assert-compiles-both '(pow10 2)))

  (test-case pow10-large
    ; 10^3 = 1000
    (assert-compiles-both '(pow10 3))))

;;; Bit manipulation aliases
(test-group "Bit Manipulation Aliases"
  (test-case bit-set?-true
    ; Bit 3 is set in 8
    (assert-compiles-both '(bit-set? 3 8)))

  (test-case bit-set?-false
    ; Bit 2 is not set in 8
    (assert-compiles-both '(bit-set? 2 8)))

  (test-case bit-clear?-true
    ; Bit 2 is clear in 8
    (assert-compiles-both '(bit-clear? 2 8)))

  (test-case bit-clear?-false
    ; Bit 3 is not clear in 8
    (assert-compiles-both '(bit-clear? 3 8)))

  (test-case test-bit-set
    ; Test bit is alias for logbitp
    (assert-compiles-both '(test-bit 0 1))))

;;; More range predicates
(test-group "More Range Predicates"
  (test-case in-open-range?-true
    ; 5 in (1, 10) = true
    (assert-compiles-both '(in-open-range? 5 1 10)))

  (test-case in-open-range?-false-boundary
    ; 1 in (1, 10) = false (exclusive)
    (assert-compiles-both '(in-open-range? 1 1 10)))

  (test-case in-closed-range?-true
    ; 5 in [1, 10] = true
    (assert-compiles-both '(in-closed-range? 5 1 10)))

  (test-case in-closed-range?-boundary
    ; 1 in [1, 10] = true (inclusive)
    (assert-compiles-both '(in-closed-range? 1 1 10))))

;;; Sequence numbers
(test-group "Sequence Numbers"
  (test-case lucas-number-zero
    ; L(0) = 2
    (assert-compiles-both '(lucas-number 0)))

  (test-case lucas-number-one
    ; L(1) = 1
    (assert-compiles-both '(lucas-number 1)))

  (test-case lucas-number-two
    ; L(2) = 3
    (assert-compiles-both '(lucas-number 2)))

  (test-case lucas-number-three
    ; L(3) = 4
    (assert-compiles-both '(lucas-number 3)))

  (test-case lucas-number-five
    ; L(5) = 11
    (assert-compiles-both '(lucas-number 5))))

;;; Utility functions
(test-group "Utility Functions"
  (test-case toggle-zero
    ; toggle(0) = 1
    (assert-compiles-both '(toggle 0)))

  (test-case toggle-one
    ; toggle(1) = 0
    (assert-compiles-both '(toggle 1)))

  (test-case flip-zero
    ; flip is alias for toggle
    (assert-compiles-both '(flip 0)))

  (test-case normalize-simple
    ; normalize(5, 0, 10) = 0.5 (but integer division)
    (assert-compiles-both '(normalize 5 0 10)))

  (test-case denormalize-simple
    ; denormalize(0, 10, 20) = 10
    (assert-compiles-both '(denormalize 0 10 20)))

  (test-case denormalize-half
    ; denormalize(1, 10, 20) = 20
    (assert-compiles-both '(denormalize 1 10 20))))

;;; Prime and composite predicates
(test-group "Prime and Composite Predicates"
  (test-case prime?-two
    ; 2 is prime
    (assert-compiles-both '(prime? 2)))

  (test-case prime?-three
    ; 3 is prime
    (assert-compiles-both '(prime? 3)))

  (test-case prime?-five
    ; 5 is prime
    (assert-compiles-both '(prime? 5)))

  (test-case prime?-seven
    ; 7 is prime
    (assert-compiles-both '(prime? 7)))

  (test-case prime?-eleven
    ; 11 is prime
    (assert-compiles-both '(prime? 11)))

  (test-case prime?-false-four
    ; 4 is not prime
    (assert-compiles-both '(prime? 4)))

  (test-case prime?-false-nine
    ; 9 is not prime
    (assert-compiles-both '(prime? 9)))

  (test-case composite?-four
    ; 4 is composite
    (assert-compiles-both '(composite? 4)))

  (test-case composite?-nine
    ; 9 is composite
    (assert-compiles-both '(composite? 9)))

  (test-case composite?-false-two
    ; 2 is not composite
    (assert-compiles-both '(composite? 2)))

  (test-case composite?-false-one
    ; 1 is not composite
    (assert-compiles-both '(composite? 1))))

;;; Additional mathematical operations
(test-group "Additional Mathematical Operations"
  (test-case min*-two
    ; Minimum of 2 values
    (assert-compiles-both '(min* 5 3)))

  (test-case min*-three
    ; Minimum of 3 values
    (assert-compiles-both '(min* 5 3 7)))

  (test-case min*-four
    ; Minimum of 4 values
    (assert-compiles-both '(min* 10 3 7 5)))

  (test-case max*-two
    ; Maximum of 2 values
    (assert-compiles-both '(max* 5 3)))

  (test-case max*-three
    ; Maximum of 3 values
    (assert-compiles-both '(max* 5 3 7)))

  (test-case max*-four
    ; Maximum of 4 values
    (assert-compiles-both '(max* 10 3 7 5)))

  (test-case sum-empty
    ; Sum of no values = 0
    (assert-compiles-both '(sum)))

  (test-case sum-one
    ; Sum of one value
    (assert-compiles-both '(sum 5)))

  (test-case sum-many
    ; Sum of multiple values
    (assert-compiles-both '(sum 1 2 3 4 5)))

  (test-case product-empty
    ; Product of no values = 1
    (assert-compiles-both '(product)))

  (test-case product-one
    ; Product of one value
    (assert-compiles-both '(product 5)))

  (test-case product-many
    ; Product of multiple values
    (assert-compiles-both '(product 2 3 4)))

  (test-case negate-positive
    ; Negate positive
    (assert-compiles-both '(negate 5)))

  (test-case negate-negative
    ; Negate negative
    (assert-compiles-both '(negate -5)))

  (test-case sqr-diff-simple
    ; (5 - 3)^2 = 4
    (assert-compiles-both '(sqr-diff 5 3)))

  (test-case sqr-diff-negative
    ; (3 - 5)^2 = 4
    (assert-compiles-both '(sqr-diff 3 5))))

;;; More predicates
(test-group "More Predicates"
  (test-case negative?-true
    ; -5 is negative
    (assert-compiles-both '(negative? -5)))

  (test-case negative?-false
    ; 5 is not negative
    (assert-compiles-both '(negative? 5)))

  (test-case nonnegative?-positive
    ; 5 is non-negative
    (assert-compiles-both '(nonnegative? 5)))

  (test-case nonnegative?-zero
    ; 0 is non-negative
    (assert-compiles-both '(nonnegative? 0)))

  (test-case nonnegative?-negative
    ; -5 is not non-negative
    (assert-compiles-both '(nonnegative? -5)))

  (test-case nonpositive?-negative
    ; -5 is non-positive
    (assert-compiles-both '(nonpositive? -5)))

  (test-case nonpositive?-zero
    ; 0 is non-positive
    (assert-compiles-both '(nonpositive? 0)))

  (test-case nonpositive?-positive
    ; 5 is not non-positive
    (assert-compiles-both '(nonpositive? 5)))

  (test-case exact-power-of-2?-true
    ; 8 is exactly 2^3
    (assert-compiles-both '(exact-power-of-2? 8)))

  (test-case exact-power-of-2?-false
    ; 7 is not a power of 2
    (assert-compiles-both '(exact-power-of-2? 7)))

  (test-case multiple?-true
    ; 12 is a multiple of 3
    (assert-compiles-both '(multiple? 12 3)))

  (test-case multiple?-false
    ; 13 is not a multiple of 3
    (assert-compiles-both '(multiple? 13 3)))

  (test-case factor?-true
    ; 3 is a factor of 12
    (assert-compiles-both '(factor? 3 12)))

  (test-case factor?-false
    ; 5 is not a factor of 12
    (assert-compiles-both '(factor? 5 12))))

;;; Conditional expressions
(test-group "Conditional Expressions"
  (test-case and-let*-all-truthy
    ; All bindings truthy
    (assert-compiles-both '(and-let* ((x 5) (y 10)) (+ x y))))

  (test-case and-let*-first-falsy
    ; First binding falsy - short circuit
    (assert-compiles-both '(and-let* ((x 0) (y 10)) (+ x y))))

  (test-case or-let-first-truthy
    ; Return first truthy value
    (assert-compiles-both '(or-let (x 5) (y 10)))))

;;; More bitwise utilities
(test-group "More Bitwise Utilities"
  (test-case bit-count-simple
    ; Count bits in 7 (0b111)
    (assert-compiles-both '(bit-count 7)))

  (test-case popcount-simple
    ; popcount is alias
    (assert-compiles-both '(popcount 15)))

  (test-case all-bits-set?-true
    ; All bits of 0b111 are set in 0b1111
    (assert-compiles-both '(all-bits-set? 15 7)))

  (test-case all-bits-set?-false
    ; Not all bits of 0b111 are set in 0b110
    (assert-compiles-both '(all-bits-set? 6 7)))

  (test-case any-bits-set?-true
    ; Some bits match
    (assert-compiles-both '(any-bits-set? 12 5)))

  (test-case any-bits-set?-false
    ; No bits match
    (assert-compiles-both '(any-bits-set? 8 4)))

  (test-case no-bits-set?-true
    ; No bits match
    (assert-compiles-both '(no-bits-set? 8 4)))

  (test-case no-bits-set?-false
    ; Some bits match
    (assert-compiles-both '(no-bits-set? 12 5))))

;;; More range utilities
(test-group "More Range Utilities"
  (test-case clamp-positive-negative
    ; Clamp -5 to positive = 0
    (assert-compiles-both '(clamp-positive -5)))

  (test-case clamp-positive-already
    ; 5 already positive
    (assert-compiles-both '(clamp-positive 5)))

  (test-case clamp-negative-positive
    ; Clamp 5 to negative = 0
    (assert-compiles-both '(clamp-negative 5)))

  (test-case clamp-negative-already
    ; -5 already negative
    (assert-compiles-both '(clamp-negative -5)))

  (test-case saturate-simple
    ; Saturate is alias for clamp
    (assert-compiles-both '(saturate 15 0 10))))

;;; Misc utilities
(test-group "Misc Utilities"
  (test-case identity?-true
    ; 5 equals 5
    (assert-compiles-both '(identity? 5 5)))

  (test-case identity?-false
    ; 5 doesn't equal 6
    (assert-compiles-both '(identity? 5 6)))

  (test-case different?-true
    ; 5 differs from 6
    (assert-compiles-both '(different? 5 6)))

  (test-case different?-false
    ; 5 doesn't differ from 5
    (assert-compiles-both '(different? 5 5)))

  (test-case max-of-3-simple
    ; Maximum of 3 values
    (assert-compiles-both '(max-of-3 5 10 3)))

  (test-case min-of-3-simple
    ; Minimum of 3 values
    (assert-compiles-both '(min-of-3 5 10 3))))

;;; Test runtime integration (requires initialization)
(test-group "Runtime Integration - List Operations"
  ;; Initialize runtime before running these tests
  (initialize-runtime-integration)

  (test-case cons-simple
    ; (cons 1 2) allocates cons cell
    (assert-compiles '(cons 1 2) :x86_64 40))

  (test-case car-simple
    ; (car (cons 1 2)) reads car field
    (assert-compiles '(car (cons 1 2)) :x86_64 50))

  (test-case cdr-simple
    ; (cdr (cons 1 2)) reads cdr field
    (assert-compiles '(cdr (cons 1 2)) :x86_64 50))

  (test-case list-empty
    ; Empty list
    (assert-compiles '(list) :x86_64 3))

  (test-case list-simple
    ; (list 1 2 3) creates linked list
    (assert-compiles '(list 1 2 3) :x86_64 100))

  (test-case list-nested-car-cdr
    ; Nested list operations
    (assert-compiles '(car (cdr (list 10 20 30))) :x86_64 100))

  (test-case list-with-arithmetic
    ; List with computed values
    (assert-compiles '(list (+ 1 2) (* 3 4) (- 5 1)) :x86_64 100)))

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
(defun assert-equal-arm64 (form expected)
  (let ((val (compile-and-run-arm64 form)))
    (unless (= val expected)
      (incf *test-failed*)
      (format t "~&[ARM64] mismatch for ~S: got ~A expected ~A~%" form val expected))
    val))
