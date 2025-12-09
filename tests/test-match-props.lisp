;;;; test-match-props.lisp - Match expansion property tests
;;;;
;;;; Property-based tests for match macro expansion.
;;;; Tests the source transformation, not compiled execution.

(defpackage :habu-test-match-props
  (:use :cl :habu-test-quickcheck))

(in-package :habu-test-match-props)

;;; ============================================================
;;; Match-Specific Generators
;;; ============================================================

(defun gen-simple-value ()
  "Generator for simple values: integers, nil, symbols."
  (make-gen
   (lambda ()
     (case (random 4)
       (0 (- (random 201) 100))  ; integer -100 to 100
       (1 nil)
       (2 t)
       (3 (nth (random 5) '(foo bar baz quux zot)))))
   (lambda (v)
     (cond ((integerp v) (if (= v 0) nil (list 0)))
           ((null v) nil)
           ((eq v t) (list nil))
           (t (list nil))))))

(defun gen-cons-value (depth)
  "Generator for cons cells with max depth."
  (if (<= depth 0)
      (gen-simple-value)
      (make-gen
       (lambda ()
         (if (= (random 3) 0)
             (gen-value (gen-simple-value))
             (cons (gen-value (gen-cons-value (1- depth)))
                   (gen-value (gen-cons-value (1- depth))))))
       (lambda (v)
         (if (consp v)
             (list (car v) (cdr v) nil)
             (shrink-value (gen-simple-value) v))))))

;;; ============================================================
;;; Expansion Analysis Helpers
;;; ============================================================

(defun expansion-has-let (exp)
  "Check if expansion starts with let."
  (and (consp exp) (eq (car exp) 'let)))

(defun expansion-has-if (exp)
  "Check if expansion contains if anywhere."
  (cond ((not (consp exp)) nil)
        ((eq (car exp) 'if) t)
        (t (or (expansion-has-if (car exp))
               (expansion-has-if (cdr exp))))))

(defun count-if-nodes (exp)
  "Count number of if forms in expansion."
  (cond ((not (consp exp)) 0)
        ((eq (car exp) 'if)
         (+ 1 (count-if-nodes (cadr exp))
            (count-if-nodes (caddr exp))
            (count-if-nodes (cadddr exp))))
        (t (+ (count-if-nodes (car exp))
              (count-if-nodes (cdr exp))))))

;;; ============================================================
;;; Match Expansion Properties
;;; ============================================================

;; Property: wildcard always produces no if (direct success)
(defproperty prop-match-wildcard-no-if ((body (gen-int 1 100)))
  (let ((expanded (expand-match 'x `((_ ,body)))))
    ;; (match x (_ body)) should expand to (let ((M x)) body)
    ;; No if needed since wildcard always matches
    (not (expansion-has-if expanded))))

;; Property: variable binding produces no if (always matches)
(defproperty prop-match-var-binding-no-if ((body (gen-int 1 100)))
  (let ((expanded (expand-match 'x `((y ,body)))))
    ;; (match x (y body)) should expand to (let ((M x)) (let ((y M)) body))
    ;; No if needed since variable always matches
    (not (expansion-has-if expanded))))

;; Property: nil pattern produces exactly one if
(defproperty prop-match-nil-one-if ((body (gen-int 1 100)))
  (let ((expanded (expand-match 'x `((nil ,body)))))
    ;; (match x (nil body)) needs one if to check null
    (= 1 (count-if-nodes expanded))))

;; Property: number pattern produces exactly one if
(defproperty prop-match-number-one-if ((n (gen-int -100 100)) (body (gen-int 1 100)))
  (let ((expanded (expand-match 'x `((,n ,body)))))
    ;; (match x (42 body)) needs one if to check =
    (= 1 (count-if-nodes expanded))))

;; Property: cons pattern produces one if (for consp check)
;; plus nested ifs for sub-patterns
(defproperty prop-match-cons-has-consp-check ((body (gen-int 1 100)))
  (let ((expanded (expand-match 'x `(((cons a b) ,body)))))
    ;; Should have at least one if for consp check
    (>= (count-if-nodes expanded) 1)))

;; Property: multiple clauses produce multiple branches
(defproperty prop-match-multi-clause-branches ((n (gen-int 2 5)))
  (let* ((clauses (loop for i from 1 to n collect `(,i ,i)))
         (expanded (expand-match 'x clauses)))
    ;; n number patterns need n if checks
    (= n (count-if-nodes expanded))))

;; Property: expansion always starts with let (to bind scrutinee once)
(defproperty prop-match-starts-with-let ((body (gen-int 1 100)))
  (let ((expanded (expand-match 'complex-expr `((_ ,body)))))
    (expansion-has-let expanded)))

;; Property: nested cons doubles the if count
(defproperty prop-match-nested-cons-ifs ((body (gen-int 1 100)))
  (let ((single (expand-match 'x `(((cons a b) ,body))))
        (nested (expand-match 'x `(((cons a (cons b c)) ,body)))))
    ;; Nested cons should have more ifs than single cons
    (> (count-if-nodes nested) (count-if-nodes single))))

;;; ============================================================
;;; Match Expansion Unit Tests
;;; ============================================================

(defun test-expand-match-wildcard ()
  "Test that wildcard expands correctly."
  (let ((exp (expand-match 'x '((_ 42)))))
    (if (and (expansion-has-let exp)
             (not (expansion-has-if exp)))
        (progn (format t "  [PASS] expand-match wildcard~%") t)
        (progn (format t "  [FAIL] expand-match wildcard: ~S~%" exp) nil))))

(defun test-expand-match-var ()
  "Test that variable binding expands correctly."
  (let ((exp (expand-match 'x '((y (+ y 1))))))
    (if (and (expansion-has-let exp)
             (not (expansion-has-if exp)))
        (progn (format t "  [PASS] expand-match var~%") t)
        (progn (format t "  [FAIL] expand-match var: ~S~%" exp) nil))))

(defun test-expand-match-nil ()
  "Test that nil pattern expands to if with null check."
  (let ((exp (expand-match 'x '((nil 42) (_ 0)))))
    (if (and (expansion-has-let exp)
             (expansion-has-if exp))
        (progn (format t "  [PASS] expand-match nil~%") t)
        (progn (format t "  [FAIL] expand-match nil: ~S~%" exp) nil))))

(defun test-expand-match-cons ()
  "Test that cons pattern expands to if with consp check."
  (let ((exp (expand-match 'x '(((cons a b) (+ a b))))))
    (if (and (expansion-has-let exp)
             (expansion-has-if exp))
        (progn (format t "  [PASS] expand-match cons~%") t)
        (progn (format t "  [FAIL] expand-match cons: ~S~%" exp) nil))))

(defun test-expand-match-list ()
  "Test that list pattern expands (converts to nested cons)."
  (let ((exp (expand-match 'x '(((list a b c) (+ a b))))))
    (if (and (expansion-has-let exp)
             (expansion-has-if exp)
             ;; list of 3 elements = 3 consp checks + 1 null check
             (>= (count-if-nodes exp) 4))
        (progn (format t "  [PASS] expand-match list~%") t)
        (progn (format t "  [FAIL] expand-match list: ~S~%" exp) nil))))

(defun test-expand-match-quoted-symbol ()
  "Test that quoted symbol expands to eq check."
  (let ((exp (expand-match 'x '(('foo 42) (_ 0)))))
    (if (and (expansion-has-let exp)
             (expansion-has-if exp))
        (progn (format t "  [PASS] expand-match quoted symbol~%") t)
        (progn (format t "  [FAIL] expand-match quoted: ~S~%" exp) nil))))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-match-prop-tests (&optional (trials *quickcheck-trials*))
  "Run all match expansion property tests."
  (format t "~%=== Match Expansion Property Tests (~D trials each) ===~%~%" trials)
  (reset-property-stats)

  (let ((unit-pass 0) (unit-fail 0))
    ;; Unit tests
    (format t "Match expansion unit tests:~%")
    (if (test-expand-match-wildcard) (incf unit-pass) (incf unit-fail))
    (if (test-expand-match-var) (incf unit-pass) (incf unit-fail))
    (if (test-expand-match-nil) (incf unit-pass) (incf unit-fail))
    (if (test-expand-match-cons) (incf unit-pass) (incf unit-fail))
    (if (test-expand-match-list) (incf unit-pass) (incf unit-fail))
    (if (test-expand-match-quoted-symbol) (incf unit-pass) (incf unit-fail))

    ;; Property tests
    (format t "~%Match expansion properties:~%")
    (run-property 'prop-match-wildcard-no-if trials)
    (run-property 'prop-match-var-binding-no-if trials)
    (run-property 'prop-match-nil-one-if trials)
    (run-property 'prop-match-number-one-if trials)
    (run-property 'prop-match-cons-has-consp-check trials)
    (run-property 'prop-match-multi-clause-branches trials)
    (run-property 'prop-match-starts-with-let trials)
    (run-property 'prop-match-nested-cons-ifs trials)

    ;; Summary
    (format t "~%Match Tests: ~D unit + ~D property = ~D passed, ~D failed~%"
            unit-pass *property-pass-count*
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))

    (values (and (= unit-fail 0) (= *property-fail-count* 0))
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))))
