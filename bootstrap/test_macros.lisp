;;;; Test suite for macro system

(load "test-harness.lisp")
(load "compiler.lisp")

(in-package :habu-compiler)

(test-group "Macros"
  ;; Test 1: Simple defmacro definition
  (test-case defmacro-simple
    (clrhash *macro-table*)
    (assert-compiles-both '(defmacro square (x) (* x x))))

  ;; Test 2: Simple macro expansion
  (test-case macro-expansion-simple
    (clrhash *macro-table*)
    (compile-expression '(defmacro double (x) (+ x x)))
    (assert-compiles-both '(double 5)))

  ;; Test 3: Macro with multiple parameters
  (test-case macro-multiple-params
    (clrhash *macro-table*)
    (compile-expression '(defmacro add-and-double (a b) (* (+ a b) 2)))
    (assert-compiles-both '(add-and-double 3 4)))

  ;; Test 4: Macro with conditional
  (test-case macro-conditional
    (clrhash *macro-table*)
    (compile-expression '(defmacro abs-diff (a b) (if (> a b) (- a b) (- b a))))
    (assert-compiles-both '(abs-diff 10 5))))

(report-test-stats)
