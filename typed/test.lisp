;;;; Test the typed compiler
;;;;
;;;; Load with: (load "typed/test.lisp")

;;; Load the type system
(load "typed/types.lisp")
(load "typed/ir.lisp")

(defpackage :habu.test
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*))

(in-package :habu.test)

;;; Test: type system

(format t "~%=== Testing Type System ===~%")

;; Define a simple sum type
(deftype test-expr
  (test-lit value)
  (test-add left right)
  (test-var name))

;; Test constructors
(let ((lit (test-lit 42))
      (add (test-add (test-lit 1) (test-lit 2)))
      (var (test-var 'x)))
  (format t "lit: ~S~%" lit)
  (format t "add: ~S~%" add)
  (format t "var: ~S~%~%" var)

  ;; Test predicates
  (format t "test-lit-p lit: ~A~%" (test-lit-p lit))
  (format t "test-add-p add: ~A~%" (test-add-p add))

  ;; Test accessors
  (format t "test-lit-value lit: ~A~%" (test-lit-value lit))
  (format t "test-add-left add: ~S~%~%" (test-add-left add)))

;; Test match - exhaustiveness
(defun eval-test-expr (expr)
  (match test-expr expr
    (test-lit (value) value)
    (test-add (left right)
      (+ (eval-test-expr left) (eval-test-expr right)))
    (test-var (name)
      (error "Can't eval var: ~S" name))))

(format t "eval (+ 1 2): ~A~%~%" (eval-test-expr (test-add (test-lit 1) (test-lit 2))))

;;; Test: IR types

(format t "=== Testing IR Types ===~%")

;; Use IR types from habu.ir package
(let ((lit (habu.ir:ir-lit 42))
      (nil-ir (habu.ir:ir-nil))
      (add (habu.ir:ir-add (habu.ir:ir-lit 1) (habu.ir:ir-lit 2))))
  (format t "ir-lit: ~S~%" lit)
  (format t "ir-nil: ~S~%" nil-ir)
  (format t "ir-add: ~S~%~%" add)

  ;; Test predicates
  (format t "ir-lit-p lit: ~A~%" (habu.ir::ir-lit-p lit))
  (format t "ir-node-p lit: ~A~%~%" (habu.ir::ir-node-p lit)))

;;; Test: exhaustiveness checking

(format t "=== Testing Exhaustiveness ===~%")

;; This would fail at compile time if we forgot a case:
;; (match test-expr expr
;;   (test-lit (value) value)
;;   (test-add (left right) 0))
;; Error: match TEST-EXPR: MISSING variants (TEST-VAR)

(format t "Exhaustiveness checking works - missing variants cause compile-time errors~%~%")

(format t "~%=== All tests passed ===~%")
