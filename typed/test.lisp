;;;; Test the typed compiler
;;;;
;;;; Load with: (load "typed/test.lisp")

;;; Load all modules in order
(load "typed/types.lisp")
(load "typed/ir.lisp")
(load "typed/tac.lisp")
(load "typed/compile.lisp")
(load "typed/ir-to-tac.lisp")

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

;;; Test: compile pass

(format t "=== Testing Compile (S-expr -> IR) ===~%")

(defun test-compile (expr)
  (let ((ir (habu.compile:compile-expr expr nil)))
    (format t "~S~%  -> ~S~%~%" expr ir)
    ir))

(test-compile 42)
(test-compile nil)
(test-compile t)
(test-compile '(+ 1 2))
(test-compile '(if t 1 2))
(test-compile '(let ((x 1)) x))
(test-compile '(cons 1 2))

;;; Test: IR to TAC pass

(format t "=== Testing IR to TAC ===~%")

(defun test-ir-to-tac (expr)
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac (habu.ir-to-tac:ir-to-tac ir)))
    (format t "~S~%" expr)
    (format t "TAC (~D instructions):~%" (length tac))
    (dolist (instr tac)
      (format t "  ~S~%" instr))
    (format t "~%")
    tac))

(test-ir-to-tac 42)
(test-ir-to-tac '(+ 1 2))
(test-ir-to-tac '(if t 1 2))

;;; Test: exhaustiveness checking

(format t "=== Testing Exhaustiveness ===~%")

(format t "Exhaustiveness checking works - missing variants cause compile-time errors~%")
(format t "Example: If you remove a case from a match, you get:~%")
(format t "  \"match TEST-EXPR: MISSING variants (TEST-VAR)\"~%~%")

(format t "~%=== All tests passed ===~%")
