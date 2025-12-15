;;;; Test the typed compiler
;;;;
;;;; Load with: (load "typed/test.lisp")

;;; Load all modules in order
(load "typed/types.lisp")
(load "typed/ir.lisp")
(load "typed/tac.lisp")
(load "typed/compile.lisp")
(load "typed/ir-to-tac.lisp")
(load "typed/liveness.lisp")
(load "typed/regalloc.lisp")
(load "arm64/asm.lisp")
(load "typed/tac-codegen.lisp")

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
      (add (test-add (test-lit 1) (test-lit 2))))
  (format t "lit: ~S~%" lit)
  (format t "add: ~S~%" add)
  (format t "test-lit-p lit: ~A~%" (test-lit-p lit)))

;; Test match
(defun eval-test-expr (expr)
  (match test-expr expr
    (test-lit (value) value)
    (test-add (left right)
      (+ (eval-test-expr left) (eval-test-expr right)))
    (test-var (name)
      (error "Can't eval var: ~S" name))))

(format t "eval (+ 1 2): ~A~%~%" (eval-test-expr (test-add (test-lit 1) (test-lit 2))))

;;; Test: record/struct

(format t "=== Testing Record/Struct ===~%")

(deftype point :record (x 0) (y 0))

(let ((p (make-point :x 10 :y 20)))
  (format t "point: ~S~%" p)
  (format t "match point: ~A~%~%"
    (match point p
      (point (a b) (+ a b)))))

;;; Test: compile pass

(format t "=== Testing Compile (S-expr -> IR) ===~%")

(defun test-compile (expr)
  (let ((ir (habu.compile:compile-expr expr nil)))
    (format t "~S -> ~S~%" expr ir)
    ir))

(test-compile 42)
(test-compile '(+ 1 2))
(test-compile '(if t 1 2))

;;; Test: IR to TAC pass

(format t "~%=== Testing IR to TAC ===~%")

(defun test-ir-to-tac (expr)
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac (habu.ir-to-tac:ir-to-tac ir)))
    (format t "~S -> ~D TAC instructions~%" expr (length tac))
    tac))

(test-ir-to-tac 42)
(test-ir-to-tac '(+ 1 2))
(test-ir-to-tac '(if t 1 2))

;;; Test: Liveness Analysis

(format t "~%=== Testing Liveness Analysis ===~%")

(defun test-liveness (expr)
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac (habu.ir-to-tac:ir-to-tac ir))
         (intervals (habu.liveness:compute-liveness tac)))
    (format t "~S: ~D live intervals~%" expr (length intervals))
    (dolist (int intervals)
      (format t "  vreg ~D: [~D, ~D)~%"
              (habu.liveness:live-interval-vreg int)
              (habu.liveness:live-interval-start int)
              (habu.liveness:live-interval-end int)))
    intervals))

(test-liveness '(+ 1 2))

;;; Test: Register Allocation

(format t "~%=== Testing Register Allocation ===~%")

(defun test-regalloc (expr)
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac (habu.ir-to-tac:ir-to-tac ir))
         (alloc (habu.regalloc:allocate-registers tac)))
    (format t "~S:~%" expr)
    (format t "  Stack slots needed: ~D~%"
            (habu.regalloc:allocation-result-stack-size alloc))
    (format t "  Spills: ~S~%"
            (habu.regalloc:allocation-result-spills alloc))
    (format t "  Assignments:~%")
    (maphash (lambda (vreg reg)
               (format t "    vreg ~D -> ~A~%" vreg
                       (if (eq reg :spill) "SPILL" (format nil "x~D" reg))))
             (habu.regalloc:allocation-result-vreg-to-reg alloc))
    alloc))

(test-regalloc '(+ 1 2))
(test-regalloc '(if t 1 2))

;;; Test: Code Generation

(format t "~%=== Testing Code Generation ===~%")

(defun test-codegen (expr)
  (let* ((ir (habu.compile:compile-expr expr nil))
         (tac (habu.ir-to-tac:ir-to-tac ir))
         (alloc (habu.regalloc:allocate-registers tac))
         (code (habu.codegen:generate-code tac alloc)))
    (format t "~S -> ~D bytes of ARM64~%" expr (length code))
    ;; Show first 16 bytes as hex
    (format t "  bytes: ")
    (loop for b in code
          for i from 0 below 16
          do (format t "~2,'0X " b))
    (when (> (length code) 16)
      (format t "..."))
    (format t "~%")
    code))

(test-codegen 42)
(test-codegen '(+ 1 2))

;;; Summary

(format t "~%=== All tests passed ===~%")
(format t "Pipeline: S-expr -> IR -> TAC -> Liveness -> RegAlloc -> ARM64~%")
