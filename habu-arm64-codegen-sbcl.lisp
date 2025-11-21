;;;; SBCL-only loader stubs for Habu codegen (keeps main file standalone)
;;;; Do NOT use in production; only for bring-up/testing in SBCL host

(defpackage :habu-sbcl-codegen
  (:use :cl :habu-shim)
  (:export codegen-expr compile-expr compile-to-arm64-with-runtime compile-to-arm64))

(in-package :habu-sbcl-codegen)

(defun has-tag? (ir tag)
  (and (consp ir) (eq (car ir) tag)))

(defun env-lookup (sym env)
  (declare (ignore sym env))
  nil)

;; Minimal ARM64 stubs for SBCL bring-up (return empty code fragments)
(defun arm64-movz (rd imm) (declare (ignore rd imm)) (list))
(defun arm64-ldr (rt rn offset) (declare (ignore rt rn offset)) (list))
(defun arm64-lsr (rd rn shift) (declare (ignore rd rn shift)) (list))
(defun arm64-add-imm (rd rn imm) (declare (ignore rd rn imm)) (list))
(defun arm64-stp (rt1 rt2 rn imm) (declare (ignore rt1 rt2 rn imm)) (list))
(defun arm64-ldp (rt1 rt2 rn imm) (declare (ignore rt1 rt2 rn imm)) (list))
(defun arm64-ret () (list))

(defun codegen-expr (ir runtime-addrs)
  "SBCL shim: simplified codegen to allow loading; returns move of literal/var or zero."
  (cond
    ((has-tag? ir 'lit)
     (let ((value (cadr ir)))
       (arm64-movz 0 (* value 16))))
    ((has-tag? ir 'var)
     (let ((offset (cadr ir)))
       (arm64-ldr 0 31 (* offset 16))))
    (t (arm64-movz 0 #x0))))

(defun compile-expr (expr env fenv)
  "SBCL shim: return trivial IR for literals/vars; else zero."
  (cond
    ((fixnum? expr) (list 'lit expr))
    ((symbol? expr)
     (let ((off (env-lookup expr env)))
       (if off (list 'var off) (list 'lit 0))))
    (t (list 'lit 0))))

(defun codegen-main-with-runtime (ir runtime-addrs)
  (declare (ignore runtime-addrs))
  (let ((body (codegen-expr ir nil)))
    (append (arm64-stp 29 30 31 -16)
            body
            (arm64-ldp 29 30 31 16)
            (arm64-ret))))

(defun compile-to-arm64-with-runtime (expr runtime-addrs)
  (codegen-main-with-runtime (compile-expr expr nil nil) runtime-addrs))

(defun compile-to-arm64 (expr)
  (compile-to-arm64-with-runtime expr nil))
