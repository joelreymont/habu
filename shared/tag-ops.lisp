;;;; Tag Operations - ARM64 Helpers
;;;;
;;;; Combines tag constants with ARM64 instruction generation.
;;;; Provides helper functions for type checking and value manipulation.

(in-package :habu)

;;; ARM64 helpers for tag operations
;;; These use the constants from tags.lisp

(defun emit-fixnum-check (dest src)
  "Generate code to check if src is a fixnum.
   Result in dest: t-value if fixnum, nil-value if not."
  (append
   (arm64:and* dest src +fixnum-bit+ :imm t)
   (arm64:cmp dest +fixnum-bit+ :imm t)
   (arm64:movz dest +nil-value+)      ; assume not fixnum
   (arm64:movz dest +t-value+ :cond :eq))) ; if fixnum bit set

(defun emit-tag-check (dest src expected-tag)
  "Generate code to check if src has expected-tag.
   Result in dest: t-value if matches, nil-value if not."
  (append
   (arm64:and* dest src +tag-mask+ :imm t)
   (arm64:cmp dest expected-tag :imm t)
   (arm64:movz dest +nil-value+)
   (arm64:movz dest +t-value+ :cond :eq)))

(defun emit-untag-ptr (dest src)
  "Generate code to extract pointer from tagged value."
  (arm64:and* dest src +ptr-mask+ :imm t))

(defun emit-tag-fixnum (dest src)
  "Generate code to tag src as fixnum into dest."
  (append
   (arm64:lsl dest src +fixnum-shift+)
   (arm64:orr dest dest +fixnum-bit+ :imm t)))

(defun emit-untag-fixnum (dest src)
  "Generate code to untag fixnum from src into dest."
  (arm64:asr dest src +fixnum-shift+))
