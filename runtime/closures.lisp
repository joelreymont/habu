;;;; Closure runtime support for Habu Lisp
;;;; Provides heap-allocated closure objects with captured environment

(in-package :habu-runtime)

;;; Closure object structure:
;;; [Header][Code Ptr][Arity][Env Size][Var1][Var2]...[VarN]
;;;    8        8        8        8       8     8       8
;;;
;;; Tag: 0x7 (closure/function type)

(defconstant +tag-closure+ #x7)

(defun make-closure (code-ptr arity captured-vars)
  "Create a closure object on the heap with captured environment
   code-ptr: Function pointer (unsigned integer)
   arity: Number of parameters (fixnum)
   captured-vars: List of tagged fixnum values to capture"
  (let* ((num-captured (length captured-vars))
         (data-size (+ 24 (* num-captured 8)))  ; code + arity + env-size + vars (no header)
         (ptr (heap-allocate *heap* data-size +tag-closure+)))
    ;; heap-allocate already wrote the header, ptr is tagged
    ;; Untag to get base address
    (let ((base (logand ptr (lognot #xF))))
      ;; Set code pointer (at offset 8 from base)
      (write-u64 *heap* (+ base 8) code-ptr)
      ;; Set arity (tagged fixnum, at offset 16)
      (write-u64 *heap* (+ base 16) (ash arity 4))
      ;; Set environment size (tagged fixnum, at offset 24)
      (write-u64 *heap* (+ base 24) (ash num-captured 4))
      ;; Copy captured variables (starting at offset 32)
      (loop for var in captured-vars
            for offset from 32 by 8
            do (write-u64 *heap* (+ base offset) var)))
    ;; Return tagged pointer
    ptr))

(defun closure? (ptr)
  "Check if a value is a closure"
  (= (logand ptr #xF) +tag-closure+))

(defun closure-code-pointer (closure-ptr)
  "Get the code pointer from a closure object"
  (unless (closure? closure-ptr)
    (error "Not a closure: ~X" closure-ptr))
  (read-u64 *heap* (+ (logand closure-ptr (lognot #xF)) 8)))

(defun closure-arity (closure-ptr)
  "Get the arity (number of parameters) from a closure"
  (unless (closure? closure-ptr)
    (error "Not a closure: ~X" closure-ptr))
  (let ((tagged-arity (read-u64 *heap* (+ (logand closure-ptr (lognot #xF)) 16))))
    (ash tagged-arity -4)))  ; Untag

(defun closure-env-size (closure-ptr)
  "Get the environment size (number of captured variables) from a closure"
  (unless (closure? closure-ptr)
    (error "Not a closure: ~X" closure-ptr))
  (let ((tagged-size (read-u64 *heap* (+ (logand closure-ptr (lognot #xF)) 24))))
    (ash tagged-size -4)))  ; Untag

(defun closure-env-ref (closure-ptr index)
  "Get a captured variable from a closure's environment
   index: 0-based index into captured variables"
  (unless (closure? closure-ptr)
    (error "Not a closure: ~X" closure-ptr))
  (let ((env-size (closure-env-size closure-ptr)))
    (when (>= index env-size)
      (error "Closure env index ~D out of range (size ~D)" index env-size))
    (read-u64 *heap* (+ (logand closure-ptr (lognot #xF)) 32 (* index 8)))))

(defun closure-info (closure-ptr)
  "Return closure information for debugging"
  (unless (closure? closure-ptr)
    (error "Not a closure: ~X" closure-ptr))
  (let ((base (logand closure-ptr (lognot #xF))))
    (format nil "Closure at ~X: code=~X, arity=~D, env-size=~D"
            closure-ptr
            (read-u64 *heap* (+ base 8))
            (closure-arity closure-ptr)
            (closure-env-size closure-ptr))))

;;; Helper functions for creating closures from compiled code
;;; These take individual arguments instead of a list

(defun make-closure-0 (code-ptr arity)
  "Create a closure with 0 captured variables"
  (make-closure code-ptr arity nil))

(defun make-closure-1 (code-ptr arity var1)
  "Create a closure with 1 captured variable"
  (make-closure code-ptr arity (list var1)))

(defun make-closure-2 (code-ptr arity var1 var2)
  "Create a closure with 2 captured variables"
  (make-closure code-ptr arity (list var1 var2)))

(defun make-closure-3 (code-ptr arity var1 var2 var3)
  "Create a closure with 3 captured variables"
  (make-closure code-ptr arity (list var1 var2 var3)))

;;; Export functions
(export '(make-closure closure? closure-code-pointer closure-arity
          closure-env-size closure-env-ref closure-info
          +tag-closure+
          make-closure-0 make-closure-1 make-closure-2 make-closure-3))
