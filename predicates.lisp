;;;; Type Predicates - Implemented in Habu Lisp
;;;; Uses get-tag primitive to check object types
;;;;
;;;; Tag constants (from object.h):
;;;;   TAG_FIXNUM  = 0x0 (0)
;;;;   TAG_CONS    = 0x1 (1)
;;;;   TAG_SYMBOL  = 0x2 (2)
;;;;   TAG_VECTOR  = 0x3 (3)
;;;;   TAG_STRING  = 0x4 (4)
;;;;   TAG_CLOSURE = 0x5 (5)

(defun fixnum? (x)
  "Check if x is a fixnum (immediate integer)"
  (= (get-tag x) (quote 0)))

(defun cons? (x)
  "Check if x is a cons cell (pair)"
  (= (get-tag x) (quote 1)))

(defun symbol? (x)
  "Check if x is a symbol"
  (= (get-tag x) (quote 2)))

(defun vector? (x)
  "Check if x is a vector (array)"
  (= (get-tag x) (quote 3)))

(defun string? (x)
  "Check if x is a string"
  (= (get-tag x) (quote 4)))

(defun closure? (x)
  "Check if x is a closure (function)"
  (= (get-tag x) (quote 5)))

(defun nil? (x)
  "Check if x is nil (false value)"
  ;; NIL is defined as fixnum 0
  (= x (quote 0)))
