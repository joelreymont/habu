;;;; Tag Constants - Single Source of Truth
;;;;
;;;; Hybrid 1+3 bit tagging scheme:
;;;; bit0=1: fixnum (63-bit value, val >> 1)
;;;; bit0=0: pointer|tag (nil = 0)
;;;;   0:cons  2:sym  4:vec  6:str  8:closure  10:keyword  14:forward

(in-package :habu)

;;; Special values
(defconstant +nil-value+ 0 "nil = 0")
(defconstant +t-value+ 3 "t = symbol tag 2 with pointer 0, but encoded as 3")

;;; Fixnum encoding
(defconstant +fixnum-bit+ 1 "Bit 0 set = fixnum")
(defconstant +fixnum-shift+ 1 "Shift amount for fixnum values")

;;; Tag mask and pointer mask
(defconstant +tag-mask+ 15 "Low 4 bits for tag extraction")
(defconstant +ptr-mask+ -16 "High bits for pointer extraction (sign-extended)")

;;; Type tags (bit0=0 for all pointers)
(defconstant +tag-cons+ 0 "Cons cell tag")
(defconstant +tag-symbol+ 2 "Symbol tag")
(defconstant +tag-vector+ 4 "Vector tag")
(defconstant +tag-string+ 6 "String tag")
(defconstant +tag-closure+ 8 "Closure tag")
(defconstant +tag-keyword+ 10 "Keyword tag")
(defconstant +tag-forward+ 14 "GC forwarding pointer tag")

;;; Keyword/String conversion
(defconstant +keyword-string-xor+ 12 "XOR to convert between keyword (10) and string (6)")

;;; Helper to tag a fixnum
(defun tag-fixnum (n)
  "Tag an integer as a fixnum."
  (logior (ash n +fixnum-shift+) +fixnum-bit+))

;;; Helper to untag a fixnum
(defun untag-fixnum (tagged)
  "Extract integer from tagged fixnum."
  (ash tagged (- +fixnum-shift+)))

;;; Type predicates (compile-time)
(defun fixnump-tag (v)
  "Check if tagged value is a fixnum."
  (= (logand v +fixnum-bit+) +fixnum-bit+))

(defun consp-tag (v)
  "Check if tagged value is a cons (not nil)."
  (and (not (zerop v))
       (zerop (logand v +fixnum-bit+))
       (= (logand v +tag-mask+) +tag-cons+)))

(defun symbolp-tag (v)
  "Check if tagged value is a symbol."
  (and (zerop (logand v +fixnum-bit+))
       (= (logand v +tag-mask+) +tag-symbol+)))

(defun vectorp-tag (v)
  "Check if tagged value is a vector."
  (and (zerop (logand v +fixnum-bit+))
       (= (logand v +tag-mask+) +tag-vector+)))

(defun stringp-tag (v)
  "Check if tagged value is a string."
  (and (zerop (logand v +fixnum-bit+))
       (= (logand v +tag-mask+) +tag-string+)))

(defun closurep-tag (v)
  "Check if tagged value is a closure."
  (and (zerop (logand v +fixnum-bit+))
       (= (logand v +tag-mask+) +tag-closure+)))

(defun keywordp-tag (v)
  "Check if tagged value is a keyword."
  (and (zerop (logand v +fixnum-bit+))
       (= (logand v +tag-mask+) +tag-keyword+)))
