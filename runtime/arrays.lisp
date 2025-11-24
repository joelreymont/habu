;;;; Habu Runtime - Array/Vector Allocation
;;;; Heap-allocated arrays for Habu Lisp

(in-package :habu-runtime)

;;; Export symbols
(export '(runtime-make-vector
          runtime-vector-ref
          runtime-vector-set
          runtime-vector-length
          runtime-vector-fill
          runtime-vector-copy
          runtime-vector->list
          ;; Backward-compatible aliases
          runtime-make-array
          runtime-aref
          runtime-aset
          runtime-array-length
          runtime-array-fill
          runtime-array-copy))

;; Legacy alias retained for older tests; vector tag is #x3.
(defconstant +tag-array+ +tag-vector+)

;;; Array structure (on heap)
;;; Layout: header(8) + length(8) + elements(N*8 bytes)
;;; length: number of elements
;;; elements: array of 64-bit values (fixnums or pointers)

;;; Vector allocation
(defun runtime-make-vector (size &optional (initial-element 0))
  "Allocate a vector on the heap with SIZE elements"
  (unless *heap*
    (error "Runtime not initialized - call (initialize-runtime)"))
  (when (< size 0)
    (error "Vector size must be non-negative: ~D" size))
  (let* ((data-size (+ #x8 (* size #x8)))  ; length + data
         (ptr (heap-allocate *heap* data-size +tag-vector+))
         (header-addr (logand ptr (lognot #xF)))
         (data-addr (+ header-addr #x8)))
    ;; Write length
    (write-u64 *heap* data-addr size)
    ;; Initialize elements
    (loop for i from 0 below size
          do (write-u64 *heap* (+ data-addr #x8 (* i #x8)) initial-element))
    ptr))

(defun runtime-vector-length (vec-ptr)
  "Get the length of a vector"
  (unless (= (logand vec-ptr #xF) +tag-vector+)
    (error "Not a vector: ~X" vec-ptr))
  (let* ((header-addr (logand vec-ptr (lognot #xF)))
         (data-addr (+ header-addr #x8)))
    (read-u64 *heap* data-addr)))

(defun runtime-vector-ref (vec-ptr index)
  "Get element at INDEX (0-based)"
  (unless (= (logand vec-ptr #xF) +tag-vector+)
    (error "Not a vector: ~X" vec-ptr))
  (let* ((header-addr (logand vec-ptr (lognot #xF)))
         (data-addr (+ header-addr #x8))
         (length (read-u64 *heap* data-addr)))
    (when (or (< index 0) (>= index length))
      (error "Vector index out of bounds: ~D (length ~D)" index length))
    (read-u64 *heap* (+ data-addr #x8 (* index #x8)))))

(defun runtime-vector-set (vec-ptr index value)
  "Set element at INDEX (0-based)"
  (unless (= (logand vec-ptr #xF) +tag-vector+)
    (error "Not a vector: ~X" vec-ptr))
  (let* ((header-addr (logand vec-ptr (lognot #xF)))
         (data-addr (+ header-addr #x8))
         (length (read-u64 *heap* data-addr)))
    (when (or (< index 0) (>= index length))
      (error "Vector index out of bounds: ~D (length ~D)" index length))
    (write-u64 *heap* (+ data-addr #x8 (* index #x8)) value))
  value)

(defun runtime-vector-fill (vec-ptr value)
  "Fill all elements of vector with VALUE"
  (unless (= (logand vec-ptr #xF) +tag-vector+)
    (error "Not a vector: ~X" vec-ptr))
  (let* ((header-addr (logand vec-ptr (lognot #xF)))
         (data-addr (+ header-addr #x8))
         (length (read-u64 *heap* data-addr)))
    (loop for i from 0 below length
          do (write-u64 *heap* (+ data-addr #x8 (* i #x8)) value)))
  vec-ptr)

(defun runtime-vector-copy (vec-ptr)
  "Create a shallow copy of a vector"
  (unless (= (logand vec-ptr #xF) +tag-vector+)
    (error "Not a vector: ~X" vec-ptr))
  (let* ((header-addr (logand vec-ptr (lognot #xF)))
         (data-addr (+ header-addr #x8))
         (length (read-u64 *heap* data-addr))
         (new-vec (runtime-make-vector length)))
    ;; Copy all elements
    (loop for i from 0 below length
          do (runtime-vector-set new-vec i (runtime-vector-ref vec-ptr i)))
    new-vec))

(defun runtime-vector->list (vec-ptr)
  "Convert runtime vector to a Lisp list of runtime values"
  (unless (= (logand vec-ptr #xF) +tag-vector+)
    (error "Not a vector: ~X" vec-ptr))
  (let ((length (runtime-vector-length vec-ptr)))
    (loop for i from 0 below length
          collect (runtime-vector-ref vec-ptr i))))

;;; Backward-compatible array aliases
(defun runtime-make-array (size &optional (initial-element 0))
  (runtime-make-vector size initial-element))

(defun runtime-aref (arr-ptr index)
  (runtime-vector-ref arr-ptr index))

(defun runtime-aset (arr-ptr index value)
  (runtime-vector-set arr-ptr index value))

(defun runtime-array-length (arr-ptr)
  (runtime-vector-length arr-ptr))

(defun runtime-array-fill (arr-ptr value)
  (runtime-vector-fill arr-ptr value))

(defun runtime-array-copy (arr-ptr)
  (runtime-vector-copy arr-ptr))

;;; GC support for arrays
(defun gc-mark-array (heap arr-ptr)
  "Mark an array and its reachable elements"
  (unless (= (logand arr-ptr #xF) +tag-array+)
    (return-from gc-mark-array))

  (let* ((header-addr (logand arr-ptr (lognot #xF)))
         (header (read-u64 heap header-addr)))

    ;; Already marked?
    (when (header-marked-p header)
      (return-from gc-mark-array))

    ;; Mark array
    (write-u64 heap header-addr (header-set-mark header))

    ;; Mark array elements that are pointers
    (let* ((data-addr (+ header-addr #x8))
           (length (read-u64 heap data-addr)))
      (loop for i from 0 below length
            for element = (read-u64 heap (+ data-addr #x8 (* i #x8)))
            do (when (and (not (zerop element))
                          (not (= (logand element #xF) +tag-fixnum+)))
                 (gc-mark-object heap element))))))

;;; Pretty printing
(defun print-array (arr-ptr)
  "Print array information"
  (format t "Array ~X:~%" arr-ptr)
  (format t "  Length: ~D~%" (runtime-array-length arr-ptr))
  (format t "  Elements: [")
  (let ((length (runtime-array-length arr-ptr)))
    (loop for i from 0 below (min length 10)
          do (if (zerop i)
                 (format t "~X" (runtime-aref arr-ptr i))
                 (format t " ~X" (runtime-aref arr-ptr i))))
    (when (> length 10)
      (format t " ...")))
  (format t "]~%"))
