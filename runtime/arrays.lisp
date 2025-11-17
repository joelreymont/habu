;;;; Habu Runtime - Array/Vector Allocation
;;;; Heap-allocated arrays for Habu Lisp

(in-package :habu-runtime)

;;; Export symbols
(export '(runtime-make-array
          runtime-aref
          runtime-aset
          runtime-array-length
          runtime-array-fill
          runtime-array-copy))

;;; Array structure (on heap)
;;; Layout: header(8) + length(8) + elements(N*8 bytes)
;;; length: number of elements
;;; elements: array of 64-bit values (fixnums or pointers)

;;; Array allocation
(defun runtime-make-array (size &optional (initial-element 0))
  "Allocate an array on the heap with SIZE elements"
  (unless *heap*
    (error "Runtime not initialized - call (initialize-runtime)"))
  (when (< size 0)
    (error "Array size must be non-negative: ~D" size))
  (let* ((data-size (+ 8 (* size 8)))  ; 8 bytes for length + size*8 for elements
         (ptr (heap-allocate *heap* data-size +tag-array+))
         (header-addr (logand ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    ;; Write length
    (write-u64 *heap* data-addr size)
    ;; Initialize elements to initial-element
    (loop for i from 0 below size
          do (write-u64 *heap* (+ data-addr 8 (* i 8)) initial-element))
    ptr))

(defun runtime-array-length (arr-ptr)
  "Get the length of an array"
  (unless (= (logand arr-ptr #xF) +tag-array+)
    (error "Not an array: ~X" arr-ptr))
  (let* ((header-addr (logand arr-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (read-u64 *heap* data-addr)))

(defun runtime-aref (arr-ptr index)
  "Get element at index (0-based)"
  (unless (= (logand arr-ptr #xF) +tag-array+)
    (error "Not an array: ~X" arr-ptr))
  (let* ((header-addr (logand arr-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr)))
    (when (or (< index 0) (>= index length))
      (error "Array index out of bounds: ~D (length ~D)" index length))
    (read-u64 *heap* (+ data-addr 8 (* index 8)))))

(defun runtime-aset (arr-ptr index value)
  "Set element at index (0-based)"
  (unless (= (logand arr-ptr #xF) +tag-array+)
    (error "Not an array: ~X" arr-ptr))
  (let* ((header-addr (logand arr-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr)))
    (when (or (< index 0) (>= index length))
      (error "Array index out of bounds: ~D (length ~D)" index length))
    (write-u64 *heap* (+ data-addr 8 (* index 8)) value))
  value)

(defun runtime-array-fill (arr-ptr value)
  "Fill all elements of array with value"
  (unless (= (logand arr-ptr #xF) +tag-array+)
    (error "Not an array: ~X" arr-ptr))
  (let* ((header-addr (logand arr-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr)))
    (loop for i from 0 below length
          do (write-u64 *heap* (+ data-addr 8 (* i 8)) value)))
  arr-ptr)

(defun runtime-array-copy (arr-ptr)
  "Create a shallow copy of an array"
  (unless (= (logand arr-ptr #xF) +tag-array+)
    (error "Not an array: ~X" arr-ptr))
  (let* ((header-addr (logand arr-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr))
         (new-arr (runtime-make-array length)))
    ;; Copy all elements
    (loop for i from 0 below length
          do (runtime-aset new-arr i (runtime-aref arr-ptr i)))
    new-arr))

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
    (let* ((data-addr (+ header-addr 8))
           (length (read-u64 heap data-addr)))
      (loop for i from 0 below length
            for element = (read-u64 heap (+ data-addr 8 (* i 8)))
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
