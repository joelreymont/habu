;;;; Habu Runtime - String Allocation and Operations
;;;; Heap-allocated strings for Habu Lisp

(in-package :habu-runtime)

;;; Export symbols
(export '(runtime-make-string
          runtime-string-length
          runtime-string-ref
          runtime-string-set
          runtime-string-equal
          runtime-string-concat
          runtime-string-substring
          runtime-string->lisp
          runtime-lisp->string))

;;; String structure (on heap)
;;; Layout: header(8) + length(8) + data(N bytes, padded to 16-byte alignment)
;;; length: number of characters (bytes for ASCII, later UTF-8)
;;; data: raw character bytes

;;; String allocation
(defun runtime-make-string (lisp-string)
  "Allocate a string on the heap from a Lisp string"
  (unless *heap*
    (error "Runtime not initialized - call (initialize-runtime)"))
  (let* ((length (length lisp-string))
         (data-size (+ 8 length))  ; 8 bytes for length + data
         (ptr (heap-allocate *heap* data-size +tag-string+))
         (header-addr (logand ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    ;; Write length
    (write-u64 *heap* data-addr length)
    ;; Write string data
    (loop for i from 0 below length
          for char = (char-code (char lisp-string i))
          do (setf (aref (heap-memory *heap*) (+ data-addr 8 i)) char))
    ptr))

(defun runtime-string-length (str-ptr)
  "Get the length of a string"
  (unless (= (logand str-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str-ptr))
  (let* ((header-addr (logand str-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (read-u64 *heap* data-addr)))

(defun runtime-string-ref (str-ptr index)
  "Get character at index (0-based)"
  (unless (= (logand str-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str-ptr))
  (let* ((header-addr (logand str-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr)))
    (when (>= index length)
      (error "String index out of bounds: ~D >= ~D" index length))
    (aref (heap-memory *heap*) (+ data-addr 8 index))))

(defun runtime-string-set (str-ptr index char-code)
  "Set character at index (0-based)"
  (unless (= (logand str-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str-ptr))
  (let* ((header-addr (logand str-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr)))
    (when (>= index length)
      (error "String index out of bounds: ~D >= ~D" index length))
    (setf (aref (heap-memory *heap*) (+ data-addr 8 index)) char-code))
  char-code)

(defun runtime-string->lisp (str-ptr)
  "Convert a runtime string to a Lisp string"
  (unless (= (logand str-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str-ptr))
  (let* ((header-addr (logand str-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr))
         (result (make-string length)))
    (loop for i from 0 below length
          do (setf (char result i)
                   (code-char (aref (heap-memory *heap*) (+ data-addr 8 i)))))
    result))

(defun runtime-lisp->string (lisp-string)
  "Alias for runtime-make-string"
  (runtime-make-string lisp-string))

(defun runtime-string-equal (str1-ptr str2-ptr)
  "Compare two runtime strings for equality"
  (unless (= (logand str1-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str1-ptr))
  (unless (= (logand str2-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str2-ptr))
  (let* ((header1 (logand str1-ptr (lognot #xF)))
         (header2 (logand str2-ptr (lognot #xF)))
         (data1 (+ header1 8))
         (data2 (+ header2 8))
         (len1 (read-u64 *heap* data1))
         (len2 (read-u64 *heap* data2)))
    (and (= len1 len2)
         (loop for i from 0 below len1
               always (= (aref (heap-memory *heap*) (+ data1 8 i))
                         (aref (heap-memory *heap*) (+ data2 8 i)))))))

(defun runtime-string-concat (str1-ptr str2-ptr)
  "Concatenate two runtime strings"
  (unless (= (logand str1-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str1-ptr))
  (unless (= (logand str2-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str2-ptr))
  (let* ((header1 (logand str1-ptr (lognot #xF)))
         (header2 (logand str2-ptr (lognot #xF)))
         (data1 (+ header1 8))
         (data2 (+ header2 8))
         (len1 (read-u64 *heap* data1))
         (len2 (read-u64 *heap* data2))
         (total-len (+ len1 len2))
         (data-size (+ 8 total-len))
         (result (heap-allocate *heap* data-size +tag-string+))
         (result-header (logand result (lognot #xF)))
         (result-data (+ result-header 8)))
    ;; Write length
    (write-u64 *heap* result-data total-len)
    ;; Copy first string
    (loop for i from 0 below len1
          do (setf (aref (heap-memory *heap*) (+ result-data 8 i))
                   (aref (heap-memory *heap*) (+ data1 8 i))))
    ;; Copy second string
    (loop for i from 0 below len2
          do (setf (aref (heap-memory *heap*) (+ result-data 8 len1 i))
                   (aref (heap-memory *heap*) (+ data2 8 i))))
    result))

(defun runtime-string-substring (str-ptr start &optional end)
  "Extract substring from start to end (or end of string)"
  (unless (= (logand str-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str-ptr))
  (let* ((header-addr (logand str-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (length (read-u64 *heap* data-addr))
         (actual-end (if end (min end length) length))
         (sub-len (- actual-end start)))
    (when (< actual-end start)
      (error "Invalid substring range: ~D to ~D" start actual-end))
    (when (> start length)
      (error "Substring start out of bounds: ~D >= ~D" start length))
    (let* ((data-size (+ 8 sub-len))
           (result (heap-allocate *heap* data-size +tag-string+))
           (result-header (logand result (lognot #xF)))
           (result-data (+ result-header 8)))
      ;; Write length
      (write-u64 *heap* result-data sub-len)
      ;; Copy substring
      (loop for i from 0 below sub-len
            do (setf (aref (heap-memory *heap*) (+ result-data 8 i))
                     (aref (heap-memory *heap*) (+ data-addr 8 start i))))
      result)))

;;; Pretty printing
(defun print-string (str-ptr)
  "Print string information"
  (format t "String ~X:~%" str-ptr)
  (format t "  Length: ~D~%" (runtime-string-length str-ptr))
  (format t "  Value:  \"~A\"~%" (runtime-string->lisp str-ptr)))
