;;;; runtime/hash-tables.lisp - Hash table support for Habu
;;;; Phase 1: Fixed-size hash tables with chaining

(in-package :habu-runtime)

;;; Foreign memory allocation for hash tables
;;; NOTE: This uses malloc/free, not the Habu GC heap
;;; Hash tables allocated this way are NOT garbage collected
;;; This is a temporary solution - proper GC integration needed (Bug 3.4)
(sb-alien:define-alien-routine "malloc" sb-alien:unsigned-long
  (size sb-alien:unsigned-long))

(sb-alien:define-alien-routine "free" sb-alien:void
  (ptr sb-alien:unsigned-long))

(defun allocate (size)
  "Allocate SIZE bytes of memory, return untagged address"
  (let ((addr (malloc size)))
    (when (zerop addr)
      (error "Out of memory: failed to allocate ~D bytes" size))
    addr))

;;; Hash table representation:
;;; Tag: 0x6
;;; Layout:
;;;   [Header: 8 bytes]    - Capacity (number of buckets)
;;;   [Count: 8 bytes]     - Number of entries
;;;   [Buckets: N*8 bytes] - Array of pointers to association lists
;;;
;;; Each bucket is a Habu list: ((key1 . value1) (key2 . value2) ...)

(defconstant +tag-hash-table+ #x6)
(defconstant +default-hash-capacity+ 16)

(defun runtime-make-hash-table (capacity-value)
  "Create a new hash table with given capacity.
   capacity-value: Habu fixnum (tagged) for number of buckets"
  ;; Extract capacity from tagged fixnum
  (let ((capacity (if (= capacity-value 0)  ; 0 means use default
                     +default-hash-capacity+
                     (ash capacity-value -4))))
    (unless (and (integerp capacity) (> capacity 0))
      (error "Hash table capacity must be positive fixnum, got ~X" capacity-value))

    ;; Allocate hash table structure
    ;; Size: 8 (header) + 8 (count) + capacity*8 (buckets)
    (let* ((size (+ 16 (* capacity 8)))
           (addr (allocate size)))

      ;; Write header: capacity as untagged integer
      (setf (sb-sys:sap-ref-64 (sb-sys:int-sap addr) 0) capacity)

      ;; Write count: 0 entries initially
      (setf (sb-sys:sap-ref-64 (sb-sys:int-sap addr) 8) 0)

      ;; Initialize all buckets to nil (0)
      (dotimes (i capacity)
        (setf (sb-sys:sap-ref-64 (sb-sys:int-sap addr) (+ 16 (* i 8))) 0))

      ;; Return tagged pointer
      (logior addr +tag-hash-table+))))

(defun hash-value (key-value)
  "Compute hash code for a Habu value.
   Returns non-negative integer."
  (let ((tag (logand key-value #xF)))
    (cond
      ;; Fixnum: use value directly
      ((= tag 0)
       (abs (ash key-value -4)))

      ;; String (tag 0x4): hash the string content
      ((= tag #x4)
       (let* ((str-addr (- key-value #x4))
              ;; Skip 8-byte header to get to data (length is first field)
              (len (sb-sys:sap-ref-64 (sb-sys:int-sap str-addr) 8))
              ;; Read string bytes and compute hash
              (hash 0))
         (dotimes (i len)
           ;; String data starts at offset 16 (8-byte header + 8-byte length)
           (let ((byte (sb-sys:sap-ref-8 (sb-sys:int-sap str-addr) (+ 16 i))))
             (setf hash (logand #xFFFFFFFFFFFFFFFF
                               (+ (* hash 31) byte)))))
         (abs hash)))

      ;; Symbol (tag 0x2): hash the symbol name
      ((= tag #x2)
       (let* ((sym-addr (- key-value #x2))
              ;; Skip 8-byte header, name pointer is first field
              (name-ptr (sb-sys:sap-ref-64 (sb-sys:int-sap sym-addr) 8)))
         ;; Hash the name string (which itself is a tagged pointer)
         (hash-key name-ptr)))

      ;; Other types: use address as hash
      (t (abs key-value)))))

(defun keys-equal? (key1 key2)
  "Test if two Habu keys are equal."
  (let ((tag1 (logand key1 #xF))
        (tag2 (logand key2 #xF)))
    (cond
      ;; Same value (works for fixnums, symbols, nil)
      ((= key1 key2) t)

      ;; Different tags => not equal
      ((/= tag1 tag2) nil)

      ;; String comparison (tag 0x4)
      ((= tag1 #x4)
       (let* ((addr1 (- key1 #x4))
              (addr2 (- key2 #x4))
              ;; Skip 8-byte header to read length
              (len1 (sb-sys:sap-ref-64 (sb-sys:int-sap addr1) 8))
              (len2 (sb-sys:sap-ref-64 (sb-sys:int-sap addr2) 8)))
         (and (= len1 len2)
              (dotimes (i len1 t)
                ;; String data starts at offset 16
                (unless (= (sb-sys:sap-ref-8 (sb-sys:int-sap addr1) (+ 16 i))
                          (sb-sys:sap-ref-8 (sb-sys:int-sap addr2) (+ 16 i)))
                  (return nil))))))

      ;; Default: not equal
      (t nil))))

(defun runtime-gethash (key-value ht-value)
  "Look up key in hash table, return value or 0 (nil) if not found."
  ;; Verify hash table tag
  (unless (= (logand ht-value #xF) +tag-hash-table+)
    (error "gethash requires hash table, got ~X" ht-value))

  (let* ((ht-addr (- ht-value +tag-hash-table+))
         (capacity (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 0))
         (hash (hash-value key-value))
         (bucket-idx (mod hash capacity))
         (bucket-ptr-addr (+ ht-addr 16 (* bucket-idx 8)))
         (bucket (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) (+ 16 (* bucket-idx 8)))))

    ;; Search association list
    (let ((current bucket))
      (loop
        (when (= current 0)  ; End of list
          (return 0))  ; Not found

        ;; Check if cons cell
        (unless (= (logand current #xF) #x2)
          (error "Corrupt hash table bucket"))

        ;; Get car (the pair) and cdr (rest of list)
        (let* ((cons-addr (- current #x2))
               (pair (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 0))
               (rest (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 8)))

          ;; pair is a cons (key . value)
          (when (= (logand pair #xF) #x2)
            (let* ((pair-addr (- pair #x2))
                   (entry-key (sb-sys:sap-ref-64 (sb-sys:int-sap pair-addr) 0))
                   (entry-val (sb-sys:sap-ref-64 (sb-sys:int-sap pair-addr) 8)))
              (when (keys-equal? key-value entry-key)
                (return entry-val))))

          ;; Move to next
          (setf current rest))))))

(defun runtime-puthash (key-value value ht-value)
  "Insert or update key-value pair in hash table. Returns value."
  ;; Verify hash table tag
  (unless (= (logand ht-value #xF) +tag-hash-table+)
    (error "puthash requires hash table, got ~X" ht-value))

  (let* ((ht-addr (- ht-value +tag-hash-table+))
         (capacity (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 0))
         (count (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 8))
         (hash (hash-value key-value))
         (bucket-idx (mod hash capacity))
         (bucket-ptr-addr (+ ht-addr 16 (* bucket-idx 8)))
         (bucket (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) (+ 16 (* bucket-idx 8)))))

    ;; Search for existing key
    (let ((current bucket)
          (prev-addr nil))
      (loop
        (when (= current 0)  ; End of list - key not found
          ;; Insert new entry at head of bucket
          (let* ((pair-addr (allocate 16))
                 (entry-addr (allocate 16)))
            ;; Create (key . value) cons
            (setf (sb-sys:sap-ref-64 (sb-sys:int-sap pair-addr) 0) key-value)
            (setf (sb-sys:sap-ref-64 (sb-sys:int-sap pair-addr) 8) value)

            ;; Create (pair . old-bucket) cons
            (setf (sb-sys:sap-ref-64 (sb-sys:int-sap entry-addr) 0)
                  (logior pair-addr #x2))  ; Tag pair as cons
            (setf (sb-sys:sap-ref-64 (sb-sys:int-sap entry-addr) 8) bucket)

            ;; Update bucket pointer
            (setf (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) (+ 16 (* bucket-idx 8)))
                  (logior entry-addr #x2))  ; Tag as cons

            ;; Increment count
            (setf (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 8) (+ count 1)))
          (return value))

        ;; Check if cons cell
        (unless (= (logand current #xF) #x2)
          (error "Corrupt hash table bucket"))

        ;; Get car (the pair) and cdr (rest of list)
        (let* ((cons-addr (- current #x2))
               (pair (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 0))
               (rest (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 8)))

          ;; pair is a cons (key . value)
          (when (= (logand pair #xF) #x2)
            (let* ((pair-addr (- pair #x2))
                   (entry-key (sb-sys:sap-ref-64 (sb-sys:int-sap pair-addr) 0)))
              (when (keys-equal? key-value entry-key)
                ;; Update existing entry
                (setf (sb-sys:sap-ref-64 (sb-sys:int-sap pair-addr) 8) value)
                (return value))))

          ;; Move to next
          (setf prev-addr cons-addr)
          (setf current rest))))))

(defun runtime-remhash (key-value ht-value)
  "Remove key from hash table. Returns non-zero if found, 0 if not found."
  ;; Verify hash table tag
  (unless (= (logand ht-value #xF) +tag-hash-table+)
    (error "remhash requires hash table, got ~X" ht-value))

  (let* ((ht-addr (- ht-value +tag-hash-table+))
         (capacity (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 0))
         (count (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 8))
         (hash (hash-value key-value))
         (bucket-idx (mod hash capacity))
         (bucket (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) (+ 16 (* bucket-idx 8)))))

    ;; Search for key
    (let ((current bucket)
          (prev nil))
      (loop
        (when (= current 0)  ; End of list - not found
          (return 0))

        ;; Check if cons cell
        (unless (= (logand current #xF) #x2)
          (error "Corrupt hash table bucket"))

        ;; Get car (the pair) and cdr (rest of list)
        (let* ((cons-addr (- current #x2))
               (pair (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 0))
               (rest (sb-sys:sap-ref-64 (sb-sys:int-sap cons-addr) 8)))

          ;; pair is a cons (key . value)
          (when (= (logand pair #xF) #x2)
            (let* ((pair-addr (- pair #x2))
                   (entry-key (sb-sys:sap-ref-64 (sb-sys:int-sap pair-addr) 0)))
              (when (keys-equal? key-value entry-key)
                ;; Found it - remove from list
                (if prev
                    ;; Update previous cons to skip this one
                    (setf (sb-sys:sap-ref-64 (sb-sys:int-sap prev) 8) rest)
                    ;; This is first element - update bucket pointer
                    (setf (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) (+ 16 (* bucket-idx 8)))
                          rest))
                ;; Decrement count
                (setf (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 8) (- count 1))
                ;; Return non-zero (tagged fixnum 1)
                (return #x10))))

          ;; Move to next
          (setf prev cons-addr)
          (setf current rest))))))

(defun runtime-hash-table-count (ht-value)
  "Return number of entries in hash table (as tagged fixnum)."
  ;; Verify hash table tag
  (unless (= (logand ht-value #xF) +tag-hash-table+)
    (error "hash-table-count requires hash table, got ~X" ht-value))

  (let* ((ht-addr (- ht-value +tag-hash-table+))
         (count (sb-sys:sap-ref-64 (sb-sys:int-sap ht-addr) 8)))
    ;; Return as tagged fixnum
    (ash count 4)))
