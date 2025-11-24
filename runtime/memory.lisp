;;;; Habu Runtime - Memory Management
;;;; Heap allocator and garbage collector for Habu Lisp

(defpackage :habu-runtime
  (:use :cl)
  (:export #:create-heap
           #:initialize-runtime
           #:heap-allocate
           #:runtime-cons
           #:runtime-car
           #:runtime-cdr
           #:gc
           #:gc-stats
           #:heap-stats
           #:print-heap-stats
           #:print-gc-stats
           #:*heap*
           #:with-heap))

(in-package :habu-runtime)

(defconstant +unbound+ #xFFFFFFFFFFFFFFFF)
;;; Heap structure
(defstruct heap
  (memory nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (size 0 :type fixnum)
  (free-pointer 0 :type fixnum)  ; Bump allocator pointer
  (allocated 0 :type fixnum)     ; Total bytes allocated
  (gc-count 0 :type fixnum)      ; Number of GC cycles
  (gc-time 0.0 :type float)      ; Total GC time in seconds
  (objects 0 :type fixnum))      ; Number of objects allocated

;;; Global heap
(defvar *heap* nil
  "Global heap for runtime allocation")

;;; GC roots registry
(defvar *gc-roots* nil
  "List of GC root pointers - objects that should not be collected")

(defun register-gc-root (ptr)
  "Register a pointer as a GC root (prevents collection)"
  (pushnew ptr *gc-roots*))

(defun unregister-gc-root (ptr)
  "Unregister a GC root"
  (setf *gc-roots* (delete ptr *gc-roots*)))

(defun clear-gc-roots ()
  "Clear all registered GC roots"
  (setf *gc-roots* nil))

;;; Object tags (lower 4 bits)
;;; MUST MATCH runtime/object.h tag definitions!
(defconstant +tag-fixnum+     #x0)  ; Fixnum (already shifted left 4)
(defconstant +tag-cons+       #x1)  ; Cons cell
(defconstant +tag-symbol+     #x2)  ; Symbol
(defconstant +tag-vector+     #x3)  ; Vector (array) - MATCHES C TAG_VECTOR
(defconstant +tag-string+     #x4)  ; String - MATCHES C TAG_STRING
(defconstant +tag-closure+    #x5)  ; Function closure - MATCHES C TAG_CLOSURE
(defconstant +tag-float+      #x6)  ; Float (boxed) - FUTURE
(defconstant +tag-bignum+     #x7)  ; Bignum - FUTURE
(defconstant +tag-forward+    #xE)  ; Forwarding pointer (for GC)
(defconstant +tag-free+       #xF)  ; Free block marker

;;; Object header (8 bytes on 64-bit)
;;; [63:56] GC mark bit and flags
;;; [55:4]  Size in bytes (52 bits)
;;; [3:0]   Type tag (4 bits)

(defun make-header (tag size &optional (marked 0))
  "Create an object header"
  (logior tag
          (ash size 4)
          (ash marked 56)))

(defun header-tag (header)
  "Extract tag from header"
  (logand header #xF))

(defun header-size (header)
  "Extract size from header"
  (logand (ash header -4) #xFFFFFFFFFFFF))

(defun header-marked-p (header)
  "Check if object is marked"
  (not (zerop (logand (ash header -56) 1))))

(defun header-set-mark (header)
  "Set mark bit in header"
  (logior header (ash 1 56)))

(defun header-clear-mark (header)
  "Clear mark bit in header"
  (logand header (lognot (ash 1 56))))

;;; Heap creation
(defun create-heap (&key (size (* 1024 1024)))  ; Default 1MB
  "Create a new heap of specified size"
  (make-heap :memory (make-array size
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0)
             :size size
             :free-pointer 0
             :allocated 0
             :gc-count 0
             :gc-time 0.0
             :objects 0))

(defun initialize-runtime (&key (heap-size (* 1024 1024)))
  "Initialize the runtime system with a heap"
  (setf *heap* (create-heap :size heap-size))
  (format t "Habu runtime initialized with ~D byte heap~%" heap-size))

;;; Low-level memory access
(defun write-u64 (heap offset value)
  "Write 64-bit unsigned value to heap at offset"
  (let ((mem (heap-memory heap)))
    (setf (aref mem (+ offset 0)) (ldb (byte 8 0) value))
    (setf (aref mem (+ offset 1)) (ldb (byte 8 8) value))
    (setf (aref mem (+ offset 2)) (ldb (byte 8 16) value))
    (setf (aref mem (+ offset 3)) (ldb (byte 8 24) value))
    (setf (aref mem (+ offset 4)) (ldb (byte 8 32) value))
    (setf (aref mem (+ offset 5)) (ldb (byte 8 40) value))
    (setf (aref mem (+ offset 6)) (ldb (byte 8 48) value))
    (setf (aref mem (+ offset 7)) (ldb (byte 8 56) value))))

(defun read-u64 (heap offset)
  "Read 64-bit unsigned value from heap at offset"
  (let ((mem (heap-memory heap)))
    (logior (aref mem (+ offset 0))
            (ash (aref mem (+ offset 1)) 8)
            (ash (aref mem (+ offset 2)) 16)
            (ash (aref mem (+ offset 3)) 24)
            (ash (aref mem (+ offset 4)) 32)
            (ash (aref mem (+ offset 5)) 40)
            (ash (aref mem (+ offset 6)) 48)
            (ash (aref mem (+ offset 7)) 56))))

;;; Bump allocator
(defun heap-allocate (heap size-bytes tag)
  "Allocate object on heap, returns pointer (tagged)"
  (let* ((total-size (+ 8 size-bytes))  ; Header + data
         (aligned-size (* 16 (ceiling total-size 16)))  ; 16-byte alignment for 4-bit tags
         (free-ptr (heap-free-pointer heap)))
    ;; Check if we have space
    (when (> (+ free-ptr aligned-size) (heap-size heap))
      ;; Try GC first with registered roots
      (gc heap *gc-roots*)
      (setf free-ptr (heap-free-pointer heap))
      ;; Check again
      (when (> (+ free-ptr aligned-size) (heap-size heap))
        (error "Out of heap memory: need ~D bytes, have ~D (after GC with ~D roots)"
               aligned-size (- (heap-size heap) free-ptr) (length *gc-roots*))))

    ;; Allocate
    (let ((header (make-header tag size-bytes 0)))
      ;; Write header
      (write-u64 heap free-ptr header)
      ;; Update heap state
      (setf (heap-free-pointer heap) (+ free-ptr aligned-size))
      (incf (heap-allocated heap) aligned-size)
      (incf (heap-objects heap))
      ;; Return pointer to header with tag in lower bits
      ;; Since free-ptr is 16-byte aligned (from previous allocation),
      ;; lower 4 bits are 0, so we can OR in the tag
      (logior free-ptr tag))))

;;; Cons cell allocation
(defun runtime-cons (car cdr)
  "Allocate a cons cell on the heap"
  (unless *heap*
    (error "Runtime not initialized - call (initialize-runtime)"))
  (let* ((ptr (heap-allocate *heap* 16 +tag-cons+))  ; 2 * 8 bytes
         (header-addr (logand ptr (lognot #xF)))     ; Remove tag to get header address
         (data-addr (+ header-addr 8)))               ; Data starts after header
    ;; Write car and cdr (both are tagged pointers or fixnums)
    (write-u64 *heap* data-addr car)
    (write-u64 *heap* (+ data-addr 8) cdr)
    ptr))

(defun runtime-car (cons-ptr)
  "Read car from cons cell"
  (unless (= (logand cons-ptr #xF) +tag-cons+)
    (error "Not a cons cell: ~X" cons-ptr))
  (let* ((header-addr (logand cons-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (read-u64 *heap* data-addr)))

(defun runtime-cdr (cons-ptr)
  "Read cdr from cons cell"
  (unless (= (logand cons-ptr #xF) +tag-cons+)
    (error "Not a cons cell: ~X" cons-ptr))
  (let* ((header-addr (logand cons-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (read-u64 *heap* (+ data-addr 8))))

;;; Mark-and-sweep garbage collector
(defun gc-mark-object (heap ptr)
  "Mark an object and its children (recursive)"
  (when (zerop ptr)  ; nil
    (return-from gc-mark-object))

  (let ((tag (logand ptr #xF)))
    (when (= tag +tag-fixnum+)  ; Fixnums don't need marking
      (return-from gc-mark-object))

    ;; Get object header
    (let* ((header-addr (logand ptr (lognot #xF)))
           (header (read-u64 heap header-addr)))

      ;; Already marked?
      (when (header-marked-p header)
        (return-from gc-mark-object))

      ;; Mark object
      (write-u64 heap header-addr (header-set-mark header))

      ;; Mark children based on type
      (let ((data-addr (+ header-addr 8)))
        (case tag
          (#.+tag-cons+
           ;; Mark car and cdr
           (gc-mark-object heap (read-u64 heap data-addr))
           (gc-mark-object heap (read-u64 heap (+ data-addr 8))))
          (#.+tag-symbol+
           ;; Mark symbol's name, value, function, plist
           (let ((name-ptr (read-u64 heap data-addr))
                 (value (read-u64 heap (+ data-addr 8)))
                 (fn (read-u64 heap (+ data-addr 16)))
                 (plist (read-u64 heap (+ data-addr 24))))
             ;; Mark name if it's a pointer (not fixnum)
             (when (and (not (zerop name-ptr))
                        (not (= (logand name-ptr #xF) +tag-fixnum+)))
               (gc-mark-object heap name-ptr))
             ;; Mark value if bound and is a pointer
             (when (and (not (zerop value))
                        (not (= value +unbound+))
                        (not (= (logand value #xF) +tag-fixnum+)))
               (gc-mark-object heap value))
             ;; Mark function if bound and is a pointer
             (when (and (not (zerop fn))
                        (not (= fn +unbound+))
                        (not (= (logand fn #xF) +tag-fixnum+)))
               (gc-mark-object heap fn))
             ;; Mark plist if it's a cons
             (when (and (not (zerop plist))
                        (= (logand plist #xF) +tag-cons+))
               (gc-mark-object heap plist))))
          (#.+tag-vector+
           ;; Mark vector elements that are pointers
           (let* ((length (read-u64 heap data-addr)))
             (loop for i from 0 below length
                   for element = (read-u64 heap (+ data-addr 8 (* i 8)))
                   do (when (and (not (zerop element))
                                 (not (= (logand element #xF) +tag-fixnum+)))
                        (gc-mark-object heap element)))))
          (#.+tag-string+
           ;; Strings have no pointers, just mark the object
           nil)
          ;; Other types would be handled here
          )))))

(defun gc-mark-roots (heap roots)
  "Mark all reachable objects from roots"
  (dolist (root roots)
    (gc-mark-object heap root)))

(defun gc-sweep (heap)
  "Sweep unmarked objects and compact heap with proper pointer fixup"
  (let ((read-ptr 0)
        (write-ptr 0)
        (freed-bytes 0)
        (freed-objects 0)
        (forwarding-table (make-hash-table)))  ;; Maps old-addr -> new-addr

    ;; PASS 1: Calculate new addresses and build forwarding table
    (loop while (< read-ptr (heap-free-pointer heap))
          do (let* ((header (read-u64 heap read-ptr))
                    (tag (header-tag header))
                    (size (header-size header))
                    (total-size (* 16 (ceiling (+ 8 size) 16))))
               (if (header-marked-p header)
                   ;; Live object - record forwarding address
                   (progn
                     (setf (gethash read-ptr forwarding-table) write-ptr)
                     (incf write-ptr total-size))
                   ;; Dead object - count as freed
                   (progn
                     (incf freed-bytes total-size)
                     (incf freed-objects)))
               (incf read-ptr total-size)))

    ;; PASS 2: Copy objects and update pointers
    (setf read-ptr 0)
    (setf write-ptr 0)
    (loop while (< read-ptr (heap-free-pointer heap))
          do (let* ((header (read-u64 heap read-ptr))
                    (tag (header-tag header))
                    (size (header-size header))
                    (total-size (* 16 (ceiling (+ 8 size) 16))))
               (when (header-marked-p header)
                 ;; Copy header with mark cleared
                 (write-u64 heap write-ptr (header-clear-mark header))

                 ;; Copy and fixup object data
                 (case tag
                   (#.+tag-cons+
                    ;; Cons: car and cdr are pointers that need fixing
                    (let ((car (read-u64 heap (+ read-ptr 8)))
                          (cdr (read-u64 heap (+ read-ptr 16))))
                      (write-u64 heap (+ write-ptr 8) (fixup-pointer car forwarding-table))
                      (write-u64 heap (+ write-ptr 16) (fixup-pointer cdr forwarding-table))))

                   (#.+tag-symbol+
                    ;; Symbol: name, value, plist are pointers
                    (let ((name (read-u64 heap (+ read-ptr 8)))
                          (value (read-u64 heap (+ read-ptr 16)))
                          (plist (read-u64 heap (+ read-ptr 24))))
                      (write-u64 heap (+ write-ptr 8) (fixup-pointer name forwarding-table))
                      (write-u64 heap (+ write-ptr 16) (fixup-pointer value forwarding-table))
                      (write-u64 heap (+ write-ptr 24) (fixup-pointer plist forwarding-table))))

                   (#.+tag-vector+
                    ;; Vector: length + array of pointers
                    (let ((length (read-u64 heap (+ read-ptr 8))))
                      (write-u64 heap (+ write-ptr 8) length)
                      (loop for i from 0 below length
                            for elem-offset = (+ read-ptr 16 (* i 8))
                            do (let ((elem (read-u64 heap elem-offset)))
                                 (write-u64 heap (+ write-ptr 16 (* i 8))
                                           (fixup-pointer elem forwarding-table))))))

                   (#.+tag-string+
                    ;; String: length + bytes (no pointers)
                    (loop for i from 8 below total-size
                          do (setf (aref (heap-memory heap) (+ write-ptr i))
                                   (aref (heap-memory heap) (+ read-ptr i)))))

                   (t
                    ;; Unknown type: just copy bytes
                    (loop for i from 8 below total-size
                          do (setf (aref (heap-memory heap) (+ write-ptr i))
                                   (aref (heap-memory heap) (+ read-ptr i))))))

                 (incf write-ptr total-size))
               (incf read-ptr total-size)))

    ;; Update heap state
    (setf (heap-free-pointer heap) write-ptr)
    (decf (heap-allocated heap) freed-bytes)
    (decf (heap-objects heap) freed-objects)

    (list :freed-bytes freed-bytes :freed-objects freed-objects)))

(defun fixup-pointer (ptr forwarding-table)
  "Update pointer using forwarding table, or return as-is if not relocated"
  (cond
    ;; NIL (0) - no fixup needed
    ((zerop ptr) 0)
    ;; Fixnum - no fixup needed
    ((= (logand ptr #xF) +tag-fixnum+) ptr)
    ;; Pointer - check forwarding table
    (t
     (let* ((tag (logand ptr #xF))
            (old-addr (- ptr tag))
            (new-addr (gethash old-addr forwarding-table)))
       (if new-addr
           (logior new-addr tag)  ;; Relocated - return new address with tag
           ptr)))))  ;; Not relocated (maybe in old gen) - return as-is

(defun gc (&optional (heap *heap*) (roots nil))
  "Perform garbage collection"
  (unless heap
    (error "No heap to collect"))

  (let ((start-time (get-internal-real-time)))
    ;; Mark phase
    (gc-mark-roots heap roots)

    ;; Sweep phase
    (let ((stats (gc-sweep heap)))
      ;; Update GC stats
      (incf (heap-gc-count heap))
      (let ((gc-time (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (incf (heap-gc-time heap) gc-time)
        (append stats (list :gc-time gc-time :gc-count (heap-gc-count heap)))))))

;;; Statistics
(defun heap-stats (&optional (heap *heap*))
  "Return heap statistics"
  (unless heap
    (error "No heap"))
  (list :size (heap-size heap)
        :allocated (heap-allocated heap)
        :free (- (heap-size heap) (heap-allocated heap))
        :objects (heap-objects heap)
        :utilization (if (zerop (heap-size heap))
                         0.0
                         (float (/ (heap-allocated heap) (heap-size heap))))))

(defun gc-stats (&optional (heap *heap*))
  "Return garbage collector statistics"
  (unless heap
    (error "No heap"))
  (list :gc-count (heap-gc-count heap)
        :gc-time (heap-gc-time heap)
        :avg-gc-time (if (zerop (heap-gc-count heap))
                         0.0
                         (/ (heap-gc-time heap) (heap-gc-count heap)))))

;;; Utility macro
(defmacro with-heap ((&key (size (* 1024 1024))) &body body)
  "Execute body with a fresh heap"
  `(let ((*heap* (create-heap :size ,size)))
     ,@body))

;;; Pretty printing
(defun print-heap-stats (&optional (heap *heap*))
  "Print heap statistics"
  (let ((stats (heap-stats heap)))
    (format t "Heap Statistics:~%")
    (format t "  Size:        ~:D bytes~%" (getf stats :size))
    (format t "  Allocated:   ~:D bytes~%" (getf stats :allocated))
    (format t "  Free:        ~:D bytes~%" (getf stats :free))
    (format t "  Objects:     ~:D~%" (getf stats :objects))
    (format t "  Utilization: ~,1F%~%" (* 100 (getf stats :utilization)))))

(defun print-gc-stats (&optional (heap *heap*))
  "Print GC statistics"
  (let ((stats (gc-stats heap)))
    (format t "GC Statistics:~%")
    (format t "  GC count:    ~:D~%" (getf stats :gc-count))
    (format t "  Total time:  ~,3F seconds~%" (getf stats :gc-time))
    (format t "  Avg time:    ~,3F seconds~%" (getf stats :avg-gc-time))))
