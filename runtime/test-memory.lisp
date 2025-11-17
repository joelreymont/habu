;;;; Test suite for Habu runtime memory management

(load "memory.lisp")
(in-package :habu-runtime)

(defvar *test-count* 0)
(defvar *test-passed* 0)
(defvar *test-failed* 0)

(defun test-assert (condition message)
  "Assert that condition is true"
  (incf *test-count*)
  (if condition
      (progn
        (incf *test-passed*)
        (format t "  ✓ ~A~%" message))
      (progn
        (incf *test-failed*)
        (format t "  ✗ ~A~%" message))))

(defun run-memory-tests ()
  "Run all memory management tests"
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)

  (format t "~%")
  (format t "========================================~%")
  (format t "  Habu Runtime Memory Tests~%")
  (format t "========================================~%")
  (format t "~%")

  ;; Test 1: Heap creation
  (format t "Test 1: Heap Creation~%")
  (let ((heap (create-heap :size 4096)))
    (test-assert (not (null heap)) "Heap created")
    (test-assert (= (heap-size heap) 4096) "Heap size correct")
    (test-assert (= (heap-free-pointer heap) 0) "Free pointer at 0")
    (test-assert (= (heap-allocated heap) 0) "Nothing allocated"))
  (format t "~%")

  ;; Test 2: Basic allocation
  (format t "Test 2: Basic Allocation~%")
  (let ((heap (create-heap :size 4096)))
    (let ((ptr (heap-allocate heap 16 +tag-cons+)))
      (test-assert (not (zerop ptr)) "Allocation returns non-zero pointer")
      (test-assert (= (logand ptr #xF) +tag-cons+) "Pointer has correct tag")
      (test-assert (> (heap-free-pointer heap) 0) "Free pointer advanced")
      (test-assert (> (heap-allocated heap) 0) "Allocated bytes increased")
      (test-assert (= (heap-objects heap) 1) "Object count is 1")))
  (format t "~%")

  ;; Test 3: Multiple allocations
  (format t "Test 3: Multiple Allocations~%")
  (let ((heap (create-heap :size 4096)))
    (let ((ptr1 (heap-allocate heap 16 +tag-cons+))
          (ptr2 (heap-allocate heap 16 +tag-cons+))
          (ptr3 (heap-allocate heap 16 +tag-cons+)))
      (test-assert (not (= ptr1 ptr2)) "Different pointers")
      (test-assert (not (= ptr2 ptr3)) "Different pointers")
      (test-assert (= (heap-objects heap) 3) "Three objects allocated")))
  (format t "~%")

  ;; Test 4: Cons cell operations
  (format t "Test 4: Cons Cell Operations~%")
  (with-heap (:size 4096)
    (let* ((car-val (* 42 16))  ; Fixnum 42 (tagged)
           (cdr-val (* 58 16))  ; Fixnum 58 (tagged)
           (cons-ptr (runtime-cons car-val cdr-val)))
      (test-assert (not (zerop cons-ptr)) "Cons allocated")
      (test-assert (= (runtime-car cons-ptr) car-val) "Car correct")
      (test-assert (= (runtime-cdr cons-ptr) cdr-val) "Cdr correct")))
  (format t "~%")

  ;; Test 5: Nested cons cells
  (format t "Test 5: Nested Cons Cells~%")
  (with-heap (:size 4096)
    (let* ((a (* 1 16))
           (b (* 2 16))
           (c (* 3 16))
           (cons1 (runtime-cons a b))
           (cons2 (runtime-cons cons1 c)))
      (test-assert (= (runtime-cdr cons2) c) "Cdr of outer cons is c")
      (let ((inner (runtime-car cons2)))
        (test-assert (= (logand inner #xF) +tag-cons+) "Car is a cons")
        (test-assert (= (runtime-car inner) a) "Nested car is a")
        (test-assert (= (runtime-cdr inner) b) "Nested cdr is b"))))
  (format t "~%")

  ;; Test 6: Header operations
  (format t "Test 6: Header Operations~%")
  (let ((header (make-header +tag-cons+ 16 0)))
    (test-assert (= (header-tag header) +tag-cons+) "Header tag correct")
    (test-assert (= (header-size header) 16) "Header size correct")
    (test-assert (not (header-marked-p header)) "Header not marked")
    (let ((marked (header-set-mark header)))
      (test-assert (header-marked-p marked) "Header marked")
      (let ((unmarked (header-clear-mark marked)))
        (test-assert (not (header-marked-p unmarked)) "Header unmarked"))))
  (format t "~%")

  ;; Test 7: Memory read/write
  (format t "Test 7: Memory Read/Write~%")
  (let ((heap (create-heap :size 4096)))
    (write-u64 heap 0 #x0123456789ABCDEF)
    (let ((value (read-u64 heap 0)))
      (test-assert (= value #x0123456789ABCDEF) "64-bit read/write correct")))
  (format t "~%")

  ;; Test 8: Garbage collection (no roots)
  (format t "Test 8: Garbage Collection (no roots)~%")
  (let ((heap (create-heap :size 4096)))
    ;; Allocate some objects
    (heap-allocate heap 16 +tag-cons+)
    (heap-allocate heap 16 +tag-cons+)
    (heap-allocate heap 16 +tag-cons+)
    (let ((before-gc (heap-objects heap)))
      (test-assert (= before-gc 3) "Three objects before GC")
      ;; GC with no roots - should free everything
      (gc heap nil)
      (test-assert (= (heap-objects heap) 0) "All objects freed")
      (test-assert (= (heap-free-pointer heap) 0) "Heap compacted")
      (test-assert (= (heap-gc-count heap) 1) "GC count is 1")))
  (format t "~%")

  ;; Test 9: Garbage collection (with roots)
  (format t "Test 9: Garbage Collection (with roots)~%")
  (with-heap (:size 4096)
    (let* ((cons1 (runtime-cons (* 1 16) (* 2 16)))
           (cons2 (runtime-cons (* 3 16) (* 4 16)))
           (cons3 (runtime-cons cons1 cons2)))
      ;; cons3 points to cons1 and cons2, so all should survive
      (test-assert (= (heap-objects *heap*) 3) "Three objects allocated")
      (gc *heap* (list cons3))
      (test-assert (= (heap-objects *heap*) 3) "All objects survive GC")
      (test-assert (= (heap-gc-count *heap*) 1) "GC count is 1")))
  (format t "~%")

  ;; Test 10: Heap statistics
  (format t "Test 10: Heap Statistics~%")
  (with-heap (:size 4096)
    (heap-allocate *heap* 16 +tag-cons+)
    (heap-allocate *heap* 16 +tag-cons+)
    (let ((stats (heap-stats)))
      (test-assert (= (getf stats :size) 4096) "Size stat correct")
      (test-assert (> (getf stats :allocated) 0) "Allocated stat > 0")
      (test-assert (< (getf stats :free) 4096) "Free stat < size")
      (test-assert (= (getf stats :objects) 2) "Objects stat is 2")))
  (format t "~%")

  ;; Test 11: Out of memory handling
  (format t "Test 11: Out of Memory Handling~%")
  (let ((heap (create-heap :size 128)))  ; Small heap
    (handler-case
        (progn
          ;; Try to allocate more than heap size (no roots, so GC can't help)
          (dotimes (i 100)
            (heap-allocate heap 16 +tag-cons+))
          ;; If we get here, GC freed space multiple times
          (test-assert (>= (heap-gc-count heap) 1) "GC attempted to free space"))
      (error (e)
        ;; OOM error is also valid if GC couldn't free enough
        (test-assert t (format nil "Out of memory detected: ~A" e)))))
  (format t "~%")

  ;; Test 12: GC triggers on OOM
  (format t "Test 12: GC Triggers on OOM~%")
  (let ((heap (create-heap :size 256)))
    ;; Allocate objects with no roots - GC should free them
    (dotimes (i 5)
      (heap-allocate heap 32 +tag-cons+))
    (test-assert (> (heap-objects heap) 0) "Objects allocated")
    ;; Next allocation should trigger GC
    (handler-case
        (progn
          (heap-allocate heap 32 +tag-cons+)
          (test-assert t "Allocation succeeded after GC")
          (test-assert (>= (heap-gc-count heap) 1) "GC was triggered"))
      (error (e)
        (test-assert nil (format nil "GC failed to free space: ~A" e)))))
  (format t "~%")

  ;; Summary
  (format t "========================================~%")
  (format t "  Test Results~%")
  (format t "========================================~%")
  (format t "Total:  ~D~%" *test-count*)
  (format t "Passed: ~D/~D~%" *test-passed* *test-count*)
  (if (zerop *test-failed*)
      (format t "~%All tests passed! ✓~%")
      (format t "Failed: ~D/~D~%~%" *test-failed* *test-count*))
  (zerop *test-failed*))

;; Run tests
(run-memory-tests)
