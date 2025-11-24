;;;; Test suite for Habu runtime array operations

(load "memory.lisp")
(load "arrays.lisp")
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

(defun run-array-tests ()
  "Run all array operation tests"
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)

  (format t "~%")
  (format t "========================================~%")
  (format t "  Habu Runtime Array Tests~%")
  (format t "========================================~%")
  (format t "~%")

  ;; Test 1: Array allocation
  (format t "Test 1: Array Allocation~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 5)))
      (test-assert (not (zerop arr)) "Array allocated")
      (test-assert (= (logand arr #xF) +tag-array+) "Array has correct tag")
      (test-assert (= (heap-objects *heap*) 1) "One object allocated")))
  (format t "~%")

  ;; Test 2: Array length
  (format t "Test 2: Array Length~%")
  (with-heap (:size 4096)
    (let ((arr1 (runtime-make-array 0))
          (arr2 (runtime-make-array 10))
          (arr3 (runtime-make-array 100)))
      (test-assert (= (runtime-array-length arr1) 0) "Empty array has length 0")
      (test-assert (= (runtime-array-length arr2) 10) "Array has length 10")
      (test-assert (= (runtime-array-length arr3) 100) "Array has length 100")))
  (format t "~%")

  ;; Test 3: Initial element
  (format t "Test 3: Initial Element~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 5 (* 42 16))))  ; Fixnum 42
      (test-assert (= (runtime-aref arr 0) (* 42 16)) "First element is initial")
      (test-assert (= (runtime-aref arr 2) (* 42 16)) "Middle element is initial")
      (test-assert (= (runtime-aref arr 4) (* 42 16)) "Last element is initial")))
  (format t "~%")

  ;; Test 4: Array access (aref)
  (format t "Test 4: Array Access~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 5)))
      (runtime-aset arr 0 (* 10 16))
      (runtime-aset arr 1 (* 20 16))
      (runtime-aset arr 4 (* 50 16))
      (test-assert (= (runtime-aref arr 0) (* 10 16)) "First element correct")
      (test-assert (= (runtime-aref arr 1) (* 20 16)) "Second element correct")
      (test-assert (= (runtime-aref arr 4) (* 50 16)) "Last element correct")))
  (format t "~%")

  ;; Test 5: Array modification (aset)
  (format t "Test 5: Array Modification~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 3 0)))
      (runtime-aset arr 0 (* 100 16))
      (test-assert (= (runtime-aref arr 0) (* 100 16)) "Element 0 set")
      (runtime-aset arr 0 (* 200 16))
      (test-assert (= (runtime-aref arr 0) (* 200 16)) "Element 0 modified")))
  (format t "~%")

  ;; Test 6: Bounds checking
  (format t "Test 6: Bounds Checking~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 5)))
      (handler-case
          (progn
            (runtime-aref arr 5)
            (test-assert nil "Should have signaled out of bounds"))
        (error (e)
          (test-assert t "Out of bounds error for aref")))
      (handler-case
          (progn
            (runtime-aset arr 10 0)
            (test-assert nil "Should have signaled out of bounds"))
        (error (e)
          (test-assert t "Out of bounds error for aset")))))
  (format t "~%")

  ;; Test 7: Array with pointers (cons cells)
  (format t "Test 7: Array with Pointers~%")
  (with-heap (:size 4096)
    (let* ((cons1 (runtime-cons (* 1 16) (* 2 16)))
           (cons2 (runtime-cons (* 3 16) (* 4 16)))
           (arr (runtime-make-array 2)))
      (runtime-aset arr 0 cons1)
      (runtime-aset arr 1 cons2)
      (test-assert (= (runtime-aref arr 0) cons1) "Array stores cons 1")
      (test-assert (= (runtime-aref arr 1) cons2) "Array stores cons 2")
      (test-assert (= (runtime-car (runtime-aref arr 0)) (* 1 16)) "Cons 1 car intact")
      (test-assert (= (runtime-cdr (runtime-aref arr 1)) (* 4 16)) "Cons 2 cdr intact")))
  (format t "~%")

  ;; Test 8: Array fill
  (format t "Test 8: Array Fill~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 10 0)))
      (runtime-array-fill arr (* 99 16))
      (test-assert (= (runtime-aref arr 0) (* 99 16)) "Element 0 filled")
      (test-assert (= (runtime-aref arr 5) (* 99 16)) "Element 5 filled")
      (test-assert (= (runtime-aref arr 9) (* 99 16)) "Element 9 filled")))
  (format t "~%")

  ;; Test 9: Array copy
  (format t "Test 9: Array Copy~%")
  (with-heap (:size 4096)
    (let ((arr1 (runtime-make-array 3)))
      (runtime-aset arr1 0 (* 10 16))
      (runtime-aset arr1 1 (* 20 16))
      (runtime-aset arr1 2 (* 30 16))
      (let ((arr2 (runtime-array-copy arr1)))
        (test-assert (not (= arr1 arr2)) "Copy is different object")
        (test-assert (= (runtime-array-length arr2) 3) "Copy has same length")
        (test-assert (= (runtime-aref arr2 0) (* 10 16)) "Copy element 0")
        (test-assert (= (runtime-aref arr2 1) (* 20 16)) "Copy element 1")
        (test-assert (= (runtime-aref arr2 2) (* 30 16)) "Copy element 2")
        ;; Modify original, copy should be unchanged
        (runtime-aset arr1 0 (* 999 16))
        (test-assert (= (runtime-aref arr2 0) (* 10 16)) "Copy independent"))))
  (format t "~%")

  ;; Test 10: GC with arrays
  (format t "Test 10: GC with Arrays~%")
  (with-heap (:size 4096)
    (let* ((arr1 (runtime-make-array 3))
           (arr2 (runtime-make-array 3)))
      (test-assert (= (heap-objects *heap*) 2) "Two arrays allocated")
      ;; GC with only arr1 as root
      (gc *heap* (list arr1))
      (test-assert (= (heap-objects *heap*) 1) "One array survives")
      ;; arr1 should still be valid
      (test-assert (= (runtime-array-length arr1) 3) "Surviving array intact")))
  (format t "~%")

  ;; Test 11: GC with array elements
  (format t "Test 11: GC with Array Elements~%")
  (with-heap (:size 4096)
    (let* ((arr (runtime-make-array 2))
           (cons1 (runtime-cons (* 1 16) (* 2 16)))
           (cons2 (runtime-cons (* 3 16) (* 4 16))))
      (runtime-aset arr 0 cons1)
      ;; cons2 not stored in array
      (test-assert (= (heap-objects *heap*) 3) "Array + 2 cons cells")
      ;; GC with arr as root - cons1 should survive, cons2 should be freed
      (gc *heap* (list arr))
      (test-assert (= (heap-objects *heap*) 2) "Array + 1 cons cell survive")
      ;; Array element should still be accessible
      (test-assert (= (runtime-aref arr 0) cons1) "Array element preserved")))
  (format t "~%")

  ;; Test 12: Nested arrays
  (format t "Test 12: Nested Arrays~%")
  (with-heap (:size 4096)
    (let* ((inner (runtime-make-array 2))
           (outer (runtime-make-array 2)))
      (runtime-aset inner 0 (* 10 16))
      (runtime-aset inner 1 (* 20 16))
      (runtime-aset outer 0 inner)
      (test-assert (= (heap-objects *heap*) 2) "Two arrays")
      (let ((retrieved (runtime-aref outer 0)))
        (test-assert (= retrieved inner) "Inner array retrieved")
        (test-assert (= (runtime-aref retrieved 0) (* 10 16)) "Inner element 0")
        (test-assert (= (runtime-aref retrieved 1) (* 20 16)) "Inner element 1"))))
  (format t "~%")

  ;; Test 13: Vector tag verification
  (format t "Test 13: Vector Tag Verification~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 5)))
      (test-assert (= (logand arr #xF) +tag-array+) "Correct array tag")
      (test-assert (= (logand arr #xF) #x3) "Tag value is #x3")))
  (format t "~%")

  ;; Test 14: Empty array
  (format t "Test 14: Empty Array~%")
  (with-heap (:size 4096)
    (let ((arr (runtime-make-array 0)))
      (test-assert (= (runtime-array-length arr) 0) "Empty array has length 0")
      (handler-case
          (progn
            (runtime-aref arr 0)
            (test-assert nil "Should signal out of bounds"))
        (error (e)
          (test-assert t "Out of bounds on empty array")))))
  (format t "~%")

  ;; Test 15: Large array
  (format t "Test 15: Large Array~%")
  (with-heap (:size 16384)
    (let ((arr (runtime-make-array 100)))
      (test-assert (= (runtime-array-length arr) 100) "Large array has correct length")
      (runtime-aset arr 0 (* 1 16))
      (runtime-aset arr 50 (* 50 16))
      (runtime-aset arr 99 (* 99 16))
      (test-assert (= (runtime-aref arr 0) (* 1 16)) "First element")
      (test-assert (= (runtime-aref arr 50) (* 50 16)) "Middle element")
      (test-assert (= (runtime-aref arr 99) (* 99 16)) "Last element")))
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
(run-array-tests)
