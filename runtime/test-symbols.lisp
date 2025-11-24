;;;; Test suite for Habu runtime symbol table

(defun load-relative (name)
  (load (merge-pathnames name (or *load-pathname* (truename ".")))))

(load-relative "memory.lisp")
(load-relative "strings.lisp")
(load-relative "symbols.lisp")
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

(defun run-symbol-tests ()
  "Run all symbol table tests"
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)

  (format t "~%")
  (format t "========================================~%")
  (format t "  Habu Runtime Symbol Tests~%")
  (format t "========================================~%")
  (format t "~%")

  ;; Test 1: Symbol allocation
  (format t "Test 1: Symbol Allocation~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((name-fixnum (* 42 16))  ; Fixnum for name
           (sym (allocate-symbol name-fixnum)))
      (test-assert (not (zerop sym)) "Symbol allocated")
      (test-assert (= (logand sym #xF) +tag-symbol+) "Symbol has correct tag")
      (test-assert (= (heap-objects *heap*) 1) "One object allocated")))
  (format t "~%")

  ;; Test 2: Interning
  (format t "Test 2: Symbol Interning~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((sym1 (runtime-intern "FOO"))
           (sym2 (runtime-intern "FOO"))
           (sym3 (runtime-intern "BAR")))
      (test-assert (= sym1 sym2) "Same symbol returned for same name")
      (test-assert (not (= sym1 sym3)) "Different symbols for different names")
      (test-assert (= (heap-objects *heap*) 4) "Two unique symbols allocated")))
  (format t "~%")

  ;; Test 3: Uninterned symbols
  (format t "Test 3: Uninterned Symbols~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((sym1 (runtime-make-symbol "TEMP"))
           (sym2 (runtime-make-symbol "TEMP")))
      (test-assert (not (= sym1 sym2)) "Different uninterned symbols")
      (test-assert (= (heap-objects *heap*) 4) "Two symbols allocated")
      ;; Neither should be in symbol table
      (test-assert (null (gethash "TEMP" *symbol-table*))
                   "Uninterned symbol not in table")))
  (format t "~%")

  ;; Test 4: Symbol value
  (format t "Test 4: Symbol Value~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let ((sym (runtime-intern "X")))
      (test-assert (not (zerop sym)) "Symbol created")
      ;; Initially unbound
      (handler-case
          (progn
            (runtime-symbol-value sym)
            (test-assert nil "Should have signaled unbound error"))
        (error (e)
          (test-assert t "Unbound variable error signaled")))
      ;; Set value
      (set-symbol-value sym (* 42 16))  ; Fixnum 42
      (test-assert (= (runtime-symbol-value sym) (* 42 16)) "Value set correctly")
      ;; Change value
      (set-symbol-value sym (* 100 16))
      (test-assert (= (runtime-symbol-value sym) (* 100 16)) "Value changed")))
  (format t "~%")

  ;; Test 5: Symbol function
  (format t "Test 5: Symbol Function~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let ((sym (runtime-intern "SQUARE")))
      ;; Initially unbound
      (handler-case
          (progn
            (runtime-symbol-function sym)
            (test-assert nil "Should have signaled undefined function error"))
        (error (e)
          (test-assert t "Undefined function error signaled")))
      ;; Set function
      (let ((fn-ptr (* 123 16)))  ; Dummy function pointer
        (set-symbol-function sym fn-ptr)
        (test-assert (= (runtime-symbol-function sym) fn-ptr) "Function set correctly"))))
  (format t "~%")

  ;; Test 6: Symbol plist
  (format t "Test 6: Symbol Property List~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let ((sym (runtime-intern "FOO")))
      (test-assert (zerop (runtime-symbol-plist sym)) "Plist initially nil")
      ;; Set plist to a cons cell
      (let* ((key (* 1 16))
             (val (* 2 16))
             (plist (runtime-cons key val)))
        (set-symbol-plist sym plist)
        (test-assert (= (runtime-symbol-plist sym) plist) "Plist set correctly")
        (test-assert (= (runtime-car plist) key) "Plist car is key")
        (test-assert (= (runtime-cdr plist) val) "Plist cdr is value"))))
  (format t "~%")

  ;; Test 7: Gensym
  (format t "Test 7: Gensym~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((sym1 (runtime-gensym))
           (sym2 (runtime-gensym))
           (sym3 (runtime-gensym "TEMP")))
      (test-assert (not (= sym1 sym2)) "Gensyms are unique")
      (test-assert (not (= sym2 sym3)) "Gensyms are unique")
      (test-assert (= (heap-objects *heap*) 6) "Three symbols allocated")
      ;; None should be interned
      (let ((found-interned nil))
        (maphash (lambda (name ptr)
                   (when (or (= ptr sym1) (= ptr sym2) (= ptr sym3))
                     (setf found-interned t)))
                 *symbol-table*)
        (test-assert (not found-interned) "Gensyms not interned"))))
  (format t "~%")

  ;; Test 8: Symbol name lookup
  (format t "Test 8: Symbol Name Lookup~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let ((sym (runtime-intern "MY-VAR")))
      (test-assert (string= (runtime-symbol-name sym) "MY-VAR")
                   "Symbol name lookup works")))
  (format t "~%")

  ;; Test 9: Multiple symbols with values
  (format t "Test 9: Multiple Symbols with Values~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((x (runtime-intern "X"))
           (y (runtime-intern "Y"))
           (z (runtime-intern "Z")))
      (set-symbol-value x (* 10 16))
      (set-symbol-value y (* 20 16))
      (set-symbol-value z (* 30 16))
      (test-assert (= (runtime-symbol-value x) (* 10 16)) "X value correct")
      (test-assert (= (runtime-symbol-value y) (* 20 16)) "Y value correct")
      (test-assert (= (runtime-symbol-value z) (* 30 16)) "Z value correct")))
  (format t "~%")

  ;; Test 10: GC with symbols
  (format t "Test 10: GC with Symbols~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((sym1 (runtime-intern "ALIVE"))
           (sym2 (runtime-make-symbol "DEAD")))
      (test-assert (= (heap-objects *heap*) 4) "Two symbols allocated")
      ;; GC with only sym1 as root
      (gc *heap* (list sym1))
      (test-assert (= (heap-objects *heap*) 2) "One symbol survives")
      ;; sym1 should still be valid
      (test-assert (= (logand sym1 #xF) +tag-symbol+) "Surviving symbol still valid")))
  (format t "~%")

  ;; Test 11: GC with symbol values
  (format t "Test 11: GC with Symbol Values~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((sym (runtime-intern "VAR"))
           (cons1 (runtime-cons (* 1 16) (* 2 16)))
           (cons2 (runtime-cons (* 3 16) (* 4 16))))
      ;; Set symbol value to cons1
      (set-symbol-value sym cons1)
      (test-assert (= (heap-objects *heap*) 4) "Symbol + 2 cons cells")
      ;; GC with sym as root - cons1 should survive, cons2 should be freed
      (gc *heap* (list sym))
      (test-assert (= (heap-objects *heap*) 3) "Symbol + 1 cons cell survive")
      ;; Symbol value should still be accessible
      (test-assert (= (runtime-symbol-value sym) cons1) "Symbol value preserved")))
  (format t "~%")

  ;; Test 12: Symbol with cons plist
  (format t "Test 12: Symbol with Cons Plist~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((sym (runtime-intern "FOO"))
           (prop (runtime-cons (* 10 16) (* 20 16))))
      (set-symbol-plist sym prop)
      (test-assert (= (heap-objects *heap*) 3) "Symbol + cons")
      ;; GC with sym as root - plist should survive
      (gc *heap* (list sym))
      (test-assert (= (heap-objects *heap*) 3) "Both objects survive")
      (test-assert (= (runtime-symbol-plist sym) prop) "Plist preserved")))
  (format t "~%")

  ;; Test 13: Clear symbol table
  (format t "Test 13: Clear Symbol Table~%")
  (with-heap (:size 4096)
    (runtime-intern "A")
    (runtime-intern "B")
    (runtime-intern "C")
    (test-assert (> (hash-table-count *symbol-table*) 0) "Table has symbols")
    (clear-symbol-table)
    (test-assert (zerop (hash-table-count *symbol-table*)) "Table cleared")
    (test-assert (zerop *habu-gensym-counter*) "Gensym counter reset"))
  (format t "~%")

  ;; Test 14: Symbol tag verification
  (format t "Test 14: Symbol Tag Verification~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let ((sym (runtime-intern "TEST")))
      (test-assert (= (logand sym #xF) +tag-symbol+) "Correct symbol tag")
      (test-assert (= (logand sym #xF) 2) "Tag value is 2")))
  (format t "~%")

  ;; Test 15: Symbol allocation size
  (format t "Test 15: Symbol Allocation Size~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((before (heap-free-pointer *heap*))
           (sym (runtime-intern "X"))
           (after (heap-free-pointer *heap*))
           (allocated (- after before)))
      ;; Symbol allocation includes name string; expect at least one aligned block.
      (test-assert (>= allocated 48) "Symbol allocation increases heap")))
  (format t "~%")

  ;; Test 16: Package export visibility
  (format t "Test 16: Package Export Visibility~%")
  (with-heap (:size 4096)
    (clear-symbol-table)
    (let* ((ax (runtime-find-symbol "FOO" "PKG-X"))
           (ay (runtime-find-symbol "BAR" "PKG-X")))
      (runtime-export-symbols (list "FOO") "PKG-X")
      (runtime-in-package "PKG-Z")
      (runtime-use-package "PKG-X")
      (let* ((foo-from-z (runtime-find-symbol "FOO"))
             (bar-from-z (runtime-find-symbol "BAR")))
        (test-assert (= ax foo-from-z) "Exported symbol visible via use-package")
        (test-assert (not (= ay bar-from-z))
                     "Unexported symbol not reused via use-package")))
    (runtime-in-package "HABU-USER"))
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
(run-symbol-tests)
