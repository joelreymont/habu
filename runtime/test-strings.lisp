;;;; Test suite for Habu runtime string operations

(load "memory.lisp")
(load "strings.lisp")
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

(defun run-string-tests ()
  "Run all string operation tests"
  (setf *test-count* 0
        *test-passed* 0
        *test-failed* 0)

  (format t "~%")
  (format t "========================================~%")
  (format t "  Habu Runtime String Tests~%")
  (format t "========================================~%")
  (format t "~%")

  ;; Test 1: String allocation
  (format t "Test 1: String Allocation~%")
  (with-heap (:size 4096)
    (let ((str (runtime-make-string "Hello")))
      (test-assert (not (zerop str)) "String allocated")
      (test-assert (= (logand str #xF) +tag-string+) "String has correct tag")
      (test-assert (= (heap-objects *heap*) 1) "One object allocated")))
  (format t "~%")

  ;; Test 2: String length
  (format t "Test 2: String Length~%")
  (with-heap (:size 4096)
    (let ((str1 (runtime-make-string "Hello"))
          (str2 (runtime-make-string ""))
          (str3 (runtime-make-string "A longer string!")))
      (test-assert (= (runtime-string-length str1) 5) "Length of 'Hello' is 5")
      (test-assert (= (runtime-string-length str2) 0) "Length of '' is 0")
      (test-assert (= (runtime-string-length str3) 16) "Length of long string is 16")))
  (format t "~%")

  ;; Test 3: String ref (character access)
  (format t "Test 3: String Character Access~%")
  (with-heap (:size 4096)
    (let ((str (runtime-make-string "Hello")))
      (test-assert (= (runtime-string-ref str 0) (char-code #\H)) "First char is H")
      (test-assert (= (runtime-string-ref str 1) (char-code #\e)) "Second char is e")
      (test-assert (= (runtime-string-ref str 4) (char-code #\o)) "Last char is o")))
  (format t "~%")

  ;; Test 4: String set (character modification)
  (format t "Test 4: String Character Modification~%")
  (with-heap (:size 4096)
    (let ((str (runtime-make-string "Hello")))
      (runtime-string-set str 0 (char-code #\h))
      (test-assert (= (runtime-string-ref str 0) (char-code #\h)) "First char changed to h")
      (runtime-string-set str 4 (char-code #\!))
      (test-assert (= (runtime-string-ref str 4) (char-code #\!)) "Last char changed to !")
      ;; "Hello" with indices 0:H 1:e 2:l 3:l 4:o, changing 0 and 4 gives "hell!"
      (test-assert (string= (runtime-string->lisp str) "hell!") "String is now 'hell!'")))
  (format t "~%")

  ;; Test 5: String to Lisp conversion
  (format t "Test 5: String to Lisp Conversion~%")
  (with-heap (:size 4096)
    (let ((str1 (runtime-make-string "Hello, World!"))
          (str2 (runtime-make-string ""))
          (str3 (runtime-make-string "ABC123")))
      (test-assert (string= (runtime-string->lisp str1) "Hello, World!") "Conversion of 'Hello, World!'")
      (test-assert (string= (runtime-string->lisp str2) "") "Conversion of empty string")
      (test-assert (string= (runtime-string->lisp str3) "ABC123") "Conversion of 'ABC123'")))
  (format t "~%")

  ;; Test 6: String equality
  (format t "Test 6: String Equality~%")
  (with-heap (:size 4096)
    (let ((str1 (runtime-make-string "Hello"))
          (str2 (runtime-make-string "Hello"))
          (str3 (runtime-make-string "World"))
          (str4 (runtime-make-string "Hell")))
      (test-assert (runtime-string-equal str1 str2) "Equal strings compare equal")
      (test-assert (not (runtime-string-equal str1 str3)) "Different strings not equal")
      (test-assert (not (runtime-string-equal str1 str4)) "Different length strings not equal")))
  (format t "~%")

  ;; Test 7: String concatenation
  (format t "Test 7: String Concatenation~%")
  (with-heap (:size 4096)
    (let* ((str1 (runtime-make-string "Hello"))
           (str2 (runtime-make-string " World"))
           (result (runtime-string-concat str1 str2)))
      (test-assert (= (runtime-string-length result) 11) "Concatenated length is 11")
      (test-assert (string= (runtime-string->lisp result) "Hello World") "Concatenation correct")))
  (format t "~%")

  ;; Test 8: Empty string concatenation
  (format t "Test 8: Empty String Concatenation~%")
  (with-heap (:size 4096)
    (let* ((str1 (runtime-make-string "Hello"))
           (str2 (runtime-make-string ""))
           (result1 (runtime-string-concat str1 str2))
           (result2 (runtime-string-concat str2 str1)))
      (test-assert (string= (runtime-string->lisp result1) "Hello") "Concat with empty (right)")
      (test-assert (string= (runtime-string->lisp result2) "Hello") "Concat with empty (left)")))
  (format t "~%")

  ;; Test 9: Substring extraction
  (format t "Test 9: Substring Extraction~%")
  (with-heap (:size 4096)
    (let* ((str (runtime-make-string "Hello World"))
           (sub1 (runtime-string-substring str 0 5))
           (sub2 (runtime-string-substring str 6 11))
           (sub3 (runtime-string-substring str 6)))
      (test-assert (string= (runtime-string->lisp sub1) "Hello") "Substring [0:5]")
      (test-assert (string= (runtime-string->lisp sub2) "World") "Substring [6:11]")
      (test-assert (string= (runtime-string->lisp sub3) "World") "Substring [6:end]")))
  (format t "~%")

  ;; Test 10: Empty substring
  (format t "Test 10: Empty Substring~%")
  (with-heap (:size 4096)
    (let* ((str (runtime-make-string "Hello"))
           (sub (runtime-string-substring str 2 2)))
      (test-assert (= (runtime-string-length sub) 0) "Empty substring has length 0")
      (test-assert (string= (runtime-string->lisp sub) "") "Empty substring is empty")))
  (format t "~%")

  ;; Test 11: GC with strings
  (format t "Test 11: GC with Strings~%")
  (with-heap (:size 4096)
    (let* ((str1 (runtime-make-string "Keep me"))
           (str2 (runtime-make-string "Free me")))
      (test-assert (= (heap-objects *heap*) 2) "Two strings allocated")
      ;; GC with only str1 as root
      (gc *heap* (list str1))
      (test-assert (= (heap-objects *heap*) 1) "One string survives")
      ;; str1 should still be valid
      (test-assert (string= (runtime-string->lisp str1) "Keep me") "Surviving string intact")))
  (format t "~%")

  ;; Test 12: String tag verification
  (format t "Test 12: String Tag Verification~%")
  (with-heap (:size 4096)
    (let ((str (runtime-make-string "Test")))
      (test-assert (= (logand str #xF) +tag-string+) "Correct string tag")
      (test-assert (= (logand str #xF) 3) "Tag value is 3")))
  (format t "~%")

  ;; Test 13: Multiple string operations
  (format t "Test 13: Multiple String Operations~%")
  (with-heap (:size 4096)
    (let* ((str1 (runtime-make-string "Hello"))
           (str2 (runtime-make-string " "))
           (str3 (runtime-make-string "World"))
           (temp (runtime-string-concat str1 str2))
           (result (runtime-string-concat temp str3)))
      (test-assert (string= (runtime-string->lisp result) "Hello World") "Multiple concatenations")
      (test-assert (= (heap-objects *heap*) 5) "5 strings allocated")))
  (format t "~%")

  ;; Test 14: String round-trip
  (format t "Test 14: String Round-trip~%")
  (with-heap (:size 4096)
    (let* ((original "The quick brown fox")
           (runtime-str (runtime-lisp->string original))
           (lisp-str (runtime-string->lisp runtime-str)))
      (test-assert (string= original lisp-str) "Round-trip preserves string")))
  (format t "~%")

  ;; Test 15: String with special characters
  (format t "Test 15: String with Special Characters~%")
  (with-heap (:size 4096)
    (let* ((str (runtime-make-string "Tab	Newline
End"))
           (lisp-str (runtime-string->lisp str)))
      ;; "Tab" (3) + tab (1) + "Newline" (7) + newline (1) + "End" (3) = 15
      (test-assert (= (runtime-string-length str) 15) "String with special chars has length 15")
      (test-assert (string= lisp-str "Tab	Newline
End") "Special characters preserved")))
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
(run-string-tests)
