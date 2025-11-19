;;;; Test file I/O operations

(load "memory.lisp")
(load "symbols.lisp")
(load "strings.lisp")
(load "files.lisp")

(in-package :habu-runtime)

(initialize-runtime)

(format t "~%Testing File I/O Operations~%")
(format t "===========================~%~%")

(defvar *test-count* 0)
(defvar *pass-count* 0)

(defun test (name condition &optional message)
  (incf *test-count*)
  (if condition
      (progn
        (incf *pass-count*)
        (format t "[32m✓[0m ~A~%" name))
      (progn
        (format t "[31m✗[0m ~A" name)
        (when message
          (format t ": ~A" message))
        (format t "~%"))))

;; Test 1: Write file
(format t "~%[34m1. Write File[0m~%")
(format t "===============~%")

(let* ((path-ptr (runtime-lisp->string "/tmp/habu-test.txt"))
       (data-ptr (runtime-lisp->string "Hello from Habu!"))
       (result (runtime-write-file path-ptr data-ptr)))
  (test "WRITE-FILE" (not (zerop result)))
  (format t "   Result: ~X~%" result))

;; Test 2: Read file
(format t "~%[34m2. Read File[0m~%")
(format t "==============~%")

(let* ((path-ptr (runtime-lisp->string "/tmp/habu-test.txt"))
       (result-ptr (runtime-read-file path-ptr)))
  (if (zerop result-ptr)
      (test "READ-FILE" nil "Failed to read file")
      (let ((result-str (runtime-string->lisp result-ptr)))
        (test "READ-FILE" (string= result-str "Hello from Habu!"))
        (format t "   Content: ~S~%" result-str))))

;; Test 3: File open/read/close
(format t "~%[34m3. File Open/Read/Close[0m~%")
(format t "=========================~%")

(let* ((path-ptr (runtime-lisp->string "/tmp/habu-test.txt"))
       (mode-ptr (runtime-lisp->string "r"))
       (handle (runtime-file-open path-ptr mode-ptr)))
  (test "FILE-OPEN" (not (zerop handle)))
  (when (not (zerop handle))
    (let ((content-ptr (runtime-file-read handle)))
      (test "FILE-READ" (not (zerop content-ptr)))
      (when (not (zerop content-ptr))
        (let ((content (runtime-string->lisp content-ptr)))
          (format t "   Content: ~S~%" content)))
      (let ((close-result (runtime-file-close handle)))
        (test "FILE-CLOSE" (not (zerop close-result)))))))

;; Test 4: File open for write/write/close
(format t "~%[34m4. File Open/Write/Close[0m~%")
(format t "==========================~%")

(let* ((path-ptr (runtime-lisp->string "/tmp/habu-test2.txt"))
       (mode-ptr (runtime-lisp->string "w"))
       (data-ptr (runtime-lisp->string "Line 1\nLine 2\n"))
       (handle (runtime-file-open path-ptr mode-ptr)))
  (test "FILE-OPEN-WRITE" (not (zerop handle)))
  (when (not (zerop handle))
    (let ((bytes (runtime-file-write handle data-ptr)))
      (test "FILE-WRITE" (not (zerop bytes)))
      (format t "   Wrote ~D bytes~%" (ash bytes -4)))
    (let ((close-result (runtime-file-close handle)))
      (test "FILE-CLOSE-WRITE" (not (zerop close-result))))))

;; Test 5: Read back the written file
(format t "~%[34m5. Read Back Written File[0m~%")
(format t "===========================~%")

(let* ((path-ptr (runtime-lisp->string "/tmp/habu-test2.txt"))
       (result-ptr (runtime-read-file path-ptr)))
  (if (zerop result-ptr)
      (test "READ-WRITTEN-FILE" nil "Failed to read file")
      (let ((result-str (runtime-string->lisp result-ptr)))
        (test "READ-WRITTEN-FILE" (string= result-str "Line 1\nLine 2\n"))
        (format t "   Content: ~S~%" result-str))))

;; Test 6: Append mode
(format t "~%[34m6. Append Mode[0m~%")
(format t "================~%")

(let* ((path-ptr (runtime-lisp->string "/tmp/habu-test2.txt"))
       (mode-ptr (runtime-lisp->string "a"))
       (data-ptr (runtime-lisp->string "Line 3\n"))
       (handle (runtime-file-open path-ptr mode-ptr)))
  (test "FILE-OPEN-APPEND" (not (zerop handle)))
  (when (not (zerop handle))
    (runtime-file-write handle data-ptr)
    (runtime-file-close handle))
  ;; Read back to verify
  (let* ((result-ptr (runtime-read-file path-ptr))
         (result-str (runtime-string->lisp result-ptr)))
    (test "APPEND-RESULT" (string= result-str "Line 1\nLine 2\nLine 3\n"))
    (format t "   Content: ~S~%" result-str)))

;; Clean up test files
(ignore-errors
  (delete-file "/tmp/habu-test.txt")
  (delete-file "/tmp/habu-test2.txt"))

;; Summary
(format t "~%=====================================~%")
(format t "  Test Results~%")
(format t "=====================================~%")
(format t "Total:  ~D~%" *test-count*)
(format t "Passed: [32m~D/~D[0m~%~%" *pass-count* *test-count*)

(when (< *pass-count* *test-count*)
  (format t "[31mSome tests failed![0m~%")
  (sb-ext:quit :unix-status 1))

(format t "[32mAll file I/O tests passed![0m~%")

(sb-ext:quit)
