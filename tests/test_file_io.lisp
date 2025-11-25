#!/usr/bin/env sbcl --script
;;; Tests for File I/O operations

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~X, got ~X)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(defun run-string-test (name forms)
  "Test that result has string tag (0x4)"
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= (logand result #xF) #x4))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected string tag 0x4, got tag ~X)~%" name (logand result #xF))
          (sb-ext:quit :unix-status 1)))))

(defun run-handle-test (name forms)
  "Test that result is a valid file handle (>= 3)"
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (>= result 3))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected handle >= 3, got ~D)~%" name result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== File I/O Tests ===~%~%")

;; Test 1: write-file creates a file and returns 0
(run-test "write-file-returns-0"
          '((write-file "/tmp/habu_test_1.txt" "Hello, World!"))
          #x0)

;; Test 2: read-file returns the file content as string
(run-string-test "read-file-returns-string"
                 '((progn
                     (write-file "/tmp/habu_test_2.txt" "Test content")
                     (read-file "/tmp/habu_test_2.txt"))))

;; Test 3: string-length of read-file result matches written content
(run-test "read-file-length-matches"
          '((progn
              (write-file "/tmp/habu_test_3.txt" "12345")
              (string-length (read-file "/tmp/habu_test_3.txt"))))
          #x5)

;; Test 4: open-file returns a handle (>= 3)
(run-handle-test "open-file-returns-handle"
                 '((progn
                     (write-file "/tmp/habu_test_4.txt" "content")
                     (open-file "/tmp/habu_test_4.txt" "r"))))

;; Test 5: close-file returns 0 on success
(run-test "close-file-returns-0"
          '((progn
              (write-file "/tmp/habu_test_5.txt" "content")
              (let ((h (open-file "/tmp/habu_test_5.txt" "r")))
                (close-file h))))
          #x0)

;; Test 6: read-line returns string
(run-string-test "read-line-returns-string"
                 '((progn
                     (write-file "/tmp/habu_test_6.txt" "first line")
                     (let ((h (open-file "/tmp/habu_test_6.txt" "r")))
                       (let ((line (read-line h)))
                         (close-file h)
                         line)))))

;; Test 7: write-string returns bytes written (5 for "Hello")
(run-test "write-string-returns-count"
          '((let ((h (open-file "/tmp/habu_test_7.txt" "w")))
              (let ((n (write-string h "Hello")))
                (close-file h)
                n)))
          #x5)

;; Test 8: Full round-trip with open/write/close/open/read/close
(run-test "round-trip-file-io"
          '((progn
              ;; Write file
              (let ((h (open-file "/tmp/habu_test_8.txt" "w")))
                (write-string h "abc")
                (close-file h))
              ;; Read back
              (let ((h (open-file "/tmp/habu_test_8.txt" "r")))
                (let ((line (read-line h)))
                  (close-file h)
                  (string-length line)))))
          #x3)

;; Test 9: write-file with empty string
(run-test "write-file-empty"
          '((write-file "/tmp/habu_test_9.txt" ""))
          #x0)

;; Test 10: read-file of empty file returns empty string
(run-test "read-empty-file-length"
          '((progn
              (write-file "/tmp/habu_test_10.txt" "")
              (string-length (read-file "/tmp/habu_test_10.txt"))))
          #x0)

(format t "~%=== All File I/O Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
