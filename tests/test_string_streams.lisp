#!/usr/bin/env sbcl --script
;;; Tests for string streams (make-string-output-stream, with-output-to-string)

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

(format t "~%=== String Stream Tests ===~%~%")

;; Test 1: make-string-output-stream returns a cons
(run-test "make-stream-is-cons"
          '((if (consp (make-string-output-stream)) #x1 #x0))
          #x1)

;; Test 2: stream car is string-output-stream symbol
(run-test "stream-car-is-symbol"
          '((if (eq (car (make-string-output-stream)) 'string-output-stream) #x1 #x0))
          #x1)

;; Test 3: write-string-to-stream works
(run-test "write-string-returns-str"
          '((let ((s (make-string-output-stream)))
              (let ((result (write-string-to-stream "hello" s)))
                (string-length result))))
          #x5)

;; Test 4: get-output-stream-string retrieves content
(run-test "get-stream-string-length"
          '((let ((s (make-string-output-stream)))
              (write-string-to-stream "hello" s)
              (string-length (get-output-stream-string s))))
          #x5)

;; Test 5: multiple writes concatenate
(run-test "multiple-writes-concat"
          '((let ((s (make-string-output-stream)))
              (write-string-to-stream "hello" s)
              (write-string-to-stream " world" s)
              (string-length (get-output-stream-string s))))
          #xB)  ; 11 = "hello world"

;; Test 6: with-output-to-string basic
(run-test "with-output-basic"
          '((string-length
             (with-output-to-string (s)
               (write-string-to-stream "test" s))))
          #x4)

;; Test 7: with-output-to-string multiple writes
(run-test "with-output-multiple"
          '((string-length
             (with-output-to-string (s)
               (write-string-to-stream "a" s)
               (write-string-to-stream "b" s)
               (write-string-to-stream "c" s))))
          #x3)

;; Test 8: get-output-stream-string clears stream
(run-test "get-stream-clears"
          '((let ((s (make-string-output-stream)))
              (write-string-to-stream "first" s)
              (get-output-stream-string s)  ; Clear
              (write-string-to-stream "second" s)
              (string-length (get-output-stream-string s))))
          #x6)  ; just "second"

(format t "~%=== All String Stream Tests Passed ===~%")
(sb-ext:quit :unix-status 0)
