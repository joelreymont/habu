#!/usr/bin/env sbcl --script
;;; Test hash table operations via run-bytecode runtime.

(load "run-habu.lisp")

(defun run-test (name forms expected)
  (multiple-value-bind (result output) (habu-sbcl:compile-and-run-forms forms)
    (declare (ignore output))
    (if (and result (= result expected))
        (format t "~A: pass~%" name)
        (progn
          (format t "~A: FAIL (expected ~A, got ~A)~%" name expected result)
          (sb-ext:quit :unix-status 1)))))

;; Test 1: Create empty hash table
(run-test "make-hash-table"
          '((let ((ht (make-hash-table)))
              (hash-table-count ht)))
          #x0)

;; Test 2: Put and get single value
(run-test "puthash-gethash"
          '((let ((ht (make-hash-table)))
              (puthash #x1 #x99 ht)
              (gethash #x1 ht)))
          #x99)

;; Test 3: Get with default (not found)
(run-test "gethash-default"
          '((let ((ht (make-hash-table)))
              (gethash #x1 ht #xFF)))
          #xFF)

;; Test 4: Multiple entries
(run-test "multiple-entries"
          '((let ((ht (make-hash-table)))
              (puthash #x1 #x10 ht)
              (puthash #x2 #x20 ht)
              (puthash #x3 #x30 ht)
              (+ (gethash #x1 ht) (gethash #x2 ht) (gethash #x3 ht))))
          #x60)

;; Test 5: Hash table count
(run-test "hash-table-count"
          '((let ((ht (make-hash-table)))
              (puthash #x1 #x10 ht)
              (puthash #x2 #x20 ht)
              (puthash #x3 #x30 ht)
              (hash-table-count ht)))
          #x3)

;; Test 6: Update existing key
(run-test "update-key"
          '((let ((ht (make-hash-table)))
              (puthash #x1 #x10 ht)
              (puthash #x1 #x99 ht)
              (gethash #x1 ht)))
          #x99)

;; Test 7: Remove key
(run-test "remhash"
          '((let ((ht (make-hash-table)))
              (puthash #x1 #x10 ht)
              (remhash #x1 ht)
              (gethash #x1 ht #xFF)))
          #xFF)

;; Test 8: setf gethash
(run-test "setf-gethash"
          '((let ((ht (make-hash-table)))
              (setf (gethash #x1 ht) #x42)
              (gethash #x1 ht)))
          #x42)

;; Test 9: hash-table-p predicate
(run-test "hash-table-p-true"
          '((let ((ht (make-hash-table)))
              (if (hash-table-p ht) #x1 #x0)))
          #x1)

;; Test 10: hash-table-p false for non-hash-table
(run-test "hash-table-p-false"
          '((if (hash-table-p (cons #x1 #x2)) #x1 #x0))
          #x0)

(format t "All hash table tests passed~%")
(sb-ext:quit :unix-status 0)
