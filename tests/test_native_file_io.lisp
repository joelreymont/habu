;; Tests for native file I/O with multiple libSystem imports
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)

(defpackage :habu-test-native-file-io
  (:use :cl)
  (:import-from :habu #:deliver))
(in-package :habu-test-native-file-io)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Native File I/O Tests ===~%~%")

(defvar *pass* 0)
(defvar *fail* 0)

(defun test-code (name source expected)
  (let ((path (format nil "/tmp/fio_~A" name)))
    (handler-case
        (progn
          (deliver source path)
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" path)
                              :output nil :error nil :wait t)
          (let* ((proc (sb-ext:run-program path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn (format t "[PASS] ~A = ~A~%" name code) (incf *pass*))
                (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code) (incf *fail*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *fail*)))))

;; Create test input file
(with-open-file (f "/tmp/test_input.txt" :direction :output :if-exists :supersede)
  (write-string "Hello" f))

;; Test 1: sys-open returns valid fd (>= 0)
(test-code "open-valid-fd"
  "(progn (sys-write 1 \"\" 0)
     (let ((fd (sys-open \"/tmp/test_input.txt\" 0 0)))
       (if (>= fd 0) 42 0)))"
  42)

;; Test 2: sys-open returns -1 for non-existent file
(test-code "open-not-found"
  "(progn (sys-write 1 \"\" 0)
     (let ((fd (sys-open \"/tmp/no_such_file.txt\" 0 0)))
       (if (< fd 0) 42 0)))"
  42)

;; Test 3: sys-read returns bytes read (> 0)
(test-code "read-positive"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_input.txt\" 0 0))
            (buf (make-vector 10))
            (n (sys-read fd buf 10)))
       (sys-close fd)
       (if (> n 0) 42 0)))"
  42)

;; Test 4: sys-read returns correct byte count (5 for "Hello")
(test-code "read-count"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_input.txt\" 0 0))
            (buf (make-vector 10))
            (n (sys-read fd buf 10)))
       (sys-close fd)
       n))"
  5)  ; "Hello" is 5 bytes

;; Test 5: sys-close succeeds (returns 0)
(test-code "close-success"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_input.txt\" 0 0))
            (result (sys-close fd)))
       (if (= result 0) 42 0)))"
  42)

;; Test 6: Multiple imports work together
(test-code "multi-import"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_input.txt\" 0 0))
            (buf (make-vector 10))
            (n (sys-read fd buf 10)))
       (sys-close fd)
       (+ n 37)))"  ; 5 + 37 = 42
  42)

(format t "~%Total: ~A passed, ~A failed~%" *pass* *fail*)
(when (> *fail* 0)
  (sb-ext:exit :code 1))
