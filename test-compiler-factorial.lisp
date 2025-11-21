#!/usr/bin/env habu

;;; Test compiling recursive factorial with the compiler

(load "habu-arm64-codegen.lisp")

(define factorial-program
  (quote
    (begin
      (define (factorial n)
        (if (= n 0)
          1
          (* n (factorial (- n 1)))))

      (factorial 5))))

(define code (compile-program-with-functions factorial-program))

(print "Compiled factorial program!")
(print (quote code-length:))
(print (count-instrs code))
(print "")

;;; Write to file for testing
(define write-bytes-to-file
  (lambda (bytes filename)
    (let ((f (fopen filename "wb")))
      (if (nil? f)
        (print "Failed to open file")
        (begin
          (write-bytes-helper bytes f)
          (fclose f)
          (print "Wrote bytes to file"))))))

(define write-bytes-helper
  (lambda (bytes f)
    (if (cons? bytes)
      (begin
        (fputc (car bytes) f)
        (write-bytes-helper (cdr bytes) f))
      nil)))

(write-bytes-to-file code "test-compiler-factorial.bin")
(print "Binary written to test-compiler-factorial.bin")
