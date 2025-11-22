#!/usr/bin/env sbcl --script
;;; compile-and-save.lisp - Compile Lisp expression to ARM64 bytecode file

(load "sbcl-habu-shim.lisp")
(load "habu-arm64-codegen-sbcl.lisp")

(defun write-bytecode-to-file (code-list filename)
  "Write bytecode (list of bytes) to binary file"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (dolist (byte code-list)
      (write-byte byte out)))
  (format t "Wrote ~D bytes to ~A~%" (length code-list) filename))

(defun compile-and-save (expr filename)
  "Compile expression to ARM64 and save to file"
  (format t "Compiling: ~S~%" expr)
  (let ((code (habu-sbcl-codegen:compile-to-arm64 expr)))
    (write-bytecode-to-file code filename)
    code))

;; Main: compile expression from command line or use default
(let ((expr (if (> (length sb-ext:*posix-argv*) 1)
                (read-from-string (second sb-ext:*posix-argv*))
                42)))
  (compile-and-save expr "output.bin")
  (format t "~%To execute: ./run-bytecode output.bin~%"))
