#!/usr/bin/env sbcl --script
;;; test-simple-let.lisp - Simple test for let bindings

(load "sbcl-habu-shim.lisp")
(load "arm64/codegen-sbcl.lisp")

(defun write-bytecode-to-file (code-list filename)
  "Write bytecode (list of bytes) to binary file"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (dolist (byte code-list)
      (write-byte byte out))))

;; Simple test
(let* ((expr '(let ((x 10)) x))
       (ir (habu-sbcl-codegen:compile-expr expr nil nil))
       (code (habu-sbcl-codegen:compile-to-arm64 expr)))

  (format t "Expression: ~S~%" expr)
  (format t "IR: ~S~%" ir)
  (format t "Code size: ~D bytes~%" (length code))
  (format t "~%First 32 bytes of code:~%")
  (loop for i from 0 below (min 32 (length code))
        do (format t "~2,'0X " (nth i code)))
  (format t "~%~%")

  (write-bytecode-to-file code "/tmp/simple-let.bin")
  (format t "Wrote to /tmp/simple-let.bin~%")
  (format t "~%Execute with: ./run-bytecode /tmp/simple-let.bin~%"))