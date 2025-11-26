#!/usr/bin/env sbcl --script
;;; test-runtime-calls.lisp - Test cons/car/cdr with actual runtime

(load "sbcl-habu-shim.lisp")
(load "arm64/codegen-sbcl.lisp")

(defun parse-runtime-addr-line (line)
  "Parse a line like 'HABU_CONS_ADDR=0x104eaeb20' into (symbol . addr)"
  (when (and (> (length line) 0) (position #\= line))
    (let* ((equals-pos (position #\= line))
           (name-str (subseq line 0 equals-pos))
           (addr-str (subseq line (+ equals-pos 1)))
           (symbol-name (intern (substitute #\_ #\- name-str) :habu-sbcl-codegen)))
      (cons symbol-name (parse-integer addr-str :start 2 :radix 16)))))

(defun split-string (string separator)
  "Simple string splitting"
  (let ((result nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) separator)
          do (progn
               (push (subseq string start i) result)
               (setf start (+ i 1))))
    (push (subseq string start) result)
    (nreverse result)))

(defun get-runtime-addrs ()
  "Execute print-runtime-addrs and parse output into alist"
  (let* ((output (with-output-to-string (stream)
                   (sb-ext:run-program "./bin/print-runtime-addrs" nil :output stream :search t)))
         (lines (split-string output #\Newline)))
    (mapcan (lambda (line)
              (let ((parsed (parse-runtime-addr-line line)))
                (when parsed (list parsed))))
            lines)))

(defun write-bytecode-to-file (code-list filename)
  "Write bytecode (list of bytes) to binary file"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
    (dolist (byte code-list)
      (write-byte byte out)))
  (format t "Wrote ~D bytes to ~A~%~%" (length code-list) filename))

(defun test-runtime-call (expr runtime-addrs filename)
  "Compile expression with runtime and save to file"
  (format t "Testing: ~S~%" expr)
  (let* ((ir (habu-sbcl-codegen:compile-expr expr nil nil))
         (code (habu-sbcl-codegen:compile-to-arm64-with-runtime expr runtime-addrs)))
    (format t "  IR: ~S~%" ir)
    (format t "  Code size: ~D bytes~%" (length code))
    (write-bytecode-to-file code filename)
    code))

;; Main
(let ((runtime-addrs (get-runtime-addrs)))
  (format t "Runtime addresses loaded: ~D entries~%~%" (length runtime-addrs))

  ;; Test cons
  (test-runtime-call '(cons 42 99) runtime-addrs "test-cons.bin")

  ;; Test car of cons
  (test-runtime-call '(car (cons 42 99)) runtime-addrs "test-car.bin")

  ;; Test cdr of cons
  (test-runtime-call '(cdr (cons 42 99)) runtime-addrs "test-cdr.bin"))

(format t "To execute: ./run-bytecode test-cons.bin~%")
