;;; Simple JIT test - compile and execute "return 42"

(defun print-string (s)
  (sys-write 1 s (string-length s)))

(defvar *newline* "
")

(defun print-newline ()
  (print-string *newline*))

(defun print-fixnum (n)
  (if (< n 0)
      (progn (print-string "-") (print-fixnum-pos (- 0 n)))
      (if (= n 0)
          (print-string "0")
          (print-fixnum-pos n))))

(defun print-fixnum-pos (n)
  (if (= n 0) nil
      (progn
        (print-fixnum-pos (/ n 10))
        (let ((s (make-string 1)))
          (string-set! s 0 (code-char (+ 48 (mod n 10))))
          (print-string s)))))

;; ARM64 machine code for: mov x0, #672; ret
;; 672 = 42 << 4 (tagged fixnum for 42)
;; mov x0, #672 = 0xD2805400
;; ret          = 0xD65F03C0
(defun make-ret-42-code ()
  (let ((code (make-vector 8)))
    ;; mov x0, #672 (little-endian: 00 54 80 D2)
    (buffer-byte-set code 0 #x00)
    (buffer-byte-set code 1 #x54)
    (buffer-byte-set code 2 #x80)
    (buffer-byte-set code 3 #xD2)
    ;; ret (little-endian: C0 03 5F D6)
    (buffer-byte-set code 4 #xC0)
    (buffer-byte-set code 5 #x03)
    (buffer-byte-set code 6 #x5F)
    (buffer-byte-set code 7 #xD6)
    code))

;; Copy code bytes to JIT memory
(defun copy-code (dst src len)
  (if (= len 0)
      nil
      (progn
        (mem-set-byte dst (- len 1) (buffer-byte-ref src (- len 1)))
        (copy-code dst src (- len 1)))))

(defun test-jit ()
  (print-string "JIT Test: Return 42")
  (print-newline)

  ;; Allocate JIT memory (16KB minimum on macOS ARM64)
  (let ((jit-mem (jit-mmap 16384)))
    (print-string "JIT memory allocated at: ")
    (print-fixnum jit-mem)
    (print-newline)

    ;; Make code
    (let ((code (make-ret-42-code)))
      ;; Enable write
      (jit-write-protect 0)

      ;; Copy code to JIT memory
      (copy-code jit-mem code 8)

      ;; Flush data cache
      (jit-dcache-flush jit-mem 8)

      ;; Enable execute (disable write)
      (jit-write-protect 1)

      ;; Invalidate instruction cache
      (jit-icache-invalidate jit-mem 8)

      ;; Call the JIT code
      (print-string "Calling JIT code...")
      (print-newline)
      (let ((result (funcall-ptr jit-mem)))
        (print-string "Result: ")
        (print-fixnum result)
        (print-newline)
        result))))

(test-jit)
