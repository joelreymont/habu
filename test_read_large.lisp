;;; Test native-read-file-large function

;; Create a test file first
(native-write-file "/tmp/test_small.txt" "Hello World")

;; Test 1: Read a small file
(let ((content (native-read-file-large "/tmp/test_small.txt")))
  (sys-write 1 "Read: " 6)
  (sys-write 1 content (string-length content))
  (sys-write 1 "\n" 1))

;; Test 2: Read a larger file (the compiler source itself)
(let ((content (native-read-file-large "bootstrap/compiler.lisp")))
  (let ((len (string-length content)))
    (sys-write 1 "Compiler source length: " 24)
    ;; For now, just check that we read something substantial
    ;; The compiler is about 256KB, so let's check > 200000
    (if (> len 200000)
        (sys-exit 42)  ;; Success
        (sys-exit 1)))) ;; Failure
