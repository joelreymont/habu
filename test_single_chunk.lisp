;;; Test native-read-file-large with file that fits in one chunk

;; Read this source file (should be < 64KB)
(let ((content (native-read-file-large "test_single_chunk.lisp")))
  (let ((len (string-length content)))
    (sys-write 1 "Length: " 8)
    ;; Can't print numbers yet, so just check > 100
    (if (> len 100)
        (sys-exit 42)
        (sys-exit 1))))
