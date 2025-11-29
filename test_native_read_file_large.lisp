;;; Test native-read-file-large with Bug #20 workaround

;; First, create a test file with known content > 64KB
(let* ((content (make-vector 70000))
       (i 0))
  (labels ((fill-vec (idx)
             (if (< idx 70000)
                 (progn
                   (vector-set content idx (+ 65 (mod idx 26)))  ; A-Z cycling
                   (fill-vec (+ idx 1))))))
    (fill-vec 0))
  (native-write-file "/tmp/test_large.txt" (make-string-from-vector content)))

;; Now read it back using native-read-file-large
(let ((result (native-read-file-large "/tmp/test_large.txt")))
  (sys-write 1 "Read " 5)
  (sys-write 1 (number-to-string (string-length result))
             (string-length (number-to-string (string-length result))))
  (sys-write 1 " bytes\n" 7)
  (sys-exit (if (= (string-length result) 70000) 42 1)))
