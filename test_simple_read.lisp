;;; Test sys-read + buffer-to-string without recursion

(let* ((fd (sys-open "/tmp/small.txt" #x0 0))
       (buf (make-vector 100))
       (n (sys-read fd buf 100)))
  (sys-close fd)
  (let ((s (buffer-to-string buf n)))
    (sys-write 1 "Read: " 6)
    (sys-write 1 s (string-length s))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
