;;; Test: simplest sys-read + concat

(let* ((fd (sys-open "/tmp/small.txt" #x0 0))
       (buf (make-vector 100))
       (n (sys-read fd buf 100))
       (chunk (buffer-to-string buf n))
       (chunks (cons chunk nil)))
  (sys-close fd)
  (let ((result (concat-string-list chunks n)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
