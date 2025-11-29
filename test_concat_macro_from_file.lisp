;;; Test concat-string-list MACRO with chunks from file reading

(let* ((fd (sys-open "/tmp/small.txt" #x0 0))
       (buf (make-vector 100)))
  (labels ((read-chunks (chunks count)
             (if (= count 0)
                 chunks
                 (let ((n (sys-read fd buf 100)))
                   (if (= n 0)
                       chunks
                       (let* ((chunk (buffer-to-string buf n))
                              (next-chunks (cons chunk chunks)))
                         (read-chunks next-chunks (- count 1))))))))
    (let ((chunks (read-chunks nil 1)))
      (sys-close fd)
      (let ((result (concat-string-list chunks 8)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
