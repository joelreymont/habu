;;; Test reverse on buffer-to-string results from labels

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
      ;; Test reverse on chunks from labels
      (let ((reversed (reverse chunks)))
        (sys-write 1 "OK\n" 3)
        (sys-exit 42)))))
