;;; Test sys-read in recursive labels

(let* ((fd (sys-open "/tmp/small.txt" #x0 0))
       (buf (make-vector 100)))
  (labels ((read-loop (count)
             (let ((n (sys-read fd buf 100)))
               (if (= n 0)
                   count
                   (read-loop (+ count n))))))
    (let ((total (read-loop 0)))
      (sys-close fd)
      (sys-write 1 (number-to-string total) (string-length (number-to-string total)))
      (sys-write 1 "\n" 1)
      (sys-exit 42))))
