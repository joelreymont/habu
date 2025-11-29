;;; Test extracting from list THEN calling concat

(let* ((fd (sys-open "/tmp/small.txt" #x0 0))
       (buf (make-vector 100)))
  (labels ((read-chunks (chunks total count)
             (if (= count 0)
                 (list chunks total)  ; Return LIST
                 (let ((n (sys-read fd buf 100)))
                   (if (= n 0)
                       (list chunks total)
                       (let* ((chunk (buffer-to-string buf n))
                              (next-chunks (cons chunk chunks))
                              (next-total (+ total n)))
                         (read-chunks next-chunks next-total (- count 1))))))))
    (let* ((result-list (read-chunks nil 0 1))
           (chunks (car result-list))        ; Extract chunks
           (total (car (cdr result-list))))  ; Extract total
      (sys-close fd)
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
