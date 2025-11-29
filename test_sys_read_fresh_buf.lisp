;;; Test: sys-read with fresh buffer allocated INSIDE labels

(let ((fd (sys-open "/tmp/small.txt" #x0 0)))
  (labels ((do-read ()
             (let* ((buf (make-vector 100))    ; Allocate INSIDE labels
                    (n (sys-read fd buf 100)))
               (if (= n 0)
                   (list nil 0)
                   (let ((chunk (buffer-to-string buf n)))
                     (list (cons chunk nil) n))))))
    (let* ((result-list (do-read))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (sys-close fd)
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
