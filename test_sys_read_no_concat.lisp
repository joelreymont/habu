;;; Test: sys-read inside labels, extract values, but NO concat

(let* ((fd (sys-open "/tmp/small.txt" #x0 0))
       (buf (make-vector 100)))
  (labels ((do-read ()
             (let ((n (sys-read fd buf 100)))
               (if (= n 0)
                   (list nil 0)
                   (let ((chunk (buffer-to-string buf n)))
                     (list (cons chunk nil) n))))))
    (let* ((result-list (do-read))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (sys-close fd)
      ;; Just print total, no concat
      (sys-write 1 (number-to-string total) (string-length (number-to-string total)))
      (sys-write 1 "\n" 1)
      (sys-exit 42))))
