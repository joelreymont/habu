;;; Test: buffer captured but fd NOT captured

(let* ((buf (make-vector 100)))  ; buf is captured  (labels ((do-read (fd-param)  ; fd passed as parameter, not captured
             (let ((n (sys-read fd-param buf 100)))
               (if (= n 0)
                   (list nil 0)
                   (let ((chunk (buffer-to-string buf n)))
                     (list (cons chunk nil) n))))))
    (let* ((fd (sys-open "/tmp/small.txt" #x0 0))
           (result-list (do-read fd))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (sys-close fd)
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
