;;; Test: sys-write inside labels, then concat

(labels ((do-write ()
           (let ((n (sys-write 1 "X" 1)))  ; Call _write via extern
             (list (cons "Test" nil) 4))))
  (let* ((result-list (do-write))
         (chunks (car result-list))
         (total (car (cdr result-list))))
    (let ((result (concat-string-list chunks total)))
      (sys-write 1 result (string-length result))
      (sys-write 1 "\n" 1)
      (sys-exit 42))))
