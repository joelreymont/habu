;;; Test: sys-open INSIDE labels returning list

(labels ((do-open ()
           (let ((fd (sys-open "/tmp/small.txt" #x0 0)))
             (list (cons "Test" nil) fd))))
  (let* ((result-list (do-open))
         (chunks (car result-list))
         (fd (car (cdr result-list))))
    (sys-close fd)
    (let ((result (concat-string-list chunks 4)))
      (sys-write 1 result (string-length result))
      (sys-write 1 "\n" 1)
      (sys-exit 42))))
