;;; Minimal test: extract from list then concat-string-list

(let* ((result-list (list (cons "Test" nil) 4))
       (chunks (car result-list))
       (total (car (cdr result-list))))
  (let ((result (concat-string-list chunks total)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
