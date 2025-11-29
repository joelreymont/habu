;;; Test: just concat-string-list, no outer-rec

(let ((chunks (cons "A" (cons "B" nil))))
  (let ((result (concat-string-list chunks 2)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
