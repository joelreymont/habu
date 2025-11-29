;;; Test nested labels with concat-string-list (simulating native-read-file-large)

(labels ((collect-chunks (n acc total)
           (if (= n 0)
               (list acc total)
               (let* ((chunk "X")  ;; simulate reading a 1-byte chunk
                      (next-acc (cons chunk acc))
                      (next-total (+ total 1)))
                 (collect-chunks (- n 1) next-acc next-total)))))
  (let* ((result-list (collect-chunks 5 nil 0))  ;; collect 5 chunks
         (chunks (car result-list))
         (total (car (cdr result-list))))
    (let ((result (concat-string-list chunks total)))
      (sys-write 1 "Result: " 8)
      (sys-write 1 result (string-length result))
      (sys-write 1 "\n" 1)
      (sys-exit (if (= (string-length result) 5) 42 1)))))
