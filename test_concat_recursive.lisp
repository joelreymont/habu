;;; Test concat-string-list with recursively built list

(labels ((build-list (n acc)
           (if (= n 0)
               acc
               (build-list (- n 1) (cons "X" acc)))))
  (let* ((chunks (build-list 3 nil))
         (result (concat-string-list chunks 3)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
