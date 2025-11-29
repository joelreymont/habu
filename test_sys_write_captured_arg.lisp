;;; Test: sys-write with captured string argument + concat

(let ((msg "Test"))  ; Captured string
  (labels ((do-write ()
             (let ((n (sys-write 1 msg 4)))  ; msg is captured
               (list (cons msg nil) n))))
    (let* ((result-list (do-write))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
