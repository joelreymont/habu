;;; Test: buffer-to-string inside labels, then concat

(let* ((buf (make-vector 10)))
  ;; Fill buffer
  (vector-set buf 0 #x54)  ; 'T'
  (vector-set buf 1 #x65)  ; 'e'
  (vector-set buf 2 #x73)  ; 's'
  (vector-set buf 3 #x74)  ; 't'
  (labels ((build ()
             (let ((chunk (buffer-to-string buf 4)))
               (list (cons chunk nil) 4))))
    (let* ((result-list (build))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
