;;; Test: buffer-to-string + extract + concat

(let* ((buf (make-vector 10)))
  ;; Fill buffer with some data
  (vector-set buf 0 #x54)  ; 'T'
  (vector-set buf 1 #x65)  ; 'e'
  (vector-set buf 2 #x73)  ; 's'
  (vector-set buf 3 #x74)  ; 't'
  (labels ((build-list (n)
             (if (= n 0)
                 (let ((chunk (buffer-to-string buf 4)))
                   (list (cons chunk nil) 4))
                 (build-list (- n 1)))))
    (let* ((result-list (build-list 1))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
