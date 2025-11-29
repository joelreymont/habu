;;; Test buffer-to-string with a small buffer

(let ((buf (make-vector 10)))
  (vector-set buf 0 72)   ;; 'H'
  (vector-set buf 1 101)  ;; 'e'
  (vector-set buf 2 108)  ;; 'l'
  (vector-set buf 3 108)  ;; 'l'
  (vector-set buf 4 111)  ;; 'o'
  (let ((result (buffer-to-string buf 5)))
    (sys-write 1 "Result: " 8)
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit (if (= (string-length result) 5) 42 1))))
