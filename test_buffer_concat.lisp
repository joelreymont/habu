;;; Test buffer-to-string + concat-string-list (no file I/O)

(let* ((buf (make-vector 10))
       ;; Manually fill buffer with bytes for "Hi"
       (_ (vector-set buf 0 72))   ; 'H'
       (_ (vector-set buf 1 105))) ; 'i'
  ;; Convert to string
  (let* ((s (buffer-to-string buf 2))
         (result (concat-string-list (cons s (cons "!" nil)) 3)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
