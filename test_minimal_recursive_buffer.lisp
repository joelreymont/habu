;;; Minimal test: single recursive call with buffer-to-string

(labels ((test-fn (count)
           (if (= count 0)
               "done"
               (let* ((buf (make-vector 10))
                      (_ (vector-set buf 0 65))  ; 'A'
                      (s (buffer-to-string buf 1)))
                 (test-fn (- count 1))))))
  (let ((result (test-fn 1)))
    (sys-write 1 result (string-length result))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
