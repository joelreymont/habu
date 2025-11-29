;;; Test: Is it mutation of captured data that's the issue?

(let ((vec (make-vector 10)))
  ;; Manually mutate the captured vector (simulating what sys-read does)
  (vector-set vec 0 #x54)
  (vector-set vec 1 #x65)
  (vector-set vec 2 #x73)
  (vector-set vec 3 #x74)
  (labels ((process ()
             ;; Access the mutated captured vector
             (let ((str (buffer-to-string vec 4)))
               (list (cons str nil) 4))))
    (let* ((result-list (process))
           (chunks (car result-list))
           (total (car (cdr result-list))))
      (let ((result (concat-string-list chunks total)))
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
