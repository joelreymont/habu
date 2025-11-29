;;; Test make-string-from-vector in recursive arg

(labels ((f (s n)
           (if (= n 0)
               (string-length s)
               (let ((v (make-vector 1)))
                 (progn
                   (vector-set v 0 88)  ; ASCII 'X'
                   (f (make-string-from-vector v) (- n 1)))))))
  (let ((v0 (make-vector 0)))
    (sys-exit (if (= (f (make-string-from-vector v0) 1) 1) 42 1))))
