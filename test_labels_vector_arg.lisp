;;; Test labels with make-vector in recursive argument

(labels ((f (v n)
           (if (= n 0)
               (vector-ref v 0)
               (let ((new-v (make-vector 1)))
                 (progn
                   (vector-set new-v 0 (+ (vector-ref v 0) 1))
                   (f new-v (- n 1)))))))
  (let ((v0 (make-vector 1)))
    (progn
      (vector-set v0 0 0)
      (let ((result (f v0 1)))
        (sys-exit (if (= result 1) 42 1))))))
