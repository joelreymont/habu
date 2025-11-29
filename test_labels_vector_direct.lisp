;;; Test labels with make-vector directly in recursive argument

(labels ((f (n)
           (if (= n 0)
               42
               (f (- n (vector-ref (make-vector 1) 0)))))))
  (sys-exit (f 1)))
