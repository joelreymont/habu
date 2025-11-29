;;; Test: Nested labels with vector allocation and 3+ outer bindings

(labels ((outer-fn (x)
           (+ x 1)))
  (let* ((v1 10)
         (v2 20)
         (v3 30)  ; THREE bindings
         (vec (make-vector 5)))
    (labels ((inner-fn (y offset)
               (vector-set vec offset y)
               (vector-ref vec offset)))
      (sys-exit (inner-fn 42 2)))))  ; Should exit with 42
