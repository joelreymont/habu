;;; Test: Doubly-nested labels (labels inside labels) with 3+ outer bindings

(labels ((outer-fn (x)
           (+ x 1)))
  (let* ((v1 10)
         (v2 20)
         (v3 30)  ; THREE bindings
         (vec (make-vector 5)))
    (labels ((middle-fn (offset)
               ;; Inner labels inside middle-fn
               (labels ((inner-fn (i)
                          (if (< i 3)
                              (progn
                                (vector-set vec i (+ offset i))
                                (inner-fn (+ i 1))))))
                 (inner-fn 0)
                 (vector-ref vec offset))))
      (sys-exit (middle-fn 2)))))  ; Should write vec[0]=2, vec[1]=3, vec[2]=4, return vec[2]=4
