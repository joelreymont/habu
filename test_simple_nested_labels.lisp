;;; Test: Simple nested labels with 3+ outer bindings

(labels ((outer-fn (x)
           (+ x 1)))
  (let* ((v1 10)
         (v2 20)
         (v3 30))  ; THREE bindings
    (labels ((inner-fn (y)
               (+ y v1)))  ; Access outer variable v1
      (sys-exit (inner-fn 12)))))  ; Should exit with 22
