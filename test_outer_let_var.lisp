;;; Test accessing outer let* variable from labels function called via funcall

(let* ((outer-vec (make-vector 10)))
  (labels ((my-fn (x)
             (vector-set outer-vec 0 x)  ; Access outer-vec from let*
             outer-vec))
    (let ((result (my-fn 42)))
      (sys-write 1 "OK\n" 3)
      (sys-exit 42))))
