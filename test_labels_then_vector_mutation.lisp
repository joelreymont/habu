;;; Test: labels funcall, then nested labels with vector mutation

(labels ((outer (n)
           (if (= n 0)
               10
               (outer (- n 1)))))
  (let ((result1 (outer 1)))
    ;; Create vector and mutate in nested labels (like concat does)
    (let ((vec (make-vector 4)))
      (labels ((fill-vec (i)
                 (if (< i 4)
                     (progn
                       (vector-set vec i (+ i 65))  ; Mutate captured vec
                       (fill-vec (+ i 1))))))
        (fill-vec 0)
        (let ((str (make-string-from-vector vec)))
          (sys-write 1 str (string-length str))
          (sys-write 1 "\n" 1)
          (sys-exit 42))))))
