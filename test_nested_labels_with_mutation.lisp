;;; Test: nested labels with captured vector mutation (like concat)

(let ((vec (make-vector 4)))
  (labels ((outer (chunks offset)
             (if (null chunks)
                 vec
                 (labels ((inner (i)
                            (if (< i 2)
                                (progn
                                  (vector-set vec (+ offset i) (+ i 65))  ; Mutate captured vec
                                  (inner (+ i 1))))))
                   (inner 0)
                   (outer (cdr chunks) (+ offset 2))))))
    (let ((result (outer (cons 1 (cons 2 nil)) 0)))
      (let ((str (make-string-from-vector result)))
        (sys-write 1 str (string-length str))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
