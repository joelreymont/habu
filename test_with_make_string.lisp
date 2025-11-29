;;; Test: recursive labels with make-string-from-vector

(labels ((f (x n)
           (if (= n 0)
               x
               (f (let ((vec (make-vector 2)))
                    (labels ((copy (i)
                               (if (< i 2)
                                   (progn
                                     (vector-set vec i 65)  ; ASCII 'A'
                                     (copy (+ i 1))))))
                      (copy 0)
                      (make-string-from-vector vec)))
                  (- n 1)))))
  (let ((result (f 0 1)))
    (sys-exit (if (= (string-length result) 2) 42 1))))
