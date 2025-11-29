;;; Test recursive + nested labels WITHOUT string operations

(labels ((outer (n acc)
           (if (= n 0)
               acc
               (outer (- n 1)
                      (let* ((x acc)
                             (y 1))
                        (labels ((inner (i)
                                   (if (< i 1)
                                       (progn
                                         (+ x y)
                                         (inner (+ i 1)))
                                       (+ x y))))
                          (inner 0)))))))
  (sys-exit (if (= (outer 5 37) 42) 42 1)))
