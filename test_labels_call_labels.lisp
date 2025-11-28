;;; Test labels calling a function that uses labels

(defun helper (n)
  (labels ((inner (m)
             (if (= m 0)
                 1
                 (* 2 (inner (- m 1))))))
    (inner n)))

(labels ((outer (n)
           (if (= n 0)
               0
               (+ (helper 2) (outer (- n 1))))))
  (let ((result (outer 3)))
    (if (= result 12)
        (sys-exit 42)
        (sys-exit 1))))
