;;; Test non-recursive but nested string operations

(labels ((outer (s)
           (let* ((s1 s)
                  (s2 "X")
                  (len1 (string-length s1))
                  (len2 (string-length s2))
                  (total (+ len1 len2))
                  (vec (make-vector total)))
             (labels ((copy1 (i)
                        (if (< i len1)
                            (progn
                              (vector-set vec i (string-ref s1 i))
                              (copy1 (+ i 1)))))
                      (copy2 (i)
                        (if (< i len2)
                            (progn
                              (vector-set vec (+ len1 i) (string-ref s2 i))
                              (copy2 (+ i 1))))))
               (copy1 0)
               (copy2 0)
               (make-string-from-vector vec)))))
  (let ((result (outer "A")))
    (sys-exit (if (= (string-length result) 2) 42 1))))
