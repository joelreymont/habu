;;; Test: exact structure as concat - labels, let*, inner labels, reverse arg

(labels ((outer-rec (n acc)
           (if (= n 0)
               (list acc n)
               (outer-rec (- n 1) (+ acc n)))))
  (let* ((result-list (outer-rec 3 0))
         (acc-val (car result-list))
         (n-val (car (cdr result-list))))
    ;; Now exact concat structure
    (let* ((chunks (cons "A" (cons "B" nil)))
           (total 2)
           (vec (make-vector total)))
      (labels ((copy-chunk (chunks-arg offset)
                 (if (null chunks-arg)
                     vec
                     (let* ((chunk (car chunks-arg))
                            (len (string-length chunk)))
                       ;; Simplified: just iterate, no inner labels
                       (copy-chunk (cdr chunks-arg) (+ offset len))))))
        ;; Call with reverse (which makes its own labels)
        (let ((result (make-string-from-vector
                        (copy-chunk
                          (labels ((rev-iter (lst acc)
                                     (if (null lst)
                                         acc
                                         (let ((next-acc (cons (car lst) acc)))
                                           (rev-iter (cdr lst) next-acc)))))
                            (rev-iter chunks nil))
                          0))))
          (sys-write 1 result (string-length result))
          (sys-write 1 "\n" 1)
          (sys-exit 42))))))
