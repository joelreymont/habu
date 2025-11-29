;;; Test exact concat-string-list pattern but simpler

;; Manually expanded version of concat-string-list for one chunk
(let* ((chunks (cons "Test" nil))
       (total 4)
       (vec (make-vector total)))
  (labels ((copy-chunk (chunks-arg offset)
             (if (null chunks-arg)
                 vec
                 (let* ((chunk (car chunks-arg))
                        (len (string-length chunk)))
                   (labels ((copy-chars (i)
                              (if (< i len)
                                  (progn (vector-set vec (+ offset i) (string-ref chunk i))
                                         (let ((next-i (+ i 1)))
                                           (copy-chars next-i))))))
                     (copy-chars 0)
                     (let ((next-chunks (cdr chunks-arg))
                           (next-offset (+ offset len)))
                       (copy-chunk next-chunks next-offset)))))))
    (let ((result (make-string-from-vector (copy-chunk (reverse chunks) 0))))
      (sys-write 1 result (string-length result))
      (sys-write 1 "\n" 1)
      (sys-exit 42))))
