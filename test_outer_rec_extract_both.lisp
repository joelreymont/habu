;;; Test: outer-rec + extract BOTH values, then concat

(labels ((outer-rec (n acc)
           (if (= n 0)
               (list acc n)
               (outer-rec (- n 1) (+ acc n)))))
  (let* ((result-list (outer-rec 3 0))
         (acc-val (car result-list))
         (n-val (car (cdr result-list))))  ; Extract BOTH like crashing test
    (sys-write 1 "Before concat\n" 14)
    (let ((chunks (cons "A" (cons "B" nil))))
      (let ((result (concat-string-list chunks 2)))
        (sys-write 1 "After concat: " 14)
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
