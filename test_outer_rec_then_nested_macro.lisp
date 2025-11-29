;;; Test: outer-rec, then a CUSTOM macro with nested labels

;; Define a custom macro with nested labels like concat has
;; We'll do this by calling concat with FEWER labels levels

(labels ((outer-rec (n acc)
           (if (= n 0)
               (list acc n)
               (outer-rec (- n 1) (+ acc n)))))
  (let* ((result-list (outer-rec 3 0))
         (acc-val (car result-list)))
    ;; Now test with LENGTH which has simple labels, then REVERSE which also has simple labels
    ;; But both called in sequence (not nested)
    (let* ((chunks (cons "A" (cons "B" nil)))
           (len1 (length chunks))
           (rev (reverse chunks)))
      (sys-write 1 (number-to-string len1) 1)
      (sys-write 1 "\n" 1)
      (sys-exit 42))))
