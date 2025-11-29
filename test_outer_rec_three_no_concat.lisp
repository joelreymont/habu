;;; Test: outer-rec + THREE bindings but NO concat

(labels ((outer-rec (n acc)
           (if (= n 0)
               (list acc n)
               (outer-rec (- n 1) (+ acc n)))))
  (let* ((result-list (outer-rec 3 0))
         (acc-val (car result-list))
         (n-val (car (cdr result-list))))  ; THREE bindings
    ;; No concat, just simple string operations
    (sys-write 1 "Value: " 7)
    (sys-write 1 (number-to-string acc-val) (if (< acc-val 10) 1 2))
    (sys-write 1 "\n" 1)
    (sys-exit 42)))
