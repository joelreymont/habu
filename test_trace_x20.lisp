;;; Minimal test to trace x20 issue with nested variable access

(let* ((outer-var 100))
  (labels ((outer-fn (x)
             (labels ((inner-fn (y)
                        (+ outer-var y)))  ; Access outer-var from nested labels
               (inner-fn x))))
    (let ((result (outer-fn 42)))
      (sys-write 1 (number-to-string result) (string-length (number-to-string result)))
      (sys-write 1 "\n" 1)
      (sys-exit (if (= result 142) 42 1)))))
