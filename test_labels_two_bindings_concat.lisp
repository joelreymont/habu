;;; Test: labels function + TWO bindings + concat

(labels ((simple-fn (x)
           (+ x 10)))
  (let* ((dummy1 10)
         (dummy2 20))  ; Only TWO bindings
    (sys-write 1 "Before concat\n" 14)
    (let ((chunks (cons "A" (cons "B" nil))))
      (let ((result (concat-string-list chunks 2)))
        (sys-write 1 "After concat: " 14)
        (sys-write 1 result (string-length result))
        (sys-write 1 "\n" 1)
        (sys-exit 42)))))
