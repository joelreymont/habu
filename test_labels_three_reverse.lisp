;;; Test: labels + 3 bindings + reverse macro (also uses labels internally)

(labels ((outer-fn (x)
           (+ x 1)))
  (let* ((v1 10)
         (v2 20)
         (v3 30))  ; THREE bindings
    (let ((lst (cons 1 (cons 2 (cons 3 nil)))))
      (let ((result (reverse lst)))
        (sys-exit (car result))))))  ; Should exit with 3
