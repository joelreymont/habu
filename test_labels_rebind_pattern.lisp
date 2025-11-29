;;; Test: labels with parameter rebinding same name as outer variable

(labels ((outer-fn (x)
           (+ x 1)))
  (let* ((v1 10)
         (v2 20)
         (v3 30)  ; THREE bindings
         (chunks (cons "A" (cons "B" nil))))  ; outer chunks variable
    (labels ((process-chunks (chunks count)  ; REBIND chunks as parameter!
               (if (null chunks)
                   count
                   (process-chunks (cdr chunks) (+ count 1)))))
      (sys-exit (process-chunks chunks 0)))))  ; Should exit with 2
