;;; Minimal test: nested labels in recursive argument

(labels ((outer (n)
           (if (= n 0)
               42
               (outer (labels ((inner (i) i))
                        (inner 1))))))
  (sys-exit (outer 1)))
