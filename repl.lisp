;;;; Habu REPL - Written in Habu Lisp itself!

(defun repl-loop ()
  (progn
    (print (quote "Habu REPL - Written in Lisp!"))
    (println)
    (print (quote "Press Ctrl-D to exit"))
    (println)
    (println)
    (repl-loop-body)))

(defun repl-loop-body ()
  (progn
    (print (quote "habu> "))
    (let ((line (fgets-line)))
      (if line
          (progn
            (if (> (string-length line) (quote 0))
                (let ((input-str (make-string-from-cstr line)))
                  (let ((expr (read-from-string input-str)))
                    (let ((result (eval expr)))
                      (progn
                        (print-value result)
                        (println)))))
                (quote nil))
            (repl-loop-body))
          (progn
            (println)
            (print (quote "Bye!"))
            (println))))))

;; Start the REPL
(repl-loop)
