1024
(defun debug-compile2 ()
  (let ((forms (read-all "(+ 1 2)")))
    (let ((first-form (car forms)))
      (let ((op (car first-form)))
        ;; The op from read-all should be the same as '+
        ;; But *op-plus* is set at startup from native intern
        ;; Let's test by comparing to a freshly interned +
        (let ((fresh-plus (car (read-all "+"))))
          (if (eq op fresh-plus)
              111  ;; read-all + matches read-all +
              222))))))
(debug-compile2)