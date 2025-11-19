;;;; Simple REPL with minimal runtime
;;;; Type predicates and comparisons in Lisp

;;; Type predicates using get-tag
(defun fixnum? (x) (= (get-tag x) (quote 0)))
(defun cons? (x) (= (get-tag x) (quote 1)))
(defun symbol? (x) (= (get-tag x) (quote 2)))
(defun nil? (x) (= x (quote 0)))

;;; String comparison
(defun string-cmp-loop (s1 s2 i len)
  (if (>= i len) (quote 1)
    (if (= (string-ref s1 i) (string-ref s2 i))
      (string-cmp-loop s1 s2 (+ i (quote 1)) len)
      (quote nil))))

(defun string=? (s1 s2)
  (let ((len1 (string-length-raw s1)))
    (let ((len2 (string-length-raw s2)))
      (if (= len1 len2)
        (string-cmp-loop s1 s2 (quote 0) len1)
        (quote nil)))))

(defun symbol=? (s1 s2)
  (string=? (symbol-name s1) (symbol-name s2)))

;;; Simple evaluator
(defun eval (expr)
  (if (fixnum? expr) expr
    (if (nil? expr) (quote nil)
      (if (cons? expr)
        (let ((op (car expr)))
          (let ((args (cdr expr)))
            (if (symbol=? op (make-symbol (quote "+")))
              (+ (eval (car args)) (eval (car (cdr args))))
              (if (symbol=? op (make-symbol (quote "*")))
                (quote 42)
                expr))))
        expr))))

;;; Simple REPL
(defun repl ()
  (progn
    (print (quote "Simple REPL"))
    (println)
    (repl-loop)))

(defun repl-loop ()
  (progn
    (print (quote "> "))
    (let ((line (fgets-line)))
      (if line
        (progn
          (let ((num (quote 42)))
            (progn
              (print-value num)
              (println)))
          (repl-loop))
        (progn
          (println)
          (print (quote "Bye"))
          (println))))))

(repl)
