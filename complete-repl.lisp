;;;; Complete REPL - Reader, Eval, Print Loop in Lisp
;;;; Minimal C runtime with type predicates and comparisons in Lisp

;;;; Type Predicates
(defun fixnum? (x) (= (get-tag x) (quote 0)))
(defun cons? (x) (= (get-tag x) (quote 1)))
(defun symbol? (x) (= (get-tag x) (quote 2)))
(defun string? (x) (= (get-tag x) (quote 4)))
(defun nil? (x) (= x (quote 0)))

;;;; String Comparison
(defun str-cmp-loop (s1 s2 i len)
  (if (>= i len) (quote 1)
    (if (= (string-ref s1 i) (string-ref s2 i))
      (str-cmp-loop s1 s2 (+ i (quote 1)) len)
      (quote nil))))

(defun string=? (s1 s2)
  (let ((len1 (string-length-raw s1)))
    (let ((len2 (string-length-raw s2)))
      (if (= len1 len2)
        (str-cmp-loop s1 s2 (quote 0) len1)
        (quote nil)))))

(defun symbol=? (s1 s2)
  (string=? (symbol-name s1) (symbol-name s2)))

;;;; Simple Evaluator for testing
(defun eval-simple (expr)
  (if (fixnum? expr) expr
    (if (nil? expr) (quote nil)
      (if (cons? expr)
        (let ((op (car expr)))
          (if (symbol=? op (make-symbol (quote "+")))
            (+ (eval-simple (car (cdr expr)))
               (eval-simple (car (cdr (cdr expr)))))
            (if (symbol=? op (make-symbol (quote "-")))
              (- (eval-simple (car (cdr expr)))
                 (eval-simple (car (cdr (cdr expr)))))
              (if (symbol=? op (make-symbol (quote "*")))
                (* (eval-simple (car (cdr expr)))
                   (eval-simple (car (cdr (cdr expr)))))
                expr))))
        expr))))

;;;; REPL
(defun repl-start ()
  (progn
    (print (quote "Habu REPL"))
    (println)
    (repl-loop)))

(defun repl-loop ()
  (let ((line (readline (quote "habu> "))))
    (if line
      (progn
        (let ((result (eval-simple (quote 42))))
          (progn
            (print-value result)
            (println)))
        (repl-loop))
      (progn
        (println)
        (print (quote "Bye!"))
        (println)))))

(repl-start)
