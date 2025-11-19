;;;; Working REPL with Reader, Evaluator, Line Editing
;;;; All in Lisp with minimal C runtime

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

;;;; Simple Evaluator
(defun eval-expr (expr)
  (if (fixnum? expr) expr
    (if (nil? expr) (quote nil)
      (if (cons? expr)
        (let ((op (car expr)))
          (let ((args (cdr expr)))
            (if (symbol=? op (make-symbol (quote "+")))
              (+ (eval-expr (car args)) (eval-expr (car (cdr args))))
              (if (symbol=? op (make-symbol (quote "-")))
                (- (eval-expr (car args)) (eval-expr (car (cdr args))))
                (if (symbol=? op (make-symbol (quote "*")))
                  (* (eval-expr (car args)) (eval-expr (car (cdr args))))
                  (if (symbol=? op (make-symbol (quote "/")))
                    (/ (eval-expr (car args)) (eval-expr (car (cdr args))))
                    expr))))))
        expr))))

;;;; Simple Reader - just parse numbers for now
(defun is-digit? (ch)
  (if (>= ch (quote 48))
    (<= ch (quote 57))
    (quote nil)))

(defun parse-number (str idx acc)
  (if (>= idx (string-length-raw str)) acc
    (let ((ch (string-ref str idx)))
      (if (is-digit? ch)
        (parse-number str (+ idx (quote 1))
                     (+ (* acc (quote 10)) (- ch (quote 48))))
        acc))))

(defun read-str (str)
  (parse-number str (quote 0) (quote 0)))

;;;; REPL
(defun repl-start ()
  (progn
    (print (quote "Habu REPL with Line Editing"))
    (println)
    (repl-loop)))

(defun repl-loop ()
  (let ((line (readline (quote "habu> "))))
    (if line
      (progn
        (let ((str (make-string-from-cstr line)))
          (let ((num (read-str str)))
            (let ((result (eval-expr num)))
              (progn
                (print-value result)
                (println)))))
        (repl-loop))
      (progn
        (println)
        (print (quote "Goodbye!"))
        (println)))))

(repl-start)
