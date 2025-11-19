;;;; Extended REPL - Adding list parsing
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

;;;; Evaluator
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

;;;; Simple Reader - numbers and simple lists
(defun is-digit? (ch)
  (if (>= ch (quote 48))
    (<= ch (quote 57))
    (quote nil)))

(defun is-whitespace? (ch)
  (if (= ch (quote 32)) (quote 1)
  (if (= ch (quote 10)) (quote 1)
  (if (= ch (quote 9)) (quote 1)
    (quote nil)))))

(defun skip-ws (str idx)
  (if (>= idx (string-length-raw str)) idx
    (let ((ch (string-ref str idx)))
      (if (is-whitespace? ch)
        (skip-ws str (+ idx (quote 1)))
        idx))))

(defun parse-number (str idx acc)
  (if (>= idx (string-length-raw str))
    (cons acc idx)
    (let ((ch (string-ref str idx)))
      (if (is-digit? ch)
        (parse-number str (+ idx (quote 1))
                     (+ (* acc (quote 10)) (- ch (quote 48))))
        (cons acc idx)))))

;;; Parse simple operator symbols
(defun parse-op (str idx)
  (let ((ch (string-ref str idx)))
    (if (= ch (quote 43))  ; +
      (cons (make-symbol (quote "+")) (+ idx (quote 1)))
      (if (= ch (quote 45))  ; -
        (cons (make-symbol (quote "-")) (+ idx (quote 1)))
        (if (= ch (quote 42))  ; *
          (cons (make-symbol (quote "*")) (+ idx (quote 1)))
          (if (= ch (quote 47))  ; /
            (cons (make-symbol (quote "/")) (+ idx (quote 1)))
            (cons (quote nil) idx)))))))

;;; Parse one expression
(defun parse-one (str idx)
  (let ((idx2 (skip-ws str idx)))
    (if (>= idx2 (string-length-raw str))
      (cons (quote nil) idx2)
      (let ((ch (string-ref str idx2)))
        (if (= ch (quote 40))  ; (
          (parse-list str (+ idx2 (quote 1)))
          (if (is-digit? ch)
            (parse-number str idx2 (quote 0))
            (parse-op str idx2)))))))

;;; Parse list
(defun parse-list (str idx)
  (let ((idx2 (skip-ws str idx)))
    (let ((op-result (parse-one str idx2)))
      (let ((op (car op-result)))
        (let ((idx3 (cdr op-result)))
          (let ((arg1-result (parse-one str idx3)))
            (let ((arg1 (car arg1-result)))
              (let ((idx4 (cdr arg1-result)))
                (let ((arg2-result (parse-one str idx4)))
                  (let ((arg2 (car arg2-result)))
                    (let ((idx5 (skip-ws str (cdr arg2-result))))
                      (let ((idx6 (+ idx5 (quote 1))))  ; skip )
                        (cons (cons op (cons arg1 (cons arg2 (quote nil)))) idx6)))))))))))))

(defun read-str (str)
  (car (parse-one str (quote 0))))

;;;; REPL
(defun repl-start ()
  (progn
    (print (quote "Habu REPL - List Support"))
    (println)
    (repl-loop)))

(defun repl-loop ()
  (let ((line (readline (quote "habu> "))))
    (if line
      (progn
        (let ((str (make-string-from-cstr line)))
          (let ((expr (read-str str)))
            (let ((result (eval-expr expr)))
              (progn
                (print-value result)
                (println)))))
        (repl-loop))
      (progn
        (println)
        (print (quote "Goodbye!"))
        (println)))))

(repl-start)
