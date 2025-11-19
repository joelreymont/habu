;;;; Enhanced REPL - Quote, General Symbols, More Forms
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

;;;; Reader - Enhanced with symbols and quote

(defun is-digit? (ch)
  (if (>= ch (quote 48))
    (<= ch (quote 57))
    (quote nil)))

(defun is-alpha? (ch)
  (if (>= ch (quote 65))
    (if (<= ch (quote 90)) (quote 1)
      (if (>= ch (quote 97))
        (<= ch (quote 122))
        (quote nil)))
    (quote nil)))

(defun is-symbol-start? (ch)
  (if (is-alpha? ch) (quote 1)
    (if (= ch (quote 43)) (quote 1)   ; +
    (if (= ch (quote 45)) (quote 1)   ; -
    (if (= ch (quote 42)) (quote 1)   ; *
    (if (= ch (quote 47)) (quote 1)   ; /
    (if (= ch (quote 61)) (quote 1)   ; =
    (if (= ch (quote 60)) (quote 1)   ; <
    (if (= ch (quote 62)) (quote 1)   ; >
    (if (= ch (quote 63)) (quote 1)   ; ?
    (if (= ch (quote 33)) (quote 1)   ; !
      (quote nil))))))))))))

(defun is-symbol-char? (ch)
  (if (is-symbol-start? ch) (quote 1)
    (if (is-digit? ch) (quote 1)
      (quote nil))))

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

;;; Build symbol from characters
(defun collect-chars (str idx chars)
  (if (>= idx (string-length-raw str))
    (cons chars idx)
    (let ((ch (string-ref str idx)))
      (if (is-symbol-char? ch)
        (collect-chars str (+ idx (quote 1)) (cons ch chars))
        (cons chars idx)))))

(defun reverse-list (lst)
  (reverse-helper lst (quote nil)))

(defun reverse-helper (lst acc)
  (if (nil? lst) acc
    (reverse-helper (cdr lst) (cons (car lst) acc))))

(defun make-sym-from-chars (chars)
  (let ((len (list-length chars (quote 0))))
    (let ((vec (make-vector len)))
      (progn
        (fill-vec chars vec (quote 0))
        (make-symbol (make-string-from-vector vec))))))

(defun list-length (lst acc)
  (if (nil? lst) acc
    (list-length (cdr lst) (+ acc (quote 1)))))

(defun fill-vec (chars vec idx)
  (if (nil? chars) vec
    (progn
      (vector-set vec idx (car chars))
      (fill-vec (cdr chars) vec (+ idx (quote 1))))))

;;; Parse number
(defun parse-num (str idx acc)
  (if (>= idx (string-length-raw str))
    (cons acc idx)
    (let ((ch (string-ref str idx)))
      (if (is-digit? ch)
        (parse-num str (+ idx (quote 1))
                   (+ (* acc (quote 10)) (- ch (quote 48))))
        (cons acc idx)))))

;;; Parse symbol
(defun parse-sym (str idx)
  (let ((result (collect-chars str idx (quote nil))))
    (let ((chars (reverse-list (car result))))
      (cons (make-sym-from-chars chars) (cdr result)))))

;;; Parse list
(defun parse-list (str idx acc)
  (let ((idx2 (skip-ws str idx)))
    (if (>= idx2 (string-length-raw str))
      (cons (reverse-list acc) idx2)
      (let ((ch (string-ref str idx2)))
        (if (= ch (quote 41))  ; )
          (cons (reverse-list acc) (+ idx2 (quote 1)))
          (let ((elem-result (parse-one str idx2)))
            (parse-list str (cdr elem-result)
                       (cons (car elem-result) acc))))))))

;;; Main parser
(defun parse-one (str idx)
  (let ((idx2 (skip-ws str idx)))
    (if (>= idx2 (string-length-raw str))
      (cons (quote nil) idx2)
      (let ((ch (string-ref str idx2)))
        (if (= ch (quote 40))  ; (
          (parse-list str (+ idx2 (quote 1)) (quote nil))
          (if (= ch (quote 39))  ; ' (quote)
            (let ((quoted-result (parse-one str (+ idx2 (quote 1)))))
              (cons (cons (make-symbol (quote "quote"))
                         (cons (car quoted-result) (quote nil)))
                   (cdr quoted-result)))
            (if (is-digit? ch)
              (parse-num str idx2 (quote 0))
              (if (is-symbol-start? ch)
                (parse-sym str idx2)
                (cons (quote nil) idx2)))))))))

(defun read-str (str)
  (car (parse-one str (quote 0))))

;;;; Evaluator with quote, if, let, lambda

(defun eval-expr (expr env)
  (if (fixnum? expr) expr
    (if (nil? expr) (quote nil)
      (if (symbol? expr)
        (env-lookup expr env)
        (if (cons? expr)
          (let ((first (car expr)))
            (if (symbol=? first (make-symbol (quote "quote")))
              (car (cdr expr))
              (if (symbol=? first (make-symbol (quote "if")))
                (eval-if (cdr expr) env)
                (if (symbol=? first (make-symbol (quote "let")))
                  (eval-let (cdr expr) env)
                  (if (symbol=? first (make-symbol (quote "lambda")))
                    (cons (make-symbol (quote "closure")) (cons env (cdr expr)))
                    (eval-apply first (cdr expr) env))))))
          expr)))))

(defun eval-if (args env)
  (let ((test (eval-expr (car args) env)))
    (if (nil? test)
      (eval-expr (car (cdr (cdr args))) env)
      (eval-expr (car (cdr args)) env))))

(defun eval-let (args env)
  (let ((bindings (car args)))
    (let ((body (car (cdr args))))
      (let ((new-env (eval-bindings bindings env)))
        (eval-expr body new-env)))))

(defun eval-bindings (bindings env)
  (if (nil? bindings) env
    (let ((binding (car bindings)))
      (let ((sym (car binding)))
        (let ((val-expr (car (cdr binding))))
          (let ((val (eval-expr val-expr env)))
            (eval-bindings (cdr bindings)
                          (env-extend sym val env))))))))

(defun eval-list (exprs env)
  (if (nil? exprs) (quote nil)
    (cons (eval-expr (car exprs) env)
          (eval-list (cdr exprs) env))))

(defun eval-apply (op args env)
  (if (symbol=? op (make-symbol (quote "+")))
    (+ (eval-expr (car args) env)
       (eval-expr (car (cdr args)) env))
    (if (symbol=? op (make-symbol (quote "-")))
      (- (eval-expr (car args) env)
         (eval-expr (car (cdr args)) env))
      (if (symbol=? op (make-symbol (quote "*")))
        (* (eval-expr (car args) env)
           (eval-expr (car (cdr args)) env))
        (if (symbol=? op (make-symbol (quote "/")))
          (/ (eval-expr (car args) env)
             (eval-expr (car (cdr args)) env))
          (if (symbol=? op (make-symbol (quote "=")))
            (if (= (eval-expr (car args) env)
                   (eval-expr (car (cdr args)) env))
              (quote 1)
              (quote nil))
            (if (symbol=? op (make-symbol (quote "<")))
              (if (< (eval-expr (car args) env)
                     (eval-expr (car (cdr args)) env))
                (quote 1)
                (quote nil))
              (if (symbol=? op (make-symbol (quote ">")))
                (if (> (eval-expr (car args) env)
                       (eval-expr (car (cdr args)) env))
                  (quote 1)
                  (quote nil))
                (if (symbol=? op (make-symbol (quote "cons")))
                  (cons (eval-expr (car args) env)
                        (eval-expr (car (cdr args)) env))
                  (if (symbol=? op (make-symbol (quote "car")))
                    (car (eval-expr (car args) env))
                    (if (symbol=? op (make-symbol (quote "cdr")))
                      (cdr (eval-expr (car args) env))
                      (if (symbol=? op (make-symbol (quote "list")))
                        (eval-list args env)
                        (let ((fn (eval-expr op env)))
                          (if (cons? fn)
                            (if (symbol=? (car fn) (make-symbol (quote "closure")))
                              (apply-lambda fn (eval-list args env) env)
                              (quote nil))
                            (quote nil)))))))))))))))

(defun append-env (env1 env2)
  (if (nil? env1) env2
    (cons (car env1) (append-env (cdr env1) env2))))

(defun apply-lambda (closure arg-vals current-env)
  (let ((closure-env (car (cdr closure))))
    (let ((params (car (cdr (cdr closure)))))
      (let ((body (car (cdr (cdr (cdr closure))))))
        (let ((combined-env (append-env current-env closure-env)))
          (let ((new-env (env-extend-list params arg-vals combined-env)))
            (eval-expr body new-env)))))))

;;;; Environment - Association List

(defun env-lookup (sym env)
  (if (nil? env) (quote nil)
    (let ((binding (car env)))
      (if (symbol=? sym (car binding))
        (cdr binding)
        (env-lookup sym (cdr env))))))

(defun env-extend (sym val env)
  (cons (cons sym val) env))

(defun env-extend-list (syms vals env)
  (if (nil? syms) env
    (env-extend-list (cdr syms) (cdr vals)
                     (env-extend (car syms) (car vals) env))))

;;;; Top-level evaluation (handles defun)

(defun is-defun? (expr)
  (if (cons? expr)
    (symbol=? (car expr) (make-symbol (quote "defun")))
    (quote nil)))

(defun eval-toplevel (expr env)
  (if (is-defun? expr)
    (let ((name (car (cdr expr))))
      (let ((params (car (cdr (cdr expr)))))
        (let ((body (car (cdr (cdr (cdr expr))))))
          (let ((closure (cons (make-symbol (quote "closure")) (cons env (cons params (cons body (quote nil)))))))
            (cons name (env-extend name closure env))))))
    (cons (eval-expr expr env) env)))

;;;; REPL
(defun repl-start ()
  (progn
    (print (quote "Habu REPL - Recursive"))
    (println)
    (print (quote "Features: let, lambda, defun"))
    (println)
    (repl-loop (quote nil))))

(defun repl-loop (env)
  (let ((line (readline (quote "habu> "))))
    (if line
      (progn
        (let ((str (make-string-from-cstr line)))
          (let ((expr (read-str str)))
            (let ((result-env (eval-toplevel expr env)))
              (let ((result (car result-env)))
                (let ((new-env (cdr result-env)))
                  (progn
                    (print-value result)
                    (println)
                    (repl-loop new-env)))))))
        (quote nil))
      (progn
        (println)
        (print (quote "Goodbye!"))
        (println)))))

(repl-start)
