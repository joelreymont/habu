(defun fixnum? (x)
  (= (get-tag x) (quote 0)))

(defun cons? (x)
  (= (get-tag x) (quote 1)))

(defun symbol? (x)
  (= (get-tag x) (quote 2)))

(defun vector? (x)
  (= (get-tag x) (quote 3)))

(defun string? (x)
  (= (get-tag x) (quote 4)))

(defun nil? (x)
  (= x (quote 0)))

(defun string-compare-loop (s1 s2 idx len)
  (if (>= idx len)
      (quote 1)
      (if (= (string-ref s1 idx) (string-ref s2 idx))
          (string-compare-loop s1 s2 (+ idx (quote 1)) len)
          (quote nil))))

(defun string=? (s1 s2)
  (if (string? s1)
      (if (string? s2)
          (let ((len1 (string-length-raw s1)))
            (let ((len2 (string-length-raw s2)))
              (if (= len1 len2)
                  (string-compare-loop s1 s2 (quote 0) len1)
                  (quote nil))))
          (quote nil))
      (quote nil)))

(defun symbol=? (sym1 sym2)
  (if (symbol? sym1)
      (if (symbol? sym2)
          (string=? (symbol-name sym1) (symbol-name sym2))
          (quote nil))
      (quote nil)))
;;;; Reader - Parse S-expressions from strings
;;;; Written in Habu Lisp - NO C dependencies except primitives

;;; Character utilities (characters are fixnums)

(defun char-code (ch-str)
  "Get character code from single-char string"
  (string-ref ch-str (quote 0)))

(defun is-whitespace? (ch)
  "Check if character is whitespace"
  (if (= ch (quote 32))  ; space
      (quote 1)
      (if (= ch (quote 10))  ; newline
          (quote 1)
          (if (= ch (quote 9))  ; tab
              (quote 1)
              (if (= ch (quote 13))  ; carriage return
                  (quote 1)
                  (quote nil))))))

(defun is-digit? (ch)
  "Check if character is a digit 0-9"
  (if (>= ch (quote 48))  ; '0'
      (<= ch (quote 57))  ; '9'
      (quote nil)))

(defun is-alpha? (ch)
  "Check if character is alphabetic"
  (if (>= ch (quote 65))  ; 'A'
      (if (<= ch (quote 90))  ; 'Z'
          (quote 1)
          (if (>= ch (quote 97))  ; 'a'
              (<= ch (quote 122))  ; 'z'
              (quote nil)))
      (quote nil)))

(defun is-special-symbol-char? (ch)
  "Check if character can be in a symbol"
  (if (= ch (quote 43)) (quote 1)  ; +
  (if (= ch (quote 45)) (quote 1)  ; -
  (if (= ch (quote 42)) (quote 1)  ; *
  (if (= ch (quote 47)) (quote 1)  ; /
  (if (= ch (quote 61)) (quote 1)  ; =
  (if (= ch (quote 60)) (quote 1)  ; <
  (if (= ch (quote 62)) (quote 1)  ; >
  (if (= ch (quote 63)) (quote 1)  ; ?
  (if (= ch (quote 33)) (quote 1)  ; !
      (quote nil)))))))))))

(defun is-symbol-char? (ch)
  "Check if character can be part of a symbol"
  (if (is-alpha? ch)
      (quote 1)
      (if (is-digit? ch)
          (quote 1)
          (if (is-special-symbol-char? ch)
              (quote 1)
              (quote nil)))))

(defun is-paren? (ch)
  "Check if character is parenthesis"
  (if (= ch (quote 40))  ; '('
      (quote 1)
      (if (= ch (quote 41))  ; ')'
          (quote 1)
          (quote nil))))

(defun digit-to-int (ch)
  "Convert digit character to integer"
  (- ch (quote 48)))

;;; String builder (using cons list of chars, then convert)

(defun build-string-from-chars (chars)
  "Build string from list of character codes"
  (if (nil? chars)
      (make-string-from-cstr (quote ""))
      (let ((len (length-helper chars (quote 0))))
        (build-string-iter chars (quote 0) len (make-vector len)))))

(defun length-helper (lst acc)
  (if (nil? lst)
      acc
      (length-helper (cdr lst) (+ acc (quote 1)))))

(defun build-string-iter (chars idx len vec)
  (if (>= idx len)
      (vector-to-string vec len)
      (progn
        (vector-set vec idx (car chars))
        (build-string-iter (cdr chars) (+ idx (quote 1)) len vec))))

(defun vector-to-string (vec len)
  "Convert vector of chars to string - PLACEHOLDER"
  (make-string-from-cstr (quote "symbol")))

;;; Reader state: (string . index)

(defun make-reader-state (str)
  (cons str (quote 0)))

(defun reader-string (state)
  (car state))

(defun reader-index (state)
  (cdr state))

(defun reader-at-end? (state)
  (let ((str (car state)))
    (let ((idx (cdr state)))
      (>= idx (string-length str)))))

(defun reader-peek (state)
  "Get current character without advancing"
  (if (reader-at-end? state)
      (quote nil)
      (let ((str (car state)))
        (string-ref str (cdr state)))))

(defun reader-advance (state)
  "Move to next character"
  (cons (car state) (+ (cdr state) (quote 1))))

;;; Skip whitespace

(defun skip-whitespace (state)
  (if (reader-at-end? state)
      state
      (let ((ch (reader-peek state)))
        (if (is-whitespace? ch)
            (skip-whitespace (reader-advance state))
            state))))

;;; Parse number

(defun parse-number-digits (state acc)
  (if (reader-at-end? state)
      (cons acc state)
      (let ((ch (reader-peek state)))
        (if (is-digit? ch)
            (parse-number-digits
              (reader-advance state)
              (+ (* acc (quote 10)) (digit-to-int ch)))
            (cons acc state)))))

(defun parse-number (state)
  (parse-number-digits state (quote 0)))

;;; Parse symbol

(defun collect-symbol-chars (state chars)
  (if (reader-at-end? state)
      (cons chars state)
      (let ((ch (reader-peek state)))
        (if (is-symbol-char? ch)
            (collect-symbol-chars
              (reader-advance state)
              (cons ch chars))
            (cons chars state)))))

(defun reverse-list (lst)
  (reverse-helper lst (quote nil)))

(defun reverse-helper (lst acc)
  (if (nil? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(defun parse-symbol (state)
  "Parse symbol and return as symbol value"
  (let ((result (collect-symbol-chars state (quote nil))))
    (let ((chars (reverse-list (car result))))
      (let ((new-state (cdr result)))
        (let ((sym-str (build-string-from-chars chars)))
          (cons (make-symbol sym-str) new-state))))))

;;; Parse list

(defun parse-list-elements (state)
  (let ((state2 (skip-whitespace state)))
    (if (reader-at-end? state2)
        (cons (quote nil) state2)
        (let ((ch (reader-peek state2)))
          (if (= ch (quote 41))  ; ')'
              (cons (quote nil) (reader-advance state2))
              (let ((elem-result (read-one state2)))
                (let ((elem (car elem-result)))
                  (let ((state3 (cdr elem-result)))
                    (let ((rest-result (parse-list-elements state3)))
                      (cons (cons elem (car rest-result))
                            (cdr rest-result)))))))))))

(defun parse-list (state)
  "Parse list starting with '('"
  (let ((state2 (reader-advance state)))  ; skip '('
    (parse-list-elements state2)))

;;; Main reader

(defun read-one (state)
  "Read one S-expression from state"
  (let ((state2 (skip-whitespace state)))
    (if (reader-at-end? state2)
        (cons (quote nil) state2)
        (let ((ch (reader-peek state2)))
          (if (= ch (quote 40))  ; '('
              (parse-list state2)
              (if (is-digit? ch)
                  (parse-number state2)
                  (if (is-symbol-char? ch)
                      (parse-symbol state2)
                      (cons (quote nil) state2))))))))

(defun read-from-string (str)
  "Parse one S-expression from string"
  (let ((state (make-reader-state str)))
    (car (read-one state))))
;;;; Evaluator - Evaluate S-expressions
;;;; Written in Habu Lisp - NO C dependencies except primitives

;;; Environment: association list ((sym1 . val1) (sym2 . val2) ...)

(defun env-lookup (sym env)
  "Look up symbol in environment"
  (if (nil? env)
      (quote nil)
      (let ((binding (car env)))
        (if (symbol=? (car binding) sym)
            (cdr binding)
            (env-lookup sym (cdr env))))))

(defun env-extend (syms vals env)
  "Extend environment with new bindings"
  (if (nil? syms)
      env
      (cons (cons (car syms) (car vals))
            (env-extend (cdr syms) (cdr vals) env))))

;;; Built-in function table

(defun make-builtin-env ()
  "Create environment with built-in functions"
  (quote nil))  ; Start with empty, builtins handled in apply

;;; Check if symbol matches name

(defun symbol-matches? (sym name-str)
  "Check if symbol matches string name"
  (if (symbol? sym)
      (string=? (symbol-name sym) name-str)
      (quote nil)))

;;; Eval list of expressions

(defun eval-list (exprs env)
  "Evaluate list of expressions"
  (if (nil? exprs)
      (quote nil)
      (cons (eval (car exprs) env)
            (eval-list (cdr exprs) env))))

;;; Apply built-in functions

(defun apply-builtin (fn args)
  "Apply built-in function"
  (if (symbol-matches? fn (quote "+"))
      (apply-plus args)
  (if (symbol-matches? fn (quote "-"))
      (apply-minus args)
  (if (symbol-matches? fn (quote "*"))
      (apply-mult args)
  (if (symbol-matches? fn (quote "/"))
      (apply-div args)
  (if (symbol-matches? fn (quote "="))
      (apply-eq args)
  (if (symbol-matches? fn (quote "<"))
      (apply-lt args)
  (if (symbol-matches? fn (quote ">"))
      (apply-gt args)
  (if (symbol-matches? fn (quote "cons"))
      (cons (car args) (car (cdr args)))
  (if (symbol-matches? fn (quote "car"))
      (car (car args))
  (if (symbol-matches? fn (quote "cdr"))
      (cdr (car args))
  (if (symbol-matches? fn (quote "list"))
      args
      (quote nil)))))))))))))

(defun apply-plus (args)
  (if (nil? args)
      (quote 0)
      (if (nil? (cdr args))
          (car args)
          (+ (car args) (apply-plus (cdr args))))))

(defun apply-minus (args)
  (if (nil? (cdr args))
      (- (quote 0) (car args))  ; unary minus
      (- (car args) (apply-plus (cdr args)))))

(defun apply-mult (args)
  (if (nil? args)
      (quote 1)
      (if (nil? (cdr args))
          (car args)
          (* (car args) (apply-mult (cdr args))))))

(defun apply-div (args)
  (if (nil? (cdr args))
      (/ (quote 1) (car args))
      (/ (car args) (apply-mult (cdr args)))))

(defun apply-eq (args)
  (if (= (car args) (car (cdr args)))
      (quote 1)
      (quote nil)))

(defun apply-lt (args)
  (if (< (car args) (car (cdr args)))
      (quote 1)
      (quote nil)))

(defun apply-gt (args)
  (if (> (car args) (car (cdr args)))
      (quote 1)
      (quote nil)))

;;; Main evaluator

(defun eval (expr env)
  "Evaluate expression in environment"
  (progn
    ;; Self-evaluating: numbers
    (if (fixnum? expr)
        expr
    ;; Self-evaluating: strings
    (if (string? expr)
        expr
    ;; Nil
    (if (nil? expr)
        (quote nil)
    ;; Symbols: variable lookup
    (if (symbol? expr)
        (env-lookup expr env)
    ;; Lists: special forms or function application
    (if (cons? expr)
        (let ((first (car expr)))
          (let ((args (cdr expr)))
            ;; Special form: quote
            (if (symbol-matches? first (quote "quote"))
                (car args)
            ;; Special form: if
            (if (symbol-matches? first (quote "if"))
                (let ((cond-val (eval (car args) env)))
                  (if (nil? cond-val)
                      (eval (car (cdr (cdr args))) env)  ; else
                      (eval (car (cdr args)) env)))       ; then
            ;; Special form: progn
            (if (symbol-matches? first (quote "progn"))
                (eval-progn args env)
            ;; Special form: let (simplified - no parallel binding)
            (if (symbol-matches? first (quote "let"))
                (let ((bindings (car args)))
                  (let ((body (car (cdr args))))
                    (let ((new-env (eval-bindings bindings env)))
                      (eval body new-env))))
            ;; Function application
            (let ((evaled-args (eval-list args env)))
              (apply-builtin first evaled-args))))))))
        ;; Default: return as-is
        expr))))))

(defun eval-progn (exprs env)
  "Evaluate sequence of expressions"
  (if (nil? exprs)
      (quote nil)
      (if (nil? (cdr exprs))
          (eval (car exprs) env)
          (progn
            (eval (car exprs) env)
            (eval-progn (cdr exprs) env)))))

(defun eval-bindings (bindings env)
  "Evaluate let bindings and extend environment"
  (if (nil? bindings)
      env
      (let ((binding (car bindings)))
        (let ((sym (car binding)))
          (let ((val (eval (car (cdr binding)) env)))
            (let ((new-env (cons (cons sym val) env)))
              (eval-bindings (cdr bindings) new-env)))))))
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
