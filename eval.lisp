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
