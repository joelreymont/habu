;;;; IR Generation (Source to IR)
;;;; Pure Habu - no SBCL dependencies
;;;; Architecture-independent

;;; Lower quoted object to IR
(defun quote->ir (obj)
  (cond
    ((fixnum? obj) (list 'lit obj))
    ((nil? obj) (list 'lit 0))
    ((symbol? obj) (list 'symbol-lit (symbol-name obj)))
    ((cons? obj) (list 'cons-call (quote->ir (car obj)) (quote->ir (cdr obj))))
    (t (list 'lit 0))))

;;; Main IR compiler: source expression -> IR
(defun compile-expr (expr env fenv)
  (cond
    ;; Fixnum literal
    ((fixnum? expr)
     (list 'lit expr))

    ;; Symbol (variable reference)
    ((symbol? expr)
     (let ((off (env-lookup expr env)))
       (if off (list 'var off) (list 'lit 0))))

    ;; List (function call or special form)
    ((cons? expr)
     (let ((op (car expr)))
       (cond
         ;; Addition (variadic)
         ((eq op '+)
          (let ((args (cdr expr)))
            (cond
              ((nil? args) (list 'lit 0))
              ((nil? (cdr args))
               (compile-expr (car args) env fenv))
              ((nil? (cdr (cdr args)))
               (list 'add
                     (compile-expr (car args) env fenv)
                     (compile-expr (car (cdr args)) env fenv)))
              (t
               (compile-expr (cons '+ (cons (list '+ (car args) (car (cdr args)))
                                            (cdr (cdr args))))
                             env fenv)))))

         ;; Subtraction (variadic)
         ((eq op '-)
          (let ((args (cdr expr)))
            (cond
              ((nil? args) (list 'lit 0))
              ((nil? (cdr args))
               (list 'sub (list 'lit 0) (compile-expr (car args) env fenv)))
              ((nil? (cdr (cdr args)))
               (list 'sub
                     (compile-expr (car args) env fenv)
                     (compile-expr (car (cdr args)) env fenv)))
              (t
               (compile-expr (cons '- (cons (list '- (car args) (car (cdr args)))
                                            (cdr (cdr args))))
                             env fenv)))))

         ;; Multiplication (variadic)
         ((eq op '*)
          (let ((args (cdr expr)))
            (cond
              ((nil? args) (list 'lit 1))
              ((nil? (cdr args))
               (compile-expr (car args) env fenv))
              ((nil? (cdr (cdr args)))
               (list 'mul
                     (compile-expr (car args) env fenv)
                     (compile-expr (car (cdr args)) env fenv)))
              (t
               (compile-expr (cons '* (cons (list '* (car args) (car (cdr args)))
                                            (cdr (cdr args))))
                             env fenv)))))

         ;; Division
         ((eq op '/)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'div
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Comparisons
         ((eq op '=)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ((eq op '<)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-lt
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ((eq op '>)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-gt
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; If expression
         ((eq op 'if)
          (if (cons? (cdr expr))
              (let ((test (car (cdr expr)))
                    (then-part (if (cons? (cdr (cdr expr))) (car (cdr (cdr expr))) 0))
                    (else-part (if (and (cons? (cdr (cdr expr)))
                                        (cons? (cdr (cdr (cdr expr)))))
                                   (car (cdr (cdr (cdr expr))))
                                   0)))
                (list 'if-expr
                      (compile-expr test env fenv)
                      (compile-expr then-part env fenv)
                      (compile-expr else-part env fenv)))
              (list 'lit 0)))

         ;; Let binding
         ((eq op 'let)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let* ((bindings (car (cdr expr)))
                     (body (car (cdr (cdr expr)))))
                (labels ((process-bindings (bs env-acc bind-vals bind-names)
                           (if (nil? bs)
                               (list env-acc (reverse bind-vals) (reverse bind-names))
                               (let* ((b (car bs))
                                      (name (if (cons? b) (car b) b))
                                      (val-expr (if (cons? b) (car (cdr b)) 0))
                                      (val-ir (compile-expr val-expr env fenv))
                                      (new-env (env-extend (list (list name)) env-acc)))
                                 (process-bindings (cdr bs) new-env
                                                   (cons val-ir bind-vals)
                                                   (cons name bind-names))))))
                  (let* ((result (process-bindings bindings env nil nil))
                         (new-env (car result))
                         (bind-values (car (cdr result)))
                         (bind-names (car (cdr (cdr result))))
                         (env-offsets (mapcar (lambda (n) (env-lookup n new-env)) bind-names))
                         (body-ir (compile-expr body new-env fenv)))
                    (list 'let-expr bind-values body-ir (length bindings) env-offsets))))
              (list 'lit 0)))

         ;; Setq (variable mutation)
         ((eq op 'setq)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let* ((var (car (cdr expr)))
                     (val-expr (car (cdr (cdr expr))))
                     (offset (env-lookup var env)))
                (if offset
                    (list 'set-var offset (compile-expr val-expr env fenv))
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Progn
         ((eq op 'progn)
          (let ((forms (cdr expr)))
            (if (nil? forms)
                (list 'lit 0)
                (if (nil? (cdr forms))
                    (compile-expr (car forms) env fenv)
                    (cons 'progn (mapcar (lambda (f) (compile-expr f env fenv)) forms))))))

         ;; Quote
         ((eq op 'quote)
          (if (cons? (cdr expr))
              (quote->ir (car (cdr expr)))
              (list 'lit 0)))

         ;; Cons
         ((eq op 'cons)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cons-call
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Car
         ((eq op 'car)
          (if (cons? (cdr expr))
              (list 'car-call (compile-expr (car (cdr expr)) env fenv))
              (list 'lit 0)))

         ;; Cdr
         ((eq op 'cdr)
          (if (cons? (cdr expr))
              (list 'cdr-call (compile-expr (car (cdr expr)) env fenv))
              (list 'lit 0)))

         ;; List (build cons chain)
         ((eq op 'list)
          (labels ((build-list (args)
                     (if (nil? args)
                         (list 'lit 0)
                         (list 'cons-call
                               (compile-expr (car args) env fenv)
                               (build-list (cdr args))))))
            (build-list (cdr expr))))

         ;; Null predicate
         ((or (eq op 'null) (op= op "NIL?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Consp predicate
         ((or (eq op 'consp) (op= op "CONS?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 1))
              (list 'lit 0)))

         ;; Numberp predicate
         ((or (eq op 'numberp) (op= op "FIXNUM?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Symbolp predicate
         ((or (eq op 'symbolp) (op= op "SYMBOL?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 2))
              (list 'lit 0)))

         ;; Eq
         ((eq op 'eq)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Default: unknown form returns 0
         (t (list 'lit 0)))))

    ;; Unknown
    (t (list 'lit 0))))
