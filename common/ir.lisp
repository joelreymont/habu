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

         ;; Modulo
         ((eq op 'mod)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'mod
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Remainder
         ((eq op 'rem)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'rem
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Equality comparison
         ((eq op '=)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Less than
         ((eq op '<)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-lt
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Greater than
         ((eq op '>)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-gt
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Less than or equal
         ((eq op '<=)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-le
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Greater than or equal
         ((eq op '>=)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-ge
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Not equal
         ((eq op '/=)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-ne
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; If expression
         ((eq op 'if)
          (cond
            ;; Three-arg if
            ((and (cons? (cdr expr))
                  (cons? (cdr (cdr expr)))
                  (cons? (cdr (cdr (cdr expr)))))
             (list 'if-expr
                   (compile-expr (car (cdr expr)) env fenv)
                   (compile-expr (car (cdr (cdr expr))) env fenv)
                   (compile-expr (car (cdr (cdr (cdr expr)))) env fenv)))
            ;; Two-arg if: (if test then) -> (if test then nil)
            ((and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
             (list 'if-expr
                   (compile-expr (car (cdr expr)) env fenv)
                   (compile-expr (car (cdr (cdr expr))) env fenv)
                   (list 'lit 0)))
            (t (list 'lit 0))))

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

         ;; Let* binding (sequential) - transform to nested lets
         ((eq op 'let*)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((bindings (car (cdr expr)))
                    (body (car (cdr (cdr expr)))))
                (if (nil? bindings)
                    (compile-expr body env fenv)
                    ;; Transform to nested lets
                    (compile-expr
                     (list 'let (list (car bindings))
                           (list 'let* (cdr bindings) body))
                     env fenv)))
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

         ;; Setf (generalized assignment)
         ((eq op 'setf)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((place (car (cdr expr)))
                    (val-expr (car (cdr (cdr expr)))))
                (cond
                  ;; Simple variable: same as setq
                  ((symbol? place)
                   (let ((offset (env-lookup place env)))
                     (if offset
                         (list 'set-var offset (compile-expr val-expr env fenv))
                         (list 'lit 0))))
                  ;; (setf (car x) val) -> setcar
                  ((and (cons? place) (eq (car place) 'car) (cons? (cdr place)))
                   (list 'setcar-call
                         (compile-expr (car (cdr place)) env fenv)
                         (compile-expr val-expr env fenv)))
                  ;; (setf (cdr x) val) -> setcdr
                  ((and (cons? place) (eq (car place) 'cdr) (cons? (cdr place)))
                   (list 'setcdr-call
                         (compile-expr (car (cdr place)) env fenv)
                         (compile-expr val-expr env fenv)))
                  (t (list 'lit 0))))
              (list 'lit 0)))

         ;; Incf (increment in place)
         ((eq op 'incf)
          (if (cons? (cdr expr))
              (let* ((place (car (cdr expr)))
                     (delta (if (cons? (cdr (cdr expr))) (car (cdr (cdr expr))) 1)))
                (if (symbol? place)
                    (compile-expr (list 'setq place (list '+ place delta)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Decf (decrement in place)
         ((eq op 'decf)
          (if (cons? (cdr expr))
              (let* ((place (car (cdr expr)))
                     (delta (if (cons? (cdr (cdr expr))) (car (cdr (cdr expr))) 1)))
                (if (symbol? place)
                    (compile-expr (list 'setq place (list '- place delta)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Push (add to front of list variable)
         ((eq op 'push)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((item (car (cdr expr)))
                    (place (car (cdr (cdr expr)))))
                (if (symbol? place)
                    (compile-expr (list 'setq place (list 'cons item place)) env fenv)
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

         ;; Not (logical negation)
         ((eq op 'not)
          (if (cons? (cdr expr))
              (list 'if-expr
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 0)
                    (list 'lit 1))
              (list 'lit 1)))

         ;; Cond (multi-way conditional)
         ((eq op 'cond)
          (labels ((expand-cond (clauses)
                     (if (nil? clauses)
                         (list 'lit 0)
                         (let* ((clause (car clauses))
                                (test (car clause))
                                (body (cdr clause)))
                           (cond
                             ;; (t body...) - default clause
                             ((eq test t)
                              (if body
                                  (if (cdr body)
                                      (compile-expr (cons 'progn body) env fenv)
                                      (compile-expr (car body) env fenv))
                                  (list 'lit 1)))
                             ;; Empty body: return test value if true
                             ((nil? body)
                              (let ((test-ir (compile-expr test env fenv)))
                                (list 'if-expr test-ir test-ir (expand-cond (cdr clauses)))))
                             ;; Normal clause with body
                             (t
                              (list 'if-expr
                                    (compile-expr test env fenv)
                                    (if (cdr body)
                                        (compile-expr (cons 'progn body) env fenv)
                                        (compile-expr (car body) env fenv))
                                    (expand-cond (cdr clauses)))))))))
            (expand-cond (cdr expr))))

         ;; When (guard form)
         ((eq op 'when)
          (if (cons? (cdr expr))
              (let ((test (car (cdr expr)))
                    (body (cdr (cdr expr))))
                (list 'if-expr
                      (compile-expr test env fenv)
                      (if body
                          (if (cdr body)
                              (compile-expr (cons 'progn body) env fenv)
                              (compile-expr (car body) env fenv))
                          (list 'lit 0))
                      (list 'lit 0)))
              (list 'lit 0)))

         ;; Unless (negated guard form)
         ((eq op 'unless)
          (if (cons? (cdr expr))
              (let ((test (car (cdr expr)))
                    (body (cdr (cdr expr))))
                (list 'if-expr
                      (compile-expr test env fenv)
                      (list 'lit 0)
                      (if body
                          (if (cdr body)
                              (compile-expr (cons 'progn body) env fenv)
                              (compile-expr (car body) env fenv))
                          (list 'lit 0))))
              (list 'lit 0)))

         ;; And (short-circuit conjunction)
         ((eq op 'and)
          (let ((args (cdr expr)))
            (cond
              ((nil? args) (list 'lit 1))
              ((nil? (cdr args)) (compile-expr (car args) env fenv))
              (t (list 'if-expr
                       (compile-expr (car args) env fenv)
                       (compile-expr (cons 'and (cdr args)) env fenv)
                       (list 'lit 0))))))

         ;; Or (short-circuit disjunction)
         ((eq op 'or)
          (let ((args (cdr expr)))
            (cond
              ((nil? args) (list 'lit 0))
              ((nil? (cdr args)) (compile-expr (car args) env fenv))
              (t (let ((first-ir (compile-expr (car args) env fenv)))
                   (list 'if-expr
                         first-ir
                         first-ir
                         (compile-expr (cons 'or (cdr args)) env fenv)))))))

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

         ;; Cadr (car of cdr)
         ((eq op 'cadr)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'cdr-call (compile-expr (car (cdr expr)) env fenv)))
              (list 'lit 0)))

         ;; Caddr (car of cdr of cdr)
         ((eq op 'caddr)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (car (cdr expr)) env fenv))))
              (list 'lit 0)))

         ;; Cadddr (car of cdr of cdr of cdr)
         ((eq op 'cadddr)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (car (cdr expr)) env fenv)))))
              (list 'lit 0)))

         ;; Cddr (cdr of cdr)
         ((eq op 'cddr)
          (if (cons? (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call (compile-expr (car (cdr expr)) env fenv)))
              (list 'lit 0)))

         ;; Cdddr (cdr of cdr of cdr)
         ((eq op 'cdddr)
          (if (cons? (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (car (cdr expr)) env fenv))))
              (list 'lit 0)))

         ;; Cddddr (cdr of cdr of cdr of cdr)
         ((eq op 'cddddr)
          (if (cons? (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (car (cdr expr)) env fenv)))))
              (list 'lit 0)))

         ;; Caar (car of car)
         ((eq op 'caar)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'car-call (compile-expr (car (cdr expr)) env fenv)))
              (list 'lit 0)))

         ;; Cdar (cdr of car)
         ((eq op 'cdar)
          (if (cons? (cdr expr))
              (list 'cdr-call
                    (list 'car-call (compile-expr (car (cdr expr)) env fenv)))
              (list 'lit 0)))

         ;; First (same as car)
         ((eq op 'first)
          (if (cons? (cdr expr))
              (list 'car-call (compile-expr (car (cdr expr)) env fenv))
              (list 'lit 0)))

         ;; Second (same as cadr)
         ((eq op 'second)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'cdr-call (compile-expr (car (cdr expr)) env fenv)))
              (list 'lit 0)))

         ;; Third (same as caddr)
         ((eq op 'third)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (car (cdr expr)) env fenv))))
              (list 'lit 0)))

         ;; Fourth
         ((eq op 'fourth)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (car (cdr expr)) env fenv)))))
              (list 'lit 0)))

         ;; Fifth
         ((eq op 'fifth)
          (if (cons? (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call
                                      (list 'cdr-call (compile-expr (car (cdr expr)) env fenv))))))
              (list 'lit 0)))

         ;; Rest (same as cdr)
         ((eq op 'rest)
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

         ;; List* (last arg is tail)
         ((eq op 'list*)
          (let ((args (cdr expr)))
            (cond
              ((nil? args) (list 'lit 0))
              ((nil? (cdr args)) (compile-expr (car args) env fenv))
              (t (labels ((build-list* (items)
                            (if (nil? (cdr items))
                                (compile-expr (car items) env fenv)
                                (list 'cons-call
                                      (compile-expr (car items) env fenv)
                                      (build-list* (cdr items))))))
                   (build-list* args))))))

         ;; Acons (add to front of alist)
         ((eq op 'acons)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))) (cons? (cdr (cdr (cdr expr)))))
              (list 'cons-call
                    (list 'cons-call
                          (compile-expr (car (cdr expr)) env fenv)
                          (compile-expr (car (cdr (cdr expr))) env fenv))
                    (compile-expr (car (cdr (cdr (cdr expr)))) env fenv))
              (list 'lit 0)))

         ;; 1+ (increment by 1)
         ((eq op '1+)
          (if (cons? (cdr expr))
              (list 'add
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 1))
              (list 'lit 0)))

         ;; 1- (decrement by 1)
         ((eq op '1-)
          (if (cons? (cdr expr))
              (list 'sub
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 1))
              (list 'lit 0)))

         ;; Abs (absolute value)
         ((eq op 'abs)
          (if (cons? (cdr expr))
              (let ((arg-ir (compile-expr (car (cdr expr)) env fenv)))
                (list 'if-expr
                      (list 'cmp-lt arg-ir (list 'lit 0))
                      (list 'sub (list 'lit 0) arg-ir)
                      arg-ir))
              (list 'lit 0)))

         ;; Max (maximum of two values)
         ((eq op 'max)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((a-ir (compile-expr (car (cdr expr)) env fenv))
                    (b-ir (compile-expr (car (cdr (cdr expr))) env fenv)))
                (list 'if-expr
                      (list 'cmp-gt a-ir b-ir)
                      a-ir
                      b-ir))
              (if (cons? (cdr expr))
                  (compile-expr (car (cdr expr)) env fenv)
                  (list 'lit 0))))

         ;; Min (minimum of two values)
         ((eq op 'min)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((a-ir (compile-expr (car (cdr expr)) env fenv))
                    (b-ir (compile-expr (car (cdr (cdr expr))) env fenv)))
                (list 'if-expr
                      (list 'cmp-lt a-ir b-ir)
                      a-ir
                      b-ir))
              (if (cons? (cdr expr))
                  (compile-expr (car (cdr expr)) env fenv)
                  (list 'lit 0))))

         ;; Null predicate (also nil?)
         ((or (eq op 'null) (op= op "NIL?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Consp predicate (tag #x1) (also cons?)
         ((or (eq op 'consp) (op= op "CONS?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 1))
              (list 'lit 0)))

         ;; Atom predicate (not consp)
         ((eq op 'atom)
          (if (cons? (cdr expr))
              (list 'if-expr
                    (list 'cmp-eq
                          (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                          (list 'lit 1))
                    (list 'lit 0)
                    (list 'lit 1))
              (list 'lit 1)))

         ;; Numberp predicate (tag #x0 = fixnum) (also fixnum?)
         ((or (eq op 'numberp) (op= op "FIXNUM?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Symbolp predicate (tag #x2) (also symbol?)
         ((or (eq op 'symbolp) (op= op "SYMBOL?"))
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 2))
              (list 'lit 0)))

         ;; Stringp predicate (tag #x4)
         ((eq op 'stringp)
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 4))
              (list 'lit 0)))

         ;; Vectorp predicate (tag #x3)
         ((eq op 'vectorp)
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 3))
              (list 'lit 0)))

         ;; Functionp predicate (tag #x5 = closure)
         ((eq op 'functionp)
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (car (cdr expr)) env fenv))
                    (list 'lit 5))
              (list 'lit 0)))

         ;; Listp predicate (null or cons)
         ((eq op 'listp)
          (if (cons? (cdr expr))
              (let ((arg-ir (compile-expr (car (cdr expr)) env fenv)))
                (list 'if-expr
                      (list 'cmp-eq arg-ir (list 'lit 0))
                      (list 'lit 1)
                      (list 'cmp-eq
                            (list 'get-tag arg-ir)
                            (list 'lit 1))))
              (list 'lit 0)))

         ;; Zerop predicate
         ((eq op 'zerop)
          (if (cons? (cdr expr))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Plusp predicate (> 0)
         ((eq op 'plusp)
          (if (cons? (cdr expr))
              (list 'cmp-gt
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Minusp predicate (< 0)
         ((eq op 'minusp)
          (if (cons? (cdr expr))
              (list 'cmp-lt
                    (compile-expr (car (cdr expr)) env fenv)
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Eq (pointer equality)
         ((eq op 'eq)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Eql (same as eq for fixnums, symbols, chars)
         ((eq op 'eql)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (list 'cmp-eq
                    (compile-expr (car (cdr expr)) env fenv)
                    (compile-expr (car (cdr (cdr expr))) env fenv))
              (list 'lit 0)))

         ;; Identity function
         ((eq op 'identity)
          (if (cons? (cdr expr))
              (compile-expr (car (cdr expr)) env fenv)
              (list 'lit 0)))

         ;; Constantly - returns arg unchanged (simplified)
         ((eq op 'constantly)
          (if (cons? (cdr expr))
              (compile-expr (car (cdr expr)) env fenv)
              (list 'lit 0)))

         ;; Length - transform to labels
         ((eq op 'length)
          (if (cons? (cdr expr))
              (let ((lst-arg (car (cdr expr))))
                (compile-expr
                 (list 'labels (list (list 'len-iter (list 'lst 'acc)
                                           (list 'if (list 'null 'lst)
                                                 'acc
                                                 (list 'len-iter (list 'cdr 'lst) (list '+ 'acc 1)))))
                       (list 'len-iter lst-arg 0))
                 env fenv))
              (list 'lit 0)))

         ;; Append - transform to labels
         ((eq op 'append)
          (cond
            ((nil? (cdr expr)) (list 'lit 0))
            ((nil? (cdr (cdr expr))) (compile-expr (car (cdr expr)) env fenv))
            ((nil? (cdr (cdr (cdr expr))))
             (let ((lst1 (car (cdr expr)))
                   (lst2 (car (cdr (cdr expr)))))
               (compile-expr
                (list 'labels (list (list 'app (list 'xs 'ys)
                                          (list 'if (list 'null 'xs)
                                                'ys
                                                (list 'cons (list 'car 'xs) (list 'app (list 'cdr 'xs) 'ys)))))
                      (list 'app lst1 lst2))
                env fenv)))
            (t (compile-expr
                (list 'append (car (cdr expr)) (cons 'append (cdr (cdr expr))))
                env fenv))))

         ;; Reverse - transform to labels
         ((eq op 'reverse)
          (if (cons? (cdr expr))
              (let ((lst (car (cdr expr))))
                (compile-expr
                 (list 'labels (list (list 'rev (list 'xs 'acc)
                                           (list 'if (list 'null 'xs)
                                                 'acc
                                                 (list 'rev (list 'cdr 'xs) (list 'cons (list 'car 'xs) 'acc)))))
                       (list 'rev lst 0))
                 env fenv))
              (list 'lit 0)))

         ;; Assoc - transform to labels
         ((eq op 'assoc)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((key (car (cdr expr)))
                    (alist (car (cdr (cdr expr)))))
                (compile-expr
                 (list 'labels (list (list 'assoc-iter (list 'k 'al)
                                           (list 'if (list 'null 'al)
                                                 0
                                                 (list 'if (list 'eq 'k (list 'car (list 'car 'al)))
                                                       (list 'car 'al)
                                                       (list 'assoc-iter 'k (list 'cdr 'al))))))
                       (list 'assoc-iter key alist))
                 env fenv))
              (list 'lit 0)))

         ;; Member - transform to labels
         ((eq op 'member)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((item (car (cdr expr)))
                    (lst (car (cdr (cdr expr)))))
                (compile-expr
                 (list 'labels (list (list 'member-iter (list 'x 'xs)
                                           (list 'if (list 'null 'xs)
                                                 0
                                                 (list 'if (list 'eq 'x (list 'car 'xs))
                                                       'xs
                                                       (list 'member-iter 'x (list 'cdr 'xs))))))
                       (list 'member-iter item lst))
                 env fenv))
              (list 'lit 0)))

         ;; Labels (local recursive functions)
         ;; Transform using cons cells as mutable boxes:
         ;; (labels ((f (x) body)) expr) ->
         ;; (let* ((f-box (cons nil nil)))
         ;;   (progn (setf (car f-box) (lambda (x) body'))
         ;;          expr'))
         ;; where body' and expr' transform (f args...) to (funcall (car f-box) args...)
         ((eq op 'labels)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let* ((fn-defs (car (cdr expr)))
                     (body (car (cdr (cdr expr))))
                     (fn-names (mapcar (lambda (def) (car def)) fn-defs)))
                ;; Build box-map: ((f . f-box) ...)
                ;; Use counter-based unique names
                (labels ((make-box-map (names acc)
                           (if (nil? names)
                               (reverse acc)
                               (let ((box-name (make-unique-name (car names))))
                                 (make-box-map (cdr names)
                                               (cons (cons (car names) box-name) acc))))))
                  (let* ((box-map (make-box-map fn-names nil))
                         (box-names (mapcar (lambda (entry) (cdr entry)) box-map))
                         ;; Build let* bindings: ((f-box (cons nil nil)) ...)
                         (let-bindings (mapcar (lambda (box) (list box (list 'cons 0 0))) box-names))
                         ;; Build setf forms: (setf (car f-box) (lambda (params) body'))
                         (setf-forms (mapcar (lambda (def)
                                               (let* ((name (car def))
                                                      (params (car (cdr def)))
                                                      (fn-body (car (cdr (cdr def))))
                                                      (box (cdr (assoc name box-map)))
                                                      (xformed-body (transform-local-calls fn-body fn-names box-map)))
                                                 (list 'setf (list 'car box) (list 'lambda params xformed-body))))
                                             fn-defs))
                         ;; Transform body
                         (xformed-body (transform-local-calls body fn-names box-map))
                         ;; Build full expression
                         (transformed (list 'let* let-bindings
                                            (cons 'progn (append setf-forms (list xformed-body))))))
                    (compile-expr transformed env fenv))))
              (list 'lit 0)))

         ;; Flet (local non-recursive functions)
         ;; Transform: (flet ((f (x) body)) expr) ->
         ;;            (let ((f (lambda (x) body))) expr')
         ;; where expr' has calls (f ...) transformed to (funcall f ...)
         ((eq op 'flet)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let* ((fn-defs (car (cdr expr)))
                     (body (car (cdr (cdr expr))))
                     (fn-names (mapcar (lambda (def) (car def)) fn-defs))
                     ;; Build let bindings with lambdas
                     (let-bindings (mapcar (lambda (def)
                                             (let* ((name (car def))
                                                    (params (car (cdr def)))
                                                    (fn-body (car (cdr (cdr def)))))
                                               (list name (list 'lambda params fn-body))))
                                           fn-defs))
                     ;; Transform body: (f args) -> (funcall f args)
                     (xformed-body (labels ((xform (x)
                                              (cond
                                                ((not (cons? x)) x)
                                                ((and (symbol? (car x)) (member (car x) fn-names))
                                                 (cons 'funcall (cons (car x) (mapcar (lambda (a) (xform a)) (cdr x)))))
                                                (t (mapcar (lambda (a) (xform a)) x)))))
                                     (xform body)))
                     (transformed (list 'let let-bindings xformed-body)))
                (compile-expr transformed env fenv))
              (list 'lit 0)))

         ;; Lambda - compile to lambda-ref (collected for later codegen)
         ;; For now, simple version without closure capture analysis
         ((eq op 'lambda)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let* ((params (car (cdr expr)))
                     (body (car (cdr (cdr expr))))
                     ;; Build parameter environment
                     (bindings (mapcar (lambda (p) (list p)) params))
                     (param-env (env-extend bindings env))
                     ;; Compile body
                     (body-ir (compile-expr body param-env fenv)))
                ;; Return lambda IR node
                (list 'lambda-ir params body-ir))
              (list 'lit 0)))

         ;; Dotimes - (dotimes (i n [result]) body...)
         ((eq op 'dotimes)
          (if (and (cons? (cdr expr)) (cons? (car (cdr expr))))
              (let* ((var-form (car (cdr expr)))
                     (var (car var-form))
                     (count-form (car (cdr var-form)))
                     (result-form (if (cons? (cdr (cdr var-form))) (car (cdr (cdr var-form))) 0))
                     (body (cdr (cdr expr)))
                     (body-expr (if (nil? (cdr body)) (car body) (cons 'progn body))))
                (compile-expr
                 (list 'labels (list (list 'dotimes-iter (list var 'limit)
                                           (list 'if (list '>= var 'limit)
                                                 result-form
                                                 (list 'progn
                                                       body-expr
                                                       (list 'dotimes-iter (list '+ var 1) 'limit)))))
                       (list 'dotimes-iter 0 count-form))
                 env fenv))
              (list 'lit 0)))

         ;; Dolist - (dolist (x list [result]) body...)
         ((eq op 'dolist)
          (if (and (cons? (cdr expr)) (cons? (car (cdr expr))))
              (let* ((var-form (car (cdr expr)))
                     (var (car var-form))
                     (list-form (car (cdr var-form)))
                     (result-form (if (cons? (cdr (cdr var-form))) (car (cdr (cdr var-form))) 0))
                     (body (cdr (cdr expr)))
                     (body-expr (if (nil? (cdr body)) (car body) (cons 'progn body))))
                (compile-expr
                 (list 'labels (list (list 'dolist-iter (list 'remaining)
                                           (list 'if (list 'null 'remaining)
                                                 result-form
                                                 (list 'let (list (list var (list 'car 'remaining)))
                                                       (list 'progn
                                                             body-expr
                                                             (list 'dolist-iter (list 'cdr 'remaining)))))))
                       (list 'dolist-iter list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Mapcar - (mapcar fn list)
         ((eq op 'mapcar)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((fn-form (car (cdr expr)))
                    (list-form (car (cdr (cdr expr)))))
                (compile-expr
                 (list 'labels (list (list 'mapcar-iter (list 'fn 'lst)
                                           (list 'if (list 'null 'lst)
                                                 0
                                                 (list 'cons (list 'funcall 'fn (list 'car 'lst))
                                                       (list 'mapcar-iter 'fn (list 'cdr 'lst))))))
                       (list 'mapcar-iter fn-form list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Mapc - (mapc fn list) - returns original list
         ((eq op 'mapc)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((fn-form (car (cdr expr)))
                    (list-form (car (cdr (cdr expr)))))
                (compile-expr
                 (list 'labels (list (list 'mapc-iter (list 'fn 'lst 'orig)
                                           (list 'if (list 'null 'lst)
                                                 'orig
                                                 (list 'progn
                                                       (list 'funcall 'fn (list 'car 'lst))
                                                       (list 'mapc-iter 'fn (list 'cdr 'lst) 'orig)))))
                       (list 'let (list (list 'the-list list-form))
                             (list 'mapc-iter fn-form 'the-list 'the-list)))
                 env fenv))
              (list 'lit 0)))

         ;; Reduce - (reduce fn list &optional init)
         ((eq op 'reduce)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let* ((fn-form (car (cdr expr)))
                     (list-form (car (cdr (cdr expr))))
                     (init-form (if (cons? (cdr (cdr (cdr expr)))) (car (cdr (cdr (cdr expr)))) nil)))
                (if init-form
                    ;; With initial value
                    (compile-expr
                     (list 'labels (list (list 'reduce-iter (list 'fn 'lst 'acc)
                                               (list 'if (list 'null 'lst)
                                                     'acc
                                                     (list 'reduce-iter 'fn (list 'cdr 'lst)
                                                           (list 'funcall 'fn 'acc (list 'car 'lst))))))
                           (list 'reduce-iter fn-form list-form init-form))
                     env fenv)
                    ;; Without initial value
                    (compile-expr
                     (list 'labels (list (list 'reduce-iter (list 'fn 'lst 'acc)
                                               (list 'if (list 'null 'lst)
                                                     'acc
                                                     (list 'reduce-iter 'fn (list 'cdr 'lst)
                                                           (list 'funcall 'fn 'acc (list 'car 'lst))))))
                           (list 'let (list (list 'the-list list-form))
                                 (list 'if (list 'null 'the-list)
                                       0
                                       (list 'reduce-iter fn-form (list 'cdr 'the-list) (list 'car 'the-list)))))
                     env fenv)))
              (list 'lit 0)))

         ;; Apply - (apply fn args-list)
         ;; Optimized for common patterns: #'append, #'max
         ((eq op 'apply)
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((fn-form (car (cdr expr)))
                    (args-form (car (cdr (cdr expr)))))
                (cond
                  ;; (apply #'append list-of-lists)
                  ((and (cons? fn-form)
                        (or (eq (car fn-form) 'function) (eq (car fn-form) 'quote))
                        (eq (car (cdr fn-form)) 'append))
                   (compile-expr
                    (list 'labels (list (list 'apply-append (list 'lists 'acc)
                                              (list 'if (list 'null 'lists)
                                                    'acc
                                                    (list 'apply-append (list 'cdr 'lists)
                                                          (list 'append 'acc (list 'car 'lists))))))
                          (list 'apply-append args-form 0))
                    env fenv))
                  ;; (apply #'max list-of-numbers)
                  ((and (cons? fn-form)
                        (or (eq (car fn-form) 'function) (eq (car fn-form) 'quote))
                        (eq (car (cdr fn-form)) 'max))
                   (compile-expr
                    (list 'labels (list (list 'apply-max (list 'lst 'best)
                                              (list 'if (list 'null 'lst)
                                                    'best
                                                    (list 'let (list (list 'el (list 'car 'lst)))
                                                          (list 'apply-max (list 'cdr 'lst)
                                                                (list 'if (list '> 'el 'best) 'el 'best))))))
                          (list 'let (list (list 'the-list args-form))
                                (list 'if (list 'null 'the-list)
                                      0
                                      (list 'apply-max (list 'cdr 'the-list) (list 'car 'the-list)))))
                    env fenv))
                  ;; General apply - dispatch based on length
                  (t
                   (compile-expr
                    (list 'let (list (list 'fn fn-form) (list 'args args-form))
                          (list 'let (list (list 'len (list 'length 'args)))
                                (list 'cond
                                      (list (list '= 'len 0) (list 'funcall 'fn))
                                      (list (list '= 'len 1) (list 'funcall 'fn (list 'car 'args)))
                                      (list (list '= 'len 2) (list 'funcall 'fn (list 'car 'args) (list 'cadr 'args)))
                                      (list (list '= 'len 3) (list 'funcall 'fn (list 'car 'args) (list 'cadr 'args) (list 'caddr 'args)))
                                      (list t (list 'funcall 'fn (list 'car 'args) (list 'cadr 'args) (list 'caddr 'args) (list 'cadddr 'args))))))
                    env fenv))))
              (list 'lit 0)))

         ;; Loop macro - subset for common patterns
         ((eq op 'loop)
          (let ((clauses (cdr expr)))
            (cond
              ;; (loop for var in list collect expr)
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (car (cdr (cdr clauses))) "IN")
                    (op= (car (cdr (cdr (cdr (cdr clauses))))) "COLLECT"))
               (let* ((var (car (cdr clauses)))
                      (list-expr (car (cdr (cdr (cdr clauses)))))
                      (collect-expr (car (cdr (cdr (cdr (cdr (cdr clauses))))))))
                 (compile-expr
                  (list 'labels (list (list 'loop-iter (list 'lst 'acc)
                                            (list 'if (list 'null 'lst)
                                                  (list 'reverse 'acc)
                                                  (list 'let (list (list var (list 'car 'lst)))
                                                        (list 'loop-iter (list 'cdr 'lst)
                                                              (list 'cons collect-expr 'acc))))))
                        (list 'loop-iter list-expr 0))
                  env fenv)))

              ;; (loop for var from start below end collect expr)
              ((and (>= (length clauses) 7)
                    (op= (car clauses) "FOR")
                    (op= (car (cdr (cdr clauses))) "FROM")
                    (op= (car (cdr (cdr (cdr (cdr clauses))))) "BELOW")
                    (op= (car (cdr (cdr (cdr (cdr (cdr clauses)))))) "COLLECT"))
               (let ((var (car (cdr clauses)))
                     (start (car (cdr (cdr (cdr clauses)))))
                     (end (car (cdr (cdr (cdr (cdr (cdr clauses)))))))
                     (collect-expr (car (cdr (cdr (cdr (cdr (cdr (cdr clauses)))))))))
                 (compile-expr
                  (list 'labels (list (list 'loop-iter (list var 'acc)
                                            (list 'if (list '>= var end)
                                                  (list 'reverse 'acc)
                                                  (list 'loop-iter (list '+ var 1)
                                                        (list 'cons collect-expr 'acc)))))
                        (list 'loop-iter start 0))
                  env fenv)))

              ;; (loop until condition do body)
              ((and (>= (length clauses) 3)
                    (op= (car clauses) "UNTIL")
                    (op= (car (cdr (cdr clauses))) "DO"))
               (let ((condition (car (cdr clauses)))
                     (do-expr (car (cdr (cdr (cdr clauses))))))
                 (compile-expr
                  (list 'labels (list (list 'loop-iter (list)
                                            (list 'if condition
                                                  0
                                                  (list 'progn
                                                        do-expr
                                                        (list 'loop-iter)))))
                        (list 'loop-iter))
                  env fenv)))

              ;; Unsupported loop pattern
              (t (list 'lit 0)))))

         ;; Error - stub (returns 0)
         ((op= op "ERROR")
          (list 'lit 0))

         ;; Remove-if
         ((op= op "REMOVE-IF")
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((pred (car (cdr expr)))
                    (lst (car (cdr (cdr expr)))))
                (compile-expr
                 (list 'labels (list (list 'filter-iter (list 'remaining 'acc)
                                           (list 'if (list 'null 'remaining)
                                                 (list 'reverse 'acc)
                                                 (list 'if (list 'funcall pred (list 'car 'remaining))
                                                       (list 'filter-iter (list 'cdr 'remaining) 'acc)
                                                       (list 'filter-iter (list 'cdr 'remaining)
                                                             (list 'cons (list 'car 'remaining) 'acc))))))
                       (list 'filter-iter lst 0))
                 env fenv))
              (list 'lit 0)))

         ;; Remove-if-not
         ((op= op "REMOVE-IF-NOT")
          (if (and (cons? (cdr expr)) (cons? (cdr (cdr expr))))
              (let ((pred (car (cdr expr)))
                    (lst (car (cdr (cdr expr)))))
                (compile-expr
                 (list 'labels (list (list 'filter-iter (list 'remaining 'acc)
                                           (list 'if (list 'null 'remaining)
                                                 (list 'reverse 'acc)
                                                 (list 'if (list 'funcall pred (list 'car 'remaining))
                                                       (list 'filter-iter (list 'cdr 'remaining)
                                                             (list 'cons (list 'car 'remaining) 'acc))
                                                       (list 'filter-iter (list 'cdr 'remaining) 'acc)))))
                       (list 'filter-iter lst 0))
                 env fenv))
              (list 'lit 0)))

         ;; Remove-duplicates
         ((op= op "REMOVE-DUPLICATES")
          (if (cons? (cdr expr))
              (let ((lst (car (cdr expr))))
                (compile-expr
                 (list 'labels (list (list 'dedup-iter (list 'remaining 'seen)
                                           (list 'if (list 'null 'remaining)
                                                 (list 'reverse 'seen)
                                                 (list 'let (list (list 'el (list 'car 'remaining)))
                                                       (list 'if (list 'member 'el 'seen)
                                                             (list 'dedup-iter (list 'cdr 'remaining) 'seen)
                                                             (list 'dedup-iter (list 'cdr 'remaining)
                                                                   (list 'cons 'el 'seen)))))))
                       (list 'dedup-iter lst 0))
                 env fenv))
              (list 'lit 0)))

         ;; Funcall: call closure value
         ((eq op 'funcall)
          (let ((fn-expr (car (cdr expr)))
                (args (cdr (cdr expr))))
            (list 'call-closure
                  (compile-expr fn-expr env fenv)
                  (mapcar (lambda (arg) (compile-expr arg env fenv)) args))))

         ;; Inline lambda application: ((lambda (...) ...) args...)
         ((cons? op)
          (let ((fn (compile-expr op env fenv))
                (args (mapcar (lambda (arg) (compile-expr arg env fenv)) (cdr expr))))
            (list 'call-closure fn args)))

         ;; Function call - check if user-defined
         (t
          (if (and fenv (assoc op fenv))
              (let ((args (cdr expr)))
                (list 'call-fn op
                      (mapcar (lambda (arg) (compile-expr arg env fenv)) args)))
              (list 'lit 0))))))

    ;; Unknown
    (t (list 'lit 0))))
