;;;; Habu Bootstrap Compiler
;;;; Compiles Habu Lisp to native x86_64 and ARM64 machine code

(defpackage :habu-compiler
  (:use :cl)
  (:export #:compile-expression
           #:compile-to-binary
           #:*target-arch*))

(in-package :habu-compiler)

;;; Target architecture (x86_64 or arm64)
(defvar *target-arch* :x86_64)

;;; Global function table for defun
(defvar *function-table* (make-hash-table :test 'eq))

;;; Global macro table for defmacro
(defvar *macro-table* (make-hash-table :test 'eq))

;;; Compiler intermediate representation
(defstruct expr
  type
  value
  args)

;;; Quasiquote expansion
(defun expand-quasiquote (form)
  "Expand quasiquote (backquote) forms with unquote and unquote-splicing"
  (cond
    ;; Unquote: (unquote x) => x
    ((and (consp form) (eq (first form) 'unquote))
     (second form))

    ;; Atom: just quote it
    ((atom form)
     `(quote ,form))

    ;; List starting with unquote-splicing in car position - error
    ((and (consp (first form)) (eq (first (first form)) 'unquote-splicing))
     (error "Unquote-splicing ,@~S in illegal position" (second (first form))))

    ;; List: process each element
    (t
     (expand-quasiquote-list form))))

(defun expand-quasiquote-list (forms)
  "Expand a list within quasiquote, handling unquote-splicing"
  (cond
    ;; Empty list
    ((null forms)
     '(quote ()))

    ;; Car is (unquote-splicing x): splice x into the list
    ((and (consp (first forms))
          (eq (first (first forms)) 'unquote-splicing))
     (let ((splicee (second (first forms)))
           (rest-expansion (expand-quasiquote-list (rest forms))))
       `(append ,splicee ,rest-expansion)))

    ;; Car is (unquote x): cons x onto the rest
    ((and (consp (first forms))
          (eq (first (first forms)) 'unquote))
     (let ((element (second (first forms)))
           (rest-expansion (expand-quasiquote-list (rest forms))))
       `(cons ,element ,rest-expansion)))

    ;; Recursively process car and cdr
    (t
     (let ((car-expansion (expand-quasiquote (first forms)))
           (cdr-expansion (expand-quasiquote-list (rest forms))))
       `(cons ,car-expansion ,cdr-expansion)))))

;;; Parse Lisp expression to IR
(defun parse (form)
  "Parse a Lisp form into compiler IR"
  (cond
    ((integerp form)
     (make-expr :type 'fixnum :value form))

    ((symbolp form)
     (make-expr :type 'variable :value form))

    ((and (consp form) (eq (first form) 'if))
     ;; Special form: (if condition then-expr else-expr)
     (let ((condition (second form))
           (then-expr (third form))
           (else-expr (fourth form)))
       (make-expr :type 'if
                  :value nil
                  :args (list (parse condition)
                              (parse then-expr)
                              (parse else-expr)))))

    ((and (consp form) (eq (first form) 'let))
     ;; Special form: (let ((var1 val1) (var2 val2) ...) body)
     ;; OR named-let: (let name ((var1 val1) ...) body) for recursion
     (let ((second-elem (second form)))
       (if (symbolp second-elem)
           ;; Named-let for recursion: (let name ((x 1) (y 2)) body)
           ;; Transform to: ((lambda (name) (name name init1 init2...))
           ;;                (lambda (name x y) body-with-name-calls))
           (let* ((name second-elem)
                  (bindings (third form))
                  (body (fourth form))
                  (vars (mapcar #'first bindings))
                  (inits (mapcar #'second bindings)))
             ;; Replace recursive calls (name ...) with (name name ...)
             (labels ((transform-recursive-calls (expr)
                        (cond
                          ((atom expr) expr)
                          ((and (consp expr) (eq (first expr) name))
                           ;; Recursive call: add name as first argument
                           `(,name ,name ,@(mapcar #'transform-recursive-calls (rest expr))))
                          (t
                           (mapcar #'transform-recursive-calls expr)))))
               (let ((transformed-body (transform-recursive-calls body)))
                 (parse `((lambda (,name)
                            (,name ,name ,@inits))
                          (lambda (,name ,@vars)
                            ,transformed-body))))))
           ;; Regular let
           (make-expr :type 'let
                      :value second-elem  ; bindings
                      :args (list (parse (third form)))))))

    ((and (consp form) (eq (first form) 'let*))
     ;; Special form: (let* ((var1 val1) (var2 val2) ...) body)
     ;; Sequential bindings - transform to nested lets
     (let ((bindings (second form))
           (body (third form)))
       (if (null bindings)
           ;; No bindings: (let* () body) -> body
           (parse body)
           ;; Transform to nested lets: (let* ((x 1) (y 2)) body) -> (let ((x 1)) (let* ((y 2)) body))
           (if (= (length bindings) 1)
               ;; Last binding: (let* ((x 1)) body) -> (let ((x 1)) body)
               (parse `(let (,(first bindings)) ,body))
               ;; Multiple bindings: recurse
               (parse `(let (,(first bindings))
                         (let* ,(rest bindings) ,body)))))))

    ((and (consp form) (eq (first form) 'lambda))
     ;; Special form: (lambda (params) body)
     (let ((params (second form))
           (body (third form)))
       (make-expr :type 'lambda
                  :value params  ; Parameter list
                  :args (list (parse body)))))

    ((and (consp form) (eq (first form) 'progn))
     ;; Special form: (progn expr1 expr2 ... exprN)
     (let ((exprs (rest form)))
       (make-expr :type 'progn
                  :value nil
                  :args (mapcar #'parse exprs))))

    ((and (consp form) (eq (first form) 'begin))
     ;; Scheme-style alias for progn
     (parse `(progn ,@(rest form))))

    ((and (consp form) (eq (first form) 'quote))
     ;; Special form: (quote datum)
     ;; Note: Don't recursively parse - keep quoted value as-is
     (let ((datum (second form)))
       (make-expr :type 'quote
                  :value datum
                  :args nil)))

    ((and (consp form) (eq (first form) 'quasiquote))
     ;; Special form: (quasiquote template)
     ;; Backquote ` - allows selective evaluation with unquote
     (parse (expand-quasiquote (second form))))

    ((and (consp form) (eq (first form) 'not))
     ;; Special form: (not expr) - logical not
     (let ((expr (second form)))
       (make-expr :type 'not
                  :value nil
                  :args (list (parse expr)))))

    ((and (consp form) (eq (first form) 'and))
     ;; Special form: (and expr1 expr2 ...) - short-circuit and
     (let ((exprs (rest form)))
       (make-expr :type 'and
                  :value nil
                  :args (mapcar #'parse exprs))))

    ((and (consp form) (eq (first form) 'or))
     ;; Special form: (or expr1 expr2 ...) - short-circuit or
     (let ((exprs (rest form)))
       (make-expr :type 'or
                  :value nil
                  :args (mapcar #'parse exprs))))

    ((and (consp form) (eq (first form) 'cond))
     ;; Special form: (cond (test1 result1) (test2 result2) ... (t default))
     (let ((clauses (rest form)))
       (make-expr :type 'cond
                  :value clauses  ; Store raw clauses (will parse during code gen)
                  :args nil)))

    ((and (consp form) (eq (first form) 'when))
     ;; Special form: (when test body...) => (if test (progn body...) 0)
     (let ((test (second form))
           (body (cddr form)))
       (parse `(if ,test (progn ,@body) 0))))

    ((and (consp form) (eq (first form) 'unless))
     ;; Special form: (unless test body...) => (if (not test) (progn body...) 0)
     (let ((test (second form))
           (body (cddr form)))
       (parse `(if (not ,test) (progn ,@body) 0))))

    ((and (consp form) (eq (first form) 'case))
     ;; Special form: (case key-form (value result) ... (t default))
     ;; Transform to (let ((#:g key-form)) (cond ((= #:g value) result) ... (t default)))
     (let* ((key-form (second form))
            (clauses (cddr form))
            (temp-var (gensym "CASE")))
       (parse `(let ((,temp-var ,key-form))
                 (cond ,@(mapcar (lambda (clause)
                                   (let ((keys (first clause))
                                         (result (second clause)))
                                     (if (or (eq keys t) (eq keys 'otherwise))
                                         `(t ,result)
                                         (if (consp keys)
                                             ;; Multiple keys: (or (= temp key1) (= temp key2) ...)
                                             `((or ,@(mapcar (lambda (k) `(= ,temp-var ,k)) keys))
                                               ,result)
                                             ;; Single key
                                             `((= ,temp-var ,keys) ,result)))))
                                 clauses))))))

    ((and (consp form) (eq (first form) 'defun))
     ;; Special form: (defun name (params) body)
     ;; Store function definition in global table
     (let ((name (second form))
           (params (third form))
           (body (fourth form)))
       (setf (gethash name *function-table*) (cons params body))
       ;; Return 0 as a placeholder (defun doesn't produce a meaningful value in our compiler)
       (make-expr :type 'fixnum :value 0)))

    ((and (consp form) (eq (first form) 'defmacro))
     ;; Special form: (defmacro name (params) body)
     ;; Store macro definition in global table
     ;; Macros are expanded at compile-time, not runtime
     (let ((name (second form))
           (params (third form))
           (body (fourth form)))
       (setf (gethash name *macro-table*) (cons params body))
       ;; Return 0 as a placeholder
       (make-expr :type 'fixnum :value 0)))

    ((and (consp form) (eq (first form) 'setq))
     ;; Special form: (setq var value)
     ;; Mutate a lexical variable
     (let ((var (second form))
           (value (third form)))
       (make-expr :type 'setq
                  :value var  ; Variable name
                  :args (list (parse value)))))  ; Value expression

    ((and (consp form) (eq (first form) 'incf))
     ;; Macro: (incf var [delta]) -> (setq var (+ var delta))
     (let ((var (second form))
           (delta (if (third form) (third form) 1)))
       (parse `(setq ,var (+ ,var ,delta)))))

    ((and (consp form) (eq (first form) 'decf))
     ;; Macro: (decf var [delta]) -> (setq var (- var delta))
     (let ((var (second form))
           (delta (if (third form) (third form) 1)))
       (parse `(setq ,var (- ,var ,delta)))))

    ((and (consp form) (eq (first form) 'equal))
     ;; Alias for = (for compatibility)
     (parse `(= ,@(rest form))))

    ((and (consp form) (eq (first form) 'null))
     ;; Predicate: (null x) - check if x is 0/nil
     ;; Alias for zerop
     (parse `(zerop ,@(rest form))))

    ((and (consp form) (eq (first form) 'identity))
     ;; Function: (identity x) - returns its argument
     (parse (second form)))

    ((and (consp form) (consp (first form)))
     ;; Function call: ((lambda ...) args) or ((fn) args)
     (let ((fn (first form))
           (args (rest form)))
       (make-expr :type 'funcall
                  :value (parse fn)  ; The function expression
                  :args (mapcar #'parse args))))  ; The arguments

    ((and (consp form) (symbolp (first form)))
     (let ((op (first form))
           (args (rest form)))
       ;; Check if this is a macro first (macros expand at compile-time)
       (let ((macro-def (gethash op *macro-table*)))
         (if macro-def
             ;; Macro: expand and re-parse
             (let ((params (car macro-def))
                   (body (cdr macro-def)))
               ;; Create binding list for macro parameters
               (let ((bindings (mapcar #'list params args)))
                 ;; Expand macro body with substitutions
                 (let ((expanded (sublis (mapcar (lambda (b) (cons (first b) (second b))) bindings)
                                         body)))
                   ;; Re-parse the expanded form
                   (parse expanded))))
             ;; Not a macro, check if this is a user-defined function
             (let ((fn-def (gethash op *function-table*)))
               (if fn-def
                   ;; User-defined function: transform to ((lambda params body) args...)
                   (let ((params (car fn-def))
                         (body (cdr fn-def)))
                     (parse `((lambda ,params ,body) ,@args)))
                   ;; Primitive operator
                   (make-expr :type 'call
                              :value op
                              :args (mapcar #'parse args))))))))

    (t
     (error "Cannot parse form: ~S" form))))

;;; Code generation for x86_64
(defun emit-x86_64 (expr &optional (env nil))
  "Generate x86_64 machine code for expression with environment"
  (ecase (expr-type expr)
    (fixnum
     ;; Load fixnum into RAX
     ;; mov rax, imm64
     (let ((val (* (expr-value expr) 16))) ; Tag as fixnum (shift left 4)
       (append (list #x48 #xB8)           ; REX.W + mov rax prefix
               (int-to-bytes val 8))))

    (variable
     ;; Look up variable in environment and load from stack
     (let* ((var-name (expr-value expr))
            (binding (assoc var-name env)))
       (if binding
           (let ((offset (cdr binding)))
             ;; mov rax, [rsp + offset]
             (if (zerop offset)
                 (list #x48 #x8B #x04 #x24)  ; mov rax, [rsp]
                 (append (list #x48 #x8B #x84 #x24)  ; mov rax, [rsp + disp32]
                         (int-to-bytes offset 4))))
           (error "Unbound variable: ~S" var-name))))

    (setq
     ;; Compile (setq var value) - mutate a lexical variable
     (let* ((var-name (expr-value expr))
            (value-expr (first (expr-args expr)))
            (binding (assoc var-name env)))
       (if binding
           (let ((offset (cdr binding)))
             (append
              ;; First, evaluate the value expression into RAX
              (emit-x86_64 value-expr env)
              ;; Then store RAX to the variable's stack location
              (if (zerop offset)
                  (list #x48 #x89 #x04 #x24)  ; mov [rsp], rax
                  (append (list #x48 #x89 #x84 #x24)  ; mov [rsp + disp32], rax
                          (int-to-bytes offset 4)))))
           (error "Cannot setq unbound variable: ~S" var-name))))

    (let
     ;; Compile (let ((var val) ...) body)
     (let* ((bindings (expr-value expr))
            (body (first (expr-args expr)))
            (num-bindings (length bindings))
            (new-env env)
            (binding-code nil))
       ;; Generate code to evaluate and push each binding
       (loop for (var val-form) in bindings
             for offset from 0 by 8
             do (let ((val-code (emit-x86_64 (parse val-form) env)))
                  (setf binding-code
                        (append binding-code
                                val-code
                                (list #x50)))  ; push rax
                  ;; Add to environment with current stack offset
                  (push (cons var (* offset 8)) new-env)))
       ;; Generate code for body with extended environment
       (let ((body-code (emit-x86_64 body (reverse new-env))))
         (append binding-code
                 body-code
                 ;; Clean up stack: add rsp, num-bindings*8
                 (if (<= (* num-bindings 8) 127)
                     (list #x48 #x83 #xC4 (* num-bindings 8))  ; add rsp, imm8
                     (append (list #x48 #x81 #xC4)  ; add rsp, imm32
                             (int-to-bytes (* num-bindings 8) 4)))))))

    (lambda
     ;; Lambda expressions are not directly compiled to code
     ;; They only make sense in funcall context
     (error "Lambda expression cannot be compiled standalone: ~S" expr))

    (progn
     ;; Compile (progn expr1 expr2 ... exprN)
     ;; Evaluate each expression, keeping only the last result
     (let ((exprs (expr-args expr)))
       (if (null exprs)
           ;; Empty progn returns 0
           (emit-x86_64 (make-expr :type 'fixnum :value 0) env)
           ;; Evaluate each expression in sequence
           (let ((code nil))
             (dolist (e exprs)
               (setf code (append code (emit-x86_64 e env))))
             code))))

    (quote
     ;; Compile (quote datum)
     ;; Return the quoted value without evaluation
     (let ((datum (expr-value expr)))
       (cond
         ((integerp datum)
          ;; Quoted integer - just return as fixnum
          (emit-x86_64 (make-expr :type 'fixnum :value datum) env))
         ((null datum)
          ;; Quoted nil - return as fixnum 0 (or special nil value)
          (emit-x86_64 (make-expr :type 'fixnum :value 0) env))
         (t
          ;; Symbols and lists need runtime support
          (error "Quote of ~S not yet supported - need runtime symbols/lists" datum)))))

    (not
     ;; Compile (not expr)
     ;; Returns 1 (true) if expr is 0 (false), else 0
     (let* ((arg-expr (first (expr-args expr)))
            (arg-code (emit-x86_64 arg-expr env)))
       (append arg-code
               (list #x48 #x85 #xC0)        ; test rax, rax
               (list #x0F #x94 #xC0)        ; setz al
               (list #x48 #x0F #xB6 #xC0)   ; movzx rax, al
               (list #x48 #xC1 #xE0 #x04)))) ; shl rax, 4 (tag as fixnum)

    (and
     ;; Compile (and expr1 expr2 ...)
     ;; Short-circuit evaluation: return first false value, else last value
     (let ((exprs (expr-args expr)))
       (cond
         ((null exprs)
          ;; Empty and is true (return 1)
          (emit-x86_64 (make-expr :type 'fixnum :value 1) env))
         ((= (length exprs) 1)
          ;; Single expression: just evaluate it
          (emit-x86_64 (first exprs) env))
         (t
          ;; Multiple expressions: short-circuit evaluation
          ;; First, generate code for each expression
          (let ((expr-codes (mapcar (lambda (e) (emit-x86_64 e env)) exprs))
                (result nil))
            ;; Build code from right to left
            (loop for i from (1- (length expr-codes)) downto 0
                  for code = (nth i expr-codes)
                  for last = (= i (1- (length expr-codes)))
                  do (if last
                         ;; Last expression: just its code
                         (setf result code)
                         ;; Not last: code + test + conditional jump to end
                         (let ((test-and-jump (append
                                              (list #x48 #x85 #xC0)  ; test rax, rax
                                              (list #x74)            ; jz (short jump)
                                              (list (length result))))) ; offset to end
                           (setf result (append code test-and-jump result)))))
            result)))))

    (or
     ;; Compile (or expr1 expr2 ...)
     ;; Short-circuit evaluation: return first non-zero value, else last value
     (let ((exprs (expr-args expr)))
       (cond
         ((null exprs)
          ;; Empty or is false (return 0)
          (emit-x86_64 (make-expr :type 'fixnum :value 0) env))
         ((= (length exprs) 1)
          ;; Single expression: just evaluate it
          (emit-x86_64 (first exprs) env))
         (t
          ;; Multiple expressions: short-circuit evaluation
          ;; First, generate code for each expression
          (let ((expr-codes (mapcar (lambda (e) (emit-x86_64 e env)) exprs))
                (result nil))
            ;; Build code from right to left
            (loop for i from (1- (length expr-codes)) downto 0
                  for code = (nth i expr-codes)
                  for last = (= i (1- (length expr-codes)))
                  do (if last
                         ;; Last expression: just its code
                         (setf result code)
                         ;; Not last: code + test + conditional jump to end
                         (let ((test-and-jump (append
                                              (list #x48 #x85 #xC0)  ; test rax, rax
                                              (list #x75)            ; jnz (short jump)
                                              (list (length result))))) ; offset to end
                           (setf result (append code test-and-jump result)))))
            result)))))

    (cond
     ;; Compile (cond (test1 result1) (test2 result2) ... (t default))
     ;; Transform to nested ifs: (if test1 result1 (if test2 result2 ... default))
     (let ((clauses (expr-value expr)))
       (labels ((compile-cond-clauses (clauses)
                  (if (null clauses)
                      ;; No clauses: return 0 (or could be error)
                      (emit-x86_64 (make-expr :type 'fixnum :value 0) env)
                      (let* ((clause (first clauses))
                             (test (first clause))
                             (result (second clause))
                             (rest-clauses (rest clauses)))
                        (if (or (eq test t) (null rest-clauses))
                            ;; Last clause or (t ...) clause: just eval result
                            (emit-x86_64 (parse result) env)
                            ;; Not last: compile as (if test result (rest...))
                            (let* ((test-code (emit-x86_64 (parse test) env))
                                   (then-code (emit-x86_64 (parse result) env))
                                   (else-code (compile-cond-clauses rest-clauses))
                                   (then-size (length then-code))
                                   (else-size (length else-code))
                                   (jmp-to-end-size 5)
                                   (jz-to-else-size 6))
                              (append test-code
                                      (list #x48 #x85 #xC0)  ; test rax, rax
                                      (list #x0F #x84)  ; jz to else
                                      (int-to-bytes (+ then-size jmp-to-end-size) 4)
                                      then-code
                                      (list #xE9)  ; jmp to end
                                      (int-to-bytes else-size 4)
                                      else-code)))))))
         (compile-cond-clauses clauses))))

    (funcall
     ;; Compile ((lambda (params) body) args)
     ;; This is like let: bind args to params, then evaluate body
     (let* ((fn-expr (expr-value expr))
            (arg-exprs (expr-args expr)))
       (if (eq (expr-type fn-expr) 'lambda)
           ;; Inline lambda call
           (let* ((params (expr-value fn-expr))
                  (body (first (expr-args fn-expr)))
                  (num-params (length params))
                  (num-args (length arg-exprs))
                  (new-env env)
                  (binding-code nil))
             (unless (= num-params num-args)
               (error "Argument count mismatch: expected ~D, got ~D"
                      num-params num-args))
             ;; Evaluate each argument and push on stack
             (loop for arg-expr in arg-exprs
                   for param in params
                   for offset from 0 by 8
                   do (let ((arg-code (emit-x86_64 arg-expr env)))
                        (setf binding-code
                              (append binding-code
                                      arg-code
                                      (list #x50)))  ; push rax
                        ;; Add parameter to environment
                        (push (cons param (* offset 8)) new-env)))
             ;; Compile body with parameters bound
             (let ((body-code (emit-x86_64 body (reverse new-env))))
               (append binding-code
                       body-code
                       ;; Clean up stack
                       (if (<= (* num-params 8) 127)
                           (list #x48 #x83 #xC4 (* num-params 8))
                           (append (list #x48 #x81 #xC4)
                                   (int-to-bytes (* num-params 8) 4))))))
           (error "Can only call lambda expressions for now"))))

    (if
     ;; Compile (if condition then-expr else-expr)
     (let* ((condition (first (expr-args expr)))
            (then-expr (second (expr-args expr)))
            (else-expr (third (expr-args expr)))
            (then-code (emit-x86_64 then-expr env))
            (else-code (emit-x86_64 else-expr env))
            (then-size (length then-code))
            (else-size (length else-code))
            ;; Jump over else to end: 5 bytes for jmp rel32
            (jmp-to-end-size 5)
            ;; Jump to else if zero: 6 bytes for jz rel32
            (jz-to-else-size 6))
       (append (emit-x86_64 condition env)           ; Evaluate condition
               (list #x48 #x85 #xC0)                 ; test rax, rax
               ;; jz to else-branch (6 bytes total: 0F 84 + 4-byte offset)
               (list #x0F #x84)
               (int-to-bytes (+ then-size jmp-to-end-size) 4)
               then-code                              ; Then branch
               ;; jmp to end (5 bytes: E9 + 4-byte offset)
               (list #xE9)
               (int-to-bytes else-size 4)
               else-code)))                          ; Else branch

    (call
     (let ((op (expr-value expr))
           (args (expr-args expr)))
       (cond
         ((eq op '+)
          ;; Compile (+ a b)
          (append (emit-x86_64 (first args) env)   ; Result in RAX
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)   ; Result in RAX
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x01 #xD8)         ; add rax, rbx
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8 (pop)

         ((eq op '-)
          ;; Compile (- a b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x89 #xD9)         ; mov rcx, rbx
                  (list #x48 #x29 #xC1)         ; sub rcx, rax
                  (list #x48 #x89 #xC8)         ; mov rax, rcx
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op '*)
          ;; Compile (* a b)
          ;; Since fixnums are value*16, after multiply we need to divide by 16
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x0F #xAF #xD8)    ; imul rbx, rax
                  (list #x48 #x89 #xD8)         ; mov rax, rbx
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (adjust for tag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op '/)
          ;; Compile (/ a b) - integer division
          ;; Need to untag before division, then retag result
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (divisor)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (dividend)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag dividend)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag divisor)
                  (list #x48 #x99)              ; cqo (sign extend rax to rdx:rax)
                  (list #x48 #xF7 #xFB)         ; idiv rbx (rax = rax / rbx)
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag result)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'mod)
          ;; Compile (mod a b) - modulo operation
          ;; Similar to division but return remainder from RDX
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (divisor)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (dividend)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag dividend)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag divisor)
                  (list #x48 #x99)              ; cqo (sign extend)
                  (list #x48 #xF7 #xFB)         ; idiv rbx (rdx = remainder)
                  (list #x48 #x89 #xD0)         ; mov rax, rdx (move remainder to rax)
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag result)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'rem)
          ;; Compile (rem a b) - remainder operation (same as mod for positive numbers)
          ;; For x86_64, idiv gives remainder in rdx (same as mod implementation)
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (divisor)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (dividend)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag dividend)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag divisor)
                  (list #x48 #x99)              ; cqo (sign extend)
                  (list #x48 #xF7 #xFB)         ; idiv rbx (rdx = remainder)
                  (list #x48 #x89 #xD0)         ; mov rax, rdx (move remainder to rax)
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag result)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op '<)
          ;; Compile (< a b) - returns 1 (true) or 0 (false) as fixnum
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x9C #xC0)         ; setl al (set if less)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (tag as fixnum)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op '>)
          ;; Compile (> a b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x9F #xC0)         ; setg al (set if greater)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op '=)
          ;; Compile (= a b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x94 #xC0)         ; sete al (set if equal)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op '<=)
          ;; Compile (<= a b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x9E #xC0)         ; setle al (set if less or equal)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op '>=)
          ;; Compile (>= a b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x9D #xC0)         ; setge al (set if greater or equal)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op '/=)
          ;; Compile (/= a b) - not equal
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x95 #xC0)         ; setne al (set if not equal)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'car)
          ;; Compile (car cons) - load car field
          ;; cons cells have car at offset 16 (after header)
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x83 #xE0 #xF0)    ; and rax, ~0xF (clear tag)
                  (list #x48 #x8B #x40 #x10))) ; mov rax, [rax + 16]

         ((eq op 'cdr)
          ;; Compile (cdr cons) - load cdr field
          ;; cdr is at offset 24 (header + car)
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x83 #xE0 #xF0)    ; and rax, ~0xF (clear tag)
                  (list #x48 #x8B #x40 #x18))) ; mov rax, [rax + 24]

         ((eq op 'logand)
          ;; Compile (logand a b) - bitwise AND
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x21 #xD8)         ; and rax, rbx
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'logior)
          ;; Compile (logior a b) - bitwise OR
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x09 #xD8)         ; or rax, rbx
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'logxor)
          ;; Compile (logxor a b) - bitwise XOR
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x31 #xD8)         ; xor rax, rbx
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'lognot)
          ;; Compile (lognot a) - bitwise NOT
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xF7 #xD0)))       ; not rax

         ((eq op 'ash)
          ;; Compile (ash a b) - arithmetic shift
          ;; Positive b: left shift, negative b: right shift
          (append (emit-x86_64 (second args) env)  ; shift count in rax
                  (list #x50)                     ; push rax
                  (emit-x86_64 (first args) env)   ; value in rax
                  (list #x48 #x8B #x0C #x24)      ; mov rcx, [rsp] (shift count)
                  (list #x48 #xC1 #xF9 #x04)      ; sar rcx, 4 (untag)
                  (list #x48 #x85 #xC9)           ; test rcx, rcx
                  (list #x78)                     ; js (jump if negative)
                  (list 6)                        ; offset to right shift
                  ;; Left shift
                  (list #x48 #xD3 #xE0)           ; shl rax, cl
                  (list #xEB)                     ; jmp
                  (list 2)                        ; offset to end
                  ;; Right shift
                  (list #x48 #xD3 #xF8)           ; sar rax, cl
                  (list #x48 #x83 #xC4 #x08)))   ; add rsp, 8

         ;; Numeric operators
         ((eq op 'min)
          ;; Compile (min a b) - return smaller value
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x4C #xC3)         ; cmovl rax, rbx (move if less)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'max)
          ;; Compile (max a b) - return larger value
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x4F #xC3)         ; cmovg rax, rbx (move if greater)
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'abs)
          ;; Compile (abs a) - absolute value
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax
                  (list #x48 #xC1 #xFB #x3F)    ; sar rbx, 63 (sign bit)
                  (list #x48 #x31 #xD8)         ; xor rax, rbx
                  (list #x48 #x29 #xD8)))       ; sub rax, rbx

         ((eq op '1+)
          ;; Compile (1+ a) - increment by 1
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x83 #xC0 #x10))) ; add rax, 16 (1 << 4)

         ((eq op '1-)
          ;; Compile (1- a) - decrement by 1
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x83 #xE8 #x10))) ; sub rax, 16 (1 << 4)

         ;; Predicates
         ((eq op 'zerop)
          ;; Compile (zerop a) - test if zero
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x0F #x94 #xC0)         ; setz al
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4

         ((eq op 'plusp)
          ;; Compile (plusp a) - test if positive
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x0F #x9F #xC0)         ; setg al
                  (list #x48 #x0F #xB6 #xC0)
                  (list #x48 #xC1 #xE0 #x04)))

         ((eq op 'minusp)
          ;; Compile (minusp a) - test if negative
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x0F #x9C #xC0)         ; setl al
                  (list #x48 #x0F #xB6 #xC0)
                  (list #x48 #xC1 #xE0 #x04)))

         ((eq op 'evenp)
          ;; Compile (evenp a) - test if even
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #x83 #xE0 #x01)    ; and rax, 1 (get low bit)
                  (list #x48 #x83 #xF0 #x01)    ; xor rax, 1 (invert)
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag)

         ((eq op 'oddp)
          ;; Compile (oddp a) - test if odd
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #x83 #xE0 #x01)    ; and rax, 1 (get low bit)
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag)

         ((eq op 'signum)
          ;; Compile (signum a) - return -1, 0, or 1 based on sign
          ;; Algorithm: (if (< a 0) -1 (if (> a 0) 1 0))
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  ;; Check if zero
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x74 #x0E)              ; jz +14 (zero case)
                  ;; Not zero: check sign
                  (list #x48 #x31 #xDB)         ; xor rbx, rbx
                  (list #x48 #x0F #x9E #xC3)    ; setle bl (1 if rax <= 0)
                  (list #x48 #xD1 #xE3)         ; shl rbx, 1 (multiply by 2)
                  (list #x48 #xFF #xCB)         ; dec rbx (2 -> 1, 0 -> -1)
                  (list #x48 #x89 #xD8)         ; mov rax, rbx
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #xEB #x05)              ; jmp +5 (skip zero case)
                  ;; Zero case:
                  (list #x48 #x31 #xC0)         ; xor rax, rax (rax = 0)
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag to 0)

         ((eq op 'logcount)
          ;; Compile (logcount a) - count number of set bits (population count)
          ;; Uses Brian Kernighan's algorithm: repeatedly clear lowest set bit
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #x31 #xDB)         ; xor rbx, rbx (counter = 0)
                  ;; Loop: while (rax != 0)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x74 #x0D)              ; jz +13 (exit loop)
                  (list #x48 #xFF #xC3)         ; inc rbx (counter++)
                  (list #x48 #x89 #xC1)         ; mov rcx, rax
                  (list #x48 #xFF #xC9)         ; dec rcx
                  (list #x48 #x21 #xC8)         ; and rax, rcx (clear lowest set bit)
                  (list #xEB #xF1)              ; jmp -15 (back to test)
                  ;; Exit: rbx has count
                  (list #x48 #x89 #xD8)         ; mov rax, rbx
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag)

         ((eq op 'logtest)
          ;; Compile (logtest a b) - test if any bits are set in both args
          ;; Returns 1 if (logand a b) != 0, else 0
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag first)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag second)
                  (list #x48 #x21 #xD8)         ; and rax, rbx
                  (list #x48 #x0F #x95 #xC0)    ; setnz al (1 if result != 0)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'gcd)
          ;; Compile (gcd a b) - greatest common divisor using Euclidean algorithm
          ;; Algorithm: gcd(a,0) = |a|, gcd(a,b) = gcd(b, a mod b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (second arg)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (first arg)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag)
                  ;; Get absolute value of rax (a = abs(a))
                  (list #x48 #x89 #xC1)         ; mov rcx, rax
                  (list #x48 #xC1 #xF9 #x3F)    ; sar rcx, 63 (sign bit)
                  (list #x48 #x31 #xC8)         ; xor rax, rcx
                  (list #x48 #x29 #xC8)         ; sub rax, rcx
                  ;; Get absolute value of rbx (b = abs(b))
                  (list #x48 #x89 #xD9)         ; mov rcx, rbx
                  (list #x48 #xC1 #xF9 #x3F)    ; sar rcx, 63
                  (list #x48 #x31 #xD9)         ; xor rbx, rcx
                  (list #x48 #x29 #xD9)         ; sub rbx, rcx
                  ;; GCD loop: while (b != 0) { temp = a % b; a = b; b = temp; }
                  (list #x48 #x85 #xDB)         ; test rbx, rbx
                  (list #x74 #x0D)              ; jz +13 (done, skip to retag)
                  (list #x48 #x99)              ; cqo (sign extend rax to rdx:rax)
                  (list #x48 #xF7 #xFB)         ; idiv rbx (rdx = remainder)
                  (list #x48 #x89 #xD8)         ; mov rax, rbx (a = b)
                  (list #x48 #x89 #xD3)         ; mov rbx, rdx (b = remainder)
                  (list #xEB #xEF)              ; jmp -17 (back to test)
                  ;; Done: rax contains GCD
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'isqrt)
          ;; Compile (isqrt n) - integer square root using Newton's method
          ;; Algorithm: x_new = (x + n/x) / 2, iterate until convergence
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  ;; Handle special cases
                  (list #x48 #x83 #xF8 #x01)    ; cmp rax, 1
                  (list #x76 #x1E)              ; jbe +30 (return rax if <= 1)
                  ;; Initialize: x = n/2
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (save n in rbx)
                  (list #x48 #xD1 #xE8)         ; shr rax, 1 (x = n/2)
                  ;; Newton loop: while (true)
                  (list #x48 #x89 #xC1)         ; mov rcx, rax (save old x)
                  (list #x48 #x89 #xD8)         ; mov rax, rbx (n)
                  (list #x48 #x99)              ; cqo
                  (list #x48 #xF7 #xF9)         ; idiv rcx (n/x)
                  (list #x48 #x01 #xC8)         ; add rax, rcx (n/x + x)
                  (list #x48 #xD1 #xE8)         ; shr rax, 1 ((n/x + x)/2)
                  (list #x48 #x39 #xC1)         ; cmp rcx, rax
                  (list #x7F #x02)              ; jg +2 (if old > new, continue)
                  (list #xEB #x05)              ; jmp +5 (converged, use old value)
                  (list #xEB #xE9)              ; jmp -23 (back to loop start)
                  (list #x48 #x89 #xC8)         ; mov rax, rcx (use old x)
                  ;; Retag and return
                  (list #x48 #xC1 #xE0 #x04)))  ; shl rax, 4 (retag)

         ;; List operations - require runtime integration
         ;; These operations need heap allocation and are not yet integrated with compiled code.
         ;; They work in the REPL (interpreted mode) which has access to the runtime heap.
         ;; Future work: Implement FFI or compile runtime functions to machine code.
         ;; See docs/RUNTIME_INTEGRATION.md for implementation plan.
         ((eq op 'cons)
          (error "cons requires runtime heap integration~%~
                  Hint: cons works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'car)
          (error "car requires runtime heap integration~%~
                  Hint: car works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'cdr)
          (error "cdr requires runtime heap integration~%~
                  Hint: cdr works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'list)
          (error "list requires runtime heap integration~%~
                  Hint: list works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         (t
          (error "Unknown operator: ~S" op)))))))

;;; Code generation for ARM64
(defun emit-arm64 (expr &optional (env nil))
  "Generate ARM64 machine code for expression with environment"
  (ecase (expr-type expr)
    (fixnum
     ;; Load fixnum into X0
     ;; mov x0, #imm
     (let ((val (* (expr-value expr) 16))) ; Tag as fixnum
       (if (< val 65536)
           ;; Use MOVZ for small immediate
           (int-to-bytes (logior #xD2800000 ; MOVZ X0, imm16
                                 (ash (logand val #xFFFF) 5))
                         4)
           ;; Use MOVZ + MOVK for larger values
           (append (int-to-bytes (logior #xD2800000
                                         (ash (logand val #xFFFF) 5))
                                 4)
                   (int-to-bytes (logior #xF2A00000 ; MOVK X0, imm16, LSL#16
                                         (ash (logand (ash val -16) #xFFFF) 5))
                                 4)))))

    (variable
     ;; Look up variable in environment and load from stack
     (let* ((var-name (expr-value expr))
            (binding (assoc var-name env)))
       (if binding
           (let ((offset (cdr binding)))
             ;; ldr x0, [sp, #offset]
             (if (< offset 256)
                 ;; Use immediate offset encoding (scaled by 8)
                 (int-to-bytes (logior #xF9400000  ; ldr x0, [sp, #imm]
                                       (ash (/ offset 8) 10))  ; offset in bits [21:10]
                             4)
                 (error "Variable offset too large: ~D" offset)))
           (error "Unbound variable: ~S" var-name))))

    (setq
     ;; Compile (setq var value) - mutate a lexical variable
     (let* ((var-name (expr-value expr))
            (value-expr (first (expr-args expr)))
            (binding (assoc var-name env)))
       (if binding
           (let ((offset (cdr binding)))
             (if (< offset 256)
                 (append
                  ;; First, evaluate the value expression into X0
                  (emit-arm64 value-expr env)
                  ;; Then store X0 to the variable's stack location
                  ;; str x0, [sp, #offset]
                  (int-to-bytes (logior #xF9000000  ; str x0, [sp, #imm]
                                        (ash (/ offset 8) 10))  ; offset in bits [21:10]
                                4))
                 (error "Variable offset too large: ~D" offset)))
           (error "Cannot setq unbound variable: ~S" var-name))))

    (let
     ;; Compile (let ((var val) ...) body) for ARM64
     (let* ((bindings (expr-value expr))
            (body (first (expr-args expr)))
            (num-bindings (length bindings))
            (new-env env)
            (binding-code nil))
       ;; Generate code to evaluate and push each binding
       (loop for (var val-form) in bindings
             for offset from 0 by 8
             do (let ((val-code (emit-arm64 (parse val-form) env)))
                  (setf binding-code
                        (append binding-code
                                val-code
                                ;; str x0, [sp, #-8]!  (pre-decrement store)
                                (list #xE0 #x0F #x1F #xF8)))  ; str x0, [sp, #-8]!
                  ;; Add to environment with current stack offset
                  (push (cons var (* offset 8)) new-env)))
       ;; Generate code for body with extended environment
       (let ((body-code (emit-arm64 body (reverse new-env))))
         (append binding-code
                 body-code
                 ;; Clean up stack: add sp, sp, #num-bindings*8
                 (if (<= (* num-bindings 8) 4095)
                     (int-to-bytes (logior #x910003E0  ; add sp, sp, #imm
                                           (ash (* num-bindings 8) 10))  ; imm12 in bits [21:10]
                                   4)
                     (error "Too many bindings for immediate encoding"))))))

    (lambda
     ;; Lambda expressions are not directly compiled to code
     (error "Lambda expression cannot be compiled standalone: ~S" expr))

    (progn
     ;; Compile (progn expr1 expr2 ... exprN) for ARM64
     (let ((exprs (expr-args expr)))
       (if (null exprs)
           (emit-arm64 (make-expr :type 'fixnum :value 0) env)
           (let ((code nil))
             (dolist (e exprs)
               (setf code (append code (emit-arm64 e env))))
             code))))

    (quote
     ;; Compile (quote datum) for ARM64
     ;; Return the quoted value without evaluation
     (let ((datum (expr-value expr)))
       (cond
         ((integerp datum)
          ;; Quoted integer - just return as fixnum
          (emit-arm64 (make-expr :type 'fixnum :value datum) env))
         ((null datum)
          ;; Quoted nil - return as fixnum 0 (or special nil value)
          (emit-arm64 (make-expr :type 'fixnum :value 0) env))
         (t
          ;; Symbols and lists need runtime support
          (error "Quote of ~S not yet supported - need runtime symbols/lists" datum)))))

    (not
     ;; Compile (not expr) for ARM64
     ;; Returns 1 (true) if expr is 0 (false), else 0
     (let* ((arg-expr (first (expr-args expr)))
            (arg-code (emit-arm64 arg-expr env)))
       (append arg-code
               ;; Compare x0 with 0
               (list #x1F #x00 #x00 #xF1)         ; cmp x0, #0
               ;; cset x0, eq - set x0 to 1 if equal, 0 otherwise
               (list #xE0 #x17 #x9F #x9A)         ; cset x0, eq
               ;; Shift left by 4 to tag as fixnum
               (list #xE0 #x13 #x00 #xD3))))      ; lsl x0, x0, #4

    (and
     ;; Compile (and expr1 expr2 ...) for ARM64
     ;; Short-circuit evaluation: return first false value, else last value
     (let ((exprs (expr-args expr)))
       (cond
         ((null exprs)
          ;; Empty and is true (return 1)
          (emit-arm64 (make-expr :type 'fixnum :value 1) env))
         ((= (length exprs) 1)
          ;; Single expression: just evaluate it
          (emit-arm64 (first exprs) env))
         (t
          ;; Multiple expressions: short-circuit evaluation
          (let ((expr-codes (mapcar (lambda (e) (emit-arm64 e env)) exprs))
                (result nil))
            ;; Build code from right to left
            (loop for i from (1- (length expr-codes)) downto 0
                  for code = (nth i expr-codes)
                  for last = (= i (1- (length expr-codes)))
                  do (if last
                         ;; Last expression: just its code
                         (setf result code)
                         ;; Not last: code + cmp + b.eq to end
                         (let* ((offset-bytes (length result))
                                (offset-insns (/ offset-bytes 4))
                                (test-and-jump (append
                                               (list #x1F #x00 #x00 #xF1)  ; cmp x0, #0
                                               ;; b.eq offset (branch if equal to zero)
                                               (list #x00  ; low byte of offset
                                                     (logand offset-insns #xFF)
                                                     (logand (ash offset-insns -8) #xFF)
                                                     #x54)))) ; b.eq condition code
                           (setf result (append code test-and-jump result)))))
            result)))))

    (or
     ;; Compile (or expr1 expr2 ...) for ARM64
     ;; Short-circuit evaluation: return first non-zero value, else last value
     (let ((exprs (expr-args expr)))
       (cond
         ((null exprs)
          ;; Empty or is false (return 0)
          (emit-arm64 (make-expr :type 'fixnum :value 0) env))
         ((= (length exprs) 1)
          ;; Single expression: just evaluate it
          (emit-arm64 (first exprs) env))
         (t
          ;; Multiple expressions: short-circuit evaluation
          (let ((expr-codes (mapcar (lambda (e) (emit-arm64 e env)) exprs))
                (result nil))
            ;; Build code from right to left
            (loop for i from (1- (length expr-codes)) downto 0
                  for code = (nth i expr-codes)
                  for last = (= i (1- (length expr-codes)))
                  do (if last
                         ;; Last expression: just its code
                         (setf result code)
                         ;; Not last: code + cmp + b.ne to end
                         (let* ((offset-bytes (length result))
                                (offset-insns (/ offset-bytes 4))
                                (test-and-jump (append
                                               (list #x1F #x00 #x00 #xF1)  ; cmp x0, #0
                                               ;; b.ne offset (branch if not equal to zero)
                                               (list #x01  ; condition code 1 = ne
                                                     (logand offset-insns #xFF)
                                                     (logand (ash offset-insns -8) #xFF)
                                                     #x54)))) ; conditional branch
                           (setf result (append code test-and-jump result)))))
            result)))))

    (cond
     ;; Compile (cond (test1 result1) (test2 result2) ... (t default)) for ARM64
     ;; Transform to nested ifs
     (let ((clauses (expr-value expr)))
       (labels ((compile-cond-clauses (clauses)
                  (if (null clauses)
                      (emit-arm64 (make-expr :type 'fixnum :value 0) env)
                      (let* ((clause (first clauses))
                             (test (first clause))
                             (result (second clause))
                             (rest-clauses (rest clauses)))
                        (if (or (eq test t) (null rest-clauses))
                            (emit-arm64 (parse result) env)
                            (let* ((test-code (emit-arm64 (parse test) env))
                                   (then-code (emit-arm64 (parse result) env))
                                   (else-code (compile-cond-clauses rest-clauses))
                                   (then-size (length then-code))
                                   (else-size (length else-code))
                                   (b-to-end-size 4)
                                   (beq-to-else-size 4))
                              (append test-code
                                      (list #x1F #x00 #x00 #xF1)  ; cmp x0, #0
                                      ;; b.eq to else-branch
                                      (let ((offset-bytes (+ then-size b-to-end-size)))
                                        (list #x40  ; condition code 0 = eq
                                              (logand (ash offset-bytes -2) #xFF)
                                              (logand (ash offset-bytes -10) #xFF)
                                              #x54))
                                      then-code
                                      ;; b to end (unconditional branch)
                                      (let ((offset-bytes else-size))
                                        (list (logand (ash offset-bytes -2) #xFF)
                                              (logand (ash offset-bytes -10) #xFF)
                                              (logand (ash offset-bytes -18) #xFF)
                                              #x14))
                                      else-code)))))))
         (compile-cond-clauses clauses))))

    (funcall
     ;; Compile ((lambda (params) body) args) for ARM64
     (let* ((fn-expr (expr-value expr))
            (arg-exprs (expr-args expr)))
       (if (eq (expr-type fn-expr) 'lambda)
           (let* ((params (expr-value fn-expr))
                  (body (first (expr-args fn-expr)))
                  (num-params (length params))
                  (num-args (length arg-exprs))
                  (new-env env)
                  (binding-code nil))
             (unless (= num-params num-args)
               (error "Argument count mismatch: expected ~D, got ~D"
                      num-params num-args))
             ;; Evaluate and push each argument
             (loop for arg-expr in arg-exprs
                   for param in params
                   for offset from 0 by 8
                   do (let ((arg-code (emit-arm64 arg-expr env)))
                        (setf binding-code
                              (append binding-code
                                      arg-code
                                      (list #xE0 #x0F #x1F #xF8)))  ; str x0, [sp, #-8]!
                        (push (cons param (* offset 8)) new-env)))
             ;; Compile body with parameters bound
             (let ((body-code (emit-arm64 body (reverse new-env))))
               (append binding-code
                       body-code
                       ;; Clean up stack
                       (if (<= (* num-params 8) 4095)
                           (int-to-bytes (logior #x910003E0
                                                 (ash (* num-params 8) 10))
                                         4)
                           (error "Too many parameters for immediate encoding")))))
           (error "Can only call lambda expressions for now"))))

    (if
     ;; Compile (if condition then-expr else-expr) for ARM64
     (let* ((condition (first (expr-args expr)))
            (then-expr (second (expr-args expr)))
            (else-expr (third (expr-args expr)))
            (then-code (emit-arm64 then-expr env))
            (else-code (emit-arm64 else-expr env))
            (then-size (length then-code))
            (else-size (length else-code))
            ;; Branch to end: 4 bytes for b (unconditional branch)
            (b-to-end-size 4)
            ;; Conditional branch to else: 4 bytes for b.eq
            (beq-to-else-size 4))
       (append (emit-arm64 condition env)               ; Evaluate condition
               ;; Compare x0 with 0
               (list #x1F #x00 #x00 #xF1)           ; cmp x0, #0
               ;; b.eq to else-branch (4 bytes: 54 + 3-byte offset in bits [23:5])
               ;; Offset is in instructions (4-byte units), and encoded specially
               (let ((offset-bytes (+ then-size b-to-end-size)))
                 (list #x40
                       (logand (ash offset-bytes -2) #xFF)
                       (logand (ash offset-bytes -10) #xFF)
                       #x54))  ; b.eq (condition code 0)
               then-code                            ; Then branch
               ;; b to end (unconditional branch)
               (let ((offset-bytes else-size))
                 (list (logand (ash offset-bytes -2) #xFF)
                       (logand (ash offset-bytes -10) #xFF)
                       (logand (ash offset-bytes -18) #xFF)
                       #x14))  ; b (unconditional)
               else-code)))                         ; Else branch

    (call
     (let ((op (expr-value expr))
           (args (expr-args expr)))
       (cond
         ((eq op '+)
          ;; Compile (+ a b) for ARM64
          (append (emit-arm64 (first args) env)        ; Result in X0
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE0 #x03 #x00 #xAA)       ; mov x0, x0 (save)
                  (emit-arm64 (second args) env)        ; Result in X0
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #xE0 #x03 #x01 #xAA)       ; mov x0, x1 (restore from stack would be here)
                  (list #x00 #x00 #x01 #x8B)       ; add x0, x0, x1
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op '-)
          ;; Compile (- a b) for ARM64
          (append (emit-arm64 (first args) env)        ; Result in X0 (first arg)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save first)
                  (emit-arm64 (second args) env)        ; Result in X0 (second arg)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (second to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (first back to x0)
                  (list #x00 #x00 #x01 #xCB)       ; sub x0, x0, x1
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op '*)
          ;; Compile (* a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save first)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (second to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (first back to x0)
                  (list #x00 #x7C #x01 #x9B)       ; mul x0, x0, x1
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (adjust for tag)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op '/)
          ;; Compile (/ a b) for ARM64 - integer division
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save dividend)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (divisor to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (dividend back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag dividend)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag divisor)
                  (list #x00 #x0C #xC1 #x9A)       ; sdiv x0, x0, x1 (signed divide)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag result)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op 'mod)
          ;; Compile (mod a b) for ARM64
          ;; remainder = dividend - (quotient * divisor)
          ;; Use MSUB: msub Xd, Xn, Xm, Xa = Xa - (Xn * Xm)
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save dividend)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (divisor to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (dividend back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag dividend)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag divisor)
                  (list #xE3 #x0C #xC1 #x9A)       ; sdiv x3, x0, x1 (quotient in x3)
                  ;; msub x0, x3, x1, x0  = x0 - (x3 * x1) = dividend - quotient*divisor
                  (list #x00 #x80 #x01 #x9B)       ; msub x0, x0, x1, x3
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag result)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op 'rem)
          ;; Compile (rem a b) for ARM64 - remainder operation
          ;; Same as mod (ARM64 sdiv gives truncating division, same as rem)
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save dividend)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (divisor to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (dividend back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag dividend)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag divisor)
                  (list #xE3 #x0C #xC1 #x9A)       ; sdiv x3, x0, x1 (quotient in x3)
                  (list #x00 #x80 #x01 #x9B)       ; msub x0, x0, x1, x3
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag result)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op '<)
          ;; Compile (< a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #xB7 #x9F #x9A)       ; cset x0, lt (less than)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (tag as fixnum)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op '>)
          ;; Compile (> a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #xC7 #x9F #x9A)       ; cset x0, gt (greater than)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))

         ((eq op '=)
          ;; Compile (= a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #x07 #x9F #x9A)       ; cset x0, eq (equal)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))

         ((eq op '<=)
          ;; Compile (<= a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #xD7 #x9F #x9A)       ; cset x0, le (less or equal)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))

         ((eq op '>=)
          ;; Compile (>= a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #xA7 #x9F #x9A)       ; cset x0, ge (greater or equal)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))

         ((eq op '/=)
          ;; Compile (/= a b) - not equal for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #x17 #x9F #x9A)       ; cset x0, ne (not equal)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))

         ((eq op 'car)
          ;; Compile (car cons) for ARM64 - load car field
          (append (emit-arm64 (first args) env)
                  (list #x00 #x3C #x40 #x92)       ; and x0, x0, #~0xF (clear tag)
                  (list #x00 #x08 #x40 #xF9)))    ; ldr x0, [x0, #16]

         ((eq op 'cdr)
          ;; Compile (cdr cons) for ARM64 - load cdr field
          (append (emit-arm64 (first args) env)
                  (list #x00 #x3C #x40 #x92)       ; and x0, x0, #~0xF (clear tag)
                  (list #x00 #x0C #x40 #xF9)))    ; ldr x0, [x0, #24]

         ((eq op 'logand)
          ;; Compile (logand a b) for ARM64 - bitwise AND
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)      ; str x0, [sp, #-8]!
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)      ; ldr x1, [sp]
                  (list #x00 #x00 #x01 #x8A)      ; and x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))    ; add sp, sp, #8

         ((eq op 'logior)
          ;; Compile (logior a b) for ARM64 - bitwise OR
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)
                  (list #x00 #x00 #x01 #xAA)      ; orr x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))

         ((eq op 'logxor)
          ;; Compile (logxor a b) for ARM64 - bitwise XOR
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)
                  (list #x00 #x00 #x01 #xCA)      ; eor x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))

         ((eq op 'lognot)
          ;; Compile (lognot a) for ARM64 - bitwise NOT
          (append (emit-arm64 (first args) env)
                  (list #x00 #x00 #x20 #xAA)))    ; mvn x0, x0

         ((eq op 'ash)
          ;; Compile (ash a b) for ARM64 - arithmetic shift
          (append (emit-arm64 (second args) env)  ; shift count in x0
                  (list #xE0 #x0F #x1F #xF8)      ; str x0, [sp, #-8]!
                  (emit-arm64 (first args) env)   ; value in x0
                  (list #xE1 #x03 #x40 #xF9)      ; ldr x1, [sp] (shift count)
                  (list #x21 #x10 #x40 #xD3)      ; lsr x1, x1, #4 (untag)
                  (list #x3F #x00 #x00 #xF1)      ; cmp x1, #0
                  ;; b.ge to left shift (skip right shift)
                  (list #x42 #x00 #x00 #x54)      ; b.ge #8
                  ;; Right shift (negative count)
                  (list #x21 #x00 #x00 #xCB)      ; neg x1, x1
                  (list #x00 #xFC #xC1 #x9A)      ; asr x0, x0, x1
                  (list #x01 #x00 #x00 #x14)      ; b #4 (skip left shift)
                  ;; Left shift
                  (list #x00 #x20 #xC1 #x9A)      ; lsl x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))    ; add sp, sp, #8

         ;; Numeric operators
         ((eq op 'min)
          ;; Compile (min a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)      ; str x0, [sp, #-8]!
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)      ; ldr x1, [sp]
                  (list #x3F #x00 #x00 #xEB)      ; cmp x1, x0
                  (list #x20 #xD0 #x81 #x9A)      ; csel x0, x1, x1, le (select x1 if x1 <= x0)
                  (list #xFF #x07 #x00 #x91)))    ; add sp, sp, #8

         ((eq op 'max)
          ;; Compile (max a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)
                  (list #x3F #x00 #x00 #xEB)      ; cmp x1, x0
                  (list #x20 #xC0 #x81 #x9A)      ; csel x0, x1, x1, gt (select x1 if x1 > x0)
                  (list #xFF #x07 #x00 #x91)))

         ((eq op 'abs)
          ;; Compile (abs a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x01 #xFC #x7F #xD3)      ; lsr x1, x0, #63 (sign bit)
                  (list #x00 #x00 #x01 #xCA)      ; eor x0, x0, x1
                  (list #x00 #x00 #x01 #xCB)))    ; sub x0, x0, x1

         ((eq op '1+)
          ;; Compile (1+ a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x40 #x00 #x91)))    ; add x0, x0, #16

         ((eq op '1-)
          ;; Compile (1- a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x40 #x00 #xD1)))    ; sub x0, x0, #16

         ;; Predicates
         ((eq op 'zerop)
          ;; Compile (zerop a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE0 #x07 #x9F #x9A)      ; cset x0, eq
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4

         ((eq op 'plusp)
          ;; Compile (plusp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE0 #xC7 #x9F #x9A)      ; cset x0, gt
                  (list #x00 #x10 #x00 #xD3)))

         ((eq op 'minusp)
          ;; Compile (minusp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE0 #xB7 #x9F #x9A)      ; cset x0, lt
                  (list #x00 #x10 #x00 #xD3)))

         ((eq op 'evenp)
          ;; Compile (evenp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x40 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x00 #x04 #x00 #x92)      ; and x0, x0, #1
                  (list #x00 #x04 #x00 #xD2)      ; eor x0, x0, #1 (invert)
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4

         ((eq op 'oddp)
          ;; Compile (oddp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x40 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x00 #x04 #x00 #x92)      ; and x0, x0, #1
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4

         ((eq op 'signum)
          ;; Compile (signum a) - return -1, 0, or 1 based on sign
          ;; Use conditional select: x < 0 ? -1 : (x > 0 ? 1 : 0)
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x44 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE1 #xB3 #x9F #x1A)      ; csetm x1, lt (x1 = -1 if neg else 0)
                  (list #xE2 #xC7 #x9A #x9A)      ; cset x2, gt (x2 = 1 if pos else 0)
                  (list #x00 #x00 #x82 #x8B)      ; add x0, x0, x2 (combine)
                  (list #x00 #x00 #x01 #x8B)      ; add x0, x0, x1
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4 (retag)

         ((eq op 'logcount)
          ;; Compile (logcount a) - count number of set bits
          ;; Uses loop to count bits (ARM64 has no single instruction for this in base ISA)
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x44 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x01 #x00 #x80 #xD2)      ; mov x1, #0 (counter)
                  ;; Loop start
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #x60 #x00 #x00 #x54)      ; b.eq +12 (exit if zero)
                  (list #x21 #x04 #x00 #x91)      ; add x1, x1, #1 (counter++)
                  (list #x02 #x00 #x00 #xD1)      ; sub x2, x0, #1
                  (list #x00 #x00 #x02 #x8A)      ; and x0, x0, x2 (clear lowest bit)
                  (list #xE0 #xFF #xFF #x17)      ; b -8 (loop back)
                  ;; Exit
                  (list #x00 #x00 #x01 #xAA)      ; mov x0, x1 (result = counter)
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4 (retag)

         ((eq op 'logtest)
          ;; Compile (logtest a b) - test if any bits are set in both
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)      ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)      ; mov x2, x0 (save first)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)      ; mov x1, x0 (second to x1)
                  (list #x40 #x10 #x44 #xD3)      ; lsr x0, x2, #4 (untag first)
                  (list #x21 #x10 #x44 #xD3)      ; lsr x1, x1, #4 (untag second)
                  (list #x00 #x00 #x01 #x8A)      ; and x0, x0, x1
                  (list #xE0 #x17 #x9F #x9A)      ; cset x0, ne (1 if result != 0)
                  (list #x00 #x10 #x00 #xD3)      ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)))    ; ldp x29, x30, [sp], #16

         ((eq op 'gcd)
          ;; Compile (gcd a b) for ARM64 - greatest common divisor
          ;; Using Euclidean algorithm with loop
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save first)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (second to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (first back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag)
                  ;; abs(x0): x0 = (x0 XOR (x0>>63)) - (x0>>63)
                  (list #x02 #xFC #x47 #x93)       ; asr x2, x0, #63 (sign extend)
                  (list #x00 #x00 #x02 #xCA)       ; eor x0, x0, x2
                  (list #x00 #x00 #x02 #xCB)       ; sub x0, x0, x2
                  ;; abs(x1): x1 = (x1 XOR (x1>>63)) - (x1>>63)
                  (list #x22 #xFC #x47 #x93)       ; asr x2, x1, #63
                  (list #x21 #x00 #x02 #xCA)       ; eor x1, x1, x2
                  (list #x21 #x00 #x02 #xCB)       ; sub x1, x1, x2
                  ;; GCD loop: while x1 != 0
                  ;; Check if x1 == 0, use cmp and conditional select approach
                  (list #x3F #x00 #x01 #xEB)       ; cmp x1, #0
                  (list #x03 #x00 #x00 #x54)       ; b.eq +6 (to done, skip 6 instructions)
                  ;; Compute remainder: x3 = x0 - (x0/x1)*x1
                  (list #xE2 #x0C #xC1 #x9A)       ; sdiv x2, x0, x1 (quotient)
                  (list #x03 #x7C #x01 #x9B)       ; msub x3, x0, x1, x2 (remainder)
                  (list #xE0 #x03 #x01 #xAA)       ; mov x0, x1 (a = b)
                  (list #xE1 #x03 #x03 #xAA)       ; mov x1, x3 (b = remainder)
                  (list #xFA #xFF #xFF #x17)       ; b -6 (back to cmp)
                  ;; Done: x0 has GCD
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op 'isqrt)
          ;; Compile (isqrt n) for ARM64 - integer square root using Newton's method
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag)
                  ;; Handle special cases: if n <= 1, return n
                  (list #x1F #x08 #x00 #xF1)       ; cmp x0, #2
                  (list #xC3 #x00 #x00 #x54)       ; b.lo +6 (skip to retag if < 2)
                  ;; Initialize: x1 = n, x2 = n/2 (initial guess)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (save n)
                  (list #x02 #x08 #x40 #xD3)       ; lsr x2, x0, #1 (x2 = n/2)
                  ;; Newton loop
                  (list #xE3 #x03 #x02 #xAA)       ; mov x3, x2 (save old guess)
                  (list #xE0 #x03 #x01 #xAA)       ; mov x0, x1 (n)
                  (list #xE0 #x0C #xC3 #x9A)       ; sdiv x0, x0, x3 (n/x)
                  (list #x00 #x00 #x03 #x8B)       ; add x0, x0, x3 (n/x + x)
                  (list #x02 #x08 #x40 #xD3)       ; lsr x2, x0, #1 ((n/x + x)/2)
                  ;; Check convergence: if old >= new, done
                  (list #x7F #x00 #x02 #xEB)       ; cmp x3, x2
                  (list #x42 #x00 #x00 #x54)       ; b.hs +2 (if old >= new, use old)
                  (list #xF9 #xFF #xFF #x17)       ; b -7 (back to loop)
                  ;; Use old value (converged)
                  (list #xE0 #x03 #x03 #xAA)       ; mov x0, x3 (result)
                  ;; Retag and return
                  (list #x00 #x10 #x00 #xD3)))     ; lsl x0, x0, #4 (retag)

         ;; List operations - require runtime integration
         ;; These operations need heap allocation and are not yet integrated with compiled code.
         ;; They work in the REPL (interpreted mode) which has access to the runtime heap.
         ;; Future work: Implement FFI or compile runtime functions to machine code.
         ;; See docs/RUNTIME_INTEGRATION.md for implementation plan.
         ((eq op 'cons)
          (error "cons requires runtime heap integration~%~
                  Hint: cons works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'car)
          (error "car requires runtime heap integration~%~
                  Hint: car works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'cdr)
          (error "cdr requires runtime heap integration~%~
                  Hint: cdr works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'list)
          (error "list requires runtime heap integration~%~
                  Hint: list works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         (t
          (error "Unknown operator: ~S" op)))))))

;;; Helper: Convert integer to little-endian byte list
(defun int-to-bytes (n size)
  "Convert integer N to SIZE bytes in little-endian order"
  (loop for i from 0 below size
        collect (ldb (byte 8 (* i 8)) n)))

;;; Helper: Convert byte list to vector
(defun bytes-to-vector (bytes)
  (make-array (length bytes)
              :element-type '(unsigned-byte 8)
              :initial-contents bytes))

;;; Main compilation entry point
(defun compile-expression (form &key (arch :x86_64))
  "Compile a Lisp form to machine code for the target architecture"
  (let ((*target-arch* arch))
    (let* ((ir (parse form))
           (code (ecase arch
                   (:x86_64 (emit-x86_64 ir))
                   (:arm64 (emit-arm64 ir)))))
      (bytes-to-vector code))))

;;; Write machine code to binary file with minimal ELF wrapper
(defun compile-to-binary (form output-file &key (arch :x86_64))
  "Compile form to executable binary"
  (let* ((code (compile-expression form :arch arch))
         (code-size (length code)))
    (with-open-file (out output-file
                         :direction :output
                         :if-exists :supersede
                         :element-type '(unsigned-byte 8))
      (ecase arch
        (:x86_64
         ;; Minimal x86_64 code - just the instructions + ret
         (write-sequence code out)
         (write-byte #xC3 out)) ; ret instruction

        (:arm64
         ;; Minimal ARM64 code - just the instructions + ret
         (write-sequence code out)
         ;; ret instruction for ARM64
         (write-sequence #(#xC0 #x03 #x5F #xD6) out))))

    ;; Return info about compilation
    (values output-file code-size)))
