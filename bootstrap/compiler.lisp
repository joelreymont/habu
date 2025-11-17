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

;;; Compiler intermediate representation
(defstruct expr
  type
  value
  args)

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
     (let ((bindings (second form))
           (body (third form)))
       (make-expr :type 'let
                  :value bindings  ; Store binding pairs
                  :args (list (parse body)))))

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

    ((and (consp form) (eq (first form) 'quote))
     ;; Special form: (quote datum)
     ;; Note: Don't recursively parse - keep quoted value as-is
     (let ((datum (second form)))
       (make-expr :type 'quote
                  :value datum
                  :args nil)))

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
       (make-expr :type 'call
                  :value op
                  :args (mapcar #'parse args))))

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
