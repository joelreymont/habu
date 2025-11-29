;;; Pure Habu Compiler - Uses ONLY Habu primitives (no SBCL dependencies)
;;; No multiple-value-bind, no values, no loop, no format
;;; This can be compiled to native and run without SBCL

(in-package :habu)

;;; ============================================================
;;; Core Helpers (Pure Habu)
;;; ============================================================

(defun pure-append (lst1 lst2)
  "Append two lists without using CL append"
  (labels ((append-iter (l acc)
             (if (null l)
                 acc
                 (append-iter (cdr l) (cons (car l) acc)))))
    (append-iter (reverse lst1) lst2)))

(defun pure-reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

(defun pure-length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

(defun pure-nth (n lst)
  "Get nth element"
  (if (= n 0)
      (car lst)
      (pure-nth (- n 1) (cdr lst))))

;;; ============================================================
;;; Pure Compiler Core
;;; ============================================================

(defun pure-compile-lit (val)
  "Compile literal to IR"
  (list 'lit val))

(defun pure-compile-var (sym env)
  "Compile variable reference"
  (let ((offset (pure-env-lookup sym env)))
    (if offset
        (list 'var offset)
        (list 'lit 0))))  ;; Unknown var = 0

(defun pure-env-lookup (sym env)
  "Look up symbol in environment, return offset or nil"
  (labels ((search-env (e offset)
             (if (null e)
                 nil
                 (if (eq (car e) sym)
                     offset
                     (search-env (cdr e) (+ offset 1))))))
    (search-env env 0)))

(defun pure-compile-if (expr env)
  "Compile (if test then else) to IR"
  (let ((test (pure-compile-expr (nth 1 expr) env))
        (then (pure-compile-expr (nth 2 expr) env))
        (else (pure-compile-expr (nth 3 expr) env)))
    (list 'if-ir test then else)))

(defun pure-compile-expr (expr env)
  "Compile expression to IR - pure Habu version"
  (cond
    ;; Literal numbers
    ((numberp expr) (pure-compile-lit expr))
    ;; Symbols
    ((symbolp expr) (pure-compile-var expr env))
    ;; Not a list - treat as lit 0
    ((not (consp expr)) (pure-compile-lit 0))
    ;; Lists: check operator
    (t
     (let ((op (car expr)))
       (cond
         ;; (if test then else)
         ((eq op 'if)
          (pure-compile-if expr env))
         ;; (+ a b)
         ((eq op '+)
          (list 'add (pure-compile-expr (nth 1 expr) env)
                     (pure-compile-expr (nth 2 expr) env)))
         ;; (* a b)
         ((eq op '*)
          (list 'mul (pure-compile-expr (nth 1 expr) env)
                     (pure-compile-expr (nth 2 expr) env)))
         ;; (- a b)
         ((eq op '-)
          (list 'sub (pure-compile-expr (nth 1 expr) env)
                     (pure-compile-expr (nth 2 expr) env)))
         ;; (= a b)
         ((eq op '=)
          (list 'cmp-eq (pure-compile-expr (nth 1 expr) env)
                        (pure-compile-expr (nth 2 expr) env)))
         ;; Default: unknown, compile to lit 0
         (t (pure-compile-lit 0)))))))

;;; Export pure compiler
(export '(pure-compile-expr pure-append pure-reverse pure-length) :habu)

;;; ============================================================
;;; Expanded Compiler - More Expression Types
;;; ============================================================

(defun pure-compile-let (expr env)
  "Compile (let ((var val) ...) body) to IR"
  (let ((bindings (nth 1 expr))
        (body (nth 2 expr)))
    ;; Build new environment with bound variables
    (labels ((extend-env (binds e)
               (if (null binds)
                   e
                   (extend-env (cdr binds)
                               (cons (car (car binds)) e)))))
      (let ((new-env (extend-env bindings env)))
        ;; Compile each binding value
        (labels ((compile-bindings (binds acc)
                   (if (null binds)
                       (pure-reverse acc)
                       (let ((var (car (car binds)))
                             (val (nth 1 (car binds))))
                         (compile-bindings (cdr binds)
                                           (cons (pure-compile-expr val env) acc))))))
          (let ((val-irs (compile-bindings bindings nil))
                (body-ir (pure-compile-expr body new-env)))
            (list 'let-ir val-irs body-ir)))))))

(defun pure-compile-quote (expr)
  "Compile (quote x) to IR"
  (let ((val (nth 1 expr)))
    (if (symbolp val)
        (list 'symbol-lit (symbol-name val))
        (list 'lit val))))

(defun pure-compile-cons (expr env)
  "Compile (cons a b) to IR"
  (list 'cons-call
        (pure-compile-expr (nth 1 expr) env)
        (pure-compile-expr (nth 2 expr) env)))

(defun pure-compile-car (expr env)
  "Compile (car x) to IR"
  (list 'car-call (pure-compile-expr (nth 1 expr) env)))

(defun pure-compile-cdr (expr env)
  "Compile (cdr x) to IR"
  (list 'cdr-call (pure-compile-expr (nth 1 expr) env)))

(defun pure-compile-list (expr env)
  "Compile (list a b c) to IR"
  ;; Expand to nested cons: (cons a (cons b (cons c nil)))
  (labels ((expand-list (elems)
             (if (null elems)
                 (list 'lit 0)  ;; nil = 0
                 (list 'cons-call
                       (pure-compile-expr (car elems) env)
                       (expand-list (cdr elems))))))
    (expand-list (cdr expr))))  ;; Skip 'list operator

(defun pure-compile-progn (expr env)
  "Compile (progn e1 e2 e3) to IR"
  (labels ((compile-exprs (exprs acc)
             (if (null exprs)
                 (pure-reverse acc)
                 (compile-exprs (cdr exprs)
                                (cons (pure-compile-expr (car exprs) env) acc)))))
    (list 'progn-ir (compile-exprs (cdr expr) nil))))

;;; Enhanced pure-compile-expr with more operators
(defun pure-compile-expr-v2 (expr env)
  "Enhanced expression compiler - handles more forms"
  (cond
    ((numberp expr) (pure-compile-lit expr))
    ((symbolp expr) (pure-compile-var expr env))
    ((not (consp expr)) (pure-compile-lit 0))
    (t
     (let ((op (car expr)))
       (cond
         ((eq op 'if) (pure-compile-if expr env))
         ((eq op 'quote) (pure-compile-quote expr))
         ((eq op 'let) (pure-compile-let expr env))
         ((eq op 'progn) (pure-compile-progn expr env))
         ((eq op '+) (list 'add (pure-compile-expr (nth 1 expr) env)
                                (pure-compile-expr (nth 2 expr) env)))
         ((eq op '-) (list 'sub (pure-compile-expr (nth 1 expr) env)
                                (pure-compile-expr (nth 2 expr) env)))
         ((eq op '*) (list 'mul (pure-compile-expr (nth 1 expr) env)
                                (pure-compile-expr (nth 2 expr) env)))
         ((eq op '/) (list 'div (pure-compile-expr (nth 1 expr) env)
                                (pure-compile-expr (nth 2 expr) env)))
         ((eq op '=) (list 'cmp-eq (pure-compile-expr (nth 1 expr) env)
                                   (pure-compile-expr (nth 2 expr) env)))
         ((eq op '<) (list 'cmp-lt (pure-compile-expr (nth 1 expr) env)
                                   (pure-compile-expr (nth 2 expr) env)))
         ((eq op 'cons) (pure-compile-cons expr env))
         ((eq op 'car) (pure-compile-car expr env))
         ((eq op 'cdr) (pure-compile-cdr expr env))
         ((eq op 'list) (pure-compile-list expr env))
         (t (pure-compile-lit 0)))))))

;;; Export enhanced compiler
(export 'pure-compile-expr-v2 :habu)

;;; ============================================================
;;; Defun and Function Call Support
;;; ============================================================

;; Function environment: alist of (name . placeholder)
;; Used for forward references during two-pass compilation

(defun pure-collect-defuns (forms acc)
  "Pass 1: Collect all defun names from forms"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (pure-collect-defuns (cdr forms) (cons (list (cadr f)) acc)))
          ((and (consp f) (eq (car f) 'progn))
           (pure-collect-defuns (cdr forms)
                                (pure-collect-defuns (cdr f) acc)))
          (t (pure-collect-defuns (cdr forms) acc))))))

(defun pure-compile-defun (name params body env fenv)
  "Compile a single defun to (name params body-ir param-base)"
  (let* ((new-env (pure-extend-env params env))
         (pb (if params (pure-env-lookup (car params) new-env) 0))
         (body-ir (pure-compile-expr-full body new-env fenv)))
    (list name params body-ir pb)))

(defun pure-extend-env (params env)
  "Extend environment with parameter bindings"
  (if (null params)
      env
      (pure-extend-env (cdr params) (cons (car params) env))))

(defun pure-compile-all-defuns (forms env fenv acc)
  "Pass 2: Compile all defuns with complete fenv"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (let* ((nm (cadr f))
                  (ps (caddr f))
                  (body-forms (cdddr f))
                  (bd (if (null (cdr body-forms))
                          (car body-forms)
                          (cons 'progn body-forms)))
                  (cf (pure-compile-defun nm ps bd env fenv)))
             (pure-compile-all-defuns (cdr forms) env fenv (cons cf acc))))
          ((and (consp f) (eq (car f) 'progn))
           (pure-compile-all-defuns (cdr forms) env fenv
                                    (pure-compile-all-defuns (cdr f) env fenv acc)))
          (t (pure-compile-all-defuns (cdr forms) env fenv acc))))))

(defun pure-find-main-form (forms acc)
  "Find all non-defun forms and wrap in progn if multiple"
  (if (null forms)
      (if (null acc)
          (list 'lit 0)
          (if (null (cdr acc))
              (car acc)
              (cons 'progn (pure-reverse acc))))
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (pure-find-main-form (cdr forms) acc))
          ((and (consp f) (eq (car f) 'progn))
           (pure-find-main-form (cdr forms)
                                (pure-find-main-form (cdr f) acc)))
          (t (pure-find-main-form (cdr forms) (cons f acc)))))))

(defun pure-compile-call (expr env fenv)
  "Compile function call (fn arg1 arg2 ...)"
  (let ((fn-name (car expr))
        (args (cdr expr)))
    ;; Look up in fenv to check if it's a defined function
    (if (pure-fenv-lookup fn-name fenv)
        ;; User-defined function call
        (list 'call-fn fn-name
              (pure-compile-args args env fenv))
        ;; Unknown function - compile as lit 0 for now
        (list 'lit 0))))

(defun pure-fenv-lookup (name fenv)
  "Look up function in function environment"
  (if (null fenv)
      nil
      (if (eq (car (car fenv)) name)
          t
          (pure-fenv-lookup name (cdr fenv)))))

(defun pure-compile-args (args env fenv)
  "Compile list of arguments"
  (if (null args)
      nil
      (cons (pure-compile-expr-full (car args) env fenv)
            (pure-compile-args (cdr args) env fenv))))

;;; ============================================================
;;; Lambda and Funcall Support
;;; ============================================================

(defun pure-compile-lambda (expr env fenv)
  "Compile (lambda (params) body) to lambda-ir"
  (let* ((params (cadr expr))
         (body-forms (cddr expr))
         (body (if (null (cdr body-forms))
                   (car body-forms)
                   (cons 'progn body-forms)))
         (new-env (pure-extend-env params env))
         ;; Find free variables (captured from enclosing scope)
         (free-vars (pure-find-free-vars body params env))
         (body-ir (pure-compile-expr-full body new-env fenv)))
    (list 'lambda-ir params body-ir free-vars)))

(defun pure-find-free-vars (expr params env)
  "Find variables referenced in expr that are in env but not in params"
  (labels ((in-list (x lst)
             (if (null lst) nil
                 (if (eq x (car lst)) t
                     (in-list x (cdr lst)))))
           (find-in-expr (e acc)
             (cond
               ((symbolp e)
                (if (and (pure-env-lookup e env)
                         (not (in-list e params))
                         (not (in-list e acc)))
                    (cons e acc)
                    acc))
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)
               ((eq (car e) 'lambda)
                ;; Don't descend into nested lambdas
                acc)
               (t (find-in-list (cdr e) (find-in-expr (car e) acc)))))
           (find-in-list (lst acc)
             (if (null lst)
                 acc
                 (find-in-list (cdr lst) (find-in-expr (car lst) acc)))))
    (pure-reverse (find-in-expr expr nil))))

(defun pure-compile-funcall (expr env fenv)
  "Compile (funcall fn arg1 arg2 ...)"
  (let ((fn-expr (cadr expr))
        (args (cddr expr)))
    (list 'funcall-ir
          (pure-compile-expr-full fn-expr env fenv)
          (pure-compile-args args env fenv))))

;;; ============================================================
;;; Labels Support (Local Recursive Functions)
;;; ============================================================

(defun pure-compile-labels (expr env fenv)
  "Compile (labels ((fn1 (args) body) ...) body)"
  (let* ((bindings (cadr expr))
         (body (caddr expr))
         ;; Extract function names
         (fn-names (pure-extract-label-names bindings nil))
         ;; Extend fenv with local function names
         (local-fenv (pure-extend-fenv fn-names fenv))
         ;; Compile each local function
         (compiled-fns (pure-compile-label-fns bindings env local-fenv nil))
         ;; Compile body with extended fenv
         (body-ir (pure-compile-expr-full body env local-fenv)))
    (list 'labels-ir compiled-fns body-ir)))

(defun pure-extract-label-names (bindings acc)
  "Extract function names from labels bindings"
  (if (null bindings)
      (pure-reverse acc)
      (pure-extract-label-names (cdr bindings)
                                (cons (car (car bindings)) acc))))

(defun pure-extend-fenv (names fenv)
  "Extend function environment with names"
  (if (null names)
      fenv
      (pure-extend-fenv (cdr names) (cons (list (car names)) fenv))))

(defun pure-compile-label-fns (bindings env fenv acc)
  "Compile all label function bindings"
  (if (null bindings)
      (pure-reverse acc)
      (let* ((binding (car bindings))
             (name (car binding))
             (params (cadr binding))
             (body-forms (cddr binding))
             (body (if (null (cdr body-forms))
                       (car body-forms)
                       (cons 'progn body-forms)))
             (cf (pure-compile-defun name params body env fenv)))
        (pure-compile-label-fns (cdr bindings) env fenv (cons cf acc)))))

;;; ============================================================
;;; Full Expression Compiler (with defun/lambda/labels)
;;; ============================================================

(defun pure-compile-expr-full (expr env fenv)
  "Full expression compiler with function support"
  (cond
    ((numberp expr) (pure-compile-lit expr))
    ((symbolp expr)
     (if (eq expr 'nil)
         (list 'lit 0)
         (if (eq expr 't)
             (list 'symbol-lit "T")
             (pure-compile-var expr env))))
    ((not (consp expr)) (pure-compile-lit 0))
    (t
     (let ((op (car expr)))
       (cond
         ;; Control flow
         ((eq op 'if) (pure-compile-if-full expr env fenv))
         ((eq op 'cond) (pure-compile-cond expr env fenv))
         ((eq op 'when) (pure-compile-when expr env fenv))
         ((eq op 'unless) (pure-compile-unless expr env fenv))

         ;; Binding forms
         ((eq op 'let) (pure-compile-let-full expr env fenv))
         ((eq op 'let*) (pure-compile-let*-full expr env fenv))
         ((eq op 'progn) (pure-compile-progn-full expr env fenv))
         ((eq op 'quote) (pure-compile-quote expr))

         ;; Functions
         ((eq op 'lambda) (pure-compile-lambda expr env fenv))
         ((eq op 'funcall) (pure-compile-funcall expr env fenv))
         ((eq op 'labels) (pure-compile-labels expr env fenv))

         ;; Arithmetic
         ((eq op '+) (list 'add (pure-compile-expr-full (nth 1 expr) env fenv)
                               (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op '-) (list 'sub (pure-compile-expr-full (nth 1 expr) env fenv)
                               (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op '*) (list 'mul (pure-compile-expr-full (nth 1 expr) env fenv)
                               (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op '/) (list 'div (pure-compile-expr-full (nth 1 expr) env fenv)
                               (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op 'mod) (list 'mod-ir (pure-compile-expr-full (nth 1 expr) env fenv)
                                     (pure-compile-expr-full (nth 2 expr) env fenv)))

         ;; Comparisons
         ((eq op '=) (list 'cmp-eq (pure-compile-expr-full (nth 1 expr) env fenv)
                                   (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op '<) (list 'cmp-lt (pure-compile-expr-full (nth 1 expr) env fenv)
                                   (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op '>) (list 'cmp-gt (pure-compile-expr-full (nth 1 expr) env fenv)
                                   (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op '<=) (list 'cmp-le (pure-compile-expr-full (nth 1 expr) env fenv)
                                    (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op '>=) (list 'cmp-ge (pure-compile-expr-full (nth 1 expr) env fenv)
                                    (pure-compile-expr-full (nth 2 expr) env fenv)))

         ;; List operations
         ((eq op 'cons) (list 'cons-call
                              (pure-compile-expr-full (nth 1 expr) env fenv)
                              (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op 'car) (list 'car-call (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'cdr) (list 'cdr-call (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'cadr) (list 'car-call (list 'cdr-call (pure-compile-expr-full (nth 1 expr) env fenv))))
         ((eq op 'caddr) (list 'car-call (list 'cdr-call (list 'cdr-call (pure-compile-expr-full (nth 1 expr) env fenv)))))
         ((eq op 'list) (pure-compile-list-full expr env fenv))

         ;; Predicates
         ((eq op 'null) (list 'null-call (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'consp) (list 'consp-call (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'numberp) (list 'numberp-call (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'symbolp) (list 'symbolp-call (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'eq) (list 'eq-call (pure-compile-expr-full (nth 1 expr) env fenv)
                                     (pure-compile-expr-full (nth 2 expr) env fenv)))

         ;; Mutation
         ((eq op 'setq) (pure-compile-setq expr env fenv))

         ;; System calls
         ((eq op 'sys-exit) (list 'sys-exit-ir (pure-compile-expr-full (nth 1 expr) env fenv)))

         ;; Unknown - try as function call
         (t (if (symbolp op)
                (pure-compile-call expr env fenv)
                (pure-compile-lit 0))))))))

;; Helper functions for full compiler

(defun pure-compile-if-full (expr env fenv)
  (let ((test (pure-compile-expr-full (nth 1 expr) env fenv))
        (then (pure-compile-expr-full (nth 2 expr) env fenv))
        (else (if (nth 3 expr)
                  (pure-compile-expr-full (nth 3 expr) env fenv)
                  (list 'lit 0))))
    (list 'if-ir test then else)))

(defun pure-compile-cond (expr env fenv)
  "Compile (cond (test1 body1) (test2 body2) ...)"
  (let ((clauses (cdr expr)))
    (if (null clauses)
        (list 'lit 0)
        (let* ((clause (car clauses))
               (test (car clause))
               (body (cadr clause)))
          (if (eq test 't)
              (pure-compile-expr-full body env fenv)
              (list 'if-ir
                    (pure-compile-expr-full test env fenv)
                    (pure-compile-expr-full body env fenv)
                    (pure-compile-cond (cons 'cond (cdr clauses)) env fenv)))))))

(defun pure-compile-when (expr env fenv)
  (list 'if-ir
        (pure-compile-expr-full (nth 1 expr) env fenv)
        (pure-compile-progn-full (cons 'progn (cddr expr)) env fenv)
        (list 'lit 0)))

(defun pure-compile-unless (expr env fenv)
  (list 'if-ir
        (pure-compile-expr-full (nth 1 expr) env fenv)
        (list 'lit 0)
        (pure-compile-progn-full (cons 'progn (cddr expr)) env fenv)))

(defun pure-compile-let-full (expr env fenv)
  "Compile (let ((var val) ...) body ...)"
  (let ((bindings (nth 1 expr))
        (body-forms (cddr expr)))
    (labels ((extract-vars (binds acc)
               (if (null binds)
                   (pure-reverse acc)
                   (extract-vars (cdr binds) (cons (car (car binds)) acc))))
             (compile-vals (binds acc)
               (if (null binds)
                   (pure-reverse acc)
                   (compile-vals (cdr binds)
                                 (cons (pure-compile-expr-full (nth 1 (car binds)) env fenv) acc)))))
      (let* ((vars (extract-vars bindings nil))
             (val-irs (compile-vals bindings nil))
             (new-env (pure-extend-env vars env))
             (body (if (null (cdr body-forms))
                       (car body-forms)
                       (cons 'progn body-forms)))
             (body-ir (pure-compile-expr-full body new-env fenv)))
        (list 'let-ir val-irs body-ir)))))

(defun pure-compile-let*-full (expr env fenv)
  "Compile (let* ((var1 val1) (var2 val2)) body)"
  (let ((bindings (nth 1 expr))
        (body-forms (cddr expr)))
    (if (null bindings)
        (pure-compile-expr-full (if (null (cdr body-forms))
                                    (car body-forms)
                                    (cons 'progn body-forms))
                                env fenv)
        ;; Compile as nested lets
        (let* ((binding (car bindings))
               (var (car binding))
               (val (nth 1 binding))
               (val-ir (pure-compile-expr-full val env fenv))
               (new-env (cons var env))
               (rest-expr (list 'let* (cdr bindings) (cons 'progn body-forms)))
               (rest-ir (pure-compile-let*-full rest-expr new-env fenv)))
          (list 'let-ir (list val-ir) rest-ir)))))

(defun pure-compile-progn-full (expr env fenv)
  (labels ((compile-exprs (exprs acc)
             (if (null exprs)
                 (pure-reverse acc)
                 (compile-exprs (cdr exprs)
                                (cons (pure-compile-expr-full (car exprs) env fenv) acc)))))
    (list 'progn-ir (compile-exprs (cdr expr) nil))))

(defun pure-compile-list-full (expr env fenv)
  (labels ((expand-list (elems)
             (if (null elems)
                 (list 'lit 0)
                 (list 'cons-call
                       (pure-compile-expr-full (car elems) env fenv)
                       (expand-list (cdr elems))))))
    (expand-list (cdr expr))))

(defun pure-compile-setq (expr env fenv)
  "Compile (setq var val)"
  (let ((var (nth 1 expr))
        (val (nth 2 expr)))
    (let ((offset (pure-env-lookup var env)))
      (if offset
          (list 'setq-ir offset (pure-compile-expr-full val env fenv))
          (list 'lit 0)))))  ;; Unknown var

;;; ============================================================
;;; Full Program Compiler
;;; ============================================================

(defun pure-compile-forms (forms)
  "Compile forms to (defun-list . main-ir)"
  (let* ((fenv (pure-collect-defuns forms nil))
         (defuns (pure-compile-all-defuns forms nil fenv nil))
         (main-form (pure-find-main-form forms nil))
         (main-ir (pure-compile-expr-full main-form nil fenv)))
    (cons defuns main-ir)))

;;; Export full compiler
(export '(pure-compile-expr-full pure-compile-forms
          pure-compile-defun pure-compile-lambda pure-compile-labels) :habu)

;;; ============================================================
;;; Integration with Existing Codegen
;;; ============================================================

(defun pure-compile-to-bytecode (expr)
  "Compile expression to ARM64 bytecode using existing codegen.
   This bridges pure compiler → existing nc-codegen (which is already pure!)"
  (let ((ir (pure-compile-expr-v2 expr nil)))
    ;; Call existing nc-codegen (it's already pure - just builds byte lists!)
    ;; nc-codegen signature: (ir rtaddrs fnoffs temp-depth)
    (let ((code-with-markers (nc-codegen ir nil nil 0)))
      ;; Resolve markers to actual bytes
      (nc-resolve-calls code-with-markers nil))))

(defun pure-compile-program-simple (forms)
  "Compile simple program (single expression) to complete bytecode.
   Uses existing nc-codegen-main which adds prologue/epilogue."
  (if (null forms)
      nil
      (let ((main-expr (if (null (cdr forms))
                           (car forms)  ;; Single form
                           (cons 'progn forms))))  ;; Multiple forms → progn
        (let ((ir (pure-compile-expr-v2 main-expr nil)))
          ;; Use existing nc-codegen-main (adds prologue/epilogue)
          (nc-codegen-main ir nil)))))

;;; Self-hosting entry point
(defun pure-self-compile (source-path output-path)
  "Pure Habu self-hosting compiler entry point.
   Reads source, compiles with pure compiler, generates ARM64, writes executable."
  (let ((source (native-read-file source-path)))
    (if source
        (progn
          (sys-write 1 "Pure compiler: Reading source...\n" 35)
          (let ((forms (read-all source)))
            (sys-write 1 "Pure compiler: Compiling to bytecode...\n" 42)
            (let ((bytecode (pure-compile-program-simple forms)))
              (sys-write 1 "Pure compiler: Generated " 26)
              (sys-write 1 (number-to-string (pure-length bytecode)) 5)
              (sys-write 1 " bytes\n" 7)
              (sys-write 1 "Pure compiler: Linking to executable...\n" 42)
              ;; Use existing Mach-O linker
              (deliver-with-libsystem source output-path :verbose nil)
              (sys-write 1 "Success!\n" 9)
              (sys-exit 0))))
        (progn
          (sys-write 2 "Error: Cannot read source\n" 27)
          (sys-exit 1)))))

;;; Export self-hosting entry point
(export '(pure-compile-to-bytecode pure-compile-program-simple pure-self-compile) :habu)
