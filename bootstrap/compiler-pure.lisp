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

(defun pure-count-if (pred lst)
  "Count elements satisfying predicate"
  (labels ((count-iter (l n)
             (if (null l)
                 n
                 (count-iter (cdr l)
                             (if (funcall pred (car l))
                                 (+ n 1)
                                 n)))))
    (count-iter lst 0)))

(defun pure-remove-if (pred lst)
  "Remove elements satisfying predicate"
  (labels ((remove-iter (l acc)
             (if (null l)
                 (pure-reverse acc)
                 (remove-iter (cdr l)
                              (if (funcall pred (car l))
                                  acc
                                  (cons (car l) acc))))))
    (remove-iter lst nil)))

(defun pure-assoc (key alist)
  "Find (key . value) pair in alist using string= comparison"
  (if (null alist)
      nil
      (if (string= key (car (car alist)))
          (car alist)
          (pure-assoc key (cdr alist)))))

(defun pure-mapcar (fn lst)
  "Map function over list"
  (labels ((map-iter (l acc)
             (if (null l)
                 (pure-reverse acc)
                 (map-iter (cdr l) (cons (funcall fn (car l)) acc)))))
    (map-iter lst nil)))

(defun pure-fold-binop (ir-tag args env fenv)
  "Fold variadic operation into nested binary operations.
   (+ a b c) => (add (add a b) c)"
  (if (null (cdr args))
      ;; Single argument: just compile it
      (pure-compile-expr-full (car args) env fenv)
      ;; Multiple arguments: fold left
      (labels ((fold (remaining acc)
                 (if (null remaining)
                     acc
                     (fold (cdr remaining)
                           (list ir-tag acc (pure-compile-expr-full (car remaining) env fenv))))))
        (fold (cddr args)
              (list ir-tag
                    (pure-compile-expr-full (car args) env fenv)
                    (pure-compile-expr-full (cadr args) env fenv))))))

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

(defun pure-quote-ir (obj)
  "Build IR for quoted value - recursively builds cons-ir for lists"
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'lit 0))  ; nil = 0
    ((symbolp obj) (list 'symbol-lit (symbol-name obj)))
    ((consp obj) (list 'cons-ir (pure-quote-ir (car obj)) (pure-quote-ir (cdr obj))))
    ((stringp obj) (list 'str-lit obj))
    (t (list 'lit 0))))

(defun pure-compile-quote (expr)
  "Compile (quote x) to IR"
  (pure-quote-ir (nth 1 expr)))

(defun pure-compile-cons (expr env)
  "Compile (cons a b) to IR"
  (list 'cons-ir
        (pure-compile-expr (nth 1 expr) env)
        (pure-compile-expr (nth 2 expr) env)))

(defun pure-compile-car (expr env)
  "Compile (car x) to IR"
  (list 'car-ir (pure-compile-expr (nth 1 expr) env)))

(defun pure-compile-cdr (expr env)
  "Compile (cdr x) to IR"
  (list 'cdr-ir (pure-compile-expr (nth 1 expr) env)))

(defun pure-compile-list (expr env)
  "Compile (list a b c) to IR"
  ;; Expand to nested cons: (cons a (cons b (cons c nil)))
  (labels ((expand-list (elems)
             (if (null elems)
                 (list 'lit 0)  ;; nil = 0
                 (list 'cons-ir
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
  "Extend environment with parameter bindings - append to preserve offset consistency"
  (pure-append env params))

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
  "Compile (lambda (params) body) to lambda-ir.
   CRITICAL: Must include free-offsets for closure capture to work!"
  (let* ((params (cadr expr))
         (body-forms (cddr expr))
         (body (if (null (cdr body-forms))
                   (car body-forms)
                   (cons 'progn body-forms)))
         ;; Find free variables (captured from enclosing scope)
         (free-vars (pure-find-free-vars body params env))
         ;; CRITICAL: Get offsets for each free var in current env
         ;; These are needed by codegen to know where to capture from
         (free-offsets (pure-mapcar (lambda (v) (pure-env-lookup v env)) free-vars))
         ;; Build environment for body: free vars + params
         ;; Free vars come first (captured in closure env), then params
         ;; This matches the regular compiler's approach
         (num-free (pure-length free-vars))
         (body-env (pure-extend-env params (pure-extend-env free-vars nil)))
         ;; Compile body with extended env
         (body-ir (pure-compile-expr-full body body-env fenv)))
    ;; Return lambda-ir with 5 elements (matching regular compiler)
    (list 'lambda-ir params body-ir free-vars free-offsets)))

(defun pure-find-free-vars (expr params env)
  "Find variables referenced in expr that are in env but not in params or local bindings"
  (labels ((in-list (x lst)
             (if (null lst) nil
                 (if (eq x (car lst)) t
                     (in-list x (cdr lst)))))
           (get-let-vars (bindings acc)
             ;; Extract variable names from let bindings
             (if (null bindings)
                 acc
                 (get-let-vars (cdr bindings)
                               (if (consp (car bindings))
                                   (cons (car (car bindings)) acc)
                                   acc))))
           (find-in-expr (e bound acc)
             ;; bound = list of locally-bound variables to exclude
             (cond
               ((symbolp e)
                (if (and (pure-env-lookup e env)
                         (not (in-list e params))
                         (not (in-list e bound))
                         (not (in-list e acc)))
                    (cons e acc)
                    acc))
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)
               ((eq (car e) 'lambda)
                ;; Don't descend into nested lambdas
                acc)
               ;; Handle let/let* - add bound vars before descending into body
               ((or (eq (car e) 'let) (eq (car e) 'LET)
                    (eq (car e) 'let*) (eq (car e) 'LET*))
                (let* ((bindings (cadr e))
                       (body (cddr e))
                       (let-vars (get-let-vars bindings nil))
                       (new-bound (append let-vars bound))
                       ;; Find free vars in binding values (use old bound)
                       (acc2 (find-in-binding-vals bindings bound acc))
                       ;; Find free vars in body (use new bound)
                       (acc3 (find-in-list body new-bound acc2)))
                  acc3))
               (t (find-in-list (cdr e) bound (find-in-expr (car e) bound acc)))))
           (find-in-binding-vals (bindings bound acc)
             ;; Find free vars in let binding values
             (if (null bindings)
                 acc
                 (let ((b (car bindings)))
                   (if (and (consp b) (cadr b))
                       (find-in-binding-vals (cdr bindings) bound
                                             (find-in-expr (cadr b) bound acc))
                       (find-in-binding-vals (cdr bindings) bound acc)))))
           (find-in-list (lst bound acc)
             (if (null lst)
                 acc
                 (find-in-list (cdr lst) bound (find-in-expr (car lst) bound acc)))))
    (pure-reverse (find-in-expr expr nil nil))))

(defun pure-compile-funcall (expr env fenv)
  "Compile (funcall fn arg1 arg2 ...)"
  (let ((fn-expr (cadr expr))
        (args (cddr expr)))
    (list 'funcall-ir
          (pure-compile-expr-full fn-expr env fenv)
          (pure-compile-args args env fenv))))

;;; ============================================================
;;; Labels Support (Local Recursive Functions) - FNTAB Transformation
;;; ============================================================

;; Labels is transformed to:
;;   (let ((f nil) ...)
;;     (setq f (lambda (FNTAB params) (let ((f (car FNTAB))) body)))
;;     (let ((FNTAB (cons f nil)))
;;       (funcall f FNTAB args)))

(defvar *pure-gensym-counter* 0)

(defun pure-gensym (prefix)
  "Generate unique symbol"
  (setq *pure-gensym-counter* (+ *pure-gensym-counter* 1))
  (intern (format nil "~A~A" prefix *pure-gensym-counter*)))

(defun pure-compile-labels (expr env fenv)
  "Compile labels by transforming to let/setq/lambda/funcall with FNTAB"
  (let* ((bindings (cadr expr))
         (body-forms (cddr expr))
         (body (if (null (cdr body-forms)) (car body-forms) (cons 'progn body-forms)))
         (fn-names (pure-extract-label-names bindings nil))
         (fntab-var (pure-gensym "FNTAB"))
         ;; Transform to: (let ((f nil)) (setq f (lambda (FNTAB x) ...)) (let ((FNTAB (cons f nil))) main))
         (transformed (pure-transform-labels fn-names bindings body fntab-var)))
    ;; Compile the transformed expression
    (pure-compile-expr-full transformed env fenv)))

(defun pure-extract-label-names (bindings acc)
  "Extract function names from labels bindings"
  (if (null bindings)
      (pure-reverse acc)
      (pure-extract-label-names (cdr bindings)
                                (cons (car (car bindings)) acc))))

(defun pure-transform-labels (fn-names bindings body fntab-var)
  "Transform labels to let/setq/funcall with FNTAB"
  ;; Build let bindings: ((f nil) ...)
  (let* ((let-bindings (pure-map-nil-bindings fn-names nil))
         ;; Build FNTAB unpack bindings for inside lambdas
         (fntab-unpack (pure-build-fntab-unpack fn-names fntab-var 0 nil))
         ;; Build setq forms for each function
         (setq-forms (pure-build-setq-forms bindings fn-names fntab-var fntab-unpack nil))
         ;; Build FNTAB cons list
         (fntab-init (pure-build-fntab-init fn-names))
         ;; Rewrite main body
         (rewritten-body (pure-rewrite-labels-body body fn-names fntab-var))
         ;; Inner let for FNTAB
         (inner-let (list 'let (list (list fntab-var fntab-init)) rewritten-body))
         ;; Full expression
         (full-progn (pure-append setq-forms (list inner-let))))
    (list 'let let-bindings (cons 'progn full-progn))))

(defun pure-map-nil-bindings (names acc)
  "Build ((name nil) ...) list"
  (if (null names)
      (pure-reverse acc)
      (pure-map-nil-bindings (cdr names) (cons (list (car names) 'nil) acc))))

(defun pure-build-fntab-unpack (names fntab-var depth acc)
  "Build ((f (car FNTAB)) (g (car (cdr FNTAB))) ...) bindings"
  (if (null names)
      (pure-reverse acc)
      (let ((accessor (pure-wrap-cdr-car fntab-var depth)))
        (pure-build-fntab-unpack (cdr names) fntab-var (+ depth 1)
                                 (cons (list (car names) accessor) acc)))))

(defun pure-wrap-cdr-car (var depth)
  "Build (car (cdr (cdr ... var))) expression"
  (if (= depth 0)
      (list 'car var)
      (list 'car (pure-wrap-cdr var depth))))

(defun pure-wrap-cdr (var n)
  "Wrap var in n cdrs"
  (if (= n 0)
      var
      (list 'cdr (pure-wrap-cdr var (- n 1)))))

(defun pure-build-setq-forms (bindings fn-names fntab-var fntab-unpack acc)
  "Build setq forms for each function"
  (if (null bindings)
      (pure-reverse acc)
      (let* ((binding (car bindings))
             (fn-name (car binding))
             (params (cadr binding))
             (fn-body-forms (cddr binding))
             (fn-body (if (null (cdr fn-body-forms))
                          (car fn-body-forms)
                          (cons 'progn fn-body-forms)))
             ;; Rewrite body to pass FNTAB
             (rewritten (pure-rewrite-labels-body fn-body fn-names fntab-var))
             ;; Wrap in let for FNTAB unpack
             (wrapped-body (list 'let fntab-unpack rewritten))
             ;; Lambda with FNTAB as first param
             (lambda-expr (list 'lambda (cons fntab-var params) wrapped-body))
             (setq-form (list 'setq fn-name lambda-expr)))
        (pure-build-setq-forms (cdr bindings) fn-names fntab-var fntab-unpack
                               (cons setq-form acc)))))

(defun pure-build-fntab-init (names)
  "Build (cons f (cons g nil)) expression"
  (if (null names)
      'nil
      (list 'cons (car names) (pure-build-fntab-init (cdr names)))))

(defun pure-rewrite-labels-body (expr fn-names fntab-var)
  "Rewrite calls to labels functions to pass FNTAB"
  (cond
    ((null expr) nil)
    ((numberp expr) expr)
    ((symbolp expr) expr)
    ((not (consp expr)) expr)
    (t
     (let ((op (car expr)))
       (cond
         ;; If calling a labels function, rewrite to (funcall fn FNTAB args...)
         ((and (symbolp op) (pure-member op fn-names))
          (cons 'funcall
                (cons op
                      (cons fntab-var
                            (pure-rewrite-args (cdr expr) fn-names fntab-var)))))
         ;; Quote - don't descend
         ((eq op 'quote) expr)
         ;; lambda - only rewrite body, not params
         ((eq op 'lambda)
          (list 'lambda (cadr expr)
                (pure-rewrite-labels-body (caddr expr) fn-names fntab-var)))
         ;; let/let* - rewrite values and body
         ((or (eq op 'let) (eq op 'LET) (eq op 'let*) (eq op 'LET*))
          (let* ((bindings (cadr expr))
                 (body-forms (cddr expr))
                 (new-bindings (pure-rewrite-let-bindings bindings fn-names fntab-var)))
            (cons op (cons new-bindings
                           (pure-rewrite-args body-forms fn-names fntab-var)))))
         ;; Default: recursively rewrite all parts
         (t (pure-rewrite-args expr fn-names fntab-var)))))))

(defun pure-rewrite-args (args fn-names fntab-var)
  "Rewrite list of arguments"
  (if (null args)
      nil
      (cons (pure-rewrite-labels-body (car args) fn-names fntab-var)
            (pure-rewrite-args (cdr args) fn-names fntab-var))))

(defun pure-rewrite-let-bindings (bindings fn-names fntab-var)
  "Rewrite let binding values"
  (if (null bindings)
      nil
      (let ((b (car bindings)))
        (if (consp b)
            (cons (list (car b) (pure-rewrite-labels-body (cadr b) fn-names fntab-var))
                  (pure-rewrite-let-bindings (cdr bindings) fn-names fntab-var))
            (cons b (pure-rewrite-let-bindings (cdr bindings) fn-names fntab-var))))))

(defun pure-member (x lst)
  "Check if x is in lst"
  (if (null lst)
      nil
      (if (eq x (car lst))
          t
          (pure-member x (cdr lst)))))

(defun pure-extend-fenv (names fenv)
  "Extend function environment with names"
  (if (null names)
      fenv
      (pure-extend-fenv (cdr names) (cons (list (car names)) fenv))))

;;; ============================================================
;;; Full Expression Compiler (with defun/lambda/labels)
;;; ============================================================

(defun pure-compile-expr-full (expr env fenv)
  "Full expression compiler with function support"
  (cond
    ((numberp expr) (pure-compile-lit expr))
    ((stringp expr) (list 'str-lit expr))  ; String literals
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
         ;; Boolean operators - transform to if forms
         ((eq op 'and) (pure-compile-and expr env fenv))
         ((eq op 'or) (pure-compile-or expr env fenv))
         ((eq op 'not) (list 'cmp-eq (pure-compile-expr-full (cadr expr) env fenv) (list 'lit 0)))

         ;; Binding forms
         ((eq op 'let) (pure-compile-let-full expr env fenv))
         ((eq op 'let*) (pure-compile-let*-full expr env fenv))
         ((eq op 'progn) (pure-compile-progn-full expr env fenv))
         ((eq op 'quote) (pure-compile-quote expr))

         ;; Functions
         ((eq op 'lambda) (pure-compile-lambda expr env fenv))
         ((eq op 'funcall) (pure-compile-funcall expr env fenv))
         ((eq op 'labels) (pure-compile-labels expr env fenv))

         ;; Arithmetic (variadic support)
         ((eq op '+) (pure-fold-binop 'add (cdr expr) env fenv))
         ((eq op '-) (pure-fold-binop 'sub (cdr expr) env fenv))
         ((eq op '*) (pure-fold-binop 'mul (cdr expr) env fenv))
         ((eq op '/) (pure-fold-binop 'div (cdr expr) env fenv))
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

         ;; List operations - use -ir suffix to match codegen
         ((eq op 'cons) (list 'cons-ir
                              (pure-compile-expr-full (nth 1 expr) env fenv)
                              (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op 'car) (list 'car-ir (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'cdr) (list 'cdr-ir (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'cadr) (list 'car-ir (list 'cdr-ir (pure-compile-expr-full (nth 1 expr) env fenv))))
         ((eq op 'caddr) (list 'car-ir (list 'cdr-ir (list 'cdr-ir (pure-compile-expr-full (nth 1 expr) env fenv)))))
         ((eq op 'list) (pure-compile-list-full expr env fenv))

         ;; Predicates - use cmp-eq/get-tag to match main compiler codegen
         ;; null: compare value to nil (0)
         ((eq op 'null) (list 'cmp-eq (pure-compile-expr-full (nth 1 expr) env fenv) (list 'lit 0)))
         ;; consp: compare tag to 1 (cons tag)
         ((eq op 'consp) (list 'cmp-eq (list 'get-tag (pure-compile-expr-full (nth 1 expr) env fenv)) (list 'lit 1)))
         ;; numberp: compare tag to 0 (fixnum tag)
         ((eq op 'numberp) (list 'cmp-eq (list 'get-tag (pure-compile-expr-full (nth 1 expr) env fenv)) (list 'lit 0)))
         ;; symbolp: compare tag to 2 (symbol tag)
         ((eq op 'symbolp) (list 'cmp-eq (list 'get-tag (pure-compile-expr-full (nth 1 expr) env fenv)) (list 'lit 2)))
         ;; stringp: compare tag to 4 (string tag)
         ((eq op 'stringp) (list 'cmp-eq (list 'get-tag (pure-compile-expr-full (nth 1 expr) env fenv)) (list 'lit 4)))
         ;; vectorp: compare tag to 3 (vector tag)
         ((eq op 'vectorp) (list 'cmp-eq (list 'get-tag (pure-compile-expr-full (nth 1 expr) env fenv)) (list 'lit 3)))
         ;; eq: compare two values directly
         ((eq op 'eq) (list 'cmp-eq (pure-compile-expr-full (nth 1 expr) env fenv)
                                   (pure-compile-expr-full (nth 2 expr) env fenv)))

         ;; String operations - use -ir suffix to match codegen
         ((eq op 'string-length) (list 'string-length-ir (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'string-ref) (list 'string-ref-ir
                                    (pure-compile-expr-full (nth 1 expr) env fenv)
                                    (pure-compile-expr-full (nth 2 expr) env fenv)))

         ;; Vector operations - use -ir suffix to match codegen
         ((eq op 'make-vector) (list 'make-vector-ir (pure-compile-expr-full (nth 1 expr) env fenv)))
         ((eq op 'vector-ref) (list 'vector-ref-ir
                                    (pure-compile-expr-full (nth 1 expr) env fenv)
                                    (pure-compile-expr-full (nth 2 expr) env fenv)))
         ((eq op 'vector-set) (list 'vector-set-ir
                                    (pure-compile-expr-full (nth 1 expr) env fenv)
                                    (pure-compile-expr-full (nth 2 expr) env fenv)
                                    (pure-compile-expr-full (nth 3 expr) env fenv)))
         ((eq op 'vector-length) (list 'vector-length-ir (pure-compile-expr-full (nth 1 expr) env fenv)))

         ;; Make string from vector (for reader)
         ((eq op 'make-string-from-vector) (list 'make-string-from-vector-ir
                                                  (pure-compile-expr-full (nth 1 expr) env fenv)))

         ;; Mutation
         ((eq op 'setq) (pure-compile-setq expr env fenv))

         ;; System calls
         ((eq op 'sys-exit) (list 'sys-exit-ir (pure-compile-expr-full (nth 1 expr) env fenv)))

         ;; Unknown - try as function call or inline lambda
         (t (cond
              ((symbolp op) (pure-compile-call expr env fenv))
              ;; Inline lambda call: ((lambda (x) ...) arg)
              ((and (consp op) (eq (car op) 'lambda))
               (list 'funcall-ir
                     (pure-compile-lambda op env fenv)
                     (pure-compile-args (cdr expr) env fenv)))
              (t (pure-compile-lit 0)))))))))

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

(defun pure-compile-and (expr env fenv)
  "Compile (and a b c ...) to nested if forms"
  (let ((args (cdr expr)))
    (cond
      ((null args) (list 'symbol-lit "T"))  ; (and) = t
      ((null (cdr args))  ; single arg - just compile it
       (pure-compile-expr-full (car args) env fenv))
      (t  ; multiple args - (if a (and b c ...) nil)
       (list 'if-ir
             (pure-compile-expr-full (car args) env fenv)
             (pure-compile-and (cons 'and (cdr args)) env fenv)
             (list 'lit 0))))))

(defun pure-compile-or (expr env fenv)
  "Compile (or a b c ...) to nested if forms"
  (let ((args (cdr expr)))
    (cond
      ((null args) (list 'lit 0))  ; (or) = nil
      ((null (cdr args))  ; single arg - just compile it
       (pure-compile-expr-full (car args) env fenv))
      (t  ; multiple args - (if a a (or b c ...)) but need temp to avoid double eval
       ;; Simplified: (let ((tmp a)) (if tmp tmp (or b c ...)))
       ;; For now, just use nested ifs assuming no side effects
       (list 'if-ir
             (pure-compile-expr-full (car args) env fenv)
             (pure-compile-expr-full (car args) env fenv)
             (pure-compile-or (cons 'or (cdr args)) env fenv))))))

(defun pure-compile-let-full (expr env fenv)
  "Compile (let ((var val) ...) body ...) to (let-ir vals body count offs)"
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
                                 (cons (pure-compile-expr-full (nth 1 (car binds)) env fenv) acc))))
             (make-offs (n base acc)
               ;; Generate offsets starting at base: (base, base+1, ...)
               (if (= n 0)
                   (pure-reverse acc)
                   (make-offs (- n 1) (+ base 1) (cons base acc)))))
      (let* ((vars (extract-vars bindings nil))
             (val-irs (compile-vals bindings nil))
             (base-offset (pure-length env))  ;; Storage starts after current env
             (offs (make-offs (pure-length bindings) base-offset nil))
             (new-env (pure-extend-env vars env))
             (body (if (null (cdr body-forms))
                       (car body-forms)
                       (cons 'progn body-forms)))
             (body-ir (pure-compile-expr-full body new-env fenv)))
        (list 'let-ir val-irs body-ir (pure-length bindings) offs)))))

(defun pure-compile-let*-full (expr env fenv)
  "Compile (let* ((var1 val1) (var2 val2)) body) to nested let-irs with count/offs"
  (let ((bindings (nth 1 expr))
        (body-forms (cddr expr)))
    (if (null bindings)
        (pure-compile-expr-full (if (null (cdr body-forms))
                                    (car body-forms)
                                    (cons 'progn body-forms))
                                env fenv)
        ;; Compile as nested lets, each with 1 binding
        (let* ((binding (car bindings))
               (var (car binding))
               (val (nth 1 binding))
               (val-ir (pure-compile-expr-full val env fenv))
               (off (pure-length env))  ;; Storage offset = current env length
               (new-env (pure-append env (list var)))  ;; Append to keep offset consistency
               (rest-expr (list 'let* (cdr bindings) (cons 'progn body-forms)))
               (rest-ir (pure-compile-let*-full rest-expr new-env fenv)))
          ;; (let-ir vals body count offs)
          (list 'let-ir (list val-ir) rest-ir 1 (list off))))))

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
                 (list 'cons-ir
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
  "Compile forms to (defun-list main-ir) - proper list like main compiler"
  (let* ((fenv (pure-collect-defuns forms nil))
         (defuns (pure-compile-all-defuns forms nil fenv nil))
         (main-form (pure-find-main-form forms nil))
         (main-ir (pure-compile-expr-full main-form nil fenv)))
    (list defuns main-ir)))

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
   Reads source, compiles with pure compiler, generates ARM64, writes executable.
   This function is designed to be compiled to native code and run standalone."
  (let ((source (native-read-file source-path)))
    (if source
        (progn
          ;; Use pure-deliver which uses the pure compiler (no SBCL dependencies)
          (pure-deliver source output-path)
          (sys-exit 0))
        (progn
          (sys-exit 1)))))

;;; ============================================================
;;; Full Program Compilation (with functions)
;;; ============================================================

(defun pure-compile-program (forms)
  "Compile forms to complete ARM64 bytecode with function linking.
   This is the full pipeline: parse → IR → lift-lambdas → codegen → link.
   Returns flat bytecode ready for Mach-O wrapping."
  (reset-symbol-table)
  (let* ((r (pure-compile-forms forms))
         (defun-fns-raw (car r))
         (mir-raw (cadr r))
         ;; Add nil for free-vars to match main compiler format
         ;; Format: (name params body-ir param-base free-vars)
         (defun-fns (mapcar (lambda (d)
                              (list (first d) (second d) (third d) (fourth d) nil))
                            defun-fns-raw)))
    ;; Lift lambdas from main IR
    (let* ((mvb-result (lift-lambdas mir-raw))
           (mir (car mvb-result))
           (main-lambdas (cdr mvb-result)))
      ;; Lift lambdas from all defun bodies
      (let* ((mvb-result2 (lift-lambdas-from-fns defun-fns nil nil))
             (lifted-defuns (car mvb-result2))
             (defun-lambdas (cdr mvb-result2))
             ;; Combine: defuns + main-lambdas + defun-lambdas
             (fns (append lifted-defuns main-lambdas defun-lambdas)))
        (if (null fns)
            ;; No functions - simple case
            (resolve-calls (codegen-main mir nil) nil)
            ;; Has functions - need linking
            (let* ((main-code-temp (append (prologue)
                                           (codegen mir nil nil 0)
                                           (epilogue)))
                   (main-size (code-size main-code-temp))
                   (fnoffs (build-fnoffs fns main-size nil))
                   (main-code (append (prologue)
                                      (codegen mir nil fnoffs 0)
                                      (epilogue)))
                   (fn-code (codegen-all-fns fns nil fnoffs nil))
                   (all-code (append main-code fn-code)))
              (resolve-calls all-code fnoffs)))))))

;;; ============================================================
;;; Pure Delivery Helper Functions (no CL runtime dependencies)
;;; ============================================================

(defun pure-collect-extern-calls (code)
  "Collect extern call markers from code. Returns ((name . pos) ...)"
  (labels ((collect (items acc)
             (if (null items)
                 (pure-reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item) (eq (car item) :extern-call))
                       (collect (cdr items) (cons (cons (cadr item) (caddr item)) acc))
                       (collect (cdr items) acc))))))
    (collect code nil)))

(defun pure-get-unique-imports (extern-calls)
  "Get unique import names from extern calls list"
  (labels ((unique (calls seen acc)
             (if (null calls)
                 (pure-reverse acc)
                 (let ((name (car (car calls))))
                   (if (pure-member name seen)
                       (unique (cdr calls) seen acc)
                       (unique (cdr calls) (cons name seen) (cons name acc)))))))
    (unique extern-calls nil nil)))

(defun pure-string= (s1 s2)
  "Compare two strings for equality"
  (string= s1 s2))

(defun pure-assoc-string (key alist)
  "Find entry in alist with string key"
  (if (null alist)
      nil
      (if (pure-string= key (car (car alist)))
          (car alist)
          (pure-assoc-string key (cdr alist)))))

(defun pure-flatten-extern-calls (code stub-alist code-base-addr)
  "Replace extern call markers with BL instructions using assoc list.
   Returns (flat-code . extern-positions)
   Note: resolve-calls emits markers followed by 3 zeros - must skip them."
  (labels ((flatten (items result positions skip-count)
             (cond
               ;; Done
               ((null items)
                (cons (pure-reverse result) (pure-reverse positions)))
               ;; Skip placeholder zeros after extern-call marker
               ((> skip-count 0)
                (flatten (cdr items) result positions (- skip-count 1)))
               ;; Extern call marker - emit BL, skip next 3 zeros
               ((and (consp (car items)) (eq (car (car items)) :extern-call))
                (let* ((item (car items))
                       (name (cadr item))
                       (pos (caddr item))
                       (bl-addr (+ code-base-addr pos))
                       (entry (pure-assoc-string name stub-alist))
                       (stub-addr (if entry (cdr entry) 0))
                       (rel-offset (- stub-addr bl-addr))
                       (off-s (ash rel-offset -2))
                       (off-m (logand off-s #x3FFFFFF))
                       (bl-instr (logior #x94000000 off-m))
                       ;; Emit BL in little-endian
                       (b0 (logand bl-instr #xFF))
                       (b1 (logand (ash bl-instr -8) #xFF))
                       (b2 (logand (ash bl-instr -16) #xFF))
                       (b3 (logand (ash bl-instr -24) #xFF)))
                  (flatten (cdr items)
                           (cons b3 (cons b2 (cons b1 (cons b0 result))))
                           (cons (cons name pos) positions)
                           3)))  ; Skip next 3 zeros
               ;; Regular byte
               (t
                (flatten (cdr items) (cons (car items) result) positions 0)))))
    (flatten code nil nil 0)))

(defun pure-build-stub-alist (imports stubs-offset stub-size)
  "Build ((name . offset) ...) alist for stub map"
  (labels ((build (remaining i acc)
             (if (null remaining)
                 (pure-reverse acc)
                 (build (cdr remaining) (+ i 1)
                        (cons (cons (car remaining) (+ stubs-offset (* i stub-size))) acc)))))
    (build imports 0 nil)))

(defun pure-is-extern-marker (x)
  "Check if x is an extern-call marker"
  (and (consp x) (eq (car x) :extern-call)))

(defun pure-deliver (source output-path)
  "Compile source string to native executable using pure compiler.
   This uses the full extern-call flattening pipeline.
   Uses only pure functions - no hash tables or CL runtime."
  (let* ((forms (read-all source))
         (bytes-with-markers (pure-compile-program forms))
         ;; Collect extern calls and get unique imports
         (extern-calls (pure-collect-extern-calls bytes-with-markers))
         (imports (pure-get-unique-imports extern-calls))
         (wrapper-size 68))  ; 17 instructions * 4 bytes

    ;; Always use imports path for consistent Mach-O structure
    (let ((imports (if (null imports) '("_exit") imports)))

      ;; Calculate stub offsets BEFORE flattening
      (let* ((num-imports (pure-length imports))
             (stubs-total (if (> num-imports 0) (* num-imports 12) 0))
             (code-offset #x400)
             ;; Calculate exact flattened code size
             (num-markers (pure-count-if #'pure-is-extern-marker bytes-with-markers))
             (non-marker-bytes (pure-remove-if #'pure-is-extern-marker bytes-with-markers))
             (exact-flat-size (+ (pure-length non-marker-bytes) (* num-markers 4)))
             (exact-code-size (+ exact-flat-size wrapper-size))
             (stubs-offset (+ code-offset exact-code-size))
             (stub-size 12))

        ;; Build stub offset alist (instead of hash table)
        (let* ((stub-alist (pure-build-stub-alist imports stubs-offset stub-size))
               ;; Flatten with correct BL instructions
               (flatten-result (pure-flatten-extern-calls bytes-with-markers stub-alist (+ code-offset wrapper-size)))
               (flat-code (car flatten-result)))

          ;; Calculate heap page offset
          (let* ((total-size (+ (pure-length flat-code) wrapper-size))
                 (stubs-end (+ code-offset total-size stubs-total))
                 (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
                 (text-pages-4kb (/ text-vmsize #x1000))
                 (data-const-pages-4kb (/ #x4000 #x1000))
                 (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
                 (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code heap-page-offset)))

            ;; Write Mach-O executable
            (write-macho-executable-with-imports-and-heap output-path wrapped-code imports 1048576)
            ;; Make executable
            #+sbcl (sb-ext:run-program "/bin/chmod" (list "+x" output-path)
                                        :output nil :error nil :wait t)))))))

;;; Export self-hosting entry point
(export '(pure-compile-to-bytecode pure-compile-program-simple pure-self-compile
          pure-compile-program pure-deliver) :habu)
