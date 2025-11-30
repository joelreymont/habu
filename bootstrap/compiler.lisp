;;; Pure Habu Compiler - Uses ONLY Habu primitives (no SBCL dependencies)
;;; No multiple-value-bind, no values, no loop, no format
;;; This can be compiled to native and run without SBCL

#+sbcl (in-package :habu)

;;; ============================================================
;;; Core Helpers (Pure Habu)
;;; ============================================================

#-sbcl
(defun append (lst1 lst2)
  "Append two lists without using CL append"
  (labels ((append-iter (l acc)
             (if (null l)
                 acc
                 (append-iter (cdr l) (cons (car l) acc)))))
    (append-iter (reverse-helper lst1 nil) lst2)))

#-sbcl
(defun reverse-helper (lst acc)
  "Tail-recursive reverse helper - defined early for use by append"
  (if (null lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

#-sbcl
(defun reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

#-sbcl
(defun length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

#-sbcl
(defun nth (n lst)
  "Get nth element"
  (if (= n 0)
      (car lst)
      (nth (- n 1) (cdr lst))))

#-sbcl
(defun count-if (pred lst)
  "Count elements satisfying predicate"
  (labels ((count-iter (l n)
             (if (null l)
                 n
                 (count-iter (cdr l)
                             (if (funcall pred (car l))
                                 (+ n 1)
                                 n)))))
    (count-iter lst 0)))

#-sbcl
(defun remove-if (pred lst)
  "Remove elements satisfying predicate"
  (labels ((remove-iter (l acc)
             (if (null l)
                 (reverse acc)
                 (remove-iter (cdr l)
                              (if (funcall pred (car l))
                                  acc
                                  (cons (car l) acc))))))
    (remove-iter lst nil)))

;; String comparison helper - no closures to avoid labels/closure bugs
#-sbcl
(defun string-equal-iter (s1 s2 i len)
  "Internal: compare strings starting at index i"
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string-equal-iter s1 s2 (+ i 1) len)
          nil)))

#-sbcl
(defun string-equal (s1 s2)
  "Compare two strings character by character - pure Habu implementation"
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (string-equal-iter s1 s2 0 len1)
        nil)))

#-sbcl
(defun assoc (key alist)
  "Find (key . value) pair in alist using string comparison"
  (if (null alist)
      nil
      (if (string-equal key (car (car alist)))
          (car alist)
          (assoc key (cdr alist)))))

#-sbcl
(defun mapcar (fn lst)
  "Map function over list"
  (labels ((map-iter (l acc)
             (if (null l)
                 (reverse acc)
                 (map-iter (cdr l) (cons (funcall fn (car l)) acc)))))
    (map-iter lst nil)))

(defun fold-binop (ir-tag args env fenv)
  "Fold variadic operation into nested binary operations.
   (+ a b c) => (add (add a b) c)"
  (if (null (cdr args))
      ;; Single argument: just compile it
      (compile-expr-full (car args) env fenv)
      ;; Multiple arguments: fold left
      (labels ((fold (remaining acc)
                 (if (null remaining)
                     acc
                     (fold (cdr remaining)
                           (list ir-tag acc (compile-expr-full (car remaining) env fenv))))))
        (fold (cddr args)
              (list ir-tag
                    (compile-expr-full (car args) env fenv)
                    (compile-expr-full (cadr args) env fenv))))))

;;; ============================================================
;;; Pure Compiler Core
;;; ============================================================

(defun compile-lit (val)
  "Compile literal to IR"
  (list 'lit val))

(defun compile-var (sym env)
  "Compile variable reference"
  (let ((offset (env-lookup sym env)))
    (if offset
        (list 'var offset)
        (list 'lit 0))))  ;; Unknown var = 0

(defun env-lookup (sym env)
  "Look up symbol in environment, return offset or nil"
  (labels ((search-env (e offset)
             (if (null e)
                 nil
                 (if (eq (car e) sym)
                     offset
                     (search-env (cdr e) (+ offset 1))))))
    (search-env env 0)))

(defun compile-if (expr env)
  "Compile (if test then else) to IR"
  (let ((test (compile-expr (nth 1 expr) env))
        (then (compile-expr (nth 2 expr) env))
        (else (compile-expr (nth 3 expr) env)))
    (list 'if-ir test then else)))

(defun compile-expr (expr env)
  "Compile expression to IR - pure Habu version"
  (cond
    ;; Literal numbers
    ((numberp expr) (compile-lit expr))
    ;; Symbols
    ((symbolp expr) (compile-var expr env))
    ;; Not a list - treat as lit 0
    ((not (consp expr)) (compile-lit 0))
    ;; Lists: check operator (avoid let inside cond - causes crash)
    ((eq (car expr) 'if)
     (compile-if expr env))
    ((eq (car expr) '+)
     (list 'add-ir (compile-expr (nth 1 expr) env)
                   (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '*)
     (list 'mul-ir (compile-expr (nth 1 expr) env)
                   (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '-)
     (list 'sub-ir (compile-expr (nth 1 expr) env)
                   (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '=)
     (list 'cmp-eq (compile-expr (nth 1 expr) env)
                   (compile-expr (nth 2 expr) env)))
    ;; Default: unknown, compile to lit 0
    (t (compile-lit 0))))

;;; Export pure compiler
#+sbcl (export '(compile-expr append reverse length) :habu)

;;; ============================================================
;;; Expanded Compiler - More Expression Types
;;; ============================================================

(defun compile-let (expr env)
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
                       (reverse acc)
                       (let ((var (car (car binds)))
                             (val (nth 1 (car binds))))
                         (compile-bindings (cdr binds)
                                           (cons (compile-expr val env) acc))))))
          (let ((val-irs (compile-bindings bindings nil))
                (body-ir (compile-expr body new-env)))
            (list 'let-ir val-irs body-ir)))))))

(defun quote-ir (obj)
  "Build IR for quoted value - recursively builds cons-ir for lists"
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'lit 0))  ; nil = 0
    ((symbolp obj) (list 'sym-lit (symbol-name obj)))
    ((consp obj) (list 'cons-ir (quote-ir (car obj)) (quote-ir (cdr obj))))
    ((stringp obj) (list 'str-lit obj))
    (t (list 'lit 0))))

(defun compile-quote (expr)
  "Compile (quote x) to IR"
  (quote-ir (nth 1 expr)))

(defun compile-cons (expr env)
  "Compile (cons a b) to IR"
  (list 'cons-ir
        (compile-expr (nth 1 expr) env)
        (compile-expr (nth 2 expr) env)))

(defun compile-car (expr env)
  "Compile (car x) to IR"
  (list 'car-ir (compile-expr (nth 1 expr) env)))

(defun compile-cdr (expr env)
  "Compile (cdr x) to IR"
  (list 'cdr-ir (compile-expr (nth 1 expr) env)))

(defun compile-list (expr env)
  "Compile (list a b c) to IR"
  ;; Expand to nested cons: (cons a (cons b (cons c nil)))
  (labels ((expand-list (elems)
             (if (null elems)
                 (list 'lit 0)  ;; nil = 0
                 (list 'cons-ir
                       (compile-expr (car elems) env)
                       (expand-list (cdr elems))))))
    (expand-list (cdr expr))))  ;; Skip 'list operator

(defun compile-progn (expr env)
  "Compile (progn e1 e2 e3) to IR"
  (labels ((compile-exprs (exprs acc)
             (if (null exprs)
                 (reverse acc)
                 (compile-exprs (cdr exprs)
                                (cons (compile-expr (car exprs) env) acc)))))
    (list 'progn-ir (compile-exprs (cdr expr) nil))))

;;; Enhanced compile-expr with more operators
(defun compile-expr-v2 (expr env)
  "Enhanced expression compiler - handles more forms"
  (cond
    ((numberp expr) (compile-lit expr))
    ((symbolp expr) (compile-var expr env))
    ((not (consp expr)) (compile-lit 0))
    ;; Lists: check operator (avoid let inside cond - causes crash)
    ((eq (car expr) 'if) (compile-if expr env))
    ((eq (car expr) 'quote) (compile-quote expr))
    ((eq (car expr) 'let) (compile-let expr env))
    ((eq (car expr) 'progn) (compile-progn expr env))
    ((eq (car expr) '+) (list 'add-ir (compile-expr (nth 1 expr) env)
                                      (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '-) (list 'sub-ir (compile-expr (nth 1 expr) env)
                                      (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '*) (list 'mul-ir (compile-expr (nth 1 expr) env)
                                      (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '/) (list 'div-ir (compile-expr (nth 1 expr) env)
                                      (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '=) (list 'cmp-eq (compile-expr (nth 1 expr) env)
                                      (compile-expr (nth 2 expr) env)))
    ((eq (car expr) '<) (list 'cmp-lt (compile-expr (nth 1 expr) env)
                                      (compile-expr (nth 2 expr) env)))
    ((eq (car expr) 'cons) (compile-cons expr env))
    ((eq (car expr) 'car) (compile-car expr env))
    ((eq (car expr) 'cdr) (compile-cdr expr env))
    ((eq (car expr) 'list) (compile-list expr env))
    (t (compile-lit 0))))

;;; Export enhanced compiler
#+sbcl (export 'compile-expr-v2 :habu)

;;; ============================================================
;;; Defun and Function Call Support
;;; ============================================================

;; Function environment: alist of (name . placeholder)
;; Used for forward references during two-pass compilation

(defun collect-defuns (forms acc)
  "Pass 1: Collect all defun names from forms"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (collect-defuns (cdr forms) (cons (list (cadr f)) acc)))
          ((and (consp f) (eq (car f) 'progn))
           (collect-defuns (cdr forms)
                                (collect-defuns (cdr f) acc)))
          (t (collect-defuns (cdr forms) acc))))))

(defun compile-defun (name params body env fenv)
  "Compile a single defun to (name params body-ir param-base)"
  (let* ((new-env (extend-env params env))
         (pb (if params (env-lookup (car params) new-env) 0))
         (body-ir (compile-expr-full body new-env fenv)))
    (list name params body-ir pb)))

(defun extend-env (params env)
  "Extend environment with parameter bindings - append to preserve offset consistency"
  (append env params))

(defun compile-all-defuns (forms env fenv acc)
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
                  (cf (compile-defun nm ps bd env fenv)))
             (compile-all-defuns (cdr forms) env fenv (cons cf acc))))
          ((and (consp f) (eq (car f) 'progn))
           (compile-all-defuns (cdr forms) env fenv
                                    (compile-all-defuns (cdr f) env fenv acc)))
          (t (compile-all-defuns (cdr forms) env fenv acc))))))

(defun find-main-form (forms acc)
  "Find all non-defun forms and wrap in progn if multiple"
  (if (null forms)
      (if (null acc)
          (list 'lit 0)
          (if (null (cdr acc))
              (car acc)
              (cons 'progn (reverse acc))))
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (find-main-form (cdr forms) acc))
          ((and (consp f) (eq (car f) 'progn))
           (find-main-form (cdr forms)
                                (find-main-form (cdr f) acc)))
          (t (find-main-form (cdr forms) (cons f acc)))))))

(defun compile-call (expr env fenv)
  "Compile function call (fn arg1 arg2 ...)"
  (let ((fn-name (car expr))
        (args (cdr expr)))
    ;; Look up in fenv to check if it's a defined function
    (if (fenv-lookup fn-name fenv)
        ;; User-defined function call
        (list 'call-fn fn-name
              (compile-args args env fenv))
        ;; Unknown function - compile as lit 0 for now
        (list 'lit 0))))

(defun fenv-lookup (name fenv)
  "Look up function in function environment"
  (if (null fenv)
      nil
      (if (eq (car (car fenv)) name)
          t
          (fenv-lookup name (cdr fenv)))))

(defun compile-args (args env fenv)
  "Compile list of arguments"
  (if (null args)
      nil
      (cons (compile-expr-full (car args) env fenv)
            (compile-args (cdr args) env fenv))))

;;; ============================================================
;;; Lambda and Funcall Support
;;; ============================================================

(defun compile-lambda (expr env fenv)
  "Compile (lambda (params) body) to lambda-ir.
   CRITICAL: Must include free-offsets for closure capture to work!"
  (let* ((params (cadr expr))
         (body-forms (cddr expr))
         (body (if (null (cdr body-forms))
                   (car body-forms)
                   (cons 'progn body-forms)))
         ;; Find free variables (captured from enclosing scope)
         (free-vars (find-free-vars body params env))
         ;; CRITICAL: Get offsets for each free var in current env
         ;; These are needed by codegen to know where to capture from
         (free-offsets (mapcar (lambda (v) (env-lookup v env)) free-vars))
         ;; Build environment for body: free vars + params
         ;; Free vars come first (captured in closure env), then params
         ;; This matches the regular compiler's approach
         (num-free (length free-vars))
         (body-env (extend-env params (extend-env free-vars nil)))
         ;; Compile body with extended env
         (body-ir (compile-expr-full body body-env fenv)))
    ;; Return lambda-ir with 5 elements (matching regular compiler)
    (list 'lambda-ir params body-ir free-vars free-offsets)))

(defun find-free-vars (expr params env)
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
                (if (and (env-lookup e env)
                         (not (in-list e params))
                         (not (in-list e bound))
                         (not (in-list e acc)))
                    (cons e acc)
                    acc))
               ((not (consp e)) acc)
               ((eq (car e) 'quote) acc)
               ((eq (car e) 'lambda)
                ;; Descend into nested lambdas to find vars they need
                ;; Add lambda params to bound list
                (let* ((lambda-params (cadr e))
                       (lambda-body (cddr e))
                       (new-bound (append lambda-params bound)))
                  (find-in-list lambda-body new-bound acc)))
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
    (reverse (find-in-expr expr nil nil))))

(defun compile-funcall (expr env fenv)
  "Compile (funcall fn arg1 arg2 ...)"
  (let ((fn-expr (cadr expr))
        (args (cddr expr)))
    (list 'funcall-ir
          (compile-expr-full fn-expr env fenv)
          (compile-args args env fenv))))

;;; ============================================================
;;; Labels Support (Local Recursive Functions) - FNTAB Transformation
;;; ============================================================

;; Labels is transformed to:
;;   (let ((f nil) ...)
;;     (setq f (lambda (FNTAB params) (let ((f (car FNTAB))) body)))
;;     (let ((FNTAB (cons f nil)))
;;       (funcall f FNTAB args)))

;; Gensym counter - global state using cons cell (works in SBCL and native)
;; The cons cell is created at load time and mutated via setcar
(defun make-gensym-state ()
  "Create initial gensym state - a cons cell holding (counter . nil)"
  (cons 0 nil))

#+sbcl (defvar *gensym-state* (make-gensym-state))

#-sbcl
(defun digit-char (n)
  "Convert digit 0-9 to ASCII character code"
  (+ n 48))  ; '0' = 48

(defun number-to-string (n)
  "Convert positive integer to string - pure Habu"
  (if (= n 0)
      "0"
      (labels ((digits (num acc)
                 (if (= num 0)
                     acc
                     (digits (/ num 10)
                             (cons (digit-char (mod num 10)) acc))))
               (chars-to-vec (chars)
                 (let* ((len (length chars))
                        (vec (make-vector len)))
                   (labels ((fill-vec (cs i)
                              (if (null cs)
                                  vec
                                  (progn
                                    (vector-set vec i (car cs))
                                    (fill-vec (cdr cs) (+ i 1))))))
                     (fill-vec chars 0)))))
        (make-string-from-vector (chars-to-vec (digits n nil))))))

(defun gensym-next (state)
  "Get and increment gensym counter from state cell"
  (let ((val (+ (car state) 1)))
    #+sbcl (setf (car state) val)
    #-sbcl (setcar state val)
    val))

#-sbcl
(defun gensym (prefix)
  "Generate unique symbol - uses pure string operations"
  ;; In native self-hosted code, uses string operations
  (make-symbol-from-string (sys:string-concat prefix "G")))

(defun compile-labels (expr env fenv)
  "Compile labels by transforming to let/setq/lambda/funcall with FNTAB"
  (let* ((bindings (cadr expr))
         (body-forms (cddr expr))
         (body (if (null (cdr body-forms)) (car body-forms) (cons 'progn body-forms)))
         (fn-names (extract-label-names bindings nil))
         (fntab-var (gensym "FNTAB"))
         ;; Transform to: (let ((f nil)) (setq f (lambda (FNTAB x) ...)) (let ((FNTAB (cons f nil))) main))
         (transformed (transform-labels fn-names bindings body fntab-var)))
    ;; Compile the transformed expression
    (compile-expr-full transformed env fenv)))

(defun extract-label-names (bindings acc)
  "Extract function names from labels bindings"
  (if (null bindings)
      (reverse acc)
      (extract-label-names (cdr bindings)
                                (cons (car (car bindings)) acc))))

(defun transform-labels (fn-names bindings body fntab-var)
  "Transform labels to let/setq/funcall with FNTAB"
  ;; Build let bindings: ((f nil) ...)
  (let* ((let-bindings (map-nil-bindings fn-names nil))
         ;; Build FNTAB unpack bindings for inside lambdas
         (fntab-unpack (build-fntab-unpack fn-names fntab-var 0 nil))
         ;; Build setq forms for each function
         (setq-forms (build-setq-forms bindings fn-names fntab-var fntab-unpack nil))
         ;; Build FNTAB cons list
         (fntab-init (build-fntab-init fn-names))
         ;; Rewrite main body
         (rewritten-body (rewrite-labels-body body fn-names fntab-var))
         ;; Inner let for FNTAB
         (inner-let (list 'let (list (list fntab-var fntab-init)) rewritten-body))
         ;; Full expression
         (full-progn (append setq-forms (list inner-let))))
    (list 'let let-bindings (cons 'progn full-progn))))

(defun map-nil-bindings (names acc)
  "Build ((name nil) ...) list"
  (if (null names)
      (reverse acc)
      (map-nil-bindings (cdr names) (cons (list (car names) 'nil) acc))))

(defun build-fntab-unpack (names fntab-var depth acc)
  "Build ((f (car FNTAB)) (g (car (cdr FNTAB))) ...) bindings"
  (if (null names)
      (reverse acc)
      (let ((accessor (wrap-cdr-car fntab-var depth)))
        (build-fntab-unpack (cdr names) fntab-var (+ depth 1)
                                 (cons (list (car names) accessor) acc)))))

(defun wrap-cdr-car (var depth)
  "Build (car (cdr (cdr ... var))) expression"
  (if (= depth 0)
      (list 'car var)
      (list 'car (wrap-cdr var depth))))

(defun wrap-cdr (var n)
  "Wrap var in n cdrs"
  (if (= n 0)
      var
      (list 'cdr (wrap-cdr var (- n 1)))))

(defun build-setq-forms (bindings fn-names fntab-var fntab-unpack acc)
  "Build setq forms for each function"
  ;; NOTE: Keep to 6 bindings (6-binding limit for recursive functions)
  (if (null bindings)
      (reverse acc)
      (let* ((fn-name (car (car bindings)))
             (params (cadr (car bindings)))
             (forms (cddr (car bindings)))
             (fn-body (if (null (cdr forms)) (car forms) (cons 'progn forms)))
             (rewritten (rewrite-labels-body fn-body fn-names fntab-var))
             (setq-form (list 'setq fn-name
                              (list 'lambda (cons fntab-var params)
                                    (list 'let fntab-unpack rewritten)))))
        (build-setq-forms (cdr bindings) fn-names fntab-var fntab-unpack
                               (cons setq-form acc)))))

(defun build-fntab-init (names)
  "Build (cons f (cons g nil)) expression"
  (if (null names)
      'nil
      (list 'cons (car names) (build-fntab-init (cdr names)))))

(defun rewrite-labels-body (expr fn-names fntab-var)
  "Rewrite calls to labels functions to pass FNTAB"
  (cond
    ((null expr) nil)
    ((numberp expr) expr)
    ((symbolp expr) expr)
    ((not (consp expr)) expr)
    ;; If calling a labels function, rewrite to (funcall fn FNTAB args...)
    ((and (symbolp (car expr)) (member (car expr) fn-names))
     (cons 'funcall
           (cons (car expr)
                 (cons fntab-var
                       (rewrite-args (cdr expr) fn-names fntab-var)))))
    ;; Quote - don't descend
    ((eq (car expr) 'quote) expr)
    ;; lambda - only rewrite body, not params
    ((eq (car expr) 'lambda)
     (list 'lambda (cadr expr)
           (rewrite-labels-body (caddr expr) fn-names fntab-var)))
    ;; let/let* - rewrite values and body
    ((or (eq (car expr) 'let) (eq (car expr) 'LET) (eq (car expr) 'let*) (eq (car expr) 'LET*))
     (let* ((bindings (cadr expr))
            (body-forms (cddr expr))
            (new-bindings (rewrite-let-bindings bindings fn-names fntab-var)))
       (cons (car expr) (cons new-bindings
                              (rewrite-args body-forms fn-names fntab-var)))))
    ;; Default: recursively rewrite all parts
    (t (rewrite-args expr fn-names fntab-var))))

(defun rewrite-args (args fn-names fntab-var)
  "Rewrite list of arguments"
  (if (null args)
      nil
      (cons (rewrite-labels-body (car args) fn-names fntab-var)
            (rewrite-args (cdr args) fn-names fntab-var))))

(defun rewrite-let-bindings (bindings fn-names fntab-var)
  "Rewrite let binding values"
  (if (null bindings)
      nil
      (let ((b (car bindings)))
        (if (consp b)
            (cons (list (car b) (rewrite-labels-body (cadr b) fn-names fntab-var))
                  (rewrite-let-bindings (cdr bindings) fn-names fntab-var))
            (cons b (rewrite-let-bindings (cdr bindings) fn-names fntab-var))))))

#-sbcl
(defun member (x lst)
  "Check if x is in lst"
  (if (null lst)
      nil
      (if (eq x (car lst))
          t
          (member x (cdr lst)))))

(defun extend-fenv (names fenv)
  "Extend function environment with names"
  (if (null names)
      fenv
      (extend-fenv (cdr names) (cons (list (car names)) fenv))))

;;; ============================================================
;;; Full Expression Compiler (with defun/lambda/labels)
;;; ============================================================

(defun compile-expr-full (expr env fenv)
  "Full expression compiler with function support"
  (cond
    ((numberp expr) (compile-lit expr))
    ((stringp expr) (list 'str-lit expr))  ; String literals
    ((symbolp expr)
     (if (eq expr 'nil)
         (list 'lit 0)
         (if (eq expr 't)
             (list 'sym-lit "T")
             (compile-var expr env))))
    ((not (consp expr)) (compile-lit 0))
    (t
     (cond
       ;; Control flow
         ((eq (car expr) 'if) (compile-if-full expr env fenv))
         ((eq (car expr) 'cond) (compile-cond expr env fenv))
         ((eq (car expr) 'when) (compile-when expr env fenv))
         ((eq (car expr) 'unless) (compile-unless expr env fenv))
         ;; Boolean operators - transform to if forms
         ((eq (car expr) 'and) (compile-and expr env fenv))
         ((eq (car expr) 'or) (compile-or expr env fenv))
         ((eq (car expr) 'not) (list 'cmp-eq (compile-expr-full (cadr expr) env fenv) (list 'lit 0)))

         ;; Binding forms
         ((eq (car expr) 'let) (compile-let-full expr env fenv))
         ((eq (car expr) 'let*) (compile-let*-full expr env fenv))
         ((eq (car expr) 'progn) (compile-progn-full expr env fenv))
         ((eq (car expr) 'quote) (compile-quote expr))

         ;; Functions
         ((eq (car expr) 'lambda) (compile-lambda expr env fenv))
         ((eq (car expr) 'funcall) (compile-funcall expr env fenv))
         ((eq (car expr) 'labels) (compile-labels expr env fenv))
         ;; (function name) - create closure for named function
         ((eq (car expr) 'function)
          (let ((name (cadr expr)))
            (if (fenv-lookup name fenv)
                ;; Create fn-ref-ir that codegen will resolve to lambda-ref
                (list 'fn-ref-ir name)
                ;; Variable might be a lambda bound in let - compile as var
                (compile-var name env))))

         ;; Arithmetic (variadic support) - codegen uses 'add not 'add-ir
         ((eq (car expr) '+) (fold-binop 'add (cdr expr) env fenv))
         ((eq (car expr) '-) (fold-binop 'sub (cdr expr) env fenv))
         ((eq (car expr) '*) (fold-binop 'mul (cdr expr) env fenv))
         ((eq (car expr) '/) (fold-binop 'div (cdr expr) env fenv))
         ((eq (car expr) 'mod) (list 'mod (compile-expr-full (nth 1 expr) env fenv)
                                     (compile-expr-full (nth 2 expr) env fenv)))

         ;; Comparisons
         ((eq (car expr) '=) (list 'cmp-eq (compile-expr-full (nth 1 expr) env fenv)
                                   (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) '<) (list 'cmp-lt (compile-expr-full (nth 1 expr) env fenv)
                                   (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) '>) (list 'cmp-gt (compile-expr-full (nth 1 expr) env fenv)
                                   (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) '<=) (list 'cmp-le (compile-expr-full (nth 1 expr) env fenv)
                                    (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) '>=) (list 'cmp-ge (compile-expr-full (nth 1 expr) env fenv)
                                    (compile-expr-full (nth 2 expr) env fenv)))
         ;; /= (not equal) - transform to (not (= a b))
         ((eq (car expr) '/=)
          (compile-expr-full (list 'not (list '= (nth 1 expr) (nth 2 expr))) env fenv))

         ;; Bitwise operations
         ((eq (car expr) 'logand) (list 'band (compile-expr-full (nth 1 expr) env fenv)
                                        (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'logior) (list 'bor (compile-expr-full (nth 1 expr) env fenv)
                                        (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'logxor) (list 'bxor (compile-expr-full (nth 1 expr) env fenv)
                                        (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'ash) (list 'bsh (compile-expr-full (nth 1 expr) env fenv)
                                     (compile-expr-full (nth 2 expr) env fenv)))

         ;; List operations - use -ir suffix to match codegen
         ((eq (car expr) 'cons) (list 'cons-ir
                              (compile-expr-full (nth 1 expr) env fenv)
                              (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'car) (list 'car-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'cdr) (list 'cdr-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'cadr) (list 'car-ir (list 'cdr-ir (compile-expr-full (nth 1 expr) env fenv))))
         ((eq (car expr) 'caddr) (list 'car-ir (list 'cdr-ir (list 'cdr-ir (compile-expr-full (nth 1 expr) env fenv)))))
         ((eq (car expr) 'cddr) (list 'cdr-ir (list 'cdr-ir (compile-expr-full (nth 1 expr) env fenv))))
         ((eq (car expr) 'cdddr) (list 'cdr-ir (list 'cdr-ir (list 'cdr-ir (compile-expr-full (nth 1 expr) env fenv)))))
         ((eq (car expr) 'cadddr) (list 'car-ir (list 'cdr-ir (list 'cdr-ir (list 'cdr-ir (compile-expr-full (nth 1 expr) env fenv))))))
         ;; nth - expand (nth n list) based on constant or variable index
         ((eq (car expr) 'nth) (compile-nth expr env fenv))
         ((eq (car expr) 'list) (compile-list-full expr env fenv))

         ;; Predicates - use cmp-eq/get-tag to match main compiler codegen
         ;; null: compare value to nil (0)
         ((eq (car expr) 'null) (list 'cmp-eq (compile-expr-full (nth 1 expr) env fenv) (list 'lit 0)))
         ;; consp: compare tag to 1 (cons tag)
         ((eq (car expr) 'consp) (list 'cmp-eq (list 'get-tag (compile-expr-full (nth 1 expr) env fenv)) (list 'lit 1)))
         ;; numberp: compare tag to 0 (fixnum tag)
         ((eq (car expr) 'numberp) (list 'cmp-eq (list 'get-tag (compile-expr-full (nth 1 expr) env fenv)) (list 'lit 0)))
         ;; symbolp: compare tag to 2 (symbol tag)
         ((eq (car expr) 'symbolp) (list 'cmp-eq (list 'get-tag (compile-expr-full (nth 1 expr) env fenv)) (list 'lit 2)))
         ;; stringp: compare tag to 4 (string tag)
         ((eq (car expr) 'stringp) (list 'cmp-eq (list 'get-tag (compile-expr-full (nth 1 expr) env fenv)) (list 'lit 4)))
         ;; vectorp: compare tag to 3 (vector tag)
         ((eq (car expr) 'vectorp) (list 'cmp-eq (list 'get-tag (compile-expr-full (nth 1 expr) env fenv)) (list 'lit 3)))
         ;; eq: compare two values directly
         ((eq (car expr) 'eq) (list 'cmp-eq (compile-expr-full (nth 1 expr) env fenv)
                                   (compile-expr-full (nth 2 expr) env fenv)))

         ;; String operations - use -ir suffix to match codegen
         ((eq (car expr) 'string-length) (list 'string-length-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'string-ref) (list 'string-ref-ir
                                    (compile-expr-full (nth 1 expr) env fenv)
                                    (compile-expr-full (nth 2 expr) env fenv)))
         ;; string-concat / sys:string-concat - concatenate two strings
         ((or (eq (car expr) 'string-concat)
              (eq (car expr) 'sys:string-concat))
          (list 'string-concat-ir
                (compile-expr-full (nth 1 expr) env fenv)
                (compile-expr-full (nth 2 expr) env fenv)))

         ;; Vector operations - use -ir suffix to match codegen
         ((eq (car expr) 'make-vector) (list 'make-vector-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'vector-ref) (list 'vector-ref-ir
                                    (compile-expr-full (nth 1 expr) env fenv)
                                    (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'vector-set) (list 'vector-set-ir
                                    (compile-expr-full (nth 1 expr) env fenv)
                                    (compile-expr-full (nth 2 expr) env fenv)
                                    (compile-expr-full (nth 3 expr) env fenv)))
         ((eq (car expr) 'vector-length) (list 'vector-length-ir (compile-expr-full (nth 1 expr) env fenv)))

         ;; Make string from vector (for reader)
         ((eq (car expr) 'make-string-from-vector) (list 'make-string-from-vector-ir
                                                  (compile-expr-full (nth 1 expr) env fenv)))

         ;; Mutation
         ((eq (car expr) 'setq) (compile-setq expr env fenv))
         ;; setcar/setcdr - mutate cons cells
         ((eq (car expr) 'setcar) (list 'setcar-ir
                                        (compile-expr-full (nth 1 expr) env fenv)
                                        (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'setcdr) (list 'setcdr-ir
                                        (compile-expr-full (nth 1 expr) env fenv)
                                        (compile-expr-full (nth 2 expr) env fenv)))

         ;; Symbol operations
         ((eq (car expr) 'symbol-name) (list 'symbol-name-ir
                                             (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'make-symbol-from-string) (list 'make-symbol-ir
                                                         (compile-expr-full (nth 1 expr) env fenv)))

         ;; System calls
         ((eq (car expr) 'sys-exit) (list 'sys-exit-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'get-intern-table) (list 'get-intern-table-ir))
         ((eq (car expr) 'set-intern-table) (list 'set-intern-table-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'sys-open) (list 'sys-open-ir
                                          (compile-expr-full (nth 1 expr) env fenv)
                                          (compile-expr-full (nth 2 expr) env fenv)
                                          (compile-expr-full (nth 3 expr) env fenv)))
         ((eq (car expr) 'sys-read) (list 'sys-read-ir
                                          (compile-expr-full (nth 1 expr) env fenv)
                                          (compile-expr-full (nth 2 expr) env fenv)
                                          (compile-expr-full (nth 3 expr) env fenv)))
         ((eq (car expr) 'sys-write) (list 'sys-write-ir
                                           (compile-expr-full (nth 1 expr) env fenv)
                                           (compile-expr-full (nth 2 expr) env fenv)
                                           (compile-expr-full (nth 3 expr) env fenv)))
         ((eq (car expr) 'sys-close) (list 'sys-close-ir (compile-expr-full (nth 1 expr) env fenv)))

         ;; Vectors and file I/O helpers
         ((eq (car expr) 'make-vector) (list 'make-vector-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'vector-ref) (list 'vector-ref-ir
                                            (compile-expr-full (nth 1 expr) env fenv)
                                            (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'vector-set) (list 'vector-set-ir
                                            (compile-expr-full (nth 1 expr) env fenv)
                                            (compile-expr-full (nth 2 expr) env fenv)
                                            (compile-expr-full (nth 3 expr) env fenv)))
         ((eq (car expr) 'vector-length) (list 'vector-length-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'buffer-to-string) (list 'buffer-to-string-ir
                                                   (compile-expr-full (nth 1 expr) env fenv)
                                                   (compile-expr-full (nth 2 expr) env fenv)))

         ;; native-read-file: expand to let* with sys-open/read/close
         ;; Expands to: (let* ((fd (sys-open path 0 0))
         ;;                     (buf (make-vector 65536))
         ;;                     (n (sys-read fd buf 65536)))
         ;;               (sys-close fd)
         ;;               (buffer-to-string buf n))
         ((eq (car expr) 'native-read-file)
          (let ((path-sym (gensym "PATH"))
                (fd-sym (gensym "FD"))
                (buf-sym (gensym "BUF"))
                (n-sym (gensym "N")))
            (compile-expr-full
             (list 'let* (list (list path-sym (nth 1 expr))
                               (list fd-sym (list 'sys-open path-sym 0 0))
                               (list buf-sym (list 'make-vector 65536))
                               (list n-sym (list 'sys-read fd-sym buf-sym 65536)))
                   (list 'progn
                         (list 'sys-close fd-sym)
                         (list 'buffer-to-string buf-sym n-sym)))
             env fenv)))

         ;; Unknown - try as function call or inline lambda
         (t (cond
              ((symbolp (car expr)) (compile-call expr env fenv))
              ;; Inline lambda call: ((lambda (x) ...) arg)
              ((and (consp (car expr)) (eq (car (car expr)) 'lambda))
               (list 'funcall-ir
                     (compile-lambda (car expr) env fenv)
                     (compile-args (cdr expr) env fenv)))
              (t (compile-lit 0))))))))

;; Helper functions for full compiler

(defun compile-if-full (expr env fenv)
  (let ((test (compile-expr-full (nth 1 expr) env fenv))
        (then (compile-expr-full (nth 2 expr) env fenv))
        (else (if (nth 3 expr)
                  (compile-expr-full (nth 3 expr) env fenv)
                  (list 'lit 0))))
    (list 'if-ir test then else)))

(defun compile-cond (expr env fenv)
  "Compile (cond (test1 body1) (test2 body2) ...)"
  (let ((clauses (cdr expr)))
    (if (null clauses)
        (list 'lit 0)
        (let* ((clause (car clauses))
               (test (car clause))
               (body (cadr clause)))
          (if (eq test 't)
              (compile-expr-full body env fenv)
              (list 'if-ir
                    (compile-expr-full test env fenv)
                    (compile-expr-full body env fenv)
                    (compile-cond (cons 'cond (cdr clauses)) env fenv)))))))

(defun compile-when (expr env fenv)
  (list 'if-ir
        (compile-expr-full (nth 1 expr) env fenv)
        (compile-progn-full (cons 'progn (cddr expr)) env fenv)
        (list 'lit 0)))

(defun compile-unless (expr env fenv)
  (list 'if-ir
        (compile-expr-full (nth 1 expr) env fenv)
        (list 'lit 0)
        (compile-progn-full (cons 'progn (cddr expr)) env fenv)))

(defun compile-nth (expr env fenv)
  "Compile (nth n list) - optimize for constant indices"
  (let ((index-expr (nth 1 expr))
        (list-expr (nth 2 expr)))
    (if (numberp index-expr)
        ;; Constant index - expand directly
        (let ((list-ir (compile-expr-full list-expr env fenv)))
          (nth-expand index-expr list-ir))
        ;; Variable index - use labels loop
        (compile-expr-full
         (list 'labels
               (list (list 'nth-loop (list 'n 'lst)
                           (list 'if (list '= 'n 0)
                                 (list 'car 'lst)
                                 (list 'nth-loop (list '- 'n 1) (list 'cdr 'lst)))))
               (list 'nth-loop index-expr list-expr))
         env fenv))))

(defun nth-expand (n list-ir)
  "Expand (nth n list-ir) to nested car/cdr for constant n"
  (if (= n 0)
      (list 'car-ir list-ir)
      (nth-expand (- n 1) (list 'cdr-ir list-ir))))

(defun compile-and (expr env fenv)
  "Compile (and a b c ...) to nested if forms"
  (let ((args (cdr expr)))
    (cond
      ((null args) (list 'sym-lit "T"))  ; (and) = t
      ((null (cdr args))  ; single arg - just compile it
       (compile-expr-full (car args) env fenv))
      (t  ; multiple args - (if a (and b c ...) nil)
       (list 'if-ir
             (compile-expr-full (car args) env fenv)
             (compile-and (cons 'and (cdr args)) env fenv)
             (list 'lit 0))))))

(defun compile-or (expr env fenv)
  "Compile (or a b c ...) to nested if forms"
  (let ((args (cdr expr)))
    (cond
      ((null args) (list 'lit 0))  ; (or) = nil
      ((null (cdr args))  ; single arg - just compile it
       (compile-expr-full (car args) env fenv))
      (t  ; multiple args - (if a a (or b c ...)) but need temp to avoid double eval
       ;; Simplified: (let ((tmp a)) (if tmp tmp (or b c ...)))
       ;; For now, just use nested ifs assuming no side effects
       (list 'if-ir
             (compile-expr-full (car args) env fenv)
             (compile-expr-full (car args) env fenv)
             (compile-or (cons 'or (cdr args)) env fenv))))))

(defun compile-let-full (expr env fenv)
  "Compile (let ((var val) ...) body ...) to (let-ir vals body count offs)"
  (let ((bindings (nth 1 expr))
        (body-forms (cddr expr)))
    (labels ((extract-vars (binds acc)
               (if (null binds)
                   (reverse acc)
                   (extract-vars (cdr binds) (cons (car (car binds)) acc))))
             (compile-vals (binds acc)
               (if (null binds)
                   (reverse acc)
                   (compile-vals (cdr binds)
                                 (cons (compile-expr-full (nth 1 (car binds)) env fenv) acc))))
             (make-offs (n base acc)
               ;; Generate offsets starting at base: (base, base+1, ...)
               (if (= n 0)
                   (reverse acc)
                   (make-offs (- n 1) (+ base 1) (cons base acc)))))
      (let* ((vars (extract-vars bindings nil))
             (val-irs (compile-vals bindings nil))
             (base-offset (length env))  ;; Storage starts after current env
             (offs (make-offs (length bindings) base-offset nil))
             (new-env (extend-env vars env))
             (body (if (null (cdr body-forms))
                       (car body-forms)
                       (cons 'progn body-forms)))
             (body-ir (compile-expr-full body new-env fenv)))
        (list 'let-ir val-irs body-ir (length bindings) offs)))))

(defun compile-let*-full (expr env fenv)
  "Compile (let* ((var1 val1) (var2 val2)) body) to nested let-irs with count/offs"
  (let ((bindings (nth 1 expr))
        (body-forms (cddr expr)))
    (if (null bindings)
        (compile-expr-full (if (null (cdr body-forms))
                                    (car body-forms)
                                    (cons 'progn body-forms))
                                env fenv)
        ;; Compile as nested lets, each with 1 binding
        ;; NOTE: Keep to 3 bindings (6-binding limit for recursive functions)
        (let* ((val-ir (compile-expr-full (nth 1 (car bindings)) env fenv))
               (off (length env))  ;; Storage offset = current env length
               (rest-ir (compile-let*-full
                         (list 'let* (cdr bindings) (cons 'progn body-forms))
                         (append env (list (car (car bindings))))
                         fenv)))
          ;; (let-ir vals body count offs)
          (list 'let-ir (list val-ir) rest-ir 1 (list off))))))

(defun compile-progn-full (expr env fenv)
  (labels ((compile-exprs (exprs acc)
             (if (null exprs)
                 (reverse acc)
                 (compile-exprs (cdr exprs)
                                (cons (compile-expr-full (car exprs) env fenv) acc)))))
    (list 'progn-ir (compile-exprs (cdr expr) nil))))

(defun compile-list-full (expr env fenv)
  (labels ((expand-list (elems)
             (if (null elems)
                 (list 'lit 0)
                 (list 'cons-ir
                       (compile-expr-full (car elems) env fenv)
                       (expand-list (cdr elems))))))
    (expand-list (cdr expr))))

(defun compile-setq (expr env fenv)
  "Compile (setq var val)"
  (let ((var (nth 1 expr))
        (val (nth 2 expr)))
    (let ((offset (env-lookup var env)))
      (if offset
          (list 'setq-ir offset (compile-expr-full val env fenv))
          (list 'lit 0)))))  ;; Unknown var

;;; ============================================================
;;; Full Program Compiler
;;; ============================================================

(defun compile-forms (forms)
  "Compile forms to (defun-list main-ir) - proper list like main compiler"
  (let* ((fenv (collect-defuns forms nil))
         (defuns (compile-all-defuns forms nil fenv nil))
         (main-form (find-main-form forms nil))
         (main-ir (compile-expr-full main-form nil fenv)))
    (list defuns main-ir)))

;;; Export full compiler
#+sbcl (export '(compile-expr-full compile-forms
          compile-defun compile-lambda compile-labels) :habu)

;;; ============================================================
;;; Integration with Existing Codegen (SBCL-only bridging functions)
;;; ============================================================

#+sbcl
(defun compile-to-bytecode (expr)
  "Compile expression to ARM64 bytecode using existing codegen.
   This bridges pure compiler → existing nc-codegen (which is already pure!)"
  (let ((ir (compile-expr-v2 expr nil)))
    ;; Call existing nc-codegen (it's already pure - just builds byte lists!)
    ;; nc-codegen signature: (ir rtaddrs fnoffs temp-depth)
    (let ((code-with-markers (nc-codegen ir nil nil 0)))
      ;; Resolve markers to actual bytes
      (nc-resolve-calls code-with-markers nil))))

#+sbcl
(defun compile-program-simple (forms)
  "Compile simple program (single expression) to complete bytecode.
   Uses existing nc-codegen-main which adds prologue/epilogue."
  (if (null forms)
      nil
      (let ((main-expr (if (null (cdr forms))
                           (car forms)  ;; Single form
                           (cons 'progn forms))))  ;; Multiple forms → progn
        (let ((ir (compile-expr-v2 main-expr nil)))
          ;; Use existing nc-codegen-main (adds prologue/epilogue)
          (nc-codegen-main ir nil)))))

;;; Self-hosting entry point (SBCL-only - uses native-read-file which needs SBCL setup)
#+sbcl
(defun self-compile (source-path output-path)
  "Pure Habu self-hosting compiler entry point.
   Reads source, compiles with pure compiler, generates ARM64, writes executable.
   This function is designed to be compiled to native code and run standalone."
  (let ((source (native-read-file source-path)))
    (if source
        (progn
          ;; Use deliver which uses the pure compiler (no SBCL dependencies)
          (deliver source output-path)
          (sys-exit 0))
        (progn
          (sys-exit 1)))))

;;; ============================================================
;;; Full Program Compilation (SBCL-only - calls main compiler helpers)
;;; ============================================================

#+sbcl
(defun compile-program (forms)
  "Compile forms to complete ARM64 bytecode with function linking.
   This is the full pipeline: parse → IR → lift-lambdas → codegen → link.
   Returns flat bytecode ready for Mach-O wrapping."
  (reset-symbol-table)
  (let* ((r (compile-forms forms))
         (defun-fns-raw (car r))
         (mir-raw (cadr r))
         ;; Add nil for free-vars to match main compiler format
         ;; Format: (name params body-ir param-base free-vars)
         (defun-fns (mapcar (lambda (d)
                                   (list (car d) (cadr d) (caddr d) (cadddr d) nil))
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

(defun collect-extern-calls (code)
  "Collect extern call markers from code. Returns ((name . pos) ...)"
  (labels ((collect (items acc)
             (if (null items)
                 (reverse acc)
                 (let ((item (car items)))
                   (if (and (consp item) (eq (car item) :extern-call))
                       (collect (cdr items) (cons (cons (cadr item) (caddr item)) acc))
                       (collect (cdr items) acc))))))
    (collect code nil)))

(defun get-unique-imports (extern-calls)
  "Get unique import names from extern calls list"
  (labels ((unique (calls seen acc)
             (if (null calls)
                 (reverse acc)
                 (let ((name (car (car calls))))
                   (if (member name seen)
                       (unique (cdr calls) seen acc)
                       (unique (cdr calls) (cons name seen) (cons name acc)))))))
    (unique extern-calls nil nil)))

#-sbcl
(defun string= (s1 s2)
  "Compare two strings for equality - use pure implementation"
  (string-equal s1 s2))

(defun assoc-string (key alist)
  "Find entry in alist with string key"
  (if (null alist)
      nil
      (if (string= key (car (car alist)))
          (car alist)
          (assoc-string key (cdr alist)))))

(defun flatten-extern-calls (code stub-alist code-base-addr)
  "Replace extern call markers with BL instructions using assoc list.
   Returns (flat-code . extern-positions)
   Note: resolve-calls emits markers followed by 3 zeros - must skip them."
  (labels ((flatten (items result positions skip-count)
             (cond
               ;; Done
               ((null items)
                (cons (reverse result) (reverse positions)))
               ;; Skip placeholder zeros after extern-call marker
               ((> skip-count 0)
                (flatten (cdr items) result positions (- skip-count 1)))
               ;; Extern call marker - emit BL, skip next 3 zeros
               ((and (consp (car items)) (eq (car (car items)) :extern-call))
                (let* ((item (car items))
                       (name (cadr item))
                       (pos (caddr item))
                       (bl-addr (+ code-base-addr pos))
                       (entry (assoc-string name stub-alist))
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

(defun build-stub-alist (imports stubs-offset stub-size)
  "Build ((name . offset) ...) alist for stub map"
  (labels ((build (remaining i acc)
             (if (null remaining)
                 (reverse acc)
                 (build (cdr remaining) (+ i 1)
                        (cons (cons (car remaining) (+ stubs-offset (* i stub-size))) acc)))))
    (build imports 0 nil)))

(defun is-extern-marker (x)
  "Check if x is an extern-call marker"
  (and (consp x) (eq (car x) :extern-call)))

;; deliver uses read-all, wrap-bytecode-with-heap-for-imports,
;; write-macho-executable-with-imports-and-heap from main compiler/macho
;; This function works in both SBCL and native Habu (no #+sbcl guard)
(defun deliver (source output-path)
  "Compile source string to native executable using pure compiler.
   This uses the full extern-call flattening pipeline.
   Uses only pure functions - no hash tables or CL runtime.
   Works in both SBCL and native Habu environments."
  (let* ((forms (read-all source))
         (bytes-with-markers (compile-program forms))
         ;; Collect extern calls and get unique imports
         (extern-calls (collect-extern-calls bytes-with-markers))
         (imports (get-unique-imports extern-calls))
         (wrapper-size 68))  ; 17 instructions * 4 bytes

    ;; Always use imports path for consistent Mach-O structure
    (let ((imports (if (null imports) '("_exit") imports)))

      ;; Calculate stub offsets BEFORE flattening
      (let* ((num-imports (length imports))
             (stubs-total (if (> num-imports 0) (* num-imports 12) 0))
             (code-offset #x400)
             ;; Calculate exact flattened code size
             ;; bytes-with-markers already has 4 items per call site (marker + 3 zeros)
             ;; After flattening: marker+zeros → 4 BL bytes, so total size stays same
             (exact-flat-size (length bytes-with-markers))
             (exact-code-size (+ exact-flat-size wrapper-size))
             (stubs-offset (+ code-offset exact-code-size))
             (stub-size 12))

        ;; Build stub offset alist (instead of hash table)
        (let* ((stub-alist (build-stub-alist imports stubs-offset stub-size))
               ;; Flatten with correct BL instructions
               (flatten-result (flatten-extern-calls bytes-with-markers stub-alist (+ code-offset wrapper-size)))
               (flat-code (car flatten-result)))

          ;; Calculate heap page offset
          (let* ((total-size (+ (length flat-code) wrapper-size))
                 (stubs-end (+ code-offset total-size stubs-total))
                 (text-vmsize (* (ceiling stubs-end #x4000) #x4000))
                 (text-pages-4kb (/ text-vmsize #x1000))
                 (data-const-pages-4kb (/ #x4000 #x1000))
                 (heap-page-offset (+ text-pages-4kb data-const-pages-4kb))
                 (wrapped-code (wrap-bytecode-with-heap-for-imports flat-code heap-page-offset)))

            ;; Write Mach-O executable (handles chmod+codesign via native-write-executable)
            (write-macho-executable-with-imports-and-heap output-path wrapped-code imports #x800000)))))))

;;; Export self-hosting entry point
#+sbcl (export '(compile-to-bytecode compile-program-simple self-compile
          compile-program deliver) :habu)
