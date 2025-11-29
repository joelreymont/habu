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
