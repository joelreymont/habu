;;; Pure Habu Compiler - Uses ONLY Habu primitives (no SBCL dependencies)
;;; No multiple-value-bind, no values, no loop, no format
;;; This can be compiled to native and run without SBCL

;; Ensure packages exist before using them
;; (normally defined in compiler-sbcl.lisp)
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :sys)
    (defpackage :sys
      (:use :cl)
      (:shadow #:read #:compile)
      (:export #:read #:compile)))
  (unless (find-package :habu)
    (defpackage :habu (:use :cl))))

#+sbcl (in-package :habu)

;;; ============================================================
;;; Symbol Registration for Native Code
;;; ============================================================
;;; In native code, the compiler's literal symbols (like 'defun) are constant
;;; pool entries, different from symbols created by the reader's intern.
;;; We pre-register the compiler's symbols in the intern table so that
;;; reader-produced symbols will be eq to compiler literals.

#+sbcl
(defun register-compiler-symbols ()
  "In SBCL, symbol interning works correctly - nothing to do."
  nil)

#-sbcl
(defun register-compiler-symbols ()
  "Register all symbols used in compiler dispatch to the intern table.
   This must be called before reading any source code."
  (set-intern-table
   (list
    ;; Special forms
    (cons "DEFUN" 'defun) (cons "PROGN" 'progn) (cons "IF" 'if)
    (cons "LET" 'let) (cons "LET*" 'let*) (cons "QUOTE" 'quote)
    (cons "LAMBDA" 'lambda) (cons "FUNCALL" 'funcall) (cons "LABELS" 'labels)
    (cons "FUNCTION" 'function) (cons "COND" 'cond) (cons "WHEN" 'when)
    (cons "UNLESS" 'unless) (cons "AND" 'and) (cons "OR" 'or) (cons "NOT" 'not)
    (cons "SETQ" 'setq)
    ;; Arithmetic
    (cons "+" '+) (cons "-" '-) (cons "*" '*) (cons "/" '/)
    (cons "MOD" 'mod)
    ;; Comparisons
    (cons "=" '=) (cons "<" '<) (cons ">" '>) (cons "<=" '<=) (cons ">=" '>=)
    (cons "/=" '/=) (cons "EQ" 'eq)
    ;; Bitwise
    (cons "LOGAND" 'logand) (cons "LOGIOR" 'logior) (cons "LOGXOR" 'logxor)
    (cons "ASH" 'ash)
    ;; List operations
    (cons "CONS" 'cons) (cons "CAR" 'car) (cons "CDR" 'cdr)
    (cons "CADR" 'cadr) (cons "CADDR" 'caddr) (cons "CDDR" 'cddr)
    (cons "CDDDR" 'cdddr) (cons "CADDDR" 'cadddr)
    (cons "NTH" 'nth) (cons "LIST" 'list) (cons "LENGTH" 'length)
    (cons "REVERSE" 'reverse) (cons "SETCAR" 'setcar) (cons "SETCDR" 'setcdr)
    ;; Predicates
    (cons "NULL" 'null) (cons "CONSP" 'consp) (cons "NUMBERP" 'numberp)
    (cons "SYMBOLP" 'symbolp) (cons "STRINGP" 'stringp) (cons "VECTORP" 'vectorp)
    ;; Strings
    (cons "STRING-LENGTH" 'string-length) (cons "STRING-REF" 'string-ref)
    (cons "STRING-CONCAT" 'string-concat) (cons "STRING-EQUAL" 'string-equal)
    ;; Vectors
    (cons "MAKE-VECTOR" 'make-vector) (cons "VECTOR-REF" 'vector-ref)
    (cons "VECTOR-SET" 'vector-set) (cons "VECTOR-LENGTH" 'vector-length)
    (cons "MAKE-STRING-FROM-VECTOR" 'make-string-from-vector)
    (cons "BUFFER-TO-STRING" 'buffer-to-string)
    (cons "BUFFER-BYTE-REF" 'buffer-byte-ref)
    (cons "BUFFER-BYTE-SET" 'buffer-byte-set)
    ;; Symbols
    (cons "SYMBOL-NAME" 'symbol-name) (cons "MAKE-SYMBOL-FROM-STRING" 'make-symbol-from-string)
    ;; System
    (cons "SYS-EXIT" 'sys-exit) (cons "SYS-OPEN" 'sys-open)
    (cons "SYS-READ" 'sys-read) (cons "SYS-WRITE" 'sys-write)
    (cons "SYS-WRITE-CHAR" 'sys-write-char) (cons "SYS-READ-BYTE" 'sys-read-byte) (cons "SYS-CLOSE" 'sys-close) (cons "NATIVE-READ-FILE" 'native-read-file)
    (cons "GET-INTERN-TABLE" 'get-intern-table) (cons "SET-INTERN-TABLE" 'set-intern-table)
    (cons "GET-LAMBDA-COUNTER" 'get-lambda-counter) (cons "SET-LAMBDA-COUNTER" 'set-lambda-counter)
    ;; JIT primitives
    (cons "JIT-MMAP" 'jit-mmap) (cons "JIT-WRITE-PROTECT" 'jit-write-protect)
    (cons "JIT-DCACHE-FLUSH" 'jit-dcache-flush) (cons "JIT-ICACHE-INVALIDATE" 'jit-icache-invalidate)
    (cons "JIT-CALL" 'jit-call)
    ;; Memory access for JIT
    (cons "MEM-SET-BYTE" 'mem-set-byte) (cons "MEM-LOAD-64" 'mem-load-64)
    ;; Special values
    (cons "NIL" 'nil) (cons "T" 't))))

;;; ============================================================
;;; Undefined Function Tracking
;;; ============================================================
;;; Tracks undefined function calls for compile-time warnings and
;;; link-time verification. Exit code 200 = undefined function at runtime.

#+sbcl (defvar *undefined-functions* nil "Functions called but not defined")
#+sbcl (defvar *all-call-targets* nil "All call-fn targets for link-time check")

#+sbcl
(defun reset-compile-warnings ()
  "Reset undefined function tracking before compilation"
  (setq *undefined-functions* nil)
  (setq *all-call-targets* nil))

#+sbcl
(defun record-undefined-function (name)
  "Record that an undefined function was called"
  (unless (member name *undefined-functions*)
    (push name *undefined-functions*)))

#+sbcl
(defun record-call-target (name)
  "Record a call-fn target for link-time verification"
  (unless (member name *all-call-targets*)
    (push name *all-call-targets*)))

#+sbcl
(defun report-compile-warnings ()
  "Report undefined functions found during compilation. Returns T if errors found."
  (when *undefined-functions*
    (format t "~%ERROR: Undefined functions referenced:~%")
    (dolist (fn (reverse *undefined-functions*))
      (format t "  - ~A~%" fn))
    t))

#+sbcl
(defun verify-link-references (defined-fns)
  "Verify all call-fn targets are in defined-fns list. Returns T if errors found."
  (let ((undefined nil))
    (dolist (target *all-call-targets*)
      (unless (member target defined-fns)
        (push target undefined)))
    (when undefined
      (format t "~%LINK ERROR: Functions called but not compiled:~%")
      (dolist (fn (reverse undefined))
        (format t "  - ~A~%" fn))
      t)))

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
(defun length (seq)
  "Length of list or string"
  (if (stringp seq)
      (string-length seq)
      (labels ((len-iter (l n)
                 (if (null l)
                     n
                     (len-iter (cdr l) (+ n 1)))))
        (len-iter seq 0))))

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
  (if (or (null s1) (null s2))
      (and (null s1) (null s2))  ; nil = nil, nil != string
      (let ((len1 (string-length s1))
            (len2 (string-length s2)))
        (if (= len1 len2)
            (string-equal-iter s1 s2 0 len1)
            nil))))

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
  "Compile variable reference using flat env list"
  (let ((offset (flat-env-lookup sym env)))
    (if offset
        (list 'var offset)
        (list 'lit 0))))  ;; Unknown var = 0

;;; Environment lookup for flat list format (sym1 sym2 ...)
;;; Used by compile-expr-full and friends in this file.
;;; Note: compiler-sbcl.lisp has its own env-lookup for alist format.

#+sbcl
(defun flat-env-lookup (sym env)
  "Look up symbol in flat environment list, return offset or nil.
   Env is (sym1 sym2 ...) where position is the offset."
  (labels ((lookup (e offset)
             (cond ((null e) nil)
                   ((eq (car e) sym) offset)
                   (t (lookup (cdr e) (1+ offset))))))
    (lookup env 0)))

#-sbcl
(defun flat-env-lookup (sym env)
  "Look up symbol in environment, return offset or nil - ITERATIVE VERSION"
  (let ((e env)
        (offset 0)
        (result nil)
        (done nil))
    (while (and (not done) (not (null e)))
      (if (eq (car e) sym)
          (progn
            (setq result offset)
            (setq done t))
          (progn
            (setq e (cdr e))
            (setq offset (+ offset 1)))))
    result))

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
    ;; Nil symbol - special case before other symbols
    ((null expr) (list 'nil-ir))
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
    ;; Default: error on unknown form
    (t (error "compile-expr: unhandled form ~S" expr))))

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
                       (let ((val (nth 1 (car binds))))
                         (compile-bindings (cdr binds)
                                           (cons (compile-expr val env) acc))))))
          (let ((val-irs (compile-bindings bindings nil))
                (body-ir (compile-expr body new-env)))
            (list 'let-ir val-irs body-ir)))))))

#-sbcl
(defun quote-ir (obj)
  "Build IR for quoted value - recursively builds cons-ir for lists"
  (cond
    ((numberp obj) (list 'lit obj))
    ((null obj) (list 'nil-ir))  ; nil has tag 6, distinct from fixnum 0
    ((symbolp obj) (list 'sym-lit (symbol-name obj)))
    ((consp obj) (list 'cons-ir (quote-ir (car obj)) (quote-ir (cdr obj))))
    ((stringp obj) (list 'str-lit obj))
    ;; Default: error on unknown quoted value type
    (t (error "quote-ir: unhandled type for ~S" obj))))

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
                 (list 'nil-ir)  ;; nil = tag 6
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
    ;; Default: error on unknown form
    (t (error "compile-expr-v2: unhandled form ~S" expr))))

;;; ============================================================
;;; Keyword Argument Support (Pure Habu)
;;; ============================================================

;; In SBCL, keywords are in KEYWORD package and symbol-name returns "FOO"
;; In native Habu, keywords are regular symbols with name ":FOO"

(defun keyword-name-p (sym)
  "Check if symbol is a keyword (name starts with :)"
  (if (symbolp sym)
      (let ((name (symbol-name sym)))
        (if (> (string-length name) 0)
            (= (string-ref name 0) 58)  ; 58 = ':'
            nil))
      nil))

;; Native Habu version - SBCL uses compiler-sbcl.lisp version
#-sbcl
(defun keyword-to-param-name (kw)
  "Extract parameter name from keyword.
   In native Habu: :FOO has name ':FOO', need to skip first char"
  (let ((name (symbol-name kw)))
    (if (and (> (string-length name) 0)
             (= (string-ref name 0) 58))
        ;; Skip leading colon - build new string
        (labels ((copy-chars (i acc)
                   (if (>= i (string-length name))
                       (make-string-from-vector acc)
                       (progn
                         (vector-set acc (- i 1) (string-ref name i))
                         (copy-chars (+ i 1) acc)))))
          (let ((result-vec (make-vector (- (string-length name) 1))))
            (copy-chars 1 result-vec)))
        name)))

;; Native Habu version - SBCL uses compiler-sbcl.lisp version
#-sbcl
(defun parse-lambda-list (params)
  "Parse lambda list, splitting at &optional and &key.
   Returns (positional-params . keyword-specs) where keyword-specs is
   a list of (name default) pairs.
   &optional params are added to positional-params (names only, defaults ignored)."
  (labels ((collect (ps pos-acc kw-acc in-opt in-keys)
             (if (null ps)
                 (cons (reverse pos-acc) (reverse kw-acc))
                 (let ((p (car ps)))
                   (cond
                     ((eq p '&optional)
                      (collect (cdr ps) pos-acc kw-acc t nil))
                     ((eq p '&key)
                      (collect (cdr ps) pos-acc kw-acc nil t))
                     (in-keys
                      ;; Keyword param: SYMBOL or (SYMBOL DEFAULT)
                      (if (consp p)
                          (collect (cdr ps) pos-acc
                                   (cons (list (car p) (cadr p)) kw-acc) nil t)
                          (collect (cdr ps) pos-acc
                                   (cons (list p nil) kw-acc) nil t)))
                     (in-opt
                      ;; Optional param: SYMBOL or (SYMBOL DEFAULT)
                      ;; Add name to positional params (default ignored for now)
                      (if (consp p)
                          (collect (cdr ps) (cons (car p) pos-acc) kw-acc t nil)
                          (collect (cdr ps) (cons p pos-acc) kw-acc t nil)))
                     (t
                      (collect (cdr ps) (cons p pos-acc) kw-acc nil nil)))))))
    (collect params nil nil nil nil)))

;; Vector access with SBCL/native compatibility
#+sbcl (defun vec-ref (v i) (svref v i))
#+sbcl (defun vec-set (v i val) (setf (svref v i) val))
#-sbcl (defun vec-ref (v i) (vector-ref v i))
#-sbcl (defun vec-set (v i val) (vector-set v i val))

(defun kw-to-param-sym (kw)
  "Convert keyword :FOO to parameter symbol FOO.
   Used for symbol-based comparison in keyword argument matching."
  #+sbcl (intern (symbol-name kw))  ; :FOO -> FOO in current package
  #-sbcl
  ;; In native Habu, keyword has name ':FOO', strip colon and intern
  (let ((name (symbol-name kw)))
    (if (and (> (string-length name) 0)
             (= (string-ref name 0) 58))  ; starts with ':'
        (make-symbol-from-string (keyword-to-param-name kw))
        kw)))

(defun find-kw-position (param-sym keyword-specs)
  "Find position of param-sym in keyword-specs using symbol equality.
   param-sym is a symbol (e.g., IMM), keyword-specs is ((NAME DEFAULT) ...)."
  (labels ((search-specs (specs pos)
             (if (null specs)
                 nil
                 (if (eq param-sym (car (car specs)))
                     pos
                     (search-specs (cdr specs) (+ pos 1))))))
    (search-specs keyword-specs 0)))

(defun rewrite-kw-call (args n-positional keyword-specs)
  "Rewrite call args with keywords to fully positional args.
   Returns list of args in positional order, with defaults for unspecified keywords."
  (let* ((n-keywords (length keyword-specs))
         (kw-values #+sbcl (make-array n-keywords :initial-element nil)
                    #-sbcl (make-vector n-keywords)))
    ;; Initialize with defaults from keyword-specs
    (labels ((init-defaults (specs idx)
               (if (null specs)
                   nil
                   (progn
                     (vec-set kw-values idx (cadr (car specs)))
                     (init-defaults (cdr specs) (+ idx 1))))))
      (init-defaults keyword-specs 0))
    ;; Extract positional args and rest
    (labels ((take-n (lst n acc)
               (if (or (null lst) (= n 0))
                   (cons (reverse acc) lst)
                   (take-n (cdr lst) (- n 1) (cons (car lst) acc)))))
      (let* ((split (take-n args n-positional nil))
             (pos-args (car split))
             (rest-args (cdr split)))
        ;; Parse keyword/value pairs from rest-args
        (labels ((parse-kws (rest)
                   (if (null rest)
                       nil
                       (if (null (cdr rest))
                           nil  ; Odd number - skip last
                           (let ((kw (car rest))
                                 (val (cadr rest)))
                             (if (keyword-name-p kw)
                                 (let* ((param-sym (kw-to-param-sym kw))
                                        (pos (find-kw-position param-sym keyword-specs)))
                                   (if pos
                                       (vec-set kw-values pos val))
                                   (parse-kws (cddr rest)))
                                 (parse-kws (cdr rest))))))))
          (parse-kws rest-args))
        ;; Build result: positional + keyword values
        (labels ((collect-kw-values (idx acc)
                   (if (>= idx n-keywords)
                       (reverse acc)
                       (collect-kw-values (+ idx 1)
                                          (cons (vec-ref kw-values idx) acc)))))
          (append pos-args (collect-kw-values 0 nil)))))))

(defun call-has-kw-p (args)
  "Check if call arguments contain keywords"
  (if (null args)
      nil
      (if (keyword-name-p (car args))
          t
          (call-has-kw-p (cdr args)))))

(defun flatten-parsed-params (parsed)
  "Convert parsed params (positional . kw-specs) to flat param list.
   Keyword specs ((NAME DEFAULT) ...) become just (NAME ...) in result."
  (let ((pos-params (car parsed))
        (kw-specs (cdr parsed)))
    (labels ((extract-names (specs acc)
               (if (null specs)
                   (reverse acc)
                   (extract-names (cdr specs)
                                  (cons (car (car specs)) acc)))))
      (append pos-params (extract-names kw-specs nil)))))

;;; ============================================================
;;; Defun and Function Call Support
;;; ============================================================

;; Function environment: alist of (name parsed-params body) for inlining
;; parsed-params is (positional-params . keyword-specs) from parse-lambda-list
;; Used for forward references during two-pass compilation

(defun collect-defuns (forms acc)
  "Pass 1: Collect all defun info (name params body) from forms"
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
                          (cons 'progn body-forms))))
             (collect-defuns (cdr forms) (cons (list nm ps bd) acc))))
          ((and (consp f) (eq (car f) 'progn))
           (collect-defuns (cdr forms)
                                (collect-defuns (cdr f) acc)))
          (t (collect-defuns (cdr forms) acc))))))

;;; ============================================================
;;; Function Inlining Support
;;; ============================================================

(defun expr-size (expr)
  "Estimate expression size for inlining decisions"
  (cond
    ((null expr) 1)
    ((not (consp expr)) 1)
    ((eq (car expr) 'quote) 1)
    ((eq (car expr) 'progn)
     (let ((sum 0))
       (let ((es (cdr expr)))
         (while (not (null es))
           (setq sum (+ sum (expr-size (car es))))
           (setq es (cdr es))))
       sum))
    ((eq (car expr) 'if)
     (+ 1 (expr-size (cadr expr)) (expr-size (caddr expr))
        (if (cadddr expr) (expr-size (cadddr expr)) 0)))
    ((eq (car expr) 'let)
     (+ 2 (expr-size (caddr expr))))
    ((eq (car expr) 'let*)
     (+ 2 (expr-size (caddr expr))))
    ((or (eq (car expr) 'or) (eq (car expr) 'and))
     (let ((sum 1))
       (let ((es (cdr expr)))
         (while (not (null es))
           (setq sum (+ sum (expr-size (car es))))
           (setq es (cdr es))))
       sum))
    (t (+ 1 (length (cdr expr))))))

(defun calls-self? (expr fn-name)
  "Check if expression calls fn-name (direct recursion)"
  (cond
    ((null expr) nil)
    ((not (consp expr)) nil)
    ((eq (car expr) 'quote) nil)
    ((and (symbolp (car expr)) (eq (car expr) fn-name)) t)
    (t (let ((found nil)
             (es (cdr expr)))
         (while (and (not found) (not (null es)))
           (setq found (calls-self? (car es) fn-name))
           (setq es (cdr es)))
         found))))

(defun inlinable? (fn-info)
  "Check if function is eligible for inlining.
   FN-INFO is (name params body).
   Inline if: small body, no recursion, simple predicates"
  (let ((name (car fn-info))
        (params (cadr fn-info))
        (body (caddr fn-info)))
    (and (< (expr-size body) 20)           ; Small enough
         (not (calls-self? body name))     ; Not recursive
         (<= (length params) 4))))         ; Few parameters

(defun substitute-params (expr params args)
  "Replace parameters with QUOTED arguments in expression.
   PARAMS is list of parameter names, ARGS is list of argument exprs.
   Arguments are quoted so they aren't evaluated when macro body is eval'd."
  (cond
    ((null expr) nil)
    ((symbolp expr)
     ;; Check if it's a parameter
     (let ((pos (find-param-pos expr params 0)))
       (if pos
           ;; Quote the argument form so it's not evaluated
           (let ((arg (nth pos args)))
             (if (or (symbolp arg) (consp arg))
                 (list 'quote arg)
                 arg))  ; Literals don't need quoting
           expr)))
    ((not (consp expr)) expr)
    ((eq (car expr) 'quote) expr)  ; Don't substitute in quotes
    (t (cons (substitute-params (car expr) params args)
             (substitute-params (cdr expr) params args)))))

(defun find-param-pos (name params idx)
  "Find position of name in params list"
  (cond
    ((null params) nil)
    ((eq name (car params)) idx)
    (t (find-param-pos name (cdr params) (+ idx 1)))))

(defun get-fn-info (name fenv)
  "Get function info (name params body) from fenv"
  (cond
    ((null fenv) nil)
    ((eq name (car (car fenv))) (car fenv))
    (t (get-fn-info name (cdr fenv)))))

#-sbcl
(defun compile-defun (name params body env fenv)
  "Compile a single defun to (name params body-ir param-base).
   Handles &key by parsing lambda list and flattening keyword params."
  (let* ((parsed (parse-lambda-list params))
         ;; Flatten params for environment (positional + keyword names)
         (flat-params (flatten-parsed-params parsed))
         (new-env (extend-env flat-params env))
         (pb (if flat-params (flat-env-lookup (car flat-params) new-env) 0))
         (body-ir (compile-expr-full body new-env fenv)))
    ;; Return flat params so codegen knows actual arity
    (list name flat-params body-ir pb)))

(defun extend-env (params env)
  "Extend environment with parameter bindings - append to preserve offset consistency"
  (append env params))

(defun skip-docstring (body-forms)
  "Skip docstring if present (string as first body element with more forms)"
  (if (and (stringp (car body-forms)) (cdr body-forms))
      (cdr body-forms)
      body-forms))

(defun compile-all-defuns (forms env fenv acc)
  "Pass 2: Compile all defuns with complete fenv"
  (if (null forms)
      acc
      (let ((f (car forms)))
        (cond
          ((and (consp f) (eq (car f) 'defun))
           (let* ((nm (cadr f))
                  (ps (caddr f))
                  (body-forms (skip-docstring (cdddr f)))
                  (bd (if (null (cdr body-forms))
                          (car body-forms)
                          (cons 'progn body-forms)))
                  (cf (compile-defun nm ps bd env fenv)))
             (compile-all-defuns (cdr forms) env fenv (cons cf acc))))
          ((and (consp f) (eq (car f) 'progn))
           (compile-all-defuns (cdr forms) env fenv
                                    (compile-all-defuns (cdr f) env fenv acc)))
          (t (compile-all-defuns (cdr forms) env fenv acc))))))

#-sbcl
(defun package-form-p (f)
  "Check if form is defpackage or in-package (handled at read time)"
  (and (consp f)
       (or (eq (car f) 'defpackage)
           (eq (car f) 'in-package))))

#-sbcl
(defun find-main-form (forms acc)
  "Find all non-defun forms and wrap in progn if multiple.
   Skips defpackage and in-package forms (handled at read time)."
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
          ((package-form-p f)
           ;; Skip defpackage and in-package forms
           (find-main-form (cdr forms) acc))
          ((and (consp f) (eq (car f) 'progn))
           (find-main-form (cdr forms)
                                (find-main-form (cdr f) acc)))
          (t (find-main-form (cdr forms) (cons f acc)))))))

(defun compile-call (expr env fenv)
  "Compile function call (fn arg1 arg2 ...).
   Handles keyword arguments by rewriting to positional form.
   Inlines small functions to avoid call overhead."
  (let ((fn-name (car expr))
        (args (cdr expr)))
    ;; Look up in fenv to check if it's a defined function
    (let ((fn-info (get-fn-info fn-name fenv)))
      (if fn-info
          (let* ((params (cadr fn-info))
                 (body (caddr fn-info))
                 ;; Parse lambda list to check for &key params
                 (parsed (parse-lambda-list params))
                 (pos-params (car parsed))
                 (kw-specs (cdr parsed))
                 ;; Rewrite args if call has keywords and fn accepts them
                 (final-args (if (and kw-specs (call-has-kw-p args))
                                 (rewrite-kw-call args (length pos-params) kw-specs)
                                 args)))
            ;; Check if we should inline
            (if (inlinable? fn-info)
                ;; Inline: substitute parameters with arguments in body
                (let ((inlined-body (substitute-params body params final-args)))
                  (compile-expr-full inlined-body env fenv))
                ;; Normal call - record for link-time verification
                (progn
                  #+sbcl (record-call-target fn-name)
                  (list 'call-fn fn-name
                        (compile-args final-args env fenv)))))
          ;; Unknown function - record warning and crash at runtime
          (progn
            #+sbcl (record-undefined-function fn-name)
            ;; Generate sys-exit with code 200 (undefined function error)
            (list 'sys-exit-ir (list 'lit 200)))))))

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
         (free-offsets (mapcar (lambda (v) (flat-env-lookup v env)) free-vars))
         ;; Build environment for body: free vars + params
         ;; Free vars come first (captured in closure env), then params
         ;; This matches the regular compiler's approach
         (body-env (extend-env params (extend-env free-vars nil)))
         ;; Compile body with extended env
         (body-ir (compile-expr-full body body-env fenv)))
    ;; Return lambda-ir with 5 elements (matching regular compiler)
    (list 'lambda-ir params body-ir free-vars free-offsets)))

#-sbcl
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
                (if (and (flat-env-lookup e env)
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

#-sbcl
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

#-sbcl
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
         (list 'nil-ir)  ; nil has tag 6, distinct from fixnum 0
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
         ((eq (car expr) 'while) (compile-while expr env fenv))
         ;; Boolean operators - transform to if forms
         ((eq (car expr) 'and) (compile-and expr env fenv))
         ((eq (car expr) 'or) (compile-or expr env fenv))
         ((eq (car expr) 'not) (list 'cmp-eq (compile-expr-full (cadr expr) env fenv) (list 'nil-ir)))

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
         ;; null: compare value to nil (tag 6)
         ((eq (car expr) 'null) (list 'cmp-eq (compile-expr-full (nth 1 expr) env fenv) (list 'nil-ir)))
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

         ;; length - list length via inline labels
         ;; NOTE: Uses list instead of backquote for portability (no SB-IMPL::COMMA)
         ((eq (car expr) 'length)
          (let ((len-iter-fn (gensym "LEN-ITER"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC")))
            (compile-expr-full
             (list 'labels
                   (list (list len-iter-fn (list lst-var acc-var)
                               (list 'if (list 'null lst-var)
                                     acc-var
                                     (list len-iter-fn (list 'cdr lst-var)
                                           (list '+ acc-var 1)))))
                   (list len-iter-fn (nth 1 expr) 0))
             env fenv)))

         ;; reverse - reverse list via inline labels
         ;; NOTE: Uses list instead of backquote for portability (no SB-IMPL::COMMA)
         ((eq (car expr) 'reverse)
          (let ((rev-iter-fn (gensym "REV-ITER"))
                (lst-var (gensym "LST"))
                (acc-var (gensym "ACC"))
                (next-acc-var (gensym "NEXT-ACC")))
            (compile-expr-full
             (list 'labels
                   (list (list rev-iter-fn (list lst-var acc-var)
                               (list 'if (list 'null lst-var)
                                     acc-var
                                     (list 'let (list (list next-acc-var
                                                            (list 'cons (list 'car lst-var) acc-var)))
                                           (list rev-iter-fn (list 'cdr lst-var) next-acc-var)))))
                   (list rev-iter-fn (nth 1 expr) nil))
             env fenv)))

         ;; String operations - use -ir suffix to match codegen
         ((eq (car expr) 'string-length) (list 'string-length-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'string-ref) (list 'string-ref-ir
                                    (compile-expr-full (nth 1 expr) env fenv)
                                    (compile-expr-full (nth 2 expr) env fenv)))
         ;; char-at - safe string-ref that returns 0 beyond end
         ;; Expands to: (if (>= pos (string-length str)) 0 (string-ref str pos))
         ((eq (car expr) 'char-at)
          (let ((str-sym (gensym "STR"))
                (pos-sym (gensym "POS")))
            (compile-expr-full
             (list 'let (list (list str-sym (nth 1 expr))
                              (list pos-sym (nth 2 expr)))
                   (list 'if (list '>= pos-sym (list 'string-length str-sym))
                         0
                         (list 'string-ref str-sym pos-sym)))
             env fenv)))
         ;; string-concat / sys:string-concat - concatenate two strings
         ((or (eq (car expr) 'string-concat)
              (eq (car expr) 'sys:string-concat))
          (list 'string-concat-ir
                (compile-expr-full (nth 1 expr) env fenv)
                (compile-expr-full (nth 2 expr) env fenv)))
         ;; string-equal - compare two strings
         ((eq (car expr) 'string-equal)
          (list 'string-equal-ir
                (compile-expr-full (nth 1 expr) env fenv)
                (compile-expr-full (nth 2 expr) env fenv)))
         ;; char-code - in Habu, characters ARE fixnums, so this is identity
         ((eq (car expr) 'char-code) (compile-expr-full (nth 1 expr) env fenv))

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
         ((eq (car expr) 'get-cmdline-args) (list 'get-cmdline-args-ir))
         ((eq (car expr) 'get-intern-table) (list 'get-intern-table-ir))
         ((eq (car expr) 'set-intern-table) (list 'set-intern-table-ir (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'get-lambda-counter) (list 'get-lambda-counter-ir))
         ((eq (car expr) 'set-lambda-counter) (list 'set-lambda-counter-ir (compile-expr-full (nth 1 expr) env fenv)))
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
         ((eq (car expr) 'sys-write-char) (list 'sys-write-char-ir
                                                (compile-expr-full (nth 1 expr) env fenv)
                                                (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'sys-read-byte) (list 'sys-read-byte-ir
                                               (compile-expr-full (nth 1 expr) env fenv)))
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
         ((eq (car expr) 'buffer-byte-ref) (list 'buffer-byte-ref-ir
                                                  (compile-expr-full (nth 1 expr) env fenv)
                                                  (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'buffer-byte-set) (list 'buffer-byte-set-ir
                                                  (compile-expr-full (nth 1 expr) env fenv)
                                                  (compile-expr-full (nth 2 expr) env fenv)
                                                  (compile-expr-full (nth 3 expr) env fenv)))

         ;; JIT primitives for native code generation
         ((eq (car expr) 'jit-mmap) (list 'mmap-jit-ir
                                          (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'jit-write-protect) (list 'pthread-jit-write-protect-np-ir
                                                   (compile-expr-full (nth 1 expr) env fenv)))
         ((eq (car expr) 'jit-dcache-flush) (list 'sys-dcache-flush-ir
                                                  (compile-expr-full (nth 1 expr) env fenv)
                                                  (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'jit-icache-invalidate) (list 'sys-icache-invalidate-ir
                                                       (compile-expr-full (nth 1 expr) env fenv)
                                                       (compile-expr-full (nth 2 expr) env fenv)))
         ((eq (car expr) 'jit-call) (list 'funcall-ptr-ir
                                          (compile-expr-full (nth 1 expr) env fenv)
                                          (compile-args (cddr expr) env fenv)))
         ;; Memory access for JIT code writing
         ((eq (car expr) 'mem-set-byte) (list 'mem-set-byte-ir
                                              (compile-expr-full (nth 1 expr) env fenv)
                                              (compile-expr-full (nth 2 expr) env fenv)
                                              (compile-expr-full (nth 3 expr) env fenv)))
         ((eq (car expr) 'mem-load-64) (list 'mem-load-64-ir
                                             (compile-expr-full (nth 1 expr) env fenv)
                                             (compile-expr-full (nth 2 expr) env fenv)))

         ;; native-read-file: expand to let* with sys-open/read/close
         ;; Expands to: (let* ((fd (sys-open path 0 0))
         ;;                     (buf (make-vector 524288))  ; 512KB buffer for combined sources
         ;;                     (n (sys-read fd buf 524288)))
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
                               (list buf-sym (list 'make-vector 524288))
                               (list n-sym (list 'sys-read fd-sym buf-sym 524288)))
                   (list 'progn
                         (list 'sys-close fd-sym)
                         (list 'buffer-to-string buf-sym n-sym)))
             env fenv)))

         ;; native-read-file-large: read file in chunks using while loop
         ;; Expands to: (let* ((fd (sys-open path 0 0))
         ;;                     (buf (make-vector 4096))
         ;;                     (chunks nil) (total 0) (n 0))
         ;;               (while (progn (setq n (sys-read fd buf 4096)) (> n 0))
         ;;                 (setq chunks (cons (buffer-to-string buf n) chunks))
         ;;                 (setq total (+ total n)))
         ;;               (sys-close fd)
         ;;               (concat-string-list-iter chunks total))
         ((eq (car expr) 'native-read-file-large)
          (let ((path-sym (gensym "PATH"))
                (fd-sym (gensym "FD"))
                (buf-sym (gensym "BUF"))
                (chunks-sym (gensym "CHUNKS"))
                (total-sym (gensym "TOTAL"))
                (n-sym (gensym "N")))
            (compile-expr-full
             (list 'let* (list (list path-sym (nth 1 expr))
                               (list fd-sym (list 'sys-open path-sym 0 0))
                               (list buf-sym (list 'make-vector 4096))
                               (list chunks-sym nil)
                               (list total-sym 0)
                               (list n-sym 0))
                   (list 'progn
                         (list 'while (list 'progn
                                           (list 'setq n-sym (list 'sys-read fd-sym buf-sym 4096))
                                           (list '> n-sym 0))
                               (list 'setq chunks-sym (list 'cons (list 'buffer-to-string buf-sym n-sym) chunks-sym))
                               (list 'setq total-sym (list '+ total-sym n-sym)))
                         (list 'sys-close fd-sym)
                         (list 'concat-string-list-iter chunks-sym total-sym)))
             env fenv)))

         ;; concat-string-list-iter: iterative string concatenation
         ;; Uses nested while loops instead of recursion
         ((eq (car expr) 'concat-string-list-iter)
          (let* ((chunks-var (gensym "CHUNKS"))
                 (total-var (gensym "TOTAL"))
                 (vec-var (gensym "VEC"))
                 (rev-chunks-var (gensym "REV-CHUNKS"))
                 (offset-var (gensym "OFFSET"))
                 (chunk-var (gensym "CHUNK"))
                 (len-var (gensym "LEN"))
                 (i-var (gensym "I")))
            (compile-expr-full
             (list 'let* (list (list chunks-var (nth 1 expr))
                               (list total-var (nth 2 expr))
                               (list vec-var (list 'make-vector total-var))
                               (list rev-chunks-var (list 'reverse chunks-var))
                               (list offset-var 0))
                   (list 'progn
                         (list 'while rev-chunks-var
                               (list 'let* (list (list chunk-var (list 'car rev-chunks-var))
                                                 (list len-var (list 'string-length chunk-var))
                                                 (list i-var 0))
                                     (list 'while (list '< i-var len-var)
                                           (list 'vector-set vec-var
                                                 (list '+ offset-var i-var)
                                                 (list 'string-ref chunk-var i-var))
                                           (list 'setq i-var (list '+ i-var 1)))
                                     (list 'setq offset-var (list '+ offset-var len-var))
                                     (list 'setq rev-chunks-var (list 'cdr rev-chunks-var))))
                         (list 'make-string-from-vector vec-var)))
             env fenv)))

         ;; Pattern matching - (match expr (pattern body)...)
         ((eq (car expr) 'match)
          (compile-match expr env fenv))

         ;; Unknown - try as function call or inline lambda
         (t (cond
              ((symbolp (car expr)) (compile-call expr env fenv))
              ;; Inline lambda call: ((lambda (x) ...) arg)
              ((and (consp (car expr)) (eq (car (car expr)) 'lambda))
               (list 'funcall-ir
                     (compile-lambda (car expr) env fenv)
                     (compile-args (cdr expr) env fenv)))
              (t (compile-lit 0))))))))

;;; Pattern matching uses shared expand-match from expand.lisp
(defun compile-match (expr env fenv)
  "Compile (match scrutinee (pattern body...)...) to IR.
   Uses expand-match from expand.lisp for source-to-source transformation."
  (compile-expr-full (expand-match (cadr expr) (cddr expr)) env fenv))

;; Helper functions for full compiler

(defun compile-if-full (expr env fenv)
  (let ((test (compile-expr-full (nth 1 expr) env fenv))
        (then (compile-expr-full (nth 2 expr) env fenv))
        (else (if (nth 3 expr)
                  (compile-expr-full (nth 3 expr) env fenv)
                  (list 'nil-ir))))
    (list 'if-ir test then else)))

;; Control flow - use shared expansions from expand.lisp
(defun compile-cond (expr env fenv)
  "Compile (cond ...) using expand-cond."
  (compile-expr-full (expand-cond (cdr expr)) env fenv))

(defun compile-when (expr env fenv)
  "Compile (when test body...) to (if test (progn body...) nil)."
  (let ((test (cadr expr))
        (body (cddr expr)))
    (compile-expr-full (list 'if test (cons 'progn body) nil) env fenv)))

(defun compile-unless (expr env fenv)
  "Compile (unless test body...) to (if test nil (progn body...))."
  (let ((test (cadr expr))
        (body (cddr expr)))
    (compile-expr-full (list 'if test nil (cons 'progn body)) env fenv)))

(defun compile-while (expr env fenv)
  "Compile (while test body...) - true iteration with no stack growth"
  (let ((test (compile-expr-full (nth 1 expr) env fenv))
        (body (compile-progn-full (cons 'progn (cddr expr)) env fenv)))
    (list 'while-ir test body)))

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

;; Boolean operators - use shared expansions (fixes double-eval bug in or)
(defun compile-and (expr env fenv)
  "Compile (and ...) using expand-and."
  (compile-expr-full (expand-and (cdr expr)) env fenv))

(defun compile-or (expr env fenv)
  "Compile (or ...) using expand-or. Properly avoids double evaluation."
  (compile-expr-full (expand-or (cdr expr)) env fenv))

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
  "Compile (let* ...) using expand-let* to nested let forms."
  (compile-expr-full (expand-let* (nth 1 expr) (cddr expr)) env fenv))

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
                 (list 'nil-ir)
                 (list 'cons-ir
                       (compile-expr-full (car elems) env fenv)
                       (expand-list (cdr elems))))))
    (expand-list (cdr expr))))

(defun compile-setq (expr env fenv)
  "Compile (setq var val)"
  (let ((var (nth 1 expr))
        (val (nth 2 expr)))
    (let ((offset (flat-env-lookup var env)))
      (if offset
          (list 'setq-ir offset (compile-expr-full val env fenv))
          (list 'nil-ir)))))  ;; Unknown var

;;; ============================================================
;;; Source-Level Inlining Nanopass
;;; ============================================================
;;; This pass inlines small functions at the SOURCE level before compilation.
;;; It transforms function calls into their expanded bodies, recursively.
;;; This is critical for performance - avoids function call overhead for
;;; small predicates like whitespace?, digit?, alpha?, symbol-char?.

(defun src-inline-expr (expr fenv)
  "Inline small functions in source expression EXPR.
   FENV is alist of (name params body) for all defuns."
  (cond
    ((null expr) nil)
    ((not (consp expr)) expr)
    ((eq (car expr) 'quote) expr)
    ;; Function call - check if inlinable
    ((and (symbolp (car expr))
          (not (src-special-form? (car expr))))
     (let ((fn-info (src-fn-lookup (car expr) fenv)))
       (if (and fn-info (src-inlinable? fn-info))
           ;; Inline: substitute params with args
           (let* ((params (cadr fn-info))
                  (body (caddr fn-info))
                  (args (cdr expr)))
             (if (= (length params) (length args))
                 ;; Recursively inline in the result
                 (src-inline-expr
                  (src-subst body params args)
                  fenv)
                 ;; Arg count mismatch - just inline args
                 (cons (car expr)
                       (src-inline-args (cdr expr) fenv))))
           ;; Not inlinable - just inline args
           (cons (car expr)
                 (src-inline-args (cdr expr) fenv)))))
    ;; Special forms
    ((eq (car expr) 'if)
     (list 'if
           (src-inline-expr (cadr expr) fenv)
           (src-inline-expr (caddr expr) fenv)
           (if (cadddr expr)
               (src-inline-expr (cadddr expr) fenv)
               nil)))
    ((eq (car expr) 'progn)
     (cons 'progn (src-inline-args (cdr expr) fenv)))
    ((or (eq (car expr) 'let) (eq (car expr) 'let*))
     (list (car expr)
           (src-inline-bindings (cadr expr) fenv)
           (src-inline-expr (caddr expr) fenv)))
    ((eq (car expr) 'lambda)
     (list 'lambda (cadr expr)
           (src-inline-expr (caddr expr) fenv)))
    ((eq (car expr) 'labels)
     ;; Don't inline into labels - local functions might shadow
     expr)
    ((eq (car expr) 'cond)
     (cons 'cond
           (mapcar (lambda (clause)
                     (src-inline-args clause fenv))
                   (cdr expr))))
    ((or (eq (car expr) 'when) (eq (car expr) 'unless))
     (cons (car expr) (src-inline-args (cdr expr) fenv)))
    ((or (eq (car expr) 'and) (eq (car expr) 'or))
     (cons (car expr) (src-inline-args (cdr expr) fenv)))
    ((eq (car expr) 'setq)
     (list 'setq (cadr expr) (src-inline-expr (caddr expr) fenv)))
    ((eq (car expr) 'while)
     (cons 'while (src-inline-args (cdr expr) fenv)))
    ((eq (car expr) 'function)
     expr)
    ((eq (car expr) 'funcall)
     (cons 'funcall (src-inline-args (cdr expr) fenv)))
    ;; Default: recurse
    (t (cons (src-inline-expr (car expr) fenv)
             (src-inline-expr (cdr expr) fenv)))))

(defun src-inline-args (args fenv)
  "Inline into a list of arguments"
  (if (null args)
      nil
      (cons (src-inline-expr (car args) fenv)
            (src-inline-args (cdr args) fenv))))

(defun src-inline-bindings (bindings fenv)
  "Inline into let/let* bindings"
  (if (null bindings)
      nil
      (let ((b (car bindings)))
        (cons (list (car b) (src-inline-expr (cadr b) fenv))
              (src-inline-bindings (cdr bindings) fenv)))))

(defun src-special-form? (sym)
  "Check if symbol is a special form"
  (let ((specials '(quote if progn let let* lambda labels cond when unless
                    and or setq defun while function funcall)))
    (if (null specials)
        nil
        (src-member? sym specials))))

(defun src-member? (x lst)
  "Check if x is in lst"
  (cond
    ((null lst) nil)
    ((eq x (car lst)) t)
    (t (src-member? x (cdr lst)))))

(defun src-fn-lookup (name fenv)
  "Look up function in fenv"
  (cond
    ((null fenv) nil)
    ((eq name (car (car fenv))) (car fenv))
    (t (src-fn-lookup name (cdr fenv)))))

(defun src-inlinable? (fn-info)
  "Check if function should be inlined.
   Inline if: small body, not recursive, few params."
  (let ((name (car fn-info))
        (params (cadr fn-info))
        (body (caddr fn-info)))
    (and (<= (src-size body) 20)   ; Allow larger functions like alpha?
         (not (src-calls? body name))
         (<= (length params) 4))))

(defun src-size (expr)
  "Estimate size of source expression"
  (cond
    ((null expr) 1)
    ((not (consp expr)) 1)
    ((eq (car expr) 'quote) 1)
    ((or (eq (car expr) 'progn)
         (eq (car expr) 'and)
         (eq (car expr) 'or))
     (let ((sum 1) (es (cdr expr)))
       (while (not (null es))
         (setq sum (+ sum (src-size (car es))))
         (setq es (cdr es)))
       sum))
    ((eq (car expr) 'if)
     (+ 1 (src-size (cadr expr))
        (src-size (caddr expr))
        (if (cadddr expr) (src-size (cadddr expr)) 0)))
    ((or (eq (car expr) 'let) (eq (car expr) 'let*))
     (+ 2 (src-size (caddr expr))))
    (t (+ 1 (length (cdr expr))))))

(defun src-calls? (expr fn-name)
  "Check if expression contains a call to fn-name"
  (cond
    ((null expr) nil)
    ((not (consp expr)) nil)
    ((eq (car expr) 'quote) nil)
    ((and (symbolp (car expr)) (eq (car expr) fn-name)) t)
    (t (or (src-calls? (car expr) fn-name)
           (src-calls? (cdr expr) fn-name)))))

(defun src-subst (expr params args)
  "Substitute params with args in expression"
  (cond
    ((null expr) nil)
    ((symbolp expr)
     (let ((pos (src-param-pos expr params 0)))
       (if pos
           (nth pos args)
           expr)))
    ((not (consp expr)) expr)
    ((eq (car expr) 'quote) expr)
    (t (cons (src-subst (car expr) params args)
             (src-subst (cdr expr) params args)))))

(defun src-param-pos (name params idx)
  "Find position of name in params"
  (cond
    ((null params) nil)
    ((eq name (car params)) idx)
    (t (src-param-pos name (cdr params) (+ idx 1)))))

(defun src-inline-defuns (forms fenv)
  "Apply source inlining to all defun bodies"
  (if (null forms)
      nil
      (let ((f (car forms)))
        (cons
         (if (and (consp f) (eq (car f) 'defun))
             (let* ((name (cadr f))
                    (params (caddr f))
                    (body-forms (cdddr f)))
               (list* 'defun name params
                      (src-inline-args body-forms fenv)))
             (src-inline-expr f fenv))
         (src-inline-defuns (cdr forms) fenv)))))

(defun src-inline-all (forms)
  "Apply source-level inlining to all forms.
   First collects all defuns, then inlines into all bodies."
  (let ((fenv (collect-defuns forms nil)))
    (src-inline-defuns forms fenv)))

;;; ============================================================
;;; Full Program Compiler
;;; ============================================================

#-sbcl
(defun compile-forms (forms)
  "Compile forms to (defun-list main-ir) - proper list like main compiler.
   Applies TCO (tail-call optimization) to all defuns as a nanopass.
   Note: Source-level inlining disabled - causes stack overflow in compiled reader."
  ;; Skip source-level inlining for now - it creates too deep expressions
  ;; (let* ((inlined-forms (src-inline-all forms)) ...)
  (let* ((fenv (collect-defuns forms nil))
         (defuns-raw (compile-all-defuns forms nil fenv nil))
         ;; Apply TCO nanopass to all compiled functions
         (defuns (apply-tco-to-all-functions defuns-raw))
         (main-form (find-main-form forms nil))
         (main-ir (compile-expr-full main-form nil fenv)))
    (list defuns main-ir)))

;;; ============================================================
;;; Integration with Existing Codegen (SBCL-only bridging functions)
;;; ============================================================

#+sbcl
(defun compile-to-bytecode (expr)
  "Compile expression to ARM64 bytecode using existing codegen.
   This bridges pure compiler to existing codegen (which is already pure!)"
  (let ((ir (compile-expr-v2 expr nil)))
    ;; Call existing codegen (it's already pure - just builds byte lists!)
    ;; codegen signature: (ir rtaddrs fnoffs temp-depth)
    (let ((code-with-markers (codegen ir nil nil 0)))
      ;; Resolve markers to actual bytes
      (resolve-calls code-with-markers nil))))

#+sbcl
(defun compile-program-simple (forms)
  "Compile simple program (single expression) to complete bytecode.
   Uses existing codegen-main which adds prologue/epilogue."
  (if (null forms)
      nil
      (let ((main-expr (if (null (cdr forms))
                           (car forms)  ;; Single form
                           (cons 'progn forms))))  ;; Multiple forms -> progn
        (let ((ir (compile-expr-v2 main-expr nil)))
          ;; Use existing codegen-main (adds prologue/epilogue)
          (codegen-main ir nil)))))

;;; Self-hosting entry point
#+sbcl
(defun self-compile (source-path output-path)
  "Pure Habu self-hosting compiler entry point (SBCL version).
   Reads source, compiles with pure compiler, generates ARM64, writes executable."
  (let ((source (native-read-file source-path)))
    (if source
        (progn
          (deliver source output-path)
          (sb-ext:exit :code 0))
        (progn
          (sb-ext:exit :code 1)))))

#-sbcl
(defun concat5 (a b c d e)
  "Concatenate 5 strings using iterative method (avoids broken string-concat)"
  (let ((total (+ (string-length a)
                  (+ (string-length b)
                     (+ (string-length c)
                        (+ (string-length d) (string-length e)))))))
    ;; Build list in reverse order for concat-string-list-iter
    (concat-string-list-iter (list e d c b a) total)))

#-sbcl
(defun concat8 (a b c d e f g h)
  "Concatenate 8 strings using iterative method"
  (let ((total (+ (string-length a)
                  (+ (string-length b)
                     (+ (string-length c)
                        (+ (string-length d)
                           (+ (string-length e)
                              (+ (string-length f)
                                 (+ (string-length g) (string-length h))))))))))
    ;; Build list in reverse order for concat-string-list-iter
    (concat-string-list-iter (list h g f e d c b a) total)))

#-sbcl
(defun self-compile (source-path output-path)
  "Pure Habu self-hosting compiler entry point (native version).
   Reads all source files, concatenates them, compiles to native executable.
   source-path is ignored - we read the hardcoded bootstrap paths.
   Uses native-read-file-large to handle files >65KB (each file can be up to 100KB).
   Now includes arm64/asm.lisp and gc.lisp for full self-hosting."
  (let* ((a (native-read-file-large "/Users/joel/Work/habu/arm64/asm.lisp"))
         (gc (native-read-file-large "/Users/joel/Work/habu/bootstrap/gc.lisp"))
         (r (native-read-file-large "/Users/joel/Work/habu/bootstrap/reader.lisp"))
         (c (native-read-file-large "/Users/joel/Work/habu/bootstrap/compiler.lisp"))
         (o (native-read-file-large "/Users/joel/Work/habu/bootstrap/optimize.lisp"))
         (g (native-read-file-large "/Users/joel/Work/habu/bootstrap/codegen.lisp"))
         (m (native-read-file-large "/Users/joel/Work/habu/bootstrap/macho-utils.lisp")))
    (if (and a gc r c o g m)
        (let ((source (concat8 a gc r c o g m "(sys-exit 42)")))
          (deliver source output-path)
          (sys-exit 0))
        (sys-exit 1))))

;;; ============================================================
;;; Full Program Compilation
;;; ============================================================

#-sbcl
(defun compile-program (forms)
  "Compile forms to complete ARM64 bytecode with function linking.
   This is the full pipeline: parse -> IR -> lift-lambdas -> codegen -> link.
   Returns flat bytecode ready for Mach-O wrapping.
   Native version - does not call reset-symbol-table.
   Uses codegen.lisp API: lift-lambdas takes (ir lambdas), returns (ir . lambdas)"
  (let* ((r (compile-forms forms))
         (defun-fns (car r))
         (mir-raw (cadr r)))
    ;; Lift lambdas from main IR - codegen.lisp takes (ir lambdas)
    (let* ((mvb-result (lift-lambdas mir-raw nil))
           (mir (car mvb-result))
           (main-lambdas (cdr mvb-result)))
      ;; Lift lambdas from all defun bodies - codegen.lisp uses lift-lambdas-from-defuns
      (let* ((mvb-result2 (lift-lambdas-from-defuns defun-fns nil main-lambdas))
             (lifted-defuns (car mvb-result2))
             (defun-lambdas (cdr mvb-result2))
             ;; Convert lambdas to defun format for codegen
             (lambda-defuns (lambdas-to-defuns defun-lambdas nil))
             ;; Combine: defuns + lambda-defuns
             (fns (append lifted-defuns lambda-defuns)))
        (if (null fns)
            ;; No functions - simple case
            (resolve-calls (codegen-main mir nil) nil)
            ;; Has functions - need linking
            (let* ((main-code-temp (append (prologue)
                                           (codegen mir nil nil 0)
                                           (epilogue)))
                   (main-size (code-size main-code-temp))
                   (fnoffs (build-fnoffs fns main-size))
                   (main-code (append (prologue)
                                      (codegen mir nil fnoffs 0)
                                      (epilogue)))
                   (fn-code (codegen-all-fns fns nil fnoffs nil))
                   (all-code (append main-code fn-code)))
              (resolve-calls all-code fnoffs)))))))

;; NOTE: compile-program for SBCL is defined in compiler-sbcl.lisp
;; with signature (forms rtaddrs &key (optimize t))
;; Do NOT redefine here as it would conflict with deliver's expectations

;;; ============================================================
;;; Pure Delivery Helper Functions (no CL runtime dependencies)
;;; ============================================================

#-sbcl
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

#-sbcl
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

#-sbcl
(defun flatten-extern-calls (code stub-alist code-base-addr)
  "Replace extern call markers with BL instructions using assoc list.
   Returns (flat-code . extern-positions)
   Note: resolve-calls emits markers followed by 3 zeros - must skip them.
   Native Habu version - SBCL uses hash-table version in compiler-sbcl.lisp."
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
;; This is the native Habu version - SBCL uses compiler-sbcl.lisp's deliver
#-sbcl
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
         (wrapper-size 120))  ; 30 instructions * 4 bytes (embedded heap)

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

;;; Native entry point for Stage 1 compiler
#-sbcl
(defun main ()
  "Entry point for Stage 1 compiler.
   Initializes runtime and calls self-compile with hardcoded paths."
  ;; Initialize symbol table
  (ensure-symbols-registered)
  ;; Compile Stage 2
  (self-compile nil "/tmp/stage2")
  ;; Exit with success
  0)
