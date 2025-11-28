;;; habu0.lisp - Minimal standalone Habu compiler/interpreter
;;;
;;; This is the entry point for the self-hosting compiler.
;;; It reads a Lisp source file, parses it, and executes it.
;;;
;;; For now, it uses a simple expression evaluator.
;;; Full compilation to native code will be added later.

;; Cached operator symbols - set on first use during eval
;; Once a symbol is identified as an operator, we cache it for eq comparison
(defvar *op-quote* nil)
(defvar *op-if* nil)
(defvar *op-let* nil)
(defvar *op-let-star* nil)
(defvar *op-defun* nil)
(defvar *op-progn* nil)
(defvar *op-cond* nil)
(defvar *op-t* nil)
(defvar *op-plus* nil)
(defvar *op-minus* nil)
(defvar *op-mul* nil)
(defvar *op-div* nil)
(defvar *op-mod* nil)
(defvar *op-eq-num* nil)
(defvar *op-lt* nil)
(defvar *op-gt* nil)
(defvar *op-le* nil)
(defvar *op-ge* nil)
(defvar *op-cons* nil)
(defvar *op-car* nil)
(defvar *op-cdr* nil)
(defvar *op-null* nil)
(defvar *op-consp* nil)
(defvar *op-list* nil)
(defvar *op-not* nil)
(defvar *op-and* nil)
(defvar *op-or* nil)

;; File I/O constants
(defun o-rdonly () #x0)

;; Read entire file into string
(defun native-read-file (path)
  (let* ((fd (sys-open path (o-rdonly) #x0)))
    (if (< fd #x0)
        nil
        (let* ((buf-size #x10000)
               (buf (make-vector buf-size))
               (bytes-read (sys-read fd buf buf-size)))
          (sys-close fd)
          (if (< bytes-read #x0)
              nil
              (buffer-to-string buf bytes-read))))))

;;; Character predicates
(defun whitespace? (ch)
  (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))

(defun digit? (ch)
  (and (>= ch #x30) (<= ch #x39)))

(defun hex-digit? (ch)
  (or (digit? ch)
      (and (>= ch #x41) (<= ch #x46))
      (and (>= ch #x61) (<= ch #x66))))

(defun alpha? (ch)
  (or (and (>= ch #x41) (<= ch #x5A))
      (and (>= ch #x61) (<= ch #x7A))))

(defun symbol-char? (ch)
  (or (alpha? ch) (digit? ch)
      (= ch #x2D) (= ch #x5F) (= ch #x2B) (= ch #x2A)
      (= ch #x2F) (= ch #x3D) (= ch #x3C) (= ch #x3E)
      (= ch #x21) (= ch #x3F) (= ch #x26) (= ch #x25) (= ch #x3A)))

(defun char-at (source pos)
  (if (< pos (string-length source))
      (string-ref source pos)
      #x0))

(defun digit-val (ch) (- ch #x30))

(defun hex-val (ch)
  (cond ((digit? ch) (- ch #x30))
        ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA))
        ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA))
        (t #x0)))

;; Convert lowercase letter to uppercase (a-z -> A-Z)
(defun h0-char-upcase (ch)
  (if (and (>= ch #x61) (<= ch #x7A))
      (- ch #x20)
      ch))

(defun skip-line (source pos)
  (let ((ch (char-at source pos)))
    (if (or (= ch #x0A) (= ch #x0))
        (+ pos #x1)
        (skip-line source (+ pos #x1)))))

(defun skip-ws (source pos)
  (let ((ch (char-at source pos)))
    (cond ((whitespace? ch) (skip-ws source (+ pos #x1)))
          ((= ch #x3B) (skip-ws source (skip-line source (+ pos #x1))))
          (t pos))))

(defun read-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (read-digits source (+ pos #x1) (+ (* n #xA) (digit-val ch)))
        (cons n pos))))

(defun read-int (source pos)
  (let ((neg nil) (start pos))
    (let ((ch (char-at source pos)))
      (cond ((= ch #x2D) (setq neg t) (setq start (+ pos #x1)))
            ((= ch #x2B) (setq start (+ pos #x1)))))
    (let* ((r (read-digits source start #x0))
           (val (car r))
           (end (cdr r)))
      (cons (if neg (- #x0 val) val) end))))

(defun read-hex-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (hex-digit? ch)
        (read-hex-digits source (+ pos #x1) (+ (* n #x10) (hex-val ch)))
        (cons n pos))))

(defun read-hex (source pos)
  (read-hex-digits source pos #x0))

(defun skip-symbol (source pos)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (skip-symbol source (+ pos #x1))
        pos)))

;; String equality check
(defun h0-string= (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (labels ((cmp (i)
                   (if (>= i len1)
                       t
                       (if (= (string-ref s1 i) (string-ref s2 i))
                           (cmp (+ i #x1))
                           nil))))
          (cmp #x0))
        nil)))

;; Operator check with caching
;; Symbol comparison using string names
;; Since symbols from reader may have different IDs than compile-time symbols,
;; we always fall back to string comparison when eq fails
(defun op=quote (sym)
  (if (eq sym *op-quote*) t
      (if (h0-string= (symbol-name sym) "QUOTE")
          (progn (setq *op-quote* sym) t)
          nil)))

(defun op=if (sym)
  (if (eq sym *op-if*) t
      (if (h0-string= (symbol-name sym) "IF")
          (progn (setq *op-if* sym) t)
          nil)))

(defun op=let (sym)
  (if (eq sym *op-let*) t
      (if (h0-string= (symbol-name sym) "LET")
          (progn (setq *op-let* sym) t)
          nil)))

(defun op=defun (sym)
  (if (eq sym *op-defun*) t
      (if (h0-string= (symbol-name sym) "DEFUN")
          (progn (setq *op-defun* sym) t)
          nil)))

(defun op=t (sym)
  (if (eq sym *op-t*) t
      (if (h0-string= (symbol-name sym) "T")
          (progn (setq *op-t* sym) t)
          nil)))

(defun op=plus (sym)
  (if (eq sym *op-plus*) t
      (if (h0-string= (symbol-name sym) "+")
          (progn (setq *op-plus* sym) t)
          nil)))

(defun op=minus (sym)
  (if (eq sym *op-minus*) t
      (if (h0-string= (symbol-name sym) "-")
          (progn (setq *op-minus* sym) t)
          nil)))

(defun op=mul (sym)
  (if (eq sym *op-mul*) t
      (if (h0-string= (symbol-name sym) "*")
          (progn (setq *op-mul* sym) t)
          nil)))

(defun op=div (sym)
  (if (eq sym *op-div*) t
      (if (h0-string= (symbol-name sym) "/")
          (progn (setq *op-div* sym) t)
          nil)))

(defun op=eq-num (sym)
  (if (eq sym *op-eq-num*) t
      (if (h0-string= (symbol-name sym) "=")
          (progn (setq *op-eq-num* sym) t)
          nil)))

(defun op=lt (sym)
  (if (eq sym *op-lt*) t
      (if (h0-string= (symbol-name sym) "<")
          (progn (setq *op-lt* sym) t)
          nil)))

(defun op=gt (sym)
  (if (eq sym *op-gt*) t
      (if (h0-string= (symbol-name sym) ">")
          (progn (setq *op-gt* sym) t)
          nil)))

(defun op=le (sym)
  (if (eq sym *op-le*) t
      (if (h0-string= (symbol-name sym) "<=")
          (progn (setq *op-le* sym) t)
          nil)))

(defun op=ge (sym)
  (if (eq sym *op-ge*) t
      (if (h0-string= (symbol-name sym) ">=")
          (progn (setq *op-ge* sym) t)
          nil)))

(defun op=let-star (sym)
  (if (eq sym *op-let-star*) t
      (if (h0-string= (symbol-name sym) "LET*")
          (progn (setq *op-let-star* sym) t)
          nil)))

(defun op=progn (sym)
  (if (eq sym *op-progn*) t
      (if (h0-string= (symbol-name sym) "PROGN")
          (progn (setq *op-progn* sym) t)
          nil)))

(defun op=cond (sym)
  (if (eq sym *op-cond*) t
      (if (h0-string= (symbol-name sym) "COND")
          (progn (setq *op-cond* sym) t)
          nil)))

(defun op=mod (sym)
  (if (eq sym *op-mod*) t
      (if (h0-string= (symbol-name sym) "MOD")
          (progn (setq *op-mod* sym) t)
          nil)))

(defun op=cons (sym)
  (if (eq sym *op-cons*) t
      (if (h0-string= (symbol-name sym) "CONS")
          (progn (setq *op-cons* sym) t)
          nil)))

(defun op=car (sym)
  (if (eq sym *op-car*) t
      (if (h0-string= (symbol-name sym) "CAR")
          (progn (setq *op-car* sym) t)
          nil)))

(defun op=cdr (sym)
  (if (eq sym *op-cdr*) t
      (if (h0-string= (symbol-name sym) "CDR")
          (progn (setq *op-cdr* sym) t)
          nil)))

(defun op=null (sym)
  (if (eq sym *op-null*) t
      (if (h0-string= (symbol-name sym) "NULL")
          (progn (setq *op-null* sym) t)
          nil)))

(defun op=consp (sym)
  (if (eq sym *op-consp*) t
      (if (h0-string= (symbol-name sym) "CONSP")
          (progn (setq *op-consp* sym) t)
          nil)))

(defun op=list (sym)
  (if (eq sym *op-list*) t
      (if (h0-string= (symbol-name sym) "LIST")
          (progn (setq *op-list* sym) t)
          nil)))

(defun op=not (sym)
  (if (eq sym *op-not*) t
      (if (h0-string= (symbol-name sym) "NOT")
          (progn (setq *op-not* sym) t)
          nil)))

(defun op=and (sym)
  (if (eq sym *op-and*) t
      (if (h0-string= (symbol-name sym) "AND")
          (progn (setq *op-and* sym) t)
          nil)))

(defun op=or (sym)
  (if (eq sym *op-or*) t
      (if (h0-string= (symbol-name sym) "OR")
          (progn (setq *op-or* sym) t)
          nil)))

;; Generic symbol name comparison for cases not covered by caching
(defun op= (sym name)
  (if (symbolp sym)
      (h0-string= (symbol-name sym) name)
      nil))

(defun chars-to-string (chars)
  (let* ((len (length chars))
         (vec (make-vector len)))
    (labels ((fill-vec (cs i)
               (if (null cs)
                   vec
                   (progn
                     (vector-set vec i (h0-char-upcase (car cs)))
                     (fill-vec (cdr cs) (+ i 1))))))
      (make-string-from-vector (fill-vec chars 0)))))

(defun read-sym-chars (source pos acc)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (read-sym-chars source (+ pos #x1) (cons (h0-char-upcase ch) acc))
        (cons (reverse acc) pos))))

(defun read-sym (source pos)
  (let* ((r (read-sym-chars source pos nil))
         (chars (car r))
         (end (cdr r)))
    (cons (make-symbol-from-string (chars-to-string chars)) end)))

;; Read string literal
(defun read-str-chars (source pos acc)
  (let ((ch (char-at source pos)))
    (cond ((= ch #x22) (cons (reverse acc) (+ pos 1)))
          ((= ch #x5C)
           (let ((next (char-at source (+ pos 1))))
             (cond ((= next #x6E) (read-str-chars source (+ pos 2) (cons #x0A acc)))
                   ((= next #x74) (read-str-chars source (+ pos 2) (cons #x09 acc)))
                   ((= next #x22) (read-str-chars source (+ pos 2) (cons #x22 acc)))
                   ((= next #x5C) (read-str-chars source (+ pos 2) (cons #x5C acc)))
                   (t (read-str-chars source (+ pos 2) (cons next acc))))))
          ((= ch #x0) (cons (reverse acc) pos))
          (t (read-str-chars source (+ pos 1) (cons ch acc))))))

(defun read-str (source pos)
  (let* ((r (read-str-chars source (+ pos 1) nil))
         (chars (car r))
         (end (cdr r))
         (len (length chars))
         (vec (make-vector len)))
    (labels ((fill-vec (cs i)
               (if (null cs)
                   vec
                   (progn
                     (vector-set vec i (car cs))
                     (fill-vec (cdr cs) (+ i 1))))))
      (cons (make-string-from-vector (fill-vec chars 0)) end))))

;; Main reader with labels for mutual recursion
(defun habu-read (source pos)
  (labels
      ((read-list-elems (p)
         (let* ((p2 (skip-ws source p))
                (ch (char-at source p2)))
           (cond
             ((= ch #x29) (cons nil (+ p2 #x1)))
             ((= ch #x2E)
              (let* ((r (read-one (+ p2 #x1)))
                     (cdr-val (car r))
                     (p3 (cdr r))
                     (p4 (skip-ws source p3)))
                (cons cdr-val (+ p4 #x1))))
             ((= ch #x0) (cons nil p2))
             (t (let* ((er (read-one p2))
                       (el (car er))
                       (p3 (cdr er))
                       (rr (read-list-elems p3)))
                  (cons (cons el (car rr)) (cdr rr)))))))
       (read-list (p) (read-list-elems (+ p #x1)))
       (read-sharp (p)
         (let ((ch (char-at source (+ p #x1))))
           (cond
             ((or (= ch #x78) (= ch #x58)) (read-hex source (+ p #x2)))
             ((= ch #x27)
              (let ((r (read-one (+ p #x2))))
                (cons (list 'function (car r)) (cdr r))))
             (t (cons nil (+ p #x2))))))
       (read-one (p)
         (let* ((p2 (skip-ws source p))
                (ch (char-at source p2)))
           (if (>= p2 (string-length source))
               (cons nil p2)
               (cond
                 ((= ch #x28) (read-list p2))
                 ((= ch #x27)
                  (let ((r (read-one (+ p2 #x1))))
                    (cons (list 'quote (car r)) (cdr r))))
                 ((= ch #x22) (read-str source p2))
                 ((= ch #x23) (read-sharp p2))
                 ((or (digit? ch)
                      (and (or (= ch #x2D) (= ch #x2B))
                           (digit? (char-at source (+ p2 #x1)))))
                  (read-int source p2))
                 ((symbol-char? ch) (read-sym source p2))
                 ((= ch #x29) (cons nil (+ p2 #x1)))
                 (t (read-one (+ p2 #x1))))))))
    (read-one pos)))

(defun read-all (source)
  (let ((len (string-length source)))
    (labels ((ra (pos acc)
               (let ((p2 (skip-ws source pos)))
                 (if (>= p2 len)
                     (reverse acc)
                     (let ((r (habu-read source p2)))
                       (ra (cdr r) (cons (car r) acc)))))))
      (ra #x0 nil))))

(defun h0-read-from-string (s)
  (car (habu-read s 0)))

;;; Simple expression evaluator with function definitions
;;; This interpreter supports defun, let, and recursion.

;; Symbol name lookup for function environment
(defun sym-name= (sym name)
  (if (symbolp sym)
      (h0-string= (symbol-name sym) name)
      nil))

;; Look up function by symbol name in fenv
;; Entry is (name-string . (params . body))
(defun fenv-lookup (sym fenv)
  (if (null fenv) nil
      (let ((entry (car fenv)))
        (if (and (symbolp sym) (h0-string= (symbol-name sym) (car entry)))
            (cdr entry)  ;; Returns (params . body)
            (fenv-lookup sym (cdr fenv))))))

;; Create binding list from params and args
;; Store symbol names (strings) as keys, not symbols
(defun bind-args (params args env)
  (if (null params) env
      (cons (cons (symbol-name (car params)) (car args))
            (bind-args (cdr params) (cdr args) env))))

;; Look up by symbol name in environment
(defun env-lookup (sym env)
  (if (null env) nil
      (let ((entry (car env)))
        (if (h0-string= (symbol-name sym) (car entry))
            (cdr entry)
            (env-lookup sym (cdr env))))))

;; Helper for let bindings - iterates through bindings without recursive symbol issue
(defun h0-eval-let (bindings body env fenv)
  (if (null bindings)
      (h0-eval body env fenv)
      (let* ((b (car bindings))
             (var (symbol-name (car b)))
             (val (h0-eval (cadr b) env fenv)))
        (h0-eval-let (cdr bindings) body (cons (cons var val) env) fenv))))

;; Helper for progn - evaluates forms in sequence, returns last value
(defun h0-eval-progn (forms env fenv)
  (if (null forms)
      nil
      (if (null (cdr forms))
          (h0-eval (car forms) env fenv)
          (progn
            (h0-eval (car forms) env fenv)
            (h0-eval-progn (cdr forms) env fenv)))))

;; Helper for cond - evaluates clauses until one matches
(defun h0-eval-cond (clauses env fenv)
  (if (null clauses)
      nil
      (let* ((clause (car clauses))
             (test (car clause))
             (body (cdr clause)))
        (if (h0-eval test env fenv)
            (if (null body)
                t
                (h0-eval-progn body env fenv))
            (h0-eval-cond (cdr clauses) env fenv)))))

;; Helper for and - short-circuit evaluation
(defun h0-eval-and (forms env fenv)
  (if (null forms)
      t
      (let ((val (h0-eval (car forms) env fenv)))
        (if val
            (if (null (cdr forms))
                val
                (h0-eval-and (cdr forms) env fenv))
            nil))))

;; Helper for or - short-circuit evaluation
(defun h0-eval-or (forms env fenv)
  (if (null forms)
      nil
      (let ((val (h0-eval (car forms) env fenv)))
        (if val
            val
            (h0-eval-or (cdr forms) env fenv)))))

;; Eval function with fenv for function definitions
;; Uses cached op= functions for O(1) amortized dispatch
(defun h0-eval (expr env fenv)
  (cond
    ;; Numbers are self-evaluating
    ((numberp expr) expr)
    ;; nil is false
    ((null expr) nil)
    ;; t is true
    ((if (symbolp expr) (op=t expr) nil) t)
    ;; Symbol lookup in variable environment
    ((symbolp expr)
     (env-lookup expr env))
    ;; List - function call or special form
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; Quote - use cached op=quote
         ((if (symbolp op) (op=quote op) nil) (cadr expr))
         ;; If - use cached op=if
         ((if (symbolp op) (op=if op) nil)
          (if (h0-eval (cadr expr) env fenv)
              (h0-eval (caddr expr) env fenv)
              (if (cadddr expr) (h0-eval (cadddr expr) env fenv) nil)))
         ;; Let - use cached op=let, delegate to helper for iteration
         ((if (symbolp op) (op=let op) nil)
          (h0-eval-let (cadr expr) (caddr expr) env fenv))
         ;; Let* - same as let for sequential binding
         ((if (symbolp op) (op=let-star op) nil)
          (h0-eval-let (cadr expr) (caddr expr) env fenv))
         ;; Progn - evaluate forms in sequence
         ((if (symbolp op) (op=progn op) nil)
          (h0-eval-progn (cdr expr) env fenv))
         ;; Cond - multi-way conditional
         ((if (symbolp op) (op=cond op) nil)
          (h0-eval-cond (cdr expr) env fenv))
         ;; Defun - returns nil but defines function
         ((if (symbolp op) (op=defun op) nil) nil)
         ;; Arithmetic - use cached op= functions
         ((if (symbolp op) (op=plus op) nil) (+ (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))
         ((if (symbolp op) (op=minus op) nil) (- (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))
         ((if (symbolp op) (op=mul op) nil) (* (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))
         ((if (symbolp op) (op=div op) nil) (/ (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))
         ((if (symbolp op) (op=mod op) nil) (mod (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))
         ;; List operations
         ((if (symbolp op) (op=cons op) nil) (cons (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)))
         ((if (symbolp op) (op=car op) nil) (car (h0-eval (cadr expr) env fenv)))
         ((if (symbolp op) (op=cdr op) nil) (cdr (h0-eval (cadr expr) env fenv)))
         ((if (symbolp op) (op=list op) nil) (h0-eval-list (cdr expr) env fenv))
         ;; Type predicates
         ((if (symbolp op) (op=null op) nil) (if (null (h0-eval (cadr expr) env fenv)) t nil))
         ((if (symbolp op) (op=consp op) nil) (if (consp (h0-eval (cadr expr) env fenv)) t nil))
         ;; Boolean operations
         ((if (symbolp op) (op=not op) nil) (if (h0-eval (cadr expr) env fenv) nil t))
         ((if (symbolp op) (op=and op) nil) (h0-eval-and (cdr expr) env fenv))
         ((if (symbolp op) (op=or op) nil) (h0-eval-or (cdr expr) env fenv))
         ;; Comparisons - use cached op= functions
         ((if (symbolp op) (op=eq-num op) nil)
          (if (= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))
         ((if (symbolp op) (op=lt op) nil)
          (if (< (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))
         ((if (symbolp op) (op=gt op) nil)
          (if (> (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))
         ((if (symbolp op) (op=le op) nil)
          (if (<= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))
         ((if (symbolp op) (op=ge op) nil)
          (if (>= (h0-eval (cadr expr) env fenv) (h0-eval (caddr expr) env fenv)) t nil))
         ;; Function call - look up in fenv
         (t
          (let ((fn-entry (fenv-lookup op fenv)))
            (if fn-entry
                (let* ((params (car fn-entry))
                       (body (cdr fn-entry))
                       (args (h0-eval-list (cdr expr) env fenv))
                       (new-env (bind-args params args nil)))
                  (h0-eval body new-env fenv))
                #x0))))))
    (t #x0)))

;; Eval a list of expressions
(defun h0-eval-list (exprs env fenv)
  (if (null exprs) nil
      (cons (h0-eval (car exprs) env fenv)
            (h0-eval-list (cdr exprs) env fenv))))

;; Collect function definitions from forms
(defun collect-defuns (forms fenv)
  (if (null forms) fenv
      (let ((form (car forms)))
        (if (and (consp form) (symbolp (car form)) (op=defun (car form)))
            (let* ((name (symbol-name (cadr form)))
                   (params (caddr form))
                   (body (cadddr form)))
              (collect-defuns (cdr forms) (cons (cons name (cons params body)) fenv)))
            (collect-defuns (cdr forms) fenv)))))

;; Eval forms with collected function definitions
(defun h0-eval-forms (forms env fenv)
  (if (null forms)
      nil
      (let ((form (car forms)))
        ;; Skip defun forms during evaluation
        (if (and (consp form) (symbolp (car form)) (op=defun (car form)))
            (h0-eval-forms (cdr forms) env fenv)
            (if (null (cdr forms))
                (h0-eval form env fenv)
                (progn
                  (h0-eval form env fenv)
                  (h0-eval-forms (cdr forms) env fenv)))))))

;;; ==========================================================================
;;; IR Compiler - Source to IR transformation
;;; ==========================================================================
;;; IR format:
;;;   (lit n)           - literal number (will be tagged as fixnum)
;;;   (var offset)      - variable reference from environment
;;;   (add left right)  - addition
;;;   (sub left right)  - subtraction
;;;   (mul left right)  - multiplication
;;;   (div left right)  - division
;;;   (mod-ir left right) - modulo
;;;   (cmp-eq left right) - equality comparison
;;;   (cmp-lt left right) - less than
;;;   (cmp-gt left right) - greater than
;;;   (cmp-le left right) - less than or equal
;;;   (cmp-ge left right) - greater than or equal
;;;   (if-ir test then else) - conditional
;;;   (cons-ir car cdr) - cons cell allocation
;;;   (car-ir val)      - car of cons cell
;;;   (cdr-ir val)      - cdr of cons cell
;;;   (null-ir val)     - null check
;;;   (progn-ir forms)  - sequence

;; Symbol comparison helper for compilation
;; Uses symbol-name for string comparison since make-symbol-from-string
;; doesn't deduplicate (each call creates new symbol)
(defun sym= (sym name)
  "Check if symbol has given name string"
  (if (symbolp sym)
      (h0-string= (symbol-name sym) name)
      nil))

;; Initialize compile ops - now a no-op since we use string comparison
(defun init-compile-ops ()
  nil)

;; Environment lookup for compilation - returns offset or nil
;; Uses inline string comparison because make-symbol-from-string
;; doesn't deduplicate (each read creates new symbol object)
;; Note: Uses separate helper functions to avoid nested closure issues
(defun c-env-lookup (sym env)
  (c-env-search (symbol-name sym) env #x0))

;; Search environment for matching name - returns (cons offset nil) or nil
;; Returns cons cell so offset 0 is distinguishable from not-found (nil)
(defun c-env-search (sym-name env offset)
  (if (null env)
      nil
      (let ((entry-name (car (car env))))
        (if (c-names-match sym-name entry-name)
            (cons offset nil)  ;; Return cons to distinguish 0 from nil
            (c-env-search sym-name (cdr env) (+ offset #x1))))))

;; Check if two name strings match
;; First checks length, then compares character by character
(defun c-names-match (s1 s2)
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (c-chars-match s1 s2 len1 #x0)
        nil)))

;; Compare characters of two strings up to length len, starting at index i
(defun c-chars-match (s1 s2 len i)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (c-chars-match s1 s2 len (+ i #x1))
          nil)))

;; Extend compilation environment with new bindings
;; Bindings is list of (name . value) pairs, we just need the names
(defun c-env-extend (bindings env)
  (if (null bindings)
      env
      (let ((b (car bindings)))
        (c-env-extend (cdr bindings)
                      (cons (cons (symbol-name (car b)) nil) env)))))

;; IR tag constants (using numbers to avoid symbol-name issues in native code)
(defun ir-tag-lit () #x1)
(defun ir-tag-var () #x2)
(defun ir-tag-add () #x3)
(defun ir-tag-sub () #x4)
(defun ir-tag-mul () #x5)
(defun ir-tag-div () #x6)
(defun ir-tag-mod () #x7)
(defun ir-tag-cmp-eq () #x8)
(defun ir-tag-cmp-lt () #x9)
(defun ir-tag-cmp-gt () #xA)
(defun ir-tag-cmp-le () #xB)
(defun ir-tag-cmp-ge () #xC)
(defun ir-tag-if () #xD)
(defun ir-tag-cons () #xE)
(defun ir-tag-car () #xF)
(defun ir-tag-cdr () #x10)
(defun ir-tag-null () #x11)
(defun ir-tag-let () #x12)
(defun ir-tag-progn () #x13)

;; Check if IR node has a specific tag (numeric comparison)
(defun h0-has-tag-n (ir tag)
  (if (consp ir)
      (= (car ir) tag)
      nil))

;; Compile expression to IR (using numeric tags)
;; Uses sym= for string-based symbol comparison (no symbol deduplication)
(defun h0-compile (expr env fenv)
  (cond
    ;; Numbers compile to literals
    ((numberp expr) (list (ir-tag-lit) expr))
    ;; nil is 0
    ((null expr) (list (ir-tag-lit) #x0))
    ;; t is 1
    ((sym= expr "T") (list (ir-tag-lit) #x1))
    ;; Symbols - variable lookup
    ((symbolp expr)
     (let ((result (c-env-lookup expr env)))
       (if result
           (list (ir-tag-var) (car result))  ;; Extract offset from (cons offset nil)
           (list (ir-tag-lit) #x0))))  ; Unknown symbol -> 0
    ;; Lists - special forms or function calls
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; Quote
         ((sym= op "QUOTE")
          (let ((val (cadr expr)))
            (if (numberp val)
                (list (ir-tag-lit) val)
                (list (ir-tag-lit) #x0))))  ; Only quote numbers for now
         ;; If
         ((sym= op "IF")
          (let* ((test-ir (h0-compile (cadr expr) env fenv))
                 (then-ir (h0-compile (caddr expr) env fenv))
                 (else-ir (if (cadddr expr)
                              (h0-compile (cadddr expr) env fenv)
                              (list (ir-tag-lit) #x0))))
            (list (ir-tag-if) test-ir then-ir else-ir)))
         ;; Let
         ((sym= op "LET")
          (h0-compile-let (cadr expr) (caddr expr) env fenv))
         ;; Let*
         ((sym= op "LET*")
          (h0-compile-let (cadr expr) (caddr expr) env fenv))
         ;; Progn
         ((sym= op "PROGN")
          (h0-compile-progn (cdr expr) env fenv))
         ;; Defun returns nil during compilation
         ((sym= op "DEFUN")
          (list (ir-tag-lit) #x0))
         ;; Arithmetic
         ((sym= op "+")
          (h0-compile-add (cdr expr) env fenv))
         ((sym= op "-")
          (h0-compile-sub (cdr expr) env fenv))
         ((sym= op "*")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-mul) l r)))
         ((sym= op "/")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-div) l r)))
         ((sym= op "MOD")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-mod) l r)))
         ;; Comparisons
         ((sym= op "=")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-eq) l r)))
         ((sym= op "<")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-lt) l r)))
         ((sym= op ">")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-gt) l r)))
         ((sym= op "<=")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-le) l r)))
         ((sym= op ">=")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cmp-ge) l r)))
         ;; List operations
         ((sym= op "CONS")
          (let* ((l (h0-compile (cadr expr) env fenv))
                 (r (h0-compile (caddr expr) env fenv)))
            (list (ir-tag-cons) l r)))
         ((sym= op "CAR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-car) v)))
         ((sym= op "CDR")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-cdr) v)))
         ((sym= op "NULL")
          (let ((v (h0-compile (cadr expr) env fenv)))
            (list (ir-tag-null) v)))
         ;; Default - unknown operator
         (t (list (ir-tag-lit) #x0)))))
    ;; Default
    (t (list (ir-tag-lit) #x0))))

;; Compile addition with constant folding
(defun h0-compile-add (args env fenv)
  (if (null args)
      (list (ir-tag-lit) #x0)
      (if (null (cdr args))
          (h0-compile (car args) env fenv)
          (let* ((left-ir (h0-compile (car args) env fenv))
                 (right-ir (h0-compile (cadr args) env fenv)))
            ;; Constant folding
            (if (and (h0-has-tag-n left-ir (ir-tag-lit)) (h0-has-tag-n right-ir (ir-tag-lit)))
                (list (ir-tag-lit) (+ (cadr left-ir) (cadr right-ir)))
                (list (ir-tag-add) left-ir right-ir))))))

;; Compile subtraction with constant folding
(defun h0-compile-sub (args env fenv)
  (if (null args)
      (list (ir-tag-lit) #x0)
      (if (null (cdr args))
          ;; Unary minus
          (let ((arg-ir (h0-compile (car args) env fenv)))
            (if (h0-has-tag-n arg-ir (ir-tag-lit))
                (list (ir-tag-lit) (- #x0 (cadr arg-ir)))
                (list (ir-tag-sub) (list (ir-tag-lit) #x0) arg-ir)))
          (let* ((left-ir (h0-compile (car args) env fenv))
                 (right-ir (h0-compile (cadr args) env fenv)))
            ;; Constant folding
            (if (and (h0-has-tag-n left-ir (ir-tag-lit)) (h0-has-tag-n right-ir (ir-tag-lit)))
                (list (ir-tag-lit) (- (cadr left-ir) (cadr right-ir)))
                (list (ir-tag-sub) left-ir right-ir))))))

;; Compile let - iterate through bindings, extending environment
;; Store symbol name (string) in env for string-based lookup
(defun h0-compile-let (bindings body env fenv)
  (if (null bindings)
      (h0-compile body env fenv)
      (let* ((b (car bindings))
             (var-sym (car b))
             (var-name (symbol-name var-sym))
             (val-ir (h0-compile (cadr b) env fenv))
             ;; Store symbol name string for h0-string= lookup
             (new-env (cons (cons var-name nil) env))
             (body-ir (h0-compile-let (cdr bindings) body new-env fenv)))
        (list (ir-tag-let) #x0 val-ir body-ir))))

;; Compile progn - sequence of forms
(defun h0-compile-progn (forms env fenv)
  (if (null forms)
      (list (ir-tag-lit) #x0)
      (if (null (cdr forms))
          (h0-compile (car forms) env fenv)
          (let* ((first-ir (h0-compile (car forms) env fenv))
                 (rest-ir (h0-compile-progn-rest (cdr forms) env fenv)))
            (list (ir-tag-progn) (cons first-ir rest-ir))))))

(defun h0-compile-progn-rest (forms env fenv)
  (if (null forms)
      nil
      (let* ((first-ir (h0-compile (car forms) env fenv))
             (rest-ir (h0-compile-progn-rest (cdr forms) env fenv)))
        (cons first-ir rest-ir))))

;;; ==========================================================================
;;; ARM64 Code Generation - IR to machine code
;;; ==========================================================================
;;; Generates ARM64 machine code bytes from IR
;;; Uses tagged fixnum representation: value << 4
;;; Registers:
;;;   x0-x4   - arguments and return value
;;;   x20     - environment base (stack frame)
;;;   x28     - heap bump pointer

;; ARM64 instruction encoders (inline for self-hosting)

;; MOVZ Xd, #imm16
(defun a64-movz (rd imm)
  (let ((inst (logior #xD2800000
                      (ash (logand imm #xFFFF) #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MOVK Xd, #imm16, LSL #shift
(defun a64-movk (rd imm shift)
  (let* ((hw (/ shift #x10))  ; hw field: 0, 1, 2, or 3 for 0, 16, 32, 48
         (inst (logior #xF2800000
                       (ash hw #x15)  ; hw at bits 21-22
                       (ash (logand imm #xFFFF) #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; ADD Xd, Xn, #imm12
(defun a64-add-imm (rd rn imm)
  (let ((inst (logior #x91000000
                      (ash (logand imm #xFFF) #xA)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; ADD Xd, Xn, Xm
(defun a64-add-reg (rd rn rm)
  (let ((inst (logior #x8B000000
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; SUB Xd, Xn, #imm12
(defun a64-sub-imm (rd rn imm)
  (let ((inst (logior #xD1000000
                      (ash (logand imm #xFFF) #xA)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; SUB Xd, Xn, Xm
(defun a64-sub-reg (rd rn rm)
  (let ((inst (logior #xCB000000
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MUL Xd, Xn, Xm (actually MADD Xd, Xn, Xm, XZR)
(defun a64-mul (rd rn rm)
  (let ((inst (logior #x9B007C00
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; SDIV Xd, Xn, Xm
(defun a64-sdiv (rd rn rm)
  (let ((inst (logior #x9AC00C00
                      (ash rm #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MSUB Xd, Xn, Xm, Xa (Xa - Xn*Xm)
(defun a64-msub (rd rn rm ra)
  (let ((inst (logior #x9B008000
                      (ash rm #x10)
                      (ash ra #xA)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LSL Xd, Xn, #shift (actually UBFM)
(defun a64-lsl-imm (rd rn shift)
  (let* ((immr (logand (- #x40 shift) #x3F))
         (imms (- #x3F shift))
         (inst (logior #xD3400000
                       (ash immr #x10)
                       (ash imms #xA)
                       (ash rn #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LSR Xd, Xn, #shift (actually UBFM)
(defun a64-lsr-imm (rd rn shift)
  (let ((inst (logior #xD340FC00
                      (ash shift #x10)
                      (ash rn #x5)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; CMP Xn, #imm12 (actually SUBS XZR, Xn, #imm)
(defun a64-cmp-imm (rn imm)
  (let ((inst (logior #xF100001F
                      (ash (logand imm #xFFF) #xA)
                      (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; CMP Xn, Xm (actually SUBS XZR, Xn, Xm)
(defun a64-cmp-reg (rn rm)
  (let ((inst (logior #xEB00001F
                      (ash rm #x10)
                      (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; CSET Xd, cond (actually CSINC Xd, XZR, XZR, invert(cond))
(defun a64-cset (rd cond)
  (let* ((inv-cond (logxor cond #x1))  ; Invert condition
         (inst (logior #x9A9F07E0
                       (ash inv-cond #xC)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; Condition codes
(defun cond-eq () #x0)
(defun cond-ne () #x1)
(defun cond-lt () #xB)
(defun cond-gt () #xC)
(defun cond-le () #xD)
(defun cond-ge () #xA)

;; B.cond offset (conditional branch)
(defun a64-b-cond (cond offset)
  (let* ((imm19 (logand (ash offset #x-2) #x7FFFF))
         (inst (logior #x54000000
                       (ash imm19 #x5)
                       cond)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; B offset (unconditional branch)
(defun a64-b (offset)
  (let* ((imm26 (logand (ash offset #x-2) #x3FFFFFF))
         (inst (logior #x14000000 imm26)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LDR Xt, [Xn, #imm12*8]
(defun a64-ldr (rt rn imm)
  (let* ((offset (ash imm #x-3))  ; Divide by 8 for scaled offset
         (inst (logior #xF9400000
                       (ash (logand offset #xFFF) #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; STR Xt, [Xn, #imm12*8]
(defun a64-str (rt rn imm)
  (let* ((offset (ash imm #x-3))  ; Divide by 8 for scaled offset
         (inst (logior #xF9000000
                       (ash (logand offset #xFFF) #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; MOV Xd, Xm (ORR Xd, XZR, Xm)
(defun a64-mov-reg (rd rm)
  (let ((inst (logior #xAA0003E0
                      (ash rm #x10)
                      rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; RET (BR LR)
(defun a64-ret ()
  (list #xC0 #x03 #x5F #xD6))

;; Append byte lists
(defun bytes-append (a b)
  (if (null a) b
      (cons (car a) (bytes-append (cdr a) b))))

(defun bytes-append-all (lists)
  (if (null lists)
      nil
      (bytes-append (car lists) (bytes-append-all (cdr lists)))))

;; Temp slot offset calculation:
;; Temp slots start at sp+48 (#x30) to avoid overlap with saved registers (sp+0..sp+40)
;; Formula: 48 + td*8 = #x30 + (* td #x8)
;; Note: Inlined everywhere because function calls have overhead in native code

;; Generate code for IR (using numeric tags)
;; td = temp slot depth (for nested expressions)
(defun h0-codegen (ir td)
  (cond
    ;; Literal - MOVZ x0, #(val << 4)
    ((h0-has-tag-n ir (ir-tag-lit))
     (let* ((val (cadr ir))
            (tagged (ash val #x4)))
       (if (< tagged #x10000)
           (a64-movz #x0 tagged)
           ;; Larger values need MOVZ + MOVK
           (let ((movz-code (a64-movz #x0 (logand tagged #xFFFF)))
                 (movk-code (a64-movk #x0 (logand (ash tagged #x-10) #xFFFF) #x10)))
             (bytes-append movz-code movk-code)))))

    ;; Variable - load from stack frame at x20
    ((h0-has-tag-n ir (ir-tag-var))
     (let* ((off (cadr ir))
            (byte-off (* off #x8))
            (sub-code (a64-sub-imm #x1 #x14 byte-off))
            (ldr-code (a64-ldr #x0 #x1 #x0)))
       (bytes-append sub-code ldr-code)))

    ;; Addition
    ((h0-has-tag-n ir (ir-tag-add))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (a64-add-reg #x0 #x0 #x1)
                       td))

    ;; Subtraction
    ((h0-has-tag-n ir (ir-tag-sub))
     (h0-codegen-binop (cadr ir) (caddr ir)
                       (a64-sub-reg #x0 #x0 #x1)
                       td))

    ;; Multiplication (need to untag one operand)
    ((h0-has-tag-n ir (ir-tag-mul))
     (let ((slot-off (+ #x30 (* td #x8))))
       (bytes-append-all
        (list (h0-codegen (cadr ir) td)
              (a64-str #x0 #x1F slot-off)             ; save left to temp
              (h0-codegen (caddr ir) (+ td #x1))
              (a64-lsr-imm #x1 #x0 #x4)               ; untag right
              (a64-ldr #x0 #x1F slot-off)             ; load left
              (a64-mul #x0 #x0 #x1)))))               ; multiply

    ;; Division
    ((h0-has-tag-n ir (ir-tag-div))
     (let ((slot-off (+ #x30 (* td #x8))))
       (bytes-append-all
        (list (h0-codegen (cadr ir) td)
              (a64-str #x0 #x1F slot-off)
              (h0-codegen (caddr ir) (+ td #x1))
              (a64-lsr-imm #x1 #x0 #x4)               ; untag right
              (a64-ldr #x0 #x1F slot-off)
              (a64-lsr-imm #x0 #x0 #x4)               ; untag left
              (a64-sdiv #x0 #x0 #x1)                  ; divide
              (a64-lsl-imm #x0 #x0 #x4)))))           ; retag result

    ;; Modulo (a mod b = a - (a/b)*b)
    ((h0-has-tag-n ir (ir-tag-mod))
     (let ((slot-off (+ #x30 (* td #x8)))
           (slot-off2 (+ #x30 (* (+ td #x1) #x8))))
       (bytes-append-all
        (list (h0-codegen (cadr ir) td)
              (a64-str #x0 #x1F slot-off)                  ; save left
              (h0-codegen (caddr ir) (+ td #x1))
              (a64-str #x0 #x1F slot-off2)                 ; save right
              (a64-lsr-imm #x1 #x0 #x4)                    ; untag right
              (a64-ldr #x0 #x1F slot-off)
              (a64-lsr-imm #x0 #x0 #x4)                    ; untag left
              (a64-sdiv #x2 #x0 #x1)                       ; x2 = left/right
              (a64-msub #x0 #x2 #x1 #x0)                   ; x0 = left - x2*right
              (a64-lsl-imm #x0 #x0 #x4)))))                ; retag

    ;; Comparisons
    ((h0-has-tag-n ir (ir-tag-cmp-eq))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-eq) td))
    ((h0-has-tag-n ir (ir-tag-cmp-lt))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-lt) td))
    ((h0-has-tag-n ir (ir-tag-cmp-gt))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-gt) td))
    ((h0-has-tag-n ir (ir-tag-cmp-le))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-le) td))
    ((h0-has-tag-n ir (ir-tag-cmp-ge))
     (h0-codegen-cmp (cadr ir) (caddr ir) (cond-ge) td))

    ;; If
    ((h0-has-tag-n ir (ir-tag-if))
     (let* ((test-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (test-code (h0-codegen test-ir td))
            (then-code (h0-codegen then-ir td))
            (else-code (h0-codegen else-ir td))
            (then-len (length then-code))
            (else-len (length else-code)))
       (bytes-append-all
        (list test-code
              (a64-cmp-imm #x0 #x0)               ; test == 0?
              (a64-b-cond (cond-eq) (+ then-len #x8)) ; skip then + jump
              then-code
              (a64-b (+ else-len #x4))            ; skip else
              else-code))))

    ;; Cons
    ((h0-has-tag-n ir (ir-tag-cons))
     (let ((slot-off (+ #x30 (* td #x8))))
       (bytes-append-all
        (list (h0-codegen (cadr ir) td)               ; compile car
              (a64-str #x0 #x1F slot-off)             ; save car
              (h0-codegen (caddr ir) (+ td #x1))      ; compile cdr
              (a64-mov-reg #x1 #x0)                   ; x1 = cdr
              (a64-ldr #x0 #x1F slot-off)             ; x0 = car
              (a64-str #x0 #x1C #x0)                  ; [x28] = car
              (a64-str #x1 #x1C #x8)                  ; [x28+8] = cdr
              (a64-mov-reg #x0 #x1C)                  ; x0 = cons ptr
              (a64-add-imm #x0 #x0 #x1)               ; tag as cons (1)
              (a64-add-imm #x1C #x1C #x10)))))        ; bump heap

    ;; Car
    ((h0-has-tag-n ir (ir-tag-car))
     (let ((arg-ir (cadr ir)))
       (bytes-append-all
        (list (h0-codegen arg-ir td)
              (a64-sub-imm #x0 #x0 #x1)          ; untag cons
              (a64-ldr #x0 #x0 #x0)))))          ; load car

    ;; Cdr
    ((h0-has-tag-n ir (ir-tag-cdr))
     (let ((arg-ir (cadr ir)))
       (bytes-append-all
        (list (h0-codegen arg-ir td)
              (a64-sub-imm #x0 #x0 #x1)          ; untag cons
              (a64-ldr #x0 #x0 #x8)))))          ; load cdr

    ;; Null check
    ((h0-has-tag-n ir (ir-tag-null))
     (let ((arg-ir (cadr ir)))
       (bytes-append-all
        (list (h0-codegen arg-ir td)
              (a64-cmp-imm #x0 #x0)
              (a64-cset #x0 (cond-eq))
              (a64-lsl-imm #x0 #x0 #x4)))))

    ;; Let binding
    ;; h0-compile assigns offset 0 to the innermost binding
    ;; Offset 0 -> [x20-0], offset 1 -> [x20-8], etc.
    ;; Nested lets: inner var gets offset 0, outer var gets offset 1
    ;; Store value at x20-0 (the slot for offset 0), decrement x20 for body
    ((h0-has-tag-n ir (ir-tag-let))
     (let* ((val-ir (caddr ir))
            (body-ir (cadddr ir))
            (val-code (h0-codegen val-ir td))
            (body-code (h0-codegen body-ir td)))
       (bytes-append-all
        (list val-code
              ;; Decrement x20 BEFORE storing (so offset 0 refers to new slot)
              (a64-sub-imm #x14 #x14 #x8)        ; x20 -= 8 (grow frame)
              (a64-str #x0 #x14 #x0)             ; [x20] = value (at new x20)
              body-code
              (a64-add-imm #x14 #x14 #x8)))))    ; x20 += 8 (restore frame)

    ;; Progn
    ((h0-has-tag-n ir (ir-tag-progn))
     (h0-codegen-progn (cadr ir) td))

    ;; Default - return 0
    (t (a64-movz #x0 #x0))))

;; Codegen helper for binary operations
;; Inline temp slot calculation: 48 + td*8
(defun h0-codegen-binop (left-ir right-ir op-instrs td)
  (let ((slot-off (+ #x30 (* td #x8))))
    (bytes-append-all
     (list (h0-codegen left-ir td)
           (a64-str #x0 #x1F slot-off)             ; save left
           (h0-codegen right-ir (+ td #x1))
           (a64-mov-reg #x1 #x0)                   ; x1 = right
           (a64-ldr #x0 #x1F slot-off)             ; x0 = left
           op-instrs))))

;; Codegen helper for comparisons
;; Inline temp slot calculation: 48 + td*8
(defun h0-codegen-cmp (left-ir right-ir cond td)
  (let ((slot-off (+ #x30 (* td #x8))))
    (bytes-append-all
     (list (h0-codegen left-ir td)
           (a64-str #x0 #x1F slot-off)
           (h0-codegen right-ir (+ td #x1))
           (a64-mov-reg #x1 #x0)
           (a64-ldr #x0 #x1F slot-off)
           (a64-cmp-reg #x0 #x1)
           (a64-cset #x0 cond)
           (a64-lsl-imm #x0 #x0 #x4)))))

;; Codegen helper for progn (list of IR forms)
(defun h0-codegen-progn (forms td)
  (if (null forms)
      (a64-movz #x0 #x0)
      (if (null (cdr forms))
          (h0-codegen (car forms) td)
          (bytes-append (h0-codegen (car forms) td)
                        (h0-codegen-progn (cdr forms) td)))))

;;; ==========================================================================
;;; IR Evaluator - for testing the compiler without native execution
;;; ==========================================================================

;; Evaluate IR directly (for testing compiler output)
;; Uses numeric tags for native code compatibility
(defun h0-eval-ir (ir env)
  (cond
    ;; Literal
    ((h0-has-tag-n ir (ir-tag-lit)) (cadr ir))
    ;; Variable reference
    ((h0-has-tag-n ir (ir-tag-var))
     (let ((off (cadr ir)))
       (ir-env-get env off)))
    ;; Arithmetic
    ((h0-has-tag-n ir (ir-tag-add))
     (+ (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-sub))
     (- (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-mul))
     (* (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-div))
     (/ (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-mod))
     (mod (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ;; Comparisons
    ((h0-has-tag-n ir (ir-tag-cmp-eq))
     (if (= (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-lt))
     (if (< (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-gt))
     (if (> (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-le))
     (if (<= (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ((h0-has-tag-n ir (ir-tag-cmp-ge))
     (if (>= (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)) #x1 #x0))
    ;; Conditional
    ((h0-has-tag-n ir (ir-tag-if))
     (if (= (h0-eval-ir (cadr ir) env) #x0)
         (h0-eval-ir (cadddr ir) env)
         (h0-eval-ir (caddr ir) env)))
    ;; Cons/car/cdr
    ((h0-has-tag-n ir (ir-tag-cons))
     (cons (h0-eval-ir (cadr ir) env) (h0-eval-ir (caddr ir) env)))
    ((h0-has-tag-n ir (ir-tag-car))
     (car (h0-eval-ir (cadr ir) env)))
    ((h0-has-tag-n ir (ir-tag-cdr))
     (cdr (h0-eval-ir (cadr ir) env)))
    ((h0-has-tag-n ir (ir-tag-null))
     (if (null (h0-eval-ir (cadr ir) env)) #x1 #x0))
    ;; Let binding
    ((h0-has-tag-n ir (ir-tag-let))
     (let* ((val (h0-eval-ir (caddr ir) env))
            (new-env (cons val env)))
       (h0-eval-ir (cadddr ir) new-env)))
    ;; Progn
    ((h0-has-tag-n ir (ir-tag-progn))
     (h0-eval-ir-progn (cadr ir) env))
    ;; Default
    (t #x0)))

(defun h0-eval-ir-progn (forms env)
  (if (null forms)
      #x0
      (if (null (cdr forms))
          (h0-eval-ir (car forms) env)
          (progn
            (h0-eval-ir (car forms) env)
            (h0-eval-ir-progn (cdr forms) env)))))

(defun ir-env-get (env off)
  (if (= off #x0)
      (car env)
      (ir-env-get (cdr env) (- off #x1))))

;;; ==========================================================================
;;; Test Mode - compile expression and evaluate IR
;;; ==========================================================================

(defun h0-compile-and-eval (expr)
  "Compile expression to IR and evaluate it"
  (let ((ir (h0-compile expr nil nil)))
    (h0-eval-ir ir nil)))

;;; ==========================================================================
;;; Mach-O Linker - Native executable generation with chained fixups
;;; ==========================================================================
;;; Generates standalone ARM64 Mach-O executables that link against libSystem.
;;; Uses chained fixups for dynamic symbol binding (modern macOS approach).

;; File I/O constants for sys-open
(defun o-wronly () #x1)
(defun o-creat () #x200)
(defun o-trunc () #x400)

;; Mach-O magic and CPU types
(defun mh-magic-64 () #xFEEDFACF)
(defun cpu-type-arm64 () #x0100000C)
(defun cpu-subtype-arm64-all () #x0)
(defun mh-execute () #x2)

;; Header flags
(defun mh-noundefs () #x1)
(defun mh-dyldlink () #x4)
(defun mh-twolevel () #x80)
(defun mh-pie () #x200000)

;; Load command types
(defun lc-segment-64 () #x19)
(defun lc-symtab () #x2)
(defun lc-dysymtab () #xB)
(defun lc-load-dylinker () #xE)
(defun lc-uuid () #x1B)
(defun lc-build-version () #x32)
(defun lc-main () #x80000028)
(defun lc-load-dylib () #xC)
(defun lc-dyld-chained-fixups () #x80000034)
(defun lc-dyld-exports-trie () #x80000033)

;; VM protection flags
(defun vm-prot-read () #x1)
(defun vm-prot-write () #x2)
(defun vm-prot-execute () #x4)

;; Section flags
(defun s-attr-pure-instructions () #x80000000)
(defun s-attr-some-instructions () #x400)
(defun s-non-lazy-symbol-pointers () #x6)
(defun s-symbol-stubs () #x8)

;; Chained fixups format
(defun dyld-chained-ptr-64-offset () #x6)

;; Page size and VM base
(defun page-size () #x4000)  ; 16KB on ARM64 macOS
(defun vm-base () #x100000000)

;; Align value up to alignment boundary
(defun align-up (val alignment)
  (let ((rem (mod val alignment)))
    (if (= rem #x0)
        val
        (+ val (- alignment rem)))))

;;; Byte buffer operations
;;; We build the executable in a list of bytes (reversed), then write it out

;; Append a single byte to buffer (returns new buffer)
(defun buf-u8 (buf val)
  (cons (logand val #xFF) buf))

;; Append u16 little-endian
(defun buf-u16-le (buf val)
  (buf-u8 (buf-u8 buf val) (ash val #x-8)))

;; Append u32 little-endian
(defun buf-u32-le (buf val)
  (buf-u16-le (buf-u16-le buf val) (ash val #x-10)))

;; Append u64 little-endian
(defun buf-u64-le (buf val)
  (buf-u32-le (buf-u32-le buf (logand val #xFFFFFFFF))
              (logand (ash val #x-20) #xFFFFFFFF)))

;; Append N zero bytes
(defun buf-zeros (buf n)
  (if (<= n #x0)
      buf
      (buf-zeros (buf-u8 buf #x0) (- n #x1))))

;; Append byte list (each byte is consed to front, so we reverse first)
(defun buf-bytes (buf bytes)
  (if (null bytes)
      buf
      (buf-bytes (buf-u8 buf (car bytes)) (cdr bytes))))

;; Append string as bytes (without null terminator)
(defun buf-string (buf str)
  (buf-string-helper buf str #x0 (string-length str)))

(defun buf-string-helper (buf str i len)
  (if (>= i len)
      buf
      (buf-string-helper (buf-u8 buf (string-ref str i)) str (+ i #x1) len)))

;; Append string padded to length with zeros
(defun buf-string-padded (buf str len)
  (let* ((slen (string-length str))
         (buf2 (buf-string-helper buf str #x0 (if (< slen len) slen len))))
    (buf-zeros buf2 (- len slen))))

;; Get current buffer length
(defun buf-length (buf)
  (length buf))

;; Convert buffer to vector (reverses the list)
(defun buf-to-vector (buf)
  (let* ((len (length buf))
         (vec (make-vector len)))
    (buf-to-vector-helper (reverse buf) vec #x0)))

(defun buf-to-vector-helper (lst vec i)
  (if (null lst)
      vec
      (progn
        (vector-set vec i (car lst))
        (buf-to-vector-helper (cdr lst) vec (+ i #x1)))))

;;; Mach-O structure writers

;; Mach-O header (32 bytes)
(defun buf-mach-header-64 (buf ncmds sizeofcmds flags)
  (let* ((b1 (buf-u32-le buf (mh-magic-64)))
         (b2 (buf-u32-le b1 (cpu-type-arm64)))
         (b3 (buf-u32-le b2 (cpu-subtype-arm64-all)))
         (b4 (buf-u32-le b3 (mh-execute)))
         (b5 (buf-u32-le b4 ncmds))
         (b6 (buf-u32-le b5 sizeofcmds))
         (b7 (buf-u32-le b6 flags))
         (b8 (buf-u32-le b7 #x0)))  ; reserved
    b8))

;; Segment command (72 bytes)
(defun buf-segment-command-64 (buf segname vmaddr vmsize fileoff filesize
                                maxprot initprot nsects flags)
  (let* ((b1 (buf-u32-le buf (lc-segment-64)))
         (cmdsize (+ #x48 (* nsects #x50)))  ; 72 + 80*nsects
         (b2 (buf-u32-le b1 cmdsize))
         (b3 (buf-string-padded b2 segname #x10))
         (b4 (buf-u64-le b3 vmaddr))
         (b5 (buf-u64-le b4 vmsize))
         (b6 (buf-u64-le b5 fileoff))
         (b7 (buf-u64-le b6 filesize))
         (b8 (buf-u32-le b7 maxprot))
         (b9 (buf-u32-le b8 initprot))
         (b10 (buf-u32-le b9 nsects))
         (b11 (buf-u32-le b10 flags)))
    b11))

;; Section (80 bytes)
(defun buf-section-64 (buf sectname segname addr size offset align
                        reloff nreloc flags reserved1 reserved2)
  (let* ((b1 (buf-string-padded buf sectname #x10))
         (b2 (buf-string-padded b1 segname #x10))
         (b3 (buf-u64-le b2 addr))
         (b4 (buf-u64-le b3 size))
         (b5 (buf-u32-le b4 offset))
         (b6 (buf-u32-le b5 align))
         (b7 (buf-u32-le b6 reloff))
         (b8 (buf-u32-le b7 nreloc))
         (b9 (buf-u32-le b8 flags))
         (b10 (buf-u32-le b9 reserved1))
         (b11 (buf-u32-le b10 reserved2))
         (b12 (buf-u32-le b11 #x0)))  ; reserved3
    b12))

;; LC_LOAD_DYLINKER command
(defun buf-dylinker-command (buf path)
  (let* ((path-len (+ (string-length path) #x1))
         (cmdsize (align-up (+ #xC path-len) #x8))
         (b1 (buf-u32-le buf (lc-load-dylinker)))
         (b2 (buf-u32-le b1 cmdsize))
         (b3 (buf-u32-le b2 #xC))  ; path.offset
         (b4 (buf-string-padded b3 path (- cmdsize #xC))))
    b4))

;; LC_UUID command (24 bytes)
(defun buf-uuid-command (buf)
  (let* ((b1 (buf-u32-le buf (lc-uuid)))
         (b2 (buf-u32-le b1 #x18))
         ;; Generate simple UUID
         (b3 (buf-u32-le b2 #xDEADBEEF))
         (b4 (buf-u32-le b3 #xCAFEBABE))
         (b5 (buf-u32-le b4 #x12345678))
         (b6 (buf-u32-le b5 #x87654321)))
    b6))

;; LC_BUILD_VERSION command (24 bytes)
(defun buf-build-version-command (buf)
  (let* ((b1 (buf-u32-le buf (lc-build-version)))
         (b2 (buf-u32-le b1 #x18))
         (b3 (buf-u32-le b2 #x1))       ; platform = macOS
         (b4 (buf-u32-le b3 #xE0000))   ; minos = 14.0
         (b5 (buf-u32-le b4 #xE0000))   ; sdk = 14.0
         (b6 (buf-u32-le b5 #x0)))      ; ntools
    b6))

;; LC_MAIN command (24 bytes)
(defun buf-main-command (buf entryoff)
  (let* ((b1 (buf-u32-le buf (lc-main)))
         (b2 (buf-u32-le b1 #x18))
         (b3 (buf-u64-le b2 entryoff))
         (b4 (buf-u64-le b3 #x0)))      ; stacksize = 0 (default)
    b4))

;; LC_LOAD_DYLIB command
(defun buf-load-dylib-command (buf path)
  (let* ((path-len (+ (string-length path) #x1))
         (cmdsize (align-up (+ #x18 path-len) #x8))
         (b1 (buf-u32-le buf (lc-load-dylib)))
         (b2 (buf-u32-le b1 cmdsize))
         (b3 (buf-u32-le b2 #x18))      ; name.offset
         (b4 (buf-u32-le b3 #x2))       ; timestamp
         (b5 (buf-u32-le b4 #x54C0000)) ; current_version
         (b6 (buf-u32-le b5 #x10000))   ; compatibility_version
         (b7 (buf-string-padded b6 path (- cmdsize #x18))))
    b7))

;; LC_SYMTAB command (24 bytes)
(defun buf-symtab-command (buf symoff nsyms stroff strsize)
  (let* ((b1 (buf-u32-le buf (lc-symtab)))
         (b2 (buf-u32-le b1 #x18))
         (b3 (buf-u32-le b2 symoff))
         (b4 (buf-u32-le b3 nsyms))
         (b5 (buf-u32-le b4 stroff))
         (b6 (buf-u32-le b5 strsize)))
    b6))

;; LC_DYSYMTAB command (80 bytes)
(defun buf-dysymtab-command (buf ilocalsym nlocalsym iextdefsym nextdefsym
                              iundefsym nundefsym indirectsymoff nindirectsyms)
  (let* ((b1 (buf-u32-le buf (lc-dysymtab)))
         (b2 (buf-u32-le b1 #x50))       ; cmdsize = 80
         (b3 (buf-u32-le b2 ilocalsym))
         (b4 (buf-u32-le b3 nlocalsym))
         (b5 (buf-u32-le b4 iextdefsym))
         (b6 (buf-u32-le b5 nextdefsym))
         (b7 (buf-u32-le b6 iundefsym))
         (b8 (buf-u32-le b7 nundefsym))
         (b9 (buf-u32-le b8 #x0))        ; tocoff
         (b10 (buf-u32-le b9 #x0))       ; ntoc
         (b11 (buf-u32-le b10 #x0))      ; modtaboff
         (b12 (buf-u32-le b11 #x0))      ; nmodtab
         (b13 (buf-u32-le b12 #x0))      ; extrefsymoff
         (b14 (buf-u32-le b13 #x0))      ; nextrefsyms
         (b15 (buf-u32-le b14 indirectsymoff))
         (b16 (buf-u32-le b15 nindirectsyms))
         (b17 (buf-u32-le b16 #x0))      ; extreloff
         (b18 (buf-u32-le b17 #x0))      ; nextrel
         (b19 (buf-u32-le b18 #x0))      ; locreloff
         (b20 (buf-u32-le b19 #x0)))     ; nlocrel
    b20))

;; LC_DYLD_CHAINED_FIXUPS command (16 bytes)
(defun buf-chained-fixups-command (buf dataoff datasize)
  (let* ((b1 (buf-u32-le buf (lc-dyld-chained-fixups)))
         (b2 (buf-u32-le b1 #x10))
         (b3 (buf-u32-le b2 dataoff))
         (b4 (buf-u32-le b3 datasize)))
    b4))

;; LC_DYLD_EXPORTS_TRIE command (16 bytes)
(defun buf-exports-trie-command (buf dataoff datasize)
  (let* ((b1 (buf-u32-le buf (lc-dyld-exports-trie)))
         (b2 (buf-u32-le b1 #x10))
         (b3 (buf-u32-le b2 dataoff))
         (b4 (buf-u32-le b3 datasize)))
    b4))

;; nlist_64 symbol entry (16 bytes)
(defun buf-nlist-64 (buf strx type sect desc value)
  (let* ((b1 (buf-u32-le buf strx))
         (b2 (buf-u8 b1 type))
         (b3 (buf-u8 b2 sect))
         (b4 (buf-u16-le b3 desc))
         (b5 (buf-u64-le b4 value)))
    b5))

;;; Stub code generation (ADRP + LDR + BR)

;; ADRP Xd, #page_offset
(defun macho-adrp (rd page-off)
  (let* ((immlo (logand page-off #x3))
         (immhi (logand (ash page-off #x-2) #x7FFFF))
         (inst (logior #x90000000
                       (ash immlo #x1D)
                       (ash immhi #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; LDR Xt, [Xn, #imm]
(defun macho-ldr (rt rn imm)
  (let* ((offset (ash imm #x-3))
         (inst (logior #xF9400000
                       (ash (logand offset #xFFF) #xA)
                       (ash rn #x5)
                       rt)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; BR Xn
(defun macho-br (rn)
  (let ((inst (logior #xD61F0000 (ash rn #x5))))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; Generate stub: ADRP x16, got_page; LDR x16, [x16, #offset]; BR x16
(defun generate-stub (got-page-diff got-slot-offset)
  (bytes-append-all
   (list (macho-adrp #x10 got-page-diff)
         (macho-ldr #x10 #x10 got-slot-offset)
         (macho-br #x10))))

;;; Chained fixups data builder

;; Build import symbol string table (NUL-separated)
(defun build-import-strings (imports)
  (if (null imports)
      (list #x0)  ; Just leading NUL
      (cons #x0 (build-import-strings-helper imports))))

(defun build-import-strings-helper (imports)
  (if (null imports)
      nil
      (let ((name (car imports)))
        (bytes-append (string-to-bytes name)
                      (cons #x0 (build-import-strings-helper (cdr imports)))))))

(defun string-to-bytes (str)
  (string-to-bytes-helper str #x0 (string-length str) nil))

(defun string-to-bytes-helper (str i len acc)
  (if (>= i len)
      (reverse acc)
      (string-to-bytes-helper str (+ i #x1) len (cons (string-ref str i) acc))))

;; Build chained fixups data blob
(defun build-chained-fixups-data (num-imports num-segments got-segment-index got-vm-offset)
  "Build chained fixups data for binding external symbols.
   Returns a byte list."
  (let* (;; Calculate sizes
         (header-size #x20)              ; 32 bytes
         (starts-header-size (+ #x4 (* #x4 num-segments)))
         (seg-info-rel-offset (align-up starts-header-size #x8))
         (seg-info-size #x18)            ; 24 bytes
         (imports-entry-size #x4)
         (starts-offset header-size)
         (imports-offset (+ starts-offset seg-info-rel-offset seg-info-size))
         (symbols-offset (+ imports-offset (* num-imports imports-entry-size)))
         ;; Build the data
         (data nil))

    ;; === dyld_chained_fixups_header (32 bytes) ===
    (setq data (buf-u32-le data #x0))             ; fixups_version = 0
    (setq data (buf-u32-le data starts-offset))   ; starts_offset
    (setq data (buf-u32-le data imports-offset))  ; imports_offset
    (setq data (buf-u32-le data symbols-offset))  ; symbols_offset
    (setq data (buf-u32-le data num-imports))     ; imports_count
    (setq data (buf-u32-le data #x1))             ; imports_format = DYLD_CHAINED_IMPORT
    (setq data (buf-u32-le data #x0))             ; symbols_format = uncompressed
    (setq data (buf-u32-le data #x0))             ; padding

    ;; === dyld_chained_starts_in_image ===
    (setq data (buf-u32-le data num-segments))    ; seg_count
    ;; seg_info_offset for each segment (only GOT segment has non-zero)
    (setq data (build-seg-offsets data #x0 num-segments got-segment-index seg-info-rel-offset))

    ;; Padding to align seg_info
    (let ((current-size (buf-length data)))
      (setq data (buf-zeros data (- (+ starts-offset seg-info-rel-offset) current-size))))

    ;; === dyld_chained_starts_in_segment (24 bytes) ===
    (setq data (buf-u32-le data #x18))            ; size = 24
    (setq data (buf-u16-le data #x4000))          ; page_size
    (setq data (buf-u16-le data (dyld-chained-ptr-64-offset)))  ; pointer_format
    (setq data (buf-u64-le data got-vm-offset))   ; segment_offset
    (setq data (buf-u32-le data #x0))             ; max_valid_pointer
    (setq data (buf-u16-le data #x1))             ; page_count = 1
    (setq data (buf-u16-le data #x0))             ; page_start[0] = 0

    ;; === Import entries (DYLD_CHAINED_IMPORT format, 4 bytes each) ===
    ;; lib_ordinal (8 bits) | weak (1 bit) | name_offset (23 bits)
    (setq data (build-import-entries data num-imports #x0 #x0))

    ;; === Symbol strings ===
    ;; Note: We don't have the actual import names here, so we'll add placeholder
    ;; The caller must ensure symbols are added separately
    (setq data (buf-u8 data #x0))                 ; Leading NUL

    (reverse data)))

(defun build-seg-offsets (buf i count got-idx offset)
  (if (>= i count)
      buf
      (if (= i got-idx)
          (build-seg-offsets (buf-u32-le buf offset) (+ i #x1) count got-idx offset)
          (build-seg-offsets (buf-u32-le buf #x0) (+ i #x1) count got-idx offset))))

(defun build-import-entries (buf count name-offset i)
  (if (>= i count)
      buf
      (let* (;; lib_ordinal = 1 (first LC_LOAD_DYLIB = libSystem)
             ;; weak = 0
             ;; name_offset at bits 9-31
             (entry (logior #x1 (ash (+ name-offset #x1) #x9))))
        ;; Each import name is at offset 1 + accumulated name length
        ;; For simplicity, assume all names are "_write" (6 chars + NUL = 7 bytes)
        (build-import-entries (buf-u32-le buf entry)
                              count
                              (+ name-offset #x7)  ; Approximate name length
                              (+ i #x1)))))

;;; Wrapper stub for heap initialization

;; Wrap bytecode with heap setup stub (80 bytes = 20 instructions)
;; Stack layout (512 bytes total):
;;   sp+0:   saved x30
;;   sp+8:   saved x28
;;   sp+16:  saved x26
;;   sp+24:  saved x27
;;   sp+32:  saved x20
;;   sp+40:  (padding)
;;   sp+48:  temp slots for h0-codegen (td=0, td=1, ...)
;;   sp+64:  environment base (x20 points here)
(defun wrap-with-heap-stub (code-bytes heap-page-offset)
  "Wrap bytecode with heap initialization for executables with imports.
   heap-page-offset is the page offset from ADRP to __DATA segment."
  (let ((stub (bytes-append-all
               (list
                ;; 1. sub sp, sp, #512
                (a64-sub-imm #x1F #x1F #x200)
                ;; 2. str x30, [sp]
                (a64-str #x1E #x1F #x0)
                ;; 3. str x28, [sp, #8]
                (a64-str #x1C #x1F #x8)
                ;; 4. str x26, [sp, #16]
                (a64-str #x1A #x1F #x10)
                ;; 5. str x27, [sp, #24]
                (a64-str #x1B #x1F #x18)
                ;; 6. str x20, [sp, #32]
                (a64-str #x14 #x1F #x20)
                ;; 7. add x20, sp, #64 (environment base)
                (a64-add-imm #x14 #x1F #x40)
                ;; 8. adrp x28, heap_page
                (macho-adrp #x1C heap-page-offset)
                ;; 9. mov x27, x28 (heap base)
                (a64-mov-reg #x1B #x1C)
                ;; 10. add x28, x28, #16 (skip reserved)
                (a64-add-imm #x1C #x1C #x10)
                ;; 11. adr x26, +40 (code base: byte 80 - byte 40 = 40)
                (macho-adr #x1A #x28)
                ;; 12. bl +9 (skip 8 epilogue instrs to reach code)
                (macho-bl #x9)
                ;; 13. lsr x0, x0, #4 (untag result)
                (a64-lsr-imm #x0 #x0 #x4)
                ;; 14. ldr x20, [sp, #32]
                (a64-ldr #x14 #x1F #x20)
                ;; 15. ldr x27, [sp, #24]
                (a64-ldr #x1B #x1F #x18)
                ;; 16. ldr x26, [sp, #16]
                (a64-ldr #x1A #x1F #x10)
                ;; 17. ldr x28, [sp, #8]
                (a64-ldr #x1C #x1F #x8)
                ;; 18. ldr x30, [sp]
                (a64-ldr #x1E #x1F #x0)
                ;; 19. add sp, sp, #512
                (a64-add-imm #x1F #x1F #x200)
                ;; 20. ret
                (a64-ret)))))
    (bytes-append stub code-bytes)))

;; ADR Xd, #offset (PC-relative)
(defun macho-adr (rd offset)
  (let* ((immlo (logand offset #x3))
         (immhi (logand (ash offset #x-2) #x7FFFF))
         (inst (logior #x10000000
                       (ash immlo #x1D)
                       (ash immhi #x5)
                       rd)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;; BL #offset (PC-relative call, offset in instructions)
(defun macho-bl (instr-offset)
  (let* ((imm26 (logand instr-offset #x3FFFFFF))
         (inst (logior #x94000000 imm26)))
    (list (logand inst #xFF)
          (logand (ash inst #x-8) #xFF)
          (logand (ash inst #x-10) #xFF)
          (logand (ash inst #x-18) #xFF))))

;;; Main linker function (split into smaller helpers to fit in 64 temp slots)

;; Calculate basic layout sizes
(defun calc-sizeofcmds ()
  (+ #x48                                         ; PAGEZERO
     (+ #x48 (* #x2 #x50))                        ; TEXT + 2 sections
     (+ #x48 #x50)                                ; DATA_CONST + 1 section
     (+ #x48 #x50)                                ; DATA + 1 section
     #x48                                         ; LINKEDIT
     #x20                                         ; LC_LOAD_DYLINKER
     #x18                                         ; LC_UUID
     #x18                                         ; LC_BUILD_VERSION
     #x18                                         ; LC_MAIN
     #x38                                         ; LC_LOAD_DYLIB
     #x10                                         ; LC_DYLD_CHAINED_FIXUPS
     #x10                                         ; LC_DYLD_EXPORTS_TRIE
     #x18                                         ; LC_SYMTAB
     #x50))                                       ; LC_DYSYMTAB

;; Calculate code offset
(defun calc-code-offset ()
  (align-up (+ #x20 (calc-sizeofcmds) #x10) #x40))

;; Calculate stubs offset based on code size
(defun calc-stubs-offset (code-size)
  (align-up (+ (calc-code-offset) code-size) #x4))

;; Calculate TEXT segment size
(defun calc-text-vmsize (code-size num-imports)
  (let* ((stubs-offset (calc-stubs-offset code-size))
         (stubs-end (+ stubs-offset (* num-imports #xC))))
    (align-up stubs-end (page-size))))

;; Calculate DATA_CONST vmaddr
(defun calc-data-const-vmaddr (code-size num-imports)
  (+ (vm-base) (calc-text-vmsize code-size num-imports)))

;; Calculate DATA vmaddr
(defun calc-data-vmaddr (code-size num-imports)
  (+ (calc-data-const-vmaddr code-size num-imports) (page-size)))

;; Calculate LINKEDIT fileoff
(defun calc-linkedit-fileoff (code-size num-imports heap-size)
  (let* ((text-vmsize (calc-text-vmsize code-size num-imports))
         (heap-vmsize (align-up heap-size (page-size))))
    (+ text-vmsize (page-size) heap-vmsize)))

;; Calculate symbol table offset
(defun calc-symtab-offset (code-size num-imports heap-size)
  (calc-linkedit-fileoff code-size num-imports heap-size))

;; Calculate string table offset
(defun calc-strtab-offset (code-size num-imports heap-size)
  (+ (calc-symtab-offset code-size num-imports heap-size)
     (* (+ #x1 num-imports) #x10)))

;; Calculate indirect symbol offset
(defun calc-indirect-offset (code-size num-imports heap-size)
  (let* ((strtab-offset (calc-strtab-offset code-size num-imports heap-size))
         (string-table-size (+ #x7 (* num-imports #x8))))
    (align-up (+ strtab-offset string-table-size) #x4)))

;; Calculate fixups offset
(defun calc-fixups-offset (code-size num-imports heap-size)
  (let* ((indirect-offset (calc-indirect-offset code-size num-imports heap-size))
         (indirect-size (* num-imports #x2 #x4)))
    (align-up (+ indirect-offset indirect-size) #x8)))

;; Calculate exports offset
(defun calc-exports-offset (code-size num-imports heap-size)
  (+ (calc-fixups-offset code-size num-imports heap-size) #x50))

;; Calculate LINKEDIT size
(defun calc-linkedit-size (num-imports)
  (let* ((nlist-size (* (+ #x1 num-imports) #x10))
         (string-table-size (+ #x7 (* num-imports #x8)))
         (indirect-size (* num-imports #x2 #x4)))
    (align-up (+ nlist-size string-table-size indirect-size #x58) (page-size))))

;; Write all load commands
(defun write-load-commands (buf code-size num-imports heap-size)
  (let* ((text-vmsize (calc-text-vmsize code-size num-imports))
         (code-offset (calc-code-offset))
         (stubs-offset (calc-stubs-offset code-size))
         (stubs-total-size (* num-imports #xC))
         (data-const-vmaddr (calc-data-const-vmaddr code-size num-imports))
         (data-vmaddr (calc-data-vmaddr code-size num-imports))
         (heap-vmsize (align-up heap-size (page-size)))
         (linkedit-fileoff (calc-linkedit-fileoff code-size num-imports heap-size))
         (linkedit-vmaddr (+ data-vmaddr heap-vmsize))
         (linkedit-size (calc-linkedit-size num-imports))
         (got-total-size (if (> num-imports #x0) (* num-imports #x8) #x8))
         (symtab-offset (calc-symtab-offset code-size num-imports heap-size))
         (strtab-offset (calc-strtab-offset code-size num-imports heap-size))
         (string-table-size (+ #x7 (* num-imports #x8)))
         (indirect-offset (calc-indirect-offset code-size num-imports heap-size))
         (fixups-offset (calc-fixups-offset code-size num-imports heap-size))
         (exports-offset (calc-exports-offset code-size num-imports heap-size))
         (b buf))
    ;; 1. __PAGEZERO
    (setq b (buf-segment-command-64 b "__PAGEZERO" #x0 (vm-base) #x0 #x0 #x0 #x0 #x0 #x0))
    ;; 2. __TEXT
    (setq b (buf-segment-command-64 b "__TEXT" (vm-base) text-vmsize #x0 text-vmsize
                                    (logior (vm-prot-read) (vm-prot-execute))
                                    (logior (vm-prot-read) (vm-prot-execute)) #x2 #x0))
    (setq b (buf-section-64 b "__text" "__TEXT" (+ (vm-base) code-offset) code-size
                            code-offset #x2 #x0 #x0
                            (logior (s-attr-pure-instructions) (s-attr-some-instructions)) #x0 #x0))
    (setq b (buf-section-64 b "__stubs" "__TEXT" (+ (vm-base) stubs-offset) stubs-total-size
                            stubs-offset #x2 #x0 #x0
                            (logior (s-symbol-stubs) (s-attr-pure-instructions)) #x0 #xC))
    ;; 3. __DATA_CONST
    (setq b (buf-segment-command-64 b "__DATA_CONST" data-const-vmaddr (page-size)
                                    text-vmsize (page-size)
                                    (logior (vm-prot-read) (vm-prot-write))
                                    (logior (vm-prot-read) (vm-prot-write)) #x1 #x0))
    (setq b (buf-section-64 b "__got" "__DATA_CONST" data-const-vmaddr got-total-size
                            text-vmsize #x3 #x0 #x0 (s-non-lazy-symbol-pointers) num-imports #x0))
    ;; 4. __DATA
    (setq b (buf-segment-command-64 b "__DATA" data-vmaddr heap-vmsize
                                    (+ text-vmsize (page-size)) heap-vmsize
                                    (logior (vm-prot-read) (vm-prot-write))
                                    (logior (vm-prot-read) (vm-prot-write)) #x1 #x0))
    (setq b (buf-section-64 b "__heap" "__DATA" data-vmaddr heap-vmsize
                            (+ text-vmsize (page-size)) #x3 #x0 #x0 #x0 #x0 #x0))
    ;; 5. __LINKEDIT
    (setq b (buf-segment-command-64 b "__LINKEDIT" linkedit-vmaddr (page-size)
                                    linkedit-fileoff linkedit-size
                                    (vm-prot-read) (vm-prot-read) #x0 #x0))
    ;; Other load commands
    (setq b (buf-dylinker-command b "/usr/lib/dyld"))
    (setq b (buf-uuid-command b))
    (setq b (buf-build-version-command b))
    (setq b (buf-main-command b code-offset))
    (setq b (buf-load-dylib-command b "/usr/lib/libSystem.B.dylib"))
    (setq b (buf-chained-fixups-command b fixups-offset #x50))
    (setq b (buf-exports-trie-command b exports-offset #x8))
    (setq b (buf-symtab-command b symtab-offset (+ #x1 num-imports) strtab-offset string-table-size))
    (setq b (buf-dysymtab-command b #x0 #x0 #x0 #x1 #x1 num-imports
                                  indirect-offset (* num-imports #x2)))
    b))

;; Write code and stubs section
(defun write-code-section (buf code-bytes num-imports)
  (let* ((code-size (length code-bytes))
         (code-offset (calc-code-offset))
         (stubs-offset (calc-stubs-offset code-size))
         (data-const-vmaddr (calc-data-const-vmaddr code-size num-imports))
         (b buf))
    ;; Pad to code
    (setq b (buf-zeros b (- code-offset (buf-length b))))
    ;; Code
    (setq b (buf-bytes b code-bytes))
    ;; Pad to stubs
    (let ((current (buf-length b)))
      (if (< current stubs-offset)
          (setq b (buf-zeros b (- stubs-offset current)))))
    ;; Stubs
    (let* ((stub-vmaddr (+ (vm-base) stubs-offset))
           (stub-page (ash stub-vmaddr #x-C))
           (got-page (ash data-const-vmaddr #x-C))
           (got-page-diff (- got-page stub-page)))
      (setq b (generate-stubs b got-page-diff num-imports #x0)))
    b))

;; Write GOT and heap sections
(defun write-data-sections (buf code-size num-imports heap-size)
  (let* ((text-vmsize (calc-text-vmsize code-size num-imports))
         (heap-vmsize (align-up heap-size (page-size)))
         (data-fileoff (+ text-vmsize (page-size)))
         (b buf))
    ;; Pad to DATA_CONST
    (let ((current (buf-length b)))
      (if (< current text-vmsize)
          (setq b (buf-zeros b (- text-vmsize current)))))
    ;; GOT entries
    (setq b (write-got-entries b num-imports #x0))
    ;; Pad to DATA
    (let ((current (buf-length b)))
      (if (< current data-fileoff)
          (setq b (buf-zeros b (- data-fileoff current)))))
    ;; Heap
    (setq b (buf-zeros b heap-vmsize))
    b))

;; Write LINKEDIT section
(defun write-linkedit-section (buf code-size num-imports heap-size imports)
  (let* ((linkedit-fileoff (calc-linkedit-fileoff code-size num-imports heap-size))
         (linkedit-size (calc-linkedit-size num-imports))
         (indirect-offset (calc-indirect-offset code-size num-imports heap-size))
         (fixups-offset (calc-fixups-offset code-size num-imports heap-size))
         (exports-offset (calc-exports-offset code-size num-imports heap-size))
         (text-vmsize (calc-text-vmsize code-size num-imports))
         (code-offset (calc-code-offset))
         (b buf))
    ;; Symbol table (_main)
    (setq b (buf-nlist-64 b #x1 #xF #x1 #x10 (+ (vm-base) code-offset)))
    (setq b (write-import-symbols b num-imports #x7 #x0))
    ;; String table
    (setq b (buf-u8 b #x0))
    (setq b (buf-string b "_main"))
    (setq b (buf-u8 b #x0))
    (setq b (write-import-strings b imports))
    ;; Pad to indirect
    (let ((current (buf-length b)))
      (if (< current indirect-offset)
          (setq b (buf-zeros b (- indirect-offset current)))))
    ;; Indirect symbols
    (setq b (write-indirect-syms b num-imports #x0))
    (setq b (write-indirect-syms b num-imports #x0))
    ;; Pad to fixups
    (let ((current (buf-length b)))
      (if (< current fixups-offset)
          (setq b (buf-zeros b (- fixups-offset current)))))
    ;; Chained fixups
    (setq b (buf-bytes b (build-chained-fixups-data num-imports #x5 #x2 text-vmsize)))
    (setq b (write-import-strings b imports))
    ;; Pad to exports
    (let ((current (buf-length b)))
      (if (< current exports-offset)
          (setq b (buf-zeros b (- exports-offset current)))))
    ;; Exports trie
    (setq b (buf-u8 b #x0))
    (setq b (buf-u8 b #x0))
    (setq b (buf-zeros b #x6))
    ;; Pad to end
    (let ((current (buf-length b))
          (target (+ linkedit-fileoff linkedit-size)))
      (if (< current target)
          (setq b (buf-zeros b (- target current)))))
    b))

(defun write-macho-with-imports-and-heap (output-path code-bytes imports heap-size)
  "Write a Mach-O executable with external imports and heap."
  (let* ((num-imports (length imports))
         (code-size (length code-bytes))
         ;; Start with header
         (buf (buf-mach-header-64 nil #xE (calc-sizeofcmds)
                                  (logior (mh-noundefs) (mh-dyldlink)
                                          (mh-twolevel) (mh-pie)))))
    ;; Write load commands
    (setq buf (write-load-commands buf code-size num-imports heap-size))
    ;; Write code and stubs
    (setq buf (write-code-section buf code-bytes num-imports))
    ;; Write data sections
    (setq buf (write-data-sections buf code-size num-imports heap-size))
    ;; Write LINKEDIT
    (setq buf (write-linkedit-section buf code-size num-imports heap-size imports))
    ;; Write to file
    (let* ((vec (buf-to-vector buf))
           (fd (sys-open output-path
                         (logior (o-wronly) (o-creat) (o-trunc))
                         #x1FF)))
      (if (< fd #x0)
          #xFF
          (progn
            (sys-write fd vec (length vec))
            (sys-close fd)
            #x0)))))                              ; Success

;; Helper: generate stubs for each import
(defun generate-stubs (buf got-page-diff num-imports i)
  (if (>= i num-imports)
      buf
      (let* ((got-slot-offset (* i #x8))
             (stub (generate-stub got-page-diff got-slot-offset)))
        (generate-stubs (buf-bytes buf stub) got-page-diff num-imports (+ i #x1)))))

;; Helper: write GOT entries (chained bind pointers)
(defun write-got-entries (buf num-imports i)
  (if (>= i num-imports)
      buf
      (let* ((is-last (= i (- num-imports #x1)))
             (ordinal i)
             (next (if is-last #x0 #x2))          ; stride = 2 (8 bytes / 4)
             (entry (logior #x8000000000000000    ; bind bit
                           ordinal
                           (ash next #x33))))     ; next at bits 51-62
        (write-got-entries (buf-u64-le buf entry) num-imports (+ i #x1)))))

;; Helper: write import symbols
(defun write-import-symbols (buf num-imports strx i)
  (if (>= i num-imports)
      buf
      (let ((b (buf-nlist-64 buf strx #x1 #x0 #x100 #x0)))
        (write-import-symbols b num-imports (+ strx #x7) (+ i #x1)))))

;; Helper: write import strings
(defun write-import-strings (buf imports)
  (if (null imports)
      buf
      (let* ((b1 (buf-string buf (car imports)))
             (b2 (buf-u8 b1 #x0)))
        (write-import-strings b2 (cdr imports)))))

;; Helper: write indirect symbol indices
(defun write-indirect-syms (buf num-imports i)
  (if (>= i num-imports)
      buf
      (write-indirect-syms (buf-u32-le buf (+ i #x1)) num-imports (+ i #x1))))

;;; High-level delivery function

(defun deliver-with-imports-and-heap (output-path code-bytes imports heap-size)
  "Create a standalone executable with imports and heap.
   Wraps code with heap initialization stub first."
  (let* ((wrapper-stub-size #x50)                 ; 80 bytes (20 instructions)
         (total-code-size (+ (length code-bytes) wrapper-stub-size))
         ;; Calculate heap page offset
         (approx-code-offset #x400)
         (stubs-offset (align-up (+ approx-code-offset total-code-size) #x4))
         (stubs-end (+ stubs-offset (* (length imports) #xC)))
         (text-vmsize (align-up stubs-end (page-size)))
         (heap-page-offset (+ (/ text-vmsize (page-size)) #x1))
         ;; Wrap code
         (wrapped-code (wrap-with-heap-stub code-bytes heap-page-offset)))
    (write-macho-with-imports-and-heap output-path wrapped-code imports heap-size)))

;;; Main entry point
;;; Mode is determined by first form in input.lisp:
;;;   #x100 - compile expression to IR and evaluate (compile-test)
;;;   #x200 - compile expression to IR and generate bytecode length (codegen-test)
;;;   #x300 - compile, codegen, and link to executable (link-test)
;;;   anything else - interpret using h0-eval
(defun main ()
  ;; Initialize compile-time operators first (uses eq, no symbol-name)
  (init-compile-ops)
  (let ((source (native-read-file "input.lisp")))
    (if (null source)
        #xFF  ;; File not found
        (let ((forms (read-all source)))
          (if (null forms)
              #xFE  ;; Parse error
              (let ((first-form (car forms)))
                (cond
                  ;; Compile test mode: compile and eval IR
                  ((if (numberp first-form) (= first-form #x100) nil)
                   (if (null (cdr forms))
                       #xFD  ;; No expression to compile
                       (h0-compile-and-eval (cadr forms))))
                  ;; Codegen test mode: compile and return bytecode length
                  ((if (numberp first-form) (= first-form #x200) nil)
                   (if (null (cdr forms))
                       #xFD
                       (let* ((ir (h0-compile (cadr forms) nil nil))
                              (code (h0-codegen ir #x0)))
                         (length code))))
                  ;; Link test mode: compile, codegen, link to /tmp/h0out
                  ((if (numberp first-form) (= first-form #x300) nil)
                   (if (null (cdr forms))
                       #xFD
                       (let* ((ir (h0-compile (cadr forms) nil nil))
                              (code (h0-codegen ir #x0)))
                         (deliver-with-imports-and-heap "/tmp/h0out"
                                                        code
                                                        (list "_write")
                                                        #x100000))))
                  ;; Normal interpretation mode
                  (t
                   (let ((fenv (collect-defuns forms nil)))
                     (h0-eval-forms forms nil fenv))))))))))

(main)
