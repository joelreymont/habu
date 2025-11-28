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

(defun char-upcase (ch)
  (if (and (>= ch #x61) (<= ch #x7A))
      (- ch #x20)
      ch))

;; String equality check
(defun string= (s1 s2)
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
;; First time: compare by name and cache the symbol
;; Subsequent times: fast eq comparison
(defun op=quote (sym)
  (if (eq sym *op-quote*) t
      (if (null *op-quote*)
          (if (string= (symbol-name sym) "QUOTE")
              (progn (setq *op-quote* sym) t)
              nil)
          nil)))

(defun op=if (sym)
  (if (eq sym *op-if*) t
      (if (null *op-if*)
          (if (string= (symbol-name sym) "IF")
              (progn (setq *op-if* sym) t)
              nil)
          nil)))

(defun op=let (sym)
  (if (eq sym *op-let*) t
      (if (null *op-let*)
          (if (string= (symbol-name sym) "LET")
              (progn (setq *op-let* sym) t)
              nil)
          nil)))

(defun op=defun (sym)
  (if (eq sym *op-defun*) t
      (if (null *op-defun*)
          (if (string= (symbol-name sym) "DEFUN")
              (progn (setq *op-defun* sym) t)
              nil)
          nil)))

(defun op=t (sym)
  (if (eq sym *op-t*) t
      (if (null *op-t*)
          (if (string= (symbol-name sym) "T")
              (progn (setq *op-t* sym) t)
              nil)
          nil)))

(defun op=plus (sym)
  (if (eq sym *op-plus*) t
      (if (null *op-plus*)
          (if (string= (symbol-name sym) "+")
              (progn (setq *op-plus* sym) t)
              nil)
          nil)))

(defun op=minus (sym)
  (if (eq sym *op-minus*) t
      (if (null *op-minus*)
          (if (string= (symbol-name sym) "-")
              (progn (setq *op-minus* sym) t)
              nil)
          nil)))

(defun op=mul (sym)
  (if (eq sym *op-mul*) t
      (if (null *op-mul*)
          (if (string= (symbol-name sym) "*")
              (progn (setq *op-mul* sym) t)
              nil)
          nil)))

(defun op=div (sym)
  (if (eq sym *op-div*) t
      (if (null *op-div*)
          (if (string= (symbol-name sym) "/")
              (progn (setq *op-div* sym) t)
              nil)
          nil)))

(defun op=eq-num (sym)
  (if (eq sym *op-eq-num*) t
      (if (null *op-eq-num*)
          (if (string= (symbol-name sym) "=")
              (progn (setq *op-eq-num* sym) t)
              nil)
          nil)))

(defun op=lt (sym)
  (if (eq sym *op-lt*) t
      (if (null *op-lt*)
          (if (string= (symbol-name sym) "<")
              (progn (setq *op-lt* sym) t)
              nil)
          nil)))

(defun op=gt (sym)
  (if (eq sym *op-gt*) t
      (if (null *op-gt*)
          (if (string= (symbol-name sym) ">")
              (progn (setq *op-gt* sym) t)
              nil)
          nil)))

(defun op=le (sym)
  (if (eq sym *op-le*) t
      (if (null *op-le*)
          (if (string= (symbol-name sym) "<=")
              (progn (setq *op-le* sym) t)
              nil)
          nil)))

(defun op=ge (sym)
  (if (eq sym *op-ge*) t
      (if (null *op-ge*)
          (if (string= (symbol-name sym) ">=")
              (progn (setq *op-ge* sym) t)
              nil)
          nil)))

(defun op=let-star (sym)
  (if (eq sym *op-let-star*) t
      (if (null *op-let-star*)
          (if (string= (symbol-name sym) "LET*")
              (progn (setq *op-let-star* sym) t)
              nil)
          nil)))

(defun op=progn (sym)
  (if (eq sym *op-progn*) t
      (if (null *op-progn*)
          (if (string= (symbol-name sym) "PROGN")
              (progn (setq *op-progn* sym) t)
              nil)
          nil)))

(defun op=cond (sym)
  (if (eq sym *op-cond*) t
      (if (null *op-cond*)
          (if (string= (symbol-name sym) "COND")
              (progn (setq *op-cond* sym) t)
              nil)
          nil)))

(defun op=mod (sym)
  (if (eq sym *op-mod*) t
      (if (null *op-mod*)
          (if (string= (symbol-name sym) "MOD")
              (progn (setq *op-mod* sym) t)
              nil)
          nil)))

(defun op=cons (sym)
  (if (eq sym *op-cons*) t
      (if (null *op-cons*)
          (if (string= (symbol-name sym) "CONS")
              (progn (setq *op-cons* sym) t)
              nil)
          nil)))

(defun op=car (sym)
  (if (eq sym *op-car*) t
      (if (null *op-car*)
          (if (string= (symbol-name sym) "CAR")
              (progn (setq *op-car* sym) t)
              nil)
          nil)))

(defun op=cdr (sym)
  (if (eq sym *op-cdr*) t
      (if (null *op-cdr*)
          (if (string= (symbol-name sym) "CDR")
              (progn (setq *op-cdr* sym) t)
              nil)
          nil)))

(defun op=null (sym)
  (if (eq sym *op-null*) t
      (if (null *op-null*)
          (if (string= (symbol-name sym) "NULL")
              (progn (setq *op-null* sym) t)
              nil)
          nil)))

(defun op=consp (sym)
  (if (eq sym *op-consp*) t
      (if (null *op-consp*)
          (if (string= (symbol-name sym) "CONSP")
              (progn (setq *op-consp* sym) t)
              nil)
          nil)))

(defun op=list (sym)
  (if (eq sym *op-list*) t
      (if (null *op-list*)
          (if (string= (symbol-name sym) "LIST")
              (progn (setq *op-list* sym) t)
              nil)
          nil)))

(defun op=not (sym)
  (if (eq sym *op-not*) t
      (if (null *op-not*)
          (if (string= (symbol-name sym) "NOT")
              (progn (setq *op-not* sym) t)
              nil)
          nil)))

(defun op=and (sym)
  (if (eq sym *op-and*) t
      (if (null *op-and*)
          (if (string= (symbol-name sym) "AND")
              (progn (setq *op-and* sym) t)
              nil)
          nil)))

(defun op=or (sym)
  (if (eq sym *op-or*) t
      (if (null *op-or*)
          (if (string= (symbol-name sym) "OR")
              (progn (setq *op-or* sym) t)
              nil)
          nil)))

;; Generic symbol name comparison for cases not covered by caching
(defun op= (sym name)
  (if (symbolp sym)
      (string= (symbol-name sym) name)
      nil))

(defun chars-to-string (chars)
  (let* ((len (length chars))
         (vec (make-vector len)))
    (labels ((fill (cs i)
               (if (null cs)
                   vec
                   (progn
                     (vector-set vec i (char-upcase (car cs)))
                     (fill (cdr cs) (+ i 1))))))
      (make-string-from-vector (fill chars 0)))))

(defun read-sym-chars (source pos acc)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (read-sym-chars source (+ pos #x1) (cons ch acc))
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
    (labels ((fill (cs i)
               (if (null cs)
                   vec
                   (progn
                     (vector-set vec i (car cs))
                     (fill (cdr cs) (+ i 1))))))
      (cons (make-string-from-vector (fill chars 0)) end))))

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

(defun read-from-string (s)
  (car (habu-read s 0)))

;;; Simple expression evaluator with function definitions
;;; This interpreter supports defun, let, and recursion.

;; Symbol name lookup for function environment
(defun sym-name= (sym name)
  (if (symbolp sym)
      (string= (symbol-name sym) name)
      nil))

;; Look up function by symbol name in fenv
;; Entry is (name-string . (params . body))
(defun fenv-lookup (sym fenv)
  (if (null fenv) nil
      (let ((entry (car fenv)))
        (if (and (symbolp sym) (string= (symbol-name sym) (car entry)))
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
        (if (string= (symbol-name sym) (car entry))
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
      (string= (symbol-name sym) name)
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
             ;; Store symbol name string for string= lookup
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
           (bytes-append-all
            (list (a64-movz #x0 (logand tagged #xFFFF))
                  (a64-movk #x0 (logand (ash tagged #x-10) #xFFFF) #x10))))))

    ;; Variable - load from stack frame at x20
    ((h0-has-tag-n ir (ir-tag-var))
     (let* ((off (cadr ir))
            (byte-off (* off #x8)))
       (bytes-append-all
        (list (a64-sub-imm #x1 #x14 byte-off)  ; x1 = x20 - offset
              (a64-ldr #x0 #x1 #x0)))))        ; x0 = [x1]

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
     (bytes-append-all
      (list (h0-codegen (cadr ir) td)
            (a64-str #x0 #x1F (* td #x8))          ; save left to temp
            (h0-codegen (caddr ir) (+ td #x1))
            (a64-lsr-imm #x1 #x0 #x4)             ; untag right
            (a64-ldr #x0 #x1F (* td #x8))         ; load left
            (a64-mul #x0 #x0 #x1))))              ; multiply

    ;; Division
    ((h0-has-tag-n ir (ir-tag-div))
     (bytes-append-all
      (list (h0-codegen (cadr ir) td)
            (a64-str #x0 #x1F (* td #x8))
            (h0-codegen (caddr ir) (+ td #x1))
            (a64-lsr-imm #x1 #x0 #x4)             ; untag right
            (a64-ldr #x0 #x1F (* td #x8))
            (a64-lsr-imm #x0 #x0 #x4)             ; untag left
            (a64-sdiv #x0 #x0 #x1)                ; divide
            (a64-lsl-imm #x0 #x0 #x4))))          ; retag result

    ;; Modulo (a mod b = a - (a/b)*b)
    ((h0-has-tag-n ir (ir-tag-mod))
     (bytes-append-all
      (list (h0-codegen (cadr ir) td)
            (a64-str #x0 #x1F (* td #x8))         ; save left
            (h0-codegen (caddr ir) (+ td #x1))
            (a64-str #x0 #x1F (* (+ td #x1) #x8)) ; save right
            (a64-lsr-imm #x1 #x0 #x4)             ; untag right
            (a64-ldr #x0 #x1F (* td #x8))
            (a64-lsr-imm #x0 #x0 #x4)             ; untag left
            (a64-sdiv #x2 #x0 #x1)                ; x2 = left/right
            (a64-msub #x0 #x2 #x1 #x0)            ; x0 = left - x2*right
            (a64-lsl-imm #x0 #x0 #x4))))          ; retag

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
     (bytes-append-all
      (list (h0-codegen (cadr ir) td)            ; compile car
            (a64-str #x0 #x1F (* td #x8))        ; save car
            (h0-codegen (caddr ir) (+ td #x1))   ; compile cdr
            (a64-mov-reg #x1 #x0)                ; x1 = cdr
            (a64-ldr #x0 #x1F (* td #x8))        ; x0 = car
            (a64-str #x0 #x1C #x0)               ; [x28] = car
            (a64-str #x1 #x1C #x8)               ; [x28+8] = cdr
            (a64-mov-reg #x0 #x1C)               ; x0 = cons ptr
            (a64-add-imm #x0 #x0 #x1)            ; tag as cons (1)
            (a64-add-imm #x1C #x1C #x10))))      ; bump heap

    ;; Car
    ((h0-has-tag-n ir (ir-tag-car))
     (bytes-append-all
      (list (h0-codegen (cadr ir) td)
            (a64-sub-imm #x0 #x0 #x1)            ; untag cons
            (a64-ldr #x0 #x0 #x0))))             ; load car

    ;; Cdr
    ((h0-has-tag-n ir (ir-tag-cdr))
     (bytes-append-all
      (list (h0-codegen (cadr ir) td)
            (a64-sub-imm #x0 #x0 #x1)            ; untag cons
            (a64-ldr #x0 #x0 #x8))))             ; load cdr

    ;; Null check
    ((h0-has-tag-n ir (ir-tag-null))
     (bytes-append-all
      (list (h0-codegen (cadr ir) td)
            (a64-cmp-imm #x0 #x0)
            (a64-cset #x0 (cond-eq))
            (a64-lsl-imm #x0 #x0 #x4))))

    ;; Let binding
    ;; h0-compile assigns offset 0 to the innermost binding
    ;; Offset 0 -> [x20-0], offset 1 -> [x20-8], etc.
    ;; Nested lets: inner var gets offset 0, outer var gets offset 1
    ;; Store value at x20-0 (the slot for offset 0), decrement x20 for body
    ((h0-has-tag-n ir (ir-tag-let))
     (let* ((val-ir (caddr ir))
            (body-ir (cadddr ir)))
       (bytes-append-all
        (list (h0-codegen val-ir td)
              ;; Decrement x20 BEFORE storing (so offset 0 refers to new slot)
              (a64-sub-imm #x14 #x14 #x8)        ; x20 -= 8 (grow frame)
              (a64-str #x0 #x14 #x0)             ; [x20] = value (at new x20)
              (h0-codegen body-ir td)
              (a64-add-imm #x14 #x14 #x8)))))    ; x20 += 8 (restore frame)

    ;; Progn
    ((h0-has-tag-n ir (ir-tag-progn))
     (h0-codegen-progn (cadr ir) td))

    ;; Default - return 0
    (t (a64-movz #x0 #x0))))

;; Codegen helper for binary operations
(defun h0-codegen-binop (left-ir right-ir op-instrs td)
  (bytes-append-all
   (list (h0-codegen left-ir td)
         (a64-str #x0 #x1F (* td #x8))           ; save left
         (h0-codegen right-ir (+ td #x1))
         (a64-mov-reg #x1 #x0)                   ; x1 = right
         (a64-ldr #x0 #x1F (* td #x8))           ; x0 = left
         op-instrs)))

;; Codegen helper for comparisons
(defun h0-codegen-cmp (left-ir right-ir cond td)
  (bytes-append-all
   (list (h0-codegen left-ir td)
         (a64-str #x0 #x1F (* td #x8))
         (h0-codegen right-ir (+ td #x1))
         (a64-mov-reg #x1 #x0)
         (a64-ldr #x0 #x1F (* td #x8))
         (a64-cmp-reg #x0 #x1)
         (a64-cset #x0 cond)
         (a64-lsl-imm #x0 #x0 #x4))))

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

;;; Main entry point
;;; Mode is determined by first form in input.lisp:
;;;   #x100 - compile expression to IR and evaluate (compile-test)
;;;   #x200 - compile expression to IR and generate bytecode length (codegen-test)
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
                  ;; Normal interpretation mode
                  (t
                   (let ((fenv (collect-defuns forms nil)))
                     (h0-eval-forms forms nil fenv))))))))))

(main)
