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

;;; Main entry point
(defun main ()
  (let ((source (native-read-file "input.lisp")))
    (if (null source)
        #xFF  ;; File not found
        (let ((forms (read-all source)))
          (if (null forms)
              #xFE  ;; Parse error
              (let ((fenv (collect-defuns forms nil)))
                (h0-eval-forms forms nil fenv)))))))

(main)
