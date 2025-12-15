;;;; Compile - S-expression to IR
;;;;
;;;; This is the front-end of the compiler.
;;;; Input: S-expressions (source code)
;;;; Output: Typed IR (ir-node)

(defpackage :habu.compile
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.ir
                ;; Literals
                :ir-lit :ir-nil :ir-t :ir-str :ir-sym :ir-kw
                ;; Variables
                :ir-var :ir-setq :ir-global :ir-set-global
                ;; Arithmetic
                :ir-add :ir-sub :ir-mul :ir-div :ir-mod :ir-neg
                ;; Comparison
                :ir-eq :ir-eql :ir-lt :ir-gt :ir-le :ir-ge :ir-zerop
                ;; Logical
                :ir-not :ir-and :ir-or
                ;; Bitwise
                :ir-band :ir-bor :ir-bxor :ir-bsh :ir-bnot
                ;; Control flow
                :ir-if :ir-progn :ir-while :ir-let
                :ir-block :ir-return-from :ir-loop :ir-continue
                :ir-dolist :ir-dotimes
                ;; Functions
                :ir-call :ir-lambda :ir-funcall :ir-lambda-ref :ir-tail-call
                ;; List operations
                :ir-cons :ir-car :ir-cdr :ir-list :ir-length
                :ir-setcar :ir-setcdr :ir-nthcdr
                ;; Type predicates
                :ir-null :ir-consp :ir-symbolp :ir-stringp :ir-numberp
                :ir-keywordp :ir-functionp
                :ir-get-tag :ir-set-tag
                ;; String operations
                :ir-string-length :ir-string-ref :ir-string-concat
                :ir-make-string :ir-make-string-from-vector
                :ir-string-equal :ir-string-set
                ;; Vector operations
                :ir-make-vector :ir-vector-ref :ir-vector-set :ir-vector-length
                :ir-buffer-byte-ref :ir-buffer-byte-set :ir-buffer-to-string
                ;; Symbol operations
                :ir-make-symbol :ir-make-symbol-from-string
                :ir-symbol-name :ir-intern
                ;; Keyword operations
                :ir-keyword-name
                ;; File I/O
                :ir-read-file :ir-write-file :ir-write-bytes :ir-println
                :ir-sys-read :ir-sys-read-byte :ir-sys-write :ir-sys-write-char
                :ir-sys-open :ir-sys-close
                ;; System/Low-level
                :ir-exit :ir-error :ir-system
                :ir-mmap :ir-mmap-jit :ir-munmap
                :ir-pthread-jit-write-protect :ir-sys-dcache-flush :ir-sys-icache-invalidate
                :ir-funcall-ptr :ir-mem-set-byte :ir-mem-load-64 :ir-mem-load-byte
                ;; Heap/Runtime access
                :ir-get-intern-table :ir-set-intern-table
                :ir-get-keyword-table :ir-set-keyword-table
                :ir-get-lambda-counter :ir-set-lambda-counter
                :ir-get-symbol-counter :ir-set-symbol-counter
                :ir-get-symbol-table :ir-set-symbol-table
                :ir-get-symtab-offset :ir-get-symtab-count
                :ir-get-frame-pointer :ir-get-code-base
                :ir-set-global-vars :ir-get-global-vars
                :ir-get-cmdline-args
                ;; Multiple values
                :ir-values :ir-mvb)
  (:export :compile-expr :compile-forms :compile-defuns
           :env-lookup :env-extend :find-free-vars
           :*constants* :*defined-globals* :*fenv* :*macros*))

(in-package :habu.compile)

;;; Environment is a simple alist: ((name . offset) ...)
;;; Offset is the stack slot for this variable

(defun env-lookup (name env)
  "Look up variable in environment. Returns offset or nil."
  (let ((entry (assoc name env :test #'eq)))
    (if entry (cdr entry) nil)))

(defun env-extend (names env)
  "Extend environment with new bindings. Returns (new-env . base-offset)."
  (let ((offset (if env
                    (1+ (apply #'max (mapcar #'cdr env)))
                    0)))
    (cons (append (loop for name in names
                        for i from offset
                        collect (cons name i))
                  env)
          offset)))

;;; Main compilation function

(defun compile-expr (expr env)
  "Compile s-expression to typed IR.
   Returns: ir-node"
  (cond
    ;; nil literal
    ((null expr) (ir-nil))

    ;; Integer literal - tag it
    ((integerp expr) (ir-lit (ash expr 1)))  ; fixnum tag: shift left 1, set bit 0

    ;; String literal
    ((stringp expr) (ir-str expr))

    ;; Keyword literal
    ((keywordp expr) (ir-kw (symbol-name expr)))

    ;; Symbol - variable reference or special
    ((symbolp expr)
     (cond
       ((eq expr t) (ir-t))
       (t
        (let ((offset (env-lookup expr env)))
          (if offset
              (ir-var offset)
              (ir-global expr))))))

    ;; Compound form
    ((consp expr)
     (compile-form (car expr) (cdr expr) env))

    (t (error "compile-expr: unknown expression ~S" expr))))

(defun compile-form (op args env)
  "Compile a compound form."
  ;; Direct symbol dispatch - no keyword conversion needed for SBCL-hosted compiler
  (case op
    ;; Special forms
    (quote (compile-quote (car args)))
    (if (compile-if args env))
    (cond (compile-cond args env))
    (case (compile-case args env))
    (when (compile-when args env))
    (unless (compile-unless args env))
    (progn (compile-progn args env))
    (let (compile-let args env))
    (let* (compile-let* args env))
    (setq (compile-setq args env))
    (while (compile-while args env))
    (lambda (compile-lambda args env))
    (labels (compile-labels args env))
    (flet (compile-flet args env))
    (loop (compile-loop args env))
    (return (compile-return args env))
    (and (compile-and args env))
    (or (compile-or args env))
    (dolist (compile-dolist args env))
    (dotimes (compile-dotimes args env))
    (ecase (compile-ecase args env))
    (length (ir-length (compile-expr (first args) env)))

    ;; Arithmetic
    (+ (ir-add (compile-expr (first args) env)
               (compile-expr (second args) env)))
    (- (if (= (length args) 1)
           (ir-neg (compile-expr (first args) env))
           (ir-sub (compile-expr (first args) env)
                   (compile-expr (second args) env))))
    (* (ir-mul (compile-expr (first args) env)
               (compile-expr (second args) env)))
    (/ (ir-div (compile-expr (first args) env)
               (compile-expr (second args) env)))
    (mod (ir-mod (compile-expr (first args) env)
                 (compile-expr (second args) env)))

    ;; Comparison
    (eq (ir-eq (compile-expr (first args) env)
               (compile-expr (second args) env)))
    (eql (ir-eql (compile-expr (first args) env)
                 (compile-expr (second args) env)))
    (= (ir-eq (compile-expr (first args) env)
              (compile-expr (second args) env)))
    (< (ir-lt (compile-expr (first args) env)
              (compile-expr (second args) env)))
    (> (ir-gt (compile-expr (first args) env)
              (compile-expr (second args) env)))
    (<= (ir-le (compile-expr (first args) env)
               (compile-expr (second args) env)))
    (>= (ir-ge (compile-expr (first args) env)
               (compile-expr (second args) env)))
    (zerop (ir-zerop (compile-expr (first args) env)))

    ;; Logical
    (not (ir-not (compile-expr (first args) env)))

    ;; Bitwise
    (logand (ir-band (compile-expr (first args) env)
                     (compile-expr (second args) env)))
    (logior (ir-bor (compile-expr (first args) env)
                    (compile-expr (second args) env)))
    (logxor (ir-bxor (compile-expr (first args) env)
                     (compile-expr (second args) env)))
    (ash (ir-bsh (compile-expr (first args) env)
                 (compile-expr (second args) env)))
    (lognot (ir-bnot (compile-expr (first args) env)))

    ;; List operations
    (cons (ir-cons (compile-expr (first args) env)
                   (compile-expr (second args) env)))
    (car (ir-car (compile-expr (first args) env)))
    (cdr (ir-cdr (compile-expr (first args) env)))
    (list (ir-list (mapcar (lambda (a) (compile-expr a env)) args)))
    ;; Car/cdr combinations
    (cadr (ir-car (ir-cdr (compile-expr (first args) env))))
    (cddr (ir-cdr (ir-cdr (compile-expr (first args) env))))
    (caar (ir-car (ir-car (compile-expr (first args) env))))
    (caddr (ir-car (ir-cdr (ir-cdr (compile-expr (first args) env)))))
    (cdddr (ir-cdr (ir-cdr (ir-cdr (compile-expr (first args) env)))))
    (caaar (ir-car (ir-car (ir-car (compile-expr (first args) env)))))
    (caadr (ir-car (ir-car (ir-cdr (compile-expr (first args) env)))))
    (cadar (ir-car (ir-cdr (ir-car (compile-expr (first args) env)))))
    (cdaar (ir-cdr (ir-car (ir-car (compile-expr (first args) env)))))
    (cdadr (ir-cdr (ir-car (ir-cdr (compile-expr (first args) env)))))
    (cddar (ir-cdr (ir-cdr (ir-car (compile-expr (first args) env)))))
    (cadddr (ir-car (ir-cdr (ir-cdr (ir-cdr (compile-expr (first args) env))))))
    ;; Aliases
    (first (ir-car (compile-expr (first args) env)))
    (rest (ir-cdr (compile-expr (first args) env)))
    (second (ir-car (ir-cdr (compile-expr (first args) env))))
    (third (ir-car (ir-cdr (ir-cdr (compile-expr (first args) env)))))
    (fourth (ir-car (ir-cdr (ir-cdr (ir-cdr (compile-expr (first args) env))))))
    ;; nth - compile as repeated cdr then car
    (nth (compile-nth args env))

    ;; Type predicates
    (null (ir-null (compile-expr (first args) env)))
    (consp (ir-consp (compile-expr (first args) env)))
    (symbolp (ir-symbolp (compile-expr (first args) env)))
    (stringp (ir-stringp (compile-expr (first args) env)))
    (numberp (ir-numberp (compile-expr (first args) env)))
    (integerp (ir-numberp (compile-expr (first args) env)))  ; same as numberp for now
    (keywordp (ir-keywordp (compile-expr (first args) env)))
    (functionp (ir-functionp (compile-expr (first args) env)))

    ;; String operations
    (string-length (ir-string-length (compile-expr (first args) env)))
    (string-ref (ir-string-ref (compile-expr (first args) env)
                               (compile-expr (second args) env)))
    (char (ir-string-ref (compile-expr (first args) env)
                         (compile-expr (second args) env)))  ; alias

    ;; Vector operations
    (make-vector (ir-make-vector (compile-expr (first args) env)
                                 (compile-expr (second args) env)))
    (vector-ref (ir-vector-ref (compile-expr (first args) env)
                               (compile-expr (second args) env)))
    (aref (ir-vector-ref (compile-expr (first args) env)
                         (compile-expr (second args) env)))  ; alias
    (vector-set (ir-vector-set (compile-expr (first args) env)
                               (compile-expr (second args) env)
                               (compile-expr (third args) env)))
    (vector-length (ir-vector-length (compile-expr (first args) env)))

    ;; Symbol operations
    (make-symbol (ir-make-symbol (compile-expr (first args) env)))
    (symbol-name (ir-symbol-name (compile-expr (first args) env)))
    (intern (ir-intern (compile-expr (first args) env)))

    ;; Keyword operations
    (keyword-name (ir-keyword-name (compile-expr (first args) env)))

    ;; System
    (exit (ir-exit (compile-expr (first args) env)))
    (error (ir-error (compile-expr (first args) env)))

    ;; Funcall - indirect function call
    (funcall (ir-funcall (compile-expr (first args) env)
                         (mapcar (lambda (a) (compile-expr a env))
                                 (rest args))))

    ;; Default: named function call
    (otherwise
     (ir-call op (mapcar (lambda (a) (compile-expr a env)) args)))))

;;; Special form compilers

(defun compile-quote (datum)
  "Compile quoted datum."
  (cond
    ((null datum) (ir-nil))
    ((eq datum t) (ir-t))
    ((integerp datum) (ir-lit (ash datum 1)))
    ((stringp datum) (ir-str datum))
    ((keywordp datum) (ir-kw (symbol-name datum)))
    ((symbolp datum) (ir-sym datum))
    ((consp datum)
     ;; Quoted list - build at compile time
     (ir-cons (compile-quote (car datum))
              (compile-quote (cdr datum))))
    (t (error "compile-quote: can't quote ~S" datum))))

(defun compile-if (args env)
  "Compile if form."
  (let ((test (compile-expr (first args) env))
        (then (compile-expr (second args) env))
        (else (if (third args)
                  (compile-expr (third args) env)
                  (ir-nil))))
    (ir-if test then else)))

(defun compile-progn (forms env)
  "Compile progn form."
  (if (null forms)
      (ir-nil)
      (ir-progn (mapcar (lambda (f) (compile-expr f env)) forms))))

(defun compile-let (args env)
  "Compile let form."
  (let* ((bindings (first args))
         (body (rest args))
         (names (mapcar #'car bindings))
         (inits (mapcar #'cadr bindings))
         (ext (env-extend names env))
         (new-env (car ext))
         (base-offset (cdr ext))
         (count (length names))
         (offsets (loop for i from base-offset below (+ base-offset count) collect i)))
    (ir-let (loop for name in names
                  for init in inits
                  for offset from base-offset
                  collect (cons offset (compile-expr init env)))
            (compile-progn body new-env)
            count
            offsets)))

(defun compile-let* (args env)
  "Compile let* form (sequential bindings)."
  (let ((bindings (first args))
        (body (rest args)))
    (if (null bindings)
        (compile-progn body env)
        (let* ((binding (car bindings))
               (name (car binding))
               (init (cadr binding))
               (ext (env-extend (list name) env))
               (new-env (car ext))
               (offset (cdr ext)))
          (ir-let (list (cons offset (compile-expr init env)))
                  (compile-let* (cons (cdr bindings) body) new-env)
                  1
                  (list offset))))))

(defun compile-setq (args env)
  "Compile setq form."
  (let* ((name (first args))
         (value (compile-expr (second args) env))
         (offset (env-lookup name env)))
    (if offset
        (ir-setq offset value)
        (ir-set-global name value))))

(defun compile-while (args env)
  "Compile while form."
  (ir-while (compile-expr (first args) env)
            (compile-progn (rest args) env)))

(defun compile-lambda (args env)
  "Compile lambda form."
  (let* ((params (first args))
         (body (rest args))
         (ext (env-extend params nil))  ; fresh env for lambda body
         (new-env (car ext))
         (captures (find-free-vars body params env))
         ;; Compute capture offsets from env
         (offsets (mapcar (lambda (c) (or (env-lookup c env) 0)) captures)))
    (ir-lambda params
               (compile-progn body new-env)
               captures
               offsets)))

(defun compile-and (args env)
  "Compile and form (short-circuit)."
  (cond
    ((null args) (ir-t))
    ((null (cdr args)) (compile-expr (car args) env))
    (t (ir-and (compile-expr (car args) env)
               (compile-and (cdr args) env)))))

(defun compile-or (args env)
  "Compile or form (short-circuit)."
  (cond
    ((null args) (ir-nil))
    ((null (cdr args)) (compile-expr (car args) env))
    (t (ir-or (compile-expr (car args) env)
              (compile-or (cdr args) env)))))

(defun compile-cond (clauses env)
  "Compile cond form to nested if."
  (if (null clauses)
      (ir-nil)
      (let* ((clause (car clauses))
             (test (car clause))
             (body (cdr clause)))
        (if (eq test t)
            ;; (t ...) clause - always true
            (compile-progn body env)
            ;; Normal clause
            (ir-if (compile-expr test env)
                   (if body
                       (compile-progn body env)
                       (compile-expr test env))  ; (cond (x)) returns x if true
                   (compile-cond (cdr clauses) env))))))

(defun compile-case (args env)
  "Compile case form to cond."
  (let ((key-var (gensym "CASE-KEY"))
        (keyform (car args))
        (clauses (cdr args)))
    ;; Compile as: (let ((key keyform)) (cond ...))
    (let* ((ext (env-extend (list key-var) env))
           (new-env (car ext))
           (offset (cdr ext)))
      (ir-let (list (cons offset (compile-expr keyform env)))
              (compile-case-clauses key-var clauses new-env)
              1
              (list offset)))))

(defun compile-case-clauses (key-var clauses env)
  "Compile case clauses to cond."
  (if (null clauses)
      (ir-nil)
      (let* ((clause (car clauses))
             (keys (car clause))
             (body (cdr clause)))
        (cond
          ;; (otherwise ...) or (t ...) clause
          ((or (eq keys 'otherwise) (eq keys t))
           (compile-progn body env))
          ;; Single key
          ((atom keys)
           (ir-if (ir-eql (ir-var (env-lookup key-var env))
                          (compile-quote keys))
                  (compile-progn body env)
                  (compile-case-clauses key-var (cdr clauses) env)))
          ;; Multiple keys - (key1 key2 ...)
          (t
           (ir-if (compile-case-key-test key-var keys env)
                  (compile-progn body env)
                  (compile-case-clauses key-var (cdr clauses) env)))))))

(defun compile-case-key-test (key-var keys env)
  "Compile test for multiple case keys as (or (eql key k1) (eql key k2) ...)"
  (if (null (cdr keys))
      (ir-eql (ir-var (env-lookup key-var env))
              (compile-quote (car keys)))
      (ir-or (ir-eql (ir-var (env-lookup key-var env))
                     (compile-quote (car keys)))
             (compile-case-key-test key-var (cdr keys) env))))

(defun compile-when (args env)
  "Compile when form: (when test body...) -> (if test (progn body...) nil)"
  (ir-if (compile-expr (car args) env)
         (compile-progn (cdr args) env)
         (ir-nil)))

(defun compile-unless (args env)
  "Compile unless form: (unless test body...) -> (if test nil (progn body...))"
  (ir-if (compile-expr (car args) env)
         (ir-nil)
         (compile-progn (cdr args) env)))

(defun compile-labels (args env)
  "Compile labels form (local recursive functions).
   For now, compile as nested let with lambdas."
  ;; (labels ((f1 (params) body) (f2 (params) body)) body)
  ;; -> Compile local functions as lambdas in let bindings
  (let* ((bindings (car args))
         (body (cdr args))
         (names (mapcar #'car bindings))
         (count (length names)))
    ;; Extend env with function names
    (let* ((ext (env-extend names env))
           (new-env (car ext))
           (base-offset (cdr ext))
           (offsets (loop for i from base-offset below (+ base-offset count) collect i)))
      ;; Create ir-let with lambda bindings
      (ir-let (loop for binding in bindings
                    for offset from base-offset
                    collect (cons offset
                                  (let* ((params (cadr binding))
                                         (fn-body (cddr binding))
                                         (fn-ext (env-extend params new-env))
                                         (fn-env (car fn-ext)))
                                    (ir-lambda params
                                               (compile-progn fn-body fn-env)
                                               nil nil))))
              (compile-progn body new-env)
              count
              offsets))))

(defun compile-flet (args env)
  "Compile flet form (local non-recursive functions)."
  ;; Same as labels but functions can't see each other
  (let* ((bindings (car args))
         (body (cdr args))
         (names (mapcar #'car bindings))
         (count (length names)))
    (let* ((ext (env-extend names env))
           (new-env (car ext))
           (base-offset (cdr ext))
           (offsets (loop for i from base-offset below (+ base-offset count) collect i)))
      (ir-let (loop for binding in bindings
                    for offset from base-offset
                    collect (cons offset
                                  (let* ((params (cadr binding))
                                         (fn-body (cddr binding))
                                         ;; Use original env, not new-env
                                         (fn-ext (env-extend params env))
                                         (fn-env (car fn-ext)))
                                    (ir-lambda params
                                               (compile-progn fn-body fn-env)
                                               nil nil))))
              (compile-progn body new-env)
              count
              offsets))))

(defun compile-loop (args env)
  "Compile simple loop form as (while t body...)."
  (ir-while (ir-t) (compile-progn args env)))

(defun compile-return (args env)
  "Compile return form.
   Note: This is a simplified version - real return needs block support."
  ;; For now, just return the value (loop exit handled by runtime)
  (if args
      (compile-expr (car args) env)
      (ir-nil)))

(defun compile-nth (args env)
  "Compile (nth n list) as repeated cdr then car.
   If n is a constant, unroll at compile time."
  (let ((n (first args))
        (list-expr (second args)))
    (if (integerp n)
        ;; Constant index - unroll
        (let ((result (compile-expr list-expr env)))
          (dotimes (i n)
            (setf result (ir-cdr result)))
          (ir-car result))
        ;; Variable index - need runtime loop (compile as function call for now)
        (ir-call 'nth (list (compile-expr n env)
                            (compile-expr list-expr env))))))

;;; Free variable analysis

(defun find-free-vars (expr bound env)
  "Find free variables in expr not in bound list but in env."
  (let ((free nil))
    (labels ((walk (e cur-bound)
               (cond
                 ((null e) nil)
                 ((symbolp e)
                  (when (and (env-lookup e env)
                             (not (member e cur-bound))
                             (not (member e free)))
                    (push e free)))
                 ((consp e)
                  (case (car e)
                    (quote nil)  ; don't walk into quotes
                    (lambda
                      ;; Lambda shadows its params
                      (let ((params (cadr e))
                            (body (cddr e)))
                        (dolist (form body)
                          (walk form (append params cur-bound)))))
                    (let
                      ;; Let shadows its bindings
                      (let* ((bindings (cadr e))
                             (body (cddr e))
                             (names (mapcar #'car bindings)))
                        (dolist (b bindings)
                          (walk (cadr b) cur-bound))
                        (dolist (form body)
                          (walk form (append names cur-bound)))))
                    (otherwise
                     (dolist (sub e)
                       (walk sub cur-bound))))))))
      (if (consp expr)
          (dolist (form expr) (walk form bound))
          (walk expr bound)))
    (nreverse free)))

;;; ============================================================
;;; Dolist/Dotimes Support
;;; ============================================================

(defun compile-dolist (args env)
  "Compile (dolist (var list [result]) body...)"
  (let* ((spec (first args))
         (var (first spec))
         (list-form (second spec))
         (result-form (third spec))
         (body (rest args))
         ;; Extend env with var
         (ext (env-extend (list var) env))
         (new-env (car ext))
         (var-offset (cdr ext))
         ;; Compile components
         (list-ir (compile-expr list-form env))
         (body-ir (compile-progn body new-env))
         (result-ir (if result-form
                        (compile-expr result-form new-env)
                        (ir-nil))))
    (ir-dolist var-offset list-ir body-ir result-ir)))

(defun compile-dotimes (args env)
  "Compile (dotimes (var count [result]) body...)"
  (let* ((spec (first args))
         (var (first spec))
         (count-form (second spec))
         (result-form (third spec))
         (body (rest args))
         ;; Extend env with var
         (ext (env-extend (list var) env))
         (new-env (car ext))
         (var-offset (cdr ext))
         ;; Compile components
         (count-ir (compile-expr count-form env))
         (body-ir (compile-progn body new-env))
         (result-ir (if result-form
                        (compile-expr result-form new-env)
                        (ir-nil))))
    (ir-dotimes var-offset count-ir body-ir result-ir)))

(defun compile-ecase (args env)
  "Compile ecase form (error on no match)."
  ;; For now, same as case - proper error handling TBD
  (compile-case args env))

;;; ============================================================
;;; compile-forms - Full Program Compilation
;;; ============================================================
;;;
;;; Multi-pass compilation of a list of forms (defuns, defmacro, etc.)
;;; Returns: compile-result (cr-result defuns main-ir)

;; Import defun-ir and compile-result constructors
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(habu.ir:defun-fn habu.ir:cr-result)))

;;; Global compiler state
(defvar *constants* nil "Alist of (name . value) for defconstant")
(defvar *defined-globals* nil "Alist of (name . init-value) for defvar/defparameter")
(defvar *fenv* nil "Function environment: ((name params . info) ...)")
(defvar *macros* (make-hash-table) "Hash table of macro name -> expander function")

(defun collect-defmacros (forms)
  "Pass 0: Collect all defmacro forms and register expanders."
  (dolist (form forms)
    (when (and (consp form) (eq (car form) 'defmacro))
      (let* ((name (second form))
             (params (third form))
             (body (cdddr form))
             ;; Create expander function using SBCL's eval
             (expander (eval `(lambda ,params ,@body))))
        (setf (gethash name *macros*) expander)))))

(defun collect-constants (forms acc)
  "Pass 1a: Collect defconstant forms."
  (if (null forms)
      (nreverse acc)
      (let ((form (car forms)))
        (if (and (consp form)
                 (eq (car form) 'defconstant)
                 (>= (length form) 3))
            (let* ((name (second form))
                   (value (eval (third form))))  ; Evaluate constant value
              (collect-constants (cdr forms) (cons (cons name value) acc)))
            (collect-constants (cdr forms) acc)))))

(defun collect-globals (forms acc)
  "Pass 1b: Collect defvar/defparameter forms."
  (if (null forms)
      (nreverse acc)
      (let ((form (car forms)))
        (if (and (consp form)
                 (or (eq (car form) 'defvar)
                     (eq (car form) 'defparameter))
                 (>= (length form) 2))
            (let* ((name (second form))
                   (init (if (>= (length form) 3)
                             (third form)
                             nil)))
              (collect-globals (cdr forms) (cons (cons name init) acc)))
            (collect-globals (cdr forms) acc)))))

(defun collect-defun-names (forms acc)
  "Pass 2: Collect all defun names with their parameter lists.
   Returns: ((name . params) ...)"
  (if (null forms)
      (nreverse acc)
      (let ((form (car forms)))
        (if (and (consp form)
                 (eq (car form) 'defun)
                 (>= (length form) 4))
            (let ((name (second form))
                  (params (third form)))
              (collect-defun-names (cdr forms) (cons (cons name params) acc)))
            (collect-defun-names (cdr forms) acc)))))

(defun compile-defuns (forms fenv acc)
  "Pass 3: Compile all defun forms.
   Returns: list of defun-ir"
  (if (null forms)
      (nreverse acc)
      (let ((form (car forms)))
        (if (and (consp form)
                 (eq (car form) 'defun)
                 (>= (length form) 4))
            (let* ((name (second form))
                   (params (third form))
                   (body-forms (cdddr form))
                   ;; Create env for function body
                   (ext (env-extend params nil))
                   (env (car ext))
                   ;; Compile body
                   (body-ir (compile-progn body-forms env))
                   ;; Create defun-ir
                   (dfn (defun-fn name params body-ir 0)))
              (compile-defuns (cdr forms) fenv (cons dfn acc)))
            (compile-defuns (cdr forms) fenv acc)))))

(defun find-main-form (forms)
  "Find the main expression (last non-definition form)."
  (let ((main nil))
    (dolist (form forms)
      (unless (and (consp form)
                   (member (car form) '(defun defmacro defconstant defvar defparameter
                                        in-package require eval-when)))
        (setf main form)))
    main))

(defun compile-forms (forms)
  "Multi-pass compilation of forms to typed IR.
   Returns: compile-result (cr-result defuns main-ir)"
  ;; Reset global state
  (setf *constants* nil)
  (setf *defined-globals* nil)
  (setf *macros* (make-hash-table))

  ;; Pass 0: Collect macros
  (collect-defmacros forms)

  ;; Pass 1: Collect constants and globals
  (setf *constants* (collect-constants forms nil))
  (setf *defined-globals* (collect-globals forms nil))

  ;; Pass 2: Collect function names for forward references
  (let* ((fn-names (collect-defun-names forms nil))
         (fenv fn-names))
    (setf *fenv* fenv)

    ;; Pass 3: Compile all defuns
    (let* ((defuns (compile-defuns forms fenv nil))
           ;; Find and compile main
           (main-form (find-main-form forms))
           (main-ir (if main-form
                        (compile-expr main-form nil)
                        (ir-nil))))
      ;; Return typed result
      (cr-result defuns main-ir))))
