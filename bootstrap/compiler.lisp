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

;;; Global function table for defun
(defvar *function-table* (make-hash-table :test 'eq))

;;; Global macro table for defmacro
(defvar *macro-table* (make-hash-table :test 'eq))

;;; Runtime integration - Bootstrap Phase 1
;;;
;;; BOOTSTRAP APPROACH (see docs/BOOTSTRAP_VS_STANDALONE.md):
;;;   - Phase 1 (NOW): Use SBCL's alien-callable to create FFI trampolines
;;;     Generated code calls runtime functions for heap allocation
;;;     SBCL-dependent but allows rapid development and testing
;;;
;;;   - Phase 2 (FUTURE): Inline allocation in generated machine code
;;;     No FFI dependencies, truly standalone operation
;;;     Requires compiling runtime/memory.lisp to machine code
;;;
(defvar *runtime-heap* nil "Reference to the runtime heap structure")
(defvar *runtime-cons-addr* nil "Address of runtime-cons trampoline")
(defvar *runtime-car-addr* nil "Address of runtime-car trampoline")
(defvar *runtime-cdr-addr* nil "Address of runtime-cdr trampoline")
(defvar *runtime-length-addr* nil "Address of runtime-length trampoline")
(defvar *runtime-nth-addr* nil "Address of runtime-nth trampoline")
(defvar *runtime-append-addr* nil "Address of runtime-append trampoline")
(defvar *runtime-reverse-addr* nil "Address of runtime-reverse trampoline")
(defvar *runtime-make-closure-0-addr* nil "Address of make-closure-0 trampoline")
(defvar *runtime-make-closure-1-addr* nil "Address of make-closure-1 trampoline")
(defvar *runtime-make-closure-2-addr* nil "Address of make-closure-2 trampoline")
(defvar *runtime-make-closure-3-addr* nil "Address of make-closure-3 trampoline")
(defvar *runtime-string-length-addr* nil "Address of runtime-string-length trampoline")
(defvar *runtime-string-concat-addr* nil "Address of runtime-string-concat trampoline")
(defvar *runtime-string-equal-addr* nil "Address of runtime-string-equal trampoline")
(defvar *runtime-string-substring-addr* nil "Address of runtime-string-substring trampoline")
(defvar *runtime-read-from-string-addr* nil "Address of runtime-read-from-string trampoline")
(defvar *runtime-print-to-string-addr* nil "Address of runtime-print-to-string trampoline")

(defun initialize-runtime-integration ()
  "Initialize runtime integration (Phase 1: Bootstrap mode with FFI trampolines)"
  ;; Load runtime if not already loaded
  (unless (find-package :habu-runtime)
    (let ((runtime-path (merge-pathnames "../runtime/memory.lisp"
                                         (or *load-truename* *default-pathname-defaults*)))
          (symbols-path (merge-pathnames "../runtime/symbols.lisp"
                                         (or *load-truename* *default-pathname-defaults*)))
          (lists-path (merge-pathnames "../runtime/lists.lisp"
                                       (or *load-truename* *default-pathname-defaults*)))
          (closures-path (merge-pathnames "../runtime/closures.lisp"
                                          (or *load-truename* *default-pathname-defaults*)))
          (strings-path (merge-pathnames "../runtime/strings.lisp"
                                         (or *load-truename* *default-pathname-defaults*)))
          (reader-path (merge-pathnames "../runtime/reader.lisp"
                                        (or *load-truename* *default-pathname-defaults*))))
      (if (probe-file runtime-path)
          (progn
            (load runtime-path)
            ;; Load symbol support
            (when (probe-file symbols-path)
              (load symbols-path))
            ;; Load list operations
            (when (probe-file lists-path)
              (load lists-path))
            ;; Load closure support
            (when (probe-file closures-path)
              (load closures-path))
            ;; Load string support
            (when (probe-file strings-path)
              (load strings-path))
            ;; Load reader/printer support
            (when (probe-file reader-path)
              (load reader-path)))
          (error "Cannot find runtime/memory.lisp at ~A" runtime-path))))

  ;; Initialize runtime heap
  (let ((heap-sym (find-symbol "*HEAP*" :habu-runtime)))
    (unless (and heap-sym (symbol-value heap-sym))
      (funcall (find-symbol "INITIALIZE-RUNTIME" :habu-runtime)))
    (setf *runtime-heap* (symbol-value heap-sym)))

  ;; Create C-callable wrappers for runtime functions using SBCL's alien FFI
  #+sbcl
  (progn
    ;; Define alien-callable wrappers that can be called from machine code
    (sb-alien:define-alien-callable habu-cons-trampoline
        sb-alien:unsigned-long ((car sb-alien:unsigned-long) (cdr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-CONS" :habu-runtime) car cdr))

    (sb-alien:define-alien-callable habu-car-trampoline
        sb-alien:unsigned-long ((cons-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-CAR" :habu-runtime) cons-ptr))

    (sb-alien:define-alien-callable habu-cdr-trampoline
        sb-alien:unsigned-long ((cons-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-CDR" :habu-runtime) cons-ptr))

    (sb-alien:define-alien-callable habu-length-trampoline
        sb-alien:unsigned-long ((list-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-LENGTH" :habu-runtime) list-ptr))

    (sb-alien:define-alien-callable habu-nth-trampoline
        sb-alien:unsigned-long ((n sb-alien:unsigned-long) (list-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-NTH" :habu-runtime) n list-ptr))

    (sb-alien:define-alien-callable habu-append-trampoline
        sb-alien:unsigned-long ((list1-ptr sb-alien:unsigned-long) (list2-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-APPEND" :habu-runtime) list1-ptr list2-ptr))

    (sb-alien:define-alien-callable habu-reverse-trampoline
        sb-alien:unsigned-long ((list-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-REVERSE" :habu-runtime) list-ptr))

    (sb-alien:define-alien-callable habu-make-closure-0-trampoline
        sb-alien:unsigned-long ((code-ptr sb-alien:unsigned-long) (arity sb-alien:unsigned-long))
      (funcall (find-symbol "MAKE-CLOSURE-0" :habu-runtime) code-ptr arity))

    (sb-alien:define-alien-callable habu-make-closure-1-trampoline
        sb-alien:unsigned-long ((code-ptr sb-alien:unsigned-long) (arity sb-alien:unsigned-long)
                                (var1 sb-alien:unsigned-long))
      (funcall (find-symbol "MAKE-CLOSURE-1" :habu-runtime) code-ptr arity var1))

    (sb-alien:define-alien-callable habu-make-closure-2-trampoline
        sb-alien:unsigned-long ((code-ptr sb-alien:unsigned-long) (arity sb-alien:unsigned-long)
                                (var1 sb-alien:unsigned-long) (var2 sb-alien:unsigned-long))
      (funcall (find-symbol "MAKE-CLOSURE-2" :habu-runtime) code-ptr arity var1 var2))

    (sb-alien:define-alien-callable habu-make-closure-3-trampoline
        sb-alien:unsigned-long ((code-ptr sb-alien:unsigned-long) (arity sb-alien:unsigned-long)
                                (var1 sb-alien:unsigned-long) (var2 sb-alien:unsigned-long)
                                (var3 sb-alien:unsigned-long))
      (funcall (find-symbol "MAKE-CLOSURE-3" :habu-runtime) code-ptr arity var1 var2 var3))

    (sb-alien:define-alien-callable habu-string-length-trampoline
        sb-alien:unsigned-long ((str-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-STRING-LENGTH" :habu-runtime) str-ptr))

    (sb-alien:define-alien-callable habu-string-concat-trampoline
        sb-alien:unsigned-long ((str1-ptr sb-alien:unsigned-long) (str2-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-STRING-CONCAT" :habu-runtime) str1-ptr str2-ptr))

    (sb-alien:define-alien-callable habu-string-equal-trampoline
        sb-alien:unsigned-long ((str1-ptr sb-alien:unsigned-long) (str2-ptr sb-alien:unsigned-long))
      (if (funcall (find-symbol "RUNTIME-STRING-EQUAL" :habu-runtime) str1-ptr str2-ptr)
          #x10  ; Tagged true (1 << 4)
          #x00)); Tagged false (0 << 4)

    (sb-alien:define-alien-callable habu-string-substring-trampoline
        sb-alien:unsigned-long ((str-ptr sb-alien:unsigned-long) (start sb-alien:unsigned-long)
                                (end sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-STRING-SUBSTRING" :habu-runtime) str-ptr start end))

    (sb-alien:define-alien-callable habu-read-from-string-trampoline
        sb-alien:unsigned-long ((str-ptr sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-READ-FROM-STRING" :habu-runtime) str-ptr))

    (sb-alien:define-alien-callable habu-print-to-string-trampoline
        sb-alien:unsigned-long ((value sb-alien:unsigned-long))
      (funcall (find-symbol "RUNTIME-PRINT-TO-STRING" :habu-runtime) value))

    ;; Get addresses of the trampolines using the correct SBCL mechanism:
    ;; 1. alien-callable-function gets the callable object
    ;; 2. alien-sap converts to System Area Pointer
    ;; 3. sap-int gets the integer address
    (setf *runtime-cons-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-cons-trampoline)))
    (setf *runtime-car-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-car-trampoline)))
    (setf *runtime-cdr-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-cdr-trampoline)))
    (setf *runtime-length-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-length-trampoline)))
    (setf *runtime-nth-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-nth-trampoline)))
    (setf *runtime-append-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-append-trampoline)))
    (setf *runtime-reverse-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-reverse-trampoline)))
    (setf *runtime-make-closure-0-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-make-closure-0-trampoline)))
    (setf *runtime-make-closure-1-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-make-closure-1-trampoline)))
    (setf *runtime-make-closure-2-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-make-closure-2-trampoline)))
    (setf *runtime-make-closure-3-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-make-closure-3-trampoline)))
    (setf *runtime-string-length-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-string-length-trampoline)))
    (setf *runtime-string-concat-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-string-concat-trampoline)))
    (setf *runtime-string-equal-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-string-equal-trampoline)))
    (setf *runtime-string-substring-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-string-substring-trampoline)))
    (setf *runtime-read-from-string-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-read-from-string-trampoline)))
    (setf *runtime-print-to-string-addr*
          (sb-alien:alien-sap (sb-alien:alien-callable-function 'habu-print-to-string-trampoline))))

  #-sbcl
  (error "Runtime integration only supported on SBCL currently")

  (format t "Runtime integration initialized:~%")
  (format t "  cons trampoline: ~X~%" (sb-sys:sap-int *runtime-cons-addr*))
  (format t "  car trampoline:  ~X~%" (sb-sys:sap-int *runtime-car-addr*))
  (format t "  cdr trampoline:  ~X~%" (sb-sys:sap-int *runtime-cdr-addr*)))

;;; Compiler intermediate representation
(defstruct expr
  type
  value
  args)

;;; Quasiquote expansion
(defun expand-quasiquote (form)
  "Expand quasiquote (backquote) forms with unquote and unquote-splicing"
  (cond
    ;; Unquote: (unquote x) => x
    ((and (consp form) (eq (first form) 'unquote))
     (second form))

    ;; Atom: just quote it
    ((atom form)
     `(quote ,form))

    ;; List starting with unquote-splicing in car position - error
    ((and (consp (first form)) (eq (first (first form)) 'unquote-splicing))
     (error "Unquote-splicing ,@~S in illegal position" (second (first form))))

    ;; List: process each element
    (t
     (expand-quasiquote-list form))))

(defun expand-quasiquote-list (forms)
  "Expand a list within quasiquote, handling unquote-splicing"
  (cond
    ;; Empty list
    ((null forms)
     '(quote ()))

    ;; Car is (unquote-splicing x): splice x into the list
    ((and (consp (first forms))
          (eq (first (first forms)) 'unquote-splicing))
     (let ((splicee (second (first forms)))
           (rest-expansion (expand-quasiquote-list (rest forms))))
       `(append ,splicee ,rest-expansion)))

    ;; Car is (unquote x): cons x onto the rest
    ((and (consp (first forms))
          (eq (first (first forms)) 'unquote))
     (let ((element (second (first forms)))
           (rest-expansion (expand-quasiquote-list (rest forms))))
       `(cons ,element ,rest-expansion)))

    ;; Recursively process car and cdr
    (t
     (let ((car-expansion (expand-quasiquote (first forms)))
           (cdr-expansion (expand-quasiquote-list (rest forms))))
       `(cons ,car-expansion ,cdr-expansion)))))

;;; Expand c[ad]r combinations
(defun expand-cadr (op arg)
  "Expand c[ad]{2,4}r combinations to nested car/cdr calls.
   Examples: cadr => (car (cdr x)), caddr => (car (cdr (cdr x)))"
  (let* ((name (symbol-name op))
         (len (length name)))
    ;; Check if it matches pattern c[ad]{2,4}r
    (when (and (>= len 4) (<= len 6)
               (char= (char name 0) #\C)
               (char= (char name (1- len)) #\R))
      ;; Extract the middle part (ad sequence)
      (let ((middle (subseq name 1 (1- len))))
        ;; Check all chars are 'A' or 'D'
        (when (every (lambda (c) (or (char= c #\A) (char= c #\D))) middle)
          ;; Build nested calls from right to left
          ;; E.g., "ADR" => (car (cdr arg))
          (let ((result arg))
            (loop for i from (1- (length middle)) downto 0
                  do (setf result
                          (if (char= (char middle i) #\A)
                              `(car ,result)
                              `(cdr ,result))))
            result))))))

;;; Recursively expand macros in a form
(defun expand-macros-in-form (form)
  "Recursively expand all macro calls in a form"
  (cond
    ;; Atoms don't need expansion
    ((not (consp form)) form)

    ;; Check if this is a macro call
    ((and (symbolp (first form))
          (gethash (first form) *macro-table*))
     ;; Expand the macro and recursively expand the result
     (let* ((macro-def (gethash (first form) *macro-table*))
            (params (car macro-def))
            (body (cdr macro-def))
            (args (rest form)))
       ;; Evaluate the lambda to do parameter substitution
       ;; Then recursively expand any macros in the result
       (let* ((macro-lambda `(lambda ,params ,body))
              (macro-fn (eval macro-lambda))
              (expanded (apply macro-fn args)))
         ;; Recursively expand macros in the result
         (expand-macros-in-form expanded))))

    ;; Not a macro call - recursively expand sub-forms
    (t (mapcar #'expand-macros-in-form form))))

;;; Free variable analysis for closures
(defparameter *builtin-operators*
  '(+ - * / mod rem div < > = <= >= not and or
    cons car cdr list null? cons? list? eq? equal?
    logand logior logxor lognot ash
    if cond case when unless
    progn begin quote quasiquote unquote unquote-splicing
    lambda let let* defun defvar defmacro funcall symbol-value set
    length nth append reverse
    string-length string-concat string-equal string-substring
    read print
    ;; Add more operators as needed
    ))

(defun builtin-operator-p (sym)
  "Check if symbol is a built-in operator"
  (member sym *builtin-operators*))

(defun collect-variables (form)
  "Collect all variable references in a form (excluding built-in operators)"
  (cond
    ((null form) nil)
    ((symbolp form)
     (if (builtin-operator-p form)
         nil
         (list form)))
    ((not (consp form)) nil)
    ((eq (first form) 'quote) nil)  ; Quoted forms don't reference variables
    ((eq (first form) 'lambda)
     ;; In lambda, collect from body but exclude parameters
     (let ((params (second form))
           (body (third form)))
       (set-difference (collect-variables body) params)))
    ((eq (first form) 'let)
     ;; In let, collect from values and body, exclude bound variables
     (let* ((bindings (second form))
            (body (third form))
            (vars (mapcar #'first bindings))
            (vals (mapcar #'second bindings)))
       (append (apply #'append (mapcar #'collect-variables vals))
               (set-difference (collect-variables body) vars))))
    ((eq (first form) 'defun)
     ;; Don't analyze defun bodies for free variables at this level
     nil)
    (t
     ;; Regular form - collect from all subforms, skip the operator
     (apply #'append (mapcar #'collect-variables (rest form))))))

(defun find-free-variables (body params &optional (env nil))
  "Find free variables in body that are not in params or env"
  (let* ((all-vars (collect-variables body))
         (bound-vars (append params env)))
    (remove-duplicates (set-difference all-vars bound-vars))))

;;; Parse Lisp expression to IR
(defun parse (form)
  "Parse a Lisp form into compiler IR"
  (cond
    ((integerp form)
     (make-expr :type 'fixnum :value form))

    ((stringp form)
     (make-expr :type 'string :value form))

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
     ;; OR named-let: (let name ((var1 val1) ...) body) for recursion
     (let ((second-elem (second form)))
       (if (symbolp second-elem)
           ;; Named-let for recursion: (let name ((x 1) (y 2)) body)
           ;; Compile as a loop structure instead of lambda transformation
           (let* ((name second-elem)
                  (bindings (third form))
                  (body (fourth form)))
             (make-expr :type 'named-let
                        :value (list name bindings)  ; store name and bindings
                        :args (list (parse body))))
           ;; Regular let
           (make-expr :type 'let
                      :value second-elem  ; bindings
                      :args (list (parse (third form)))))))

    ((and (consp form) (eq (first form) 'let*))
     ;; Special form: (let* ((var1 val1) (var2 val2) ...) body)
     ;; Sequential bindings - transform to nested lets
     (let ((bindings (second form))
           (body (third form)))
       (if (null bindings)
           ;; No bindings: (let* () body) -> body
           (parse body)
           ;; Transform to nested lets: (let* ((x 1) (y 2)) body) -> (let ((x 1)) (let* ((y 2)) body))
           (if (= (length bindings) 1)
               ;; Last binding: (let* ((x 1)) body) -> (let ((x 1)) body)
               (parse `(let (,(first bindings)) ,body))
               ;; Multiple bindings: recurse
               (parse `(let (,(first bindings))
                         (let* ,(rest bindings) ,body)))))))

    ((and (consp form) (eq (first form) 'lambda))
     ;; Special form: (lambda (params) body)
     ;; Analyze for free variables to determine if closure is needed
     (let* ((params (second form))
            (body (third form))
            (free-vars (find-free-variables body params)))
       (if free-vars
           ;; Lambda with free variables - needs closure
           ;; Store: parsed-body, free-vars, original-body (for runtime wrapper creation)
           (make-expr :type 'closure
                      :value params      ; Parameter list
                      :args (list (parse body) free-vars body))
           ;; Lambda with no free variables - simple lambda
           (make-expr :type 'lambda
                      :value params      ; Parameter list
                      :args (list (parse body))))))

    ((and (consp form) (eq (first form) 'progn))
     ;; Special form: (progn expr1 expr2 ... exprN)
     (let ((exprs (rest form)))
       (make-expr :type 'progn
                  :value nil
                  :args (mapcar #'parse exprs))))

    ((and (consp form) (eq (first form) 'begin))
     ;; Scheme-style alias for progn
     (parse `(progn ,@(rest form))))

    ((and (consp form) (eq (first form) 'quote))
     ;; Special form: (quote datum)
     ;; Note: Don't recursively parse - keep quoted value as-is
     (let ((datum (second form)))
       (make-expr :type 'quote
                  :value datum
                  :args nil)))

    ((and (consp form) (eq (first form) 'quasiquote))
     ;; Special form: (quasiquote template)
     ;; Backquote ` - allows selective evaluation with unquote
     (parse (expand-quasiquote (second form))))

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

    ((and (consp form) (eq (first form) 'defun))
     ;; Special form: (defun name (params) body)
     ;; Store function definition in global table for compile-time inlining
     (let ((name (second form))
           (params (third form))
           (body (fourth form)))
       ;; Store in function table (for compile-time inlining)
       (setf (gethash name *function-table*) (cons params body))

       ;; ALSO compile to executable code and store in symbol's function slot
       ;; This enables runtime funcall
       (when (find-package :habu-runtime)
         (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
               (set-fn-fn (find-symbol "SET-SYMBOL-FUNCTION" :habu-runtime)))
           (when (and intern-fn set-fn-fn)
             (let* ((name-str (string name))
                    (sym (funcall intern-fn name-str))
                    ;; Create alien-callable wrapper for this function
                    ;; This allows calling from generated machine code
                    (callable-name (intern (format nil "HABU-FUNCTION-~A" (string-upcase (string name)))
                                          (find-package :habu-compiler))))
               ;; Create the Lisp function that implements the body
               (eval `(defun ,callable-name ,params ,body))

               ;; Create alien-callable wrapper (SBCL-specific for Phase 1)
               #+sbcl
               (let ((num-params (length params)))
                 (cond
                   ((= num-params 0)
                    (eval `(sb-alien:define-alien-callable ,callable-name
                               sb-alien:unsigned-long ()
                             (,callable-name))))
                   ((= num-params 1)
                    (eval `(sb-alien:define-alien-callable ,callable-name
                               sb-alien:unsigned-long ((,(first params) sb-alien:unsigned-long))
                             (,callable-name ,(first params)))))
                   ((= num-params 2)
                    (eval `(sb-alien:define-alien-callable ,callable-name
                               sb-alien:unsigned-long ((,(first params) sb-alien:unsigned-long)
                                                       (,(second params) sb-alien:unsigned-long))
                             (,callable-name ,(first params) ,(second params)))))
                   ((= num-params 3)
                    (eval `(sb-alien:define-alien-callable ,callable-name
                               sb-alien:unsigned-long ((,(first params) sb-alien:unsigned-long)
                                                       (,(second params) sb-alien:unsigned-long)
                                                       (,(third params) sb-alien:unsigned-long))
                             (,callable-name ,(first params) ,(second params) ,(third params)))))
                   (t (error "defun currently supports up to 3 parameters for runtime funcall")))

                 ;; Get the function pointer address
                 (let ((func-addr (sb-sys:sap-int
                                  (sb-alien:alien-sap
                                   (sb-alien:alien-callable-function callable-name)))))
                   (funcall set-fn-fn sym func-addr)
                   (format t "; defun ~A -> symbol ~X, code at ~X~%" name sym func-addr)))))))

       ;; Return 0 as a placeholder (defun doesn't produce runtime value)
       ;; The symbol is interned and function slot is set as a side effect
       (make-expr :type 'fixnum :value 0)))

    ((and (consp form) (eq (first form) 'defvar))
     ;; Special form: (defvar name initial-value)
     ;; Store global variable definition and set symbol-value slot
     (let ((name (second form))
           (initial-value (if (third form) (third form) nil)))
       ;; Intern the symbol and set its value slot
       (when (find-package :habu-runtime)
         (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
               (set-val-fn (find-symbol "SET-SYMBOL-VALUE" :habu-runtime)))
           (when (and intern-fn set-val-fn)
             (let* ((name-str (string name))
                    (sym (funcall intern-fn name-str))
                    ;; Convert the initial value to a tagged fixnum
                    ;; For now, only support fixnum constants and nil
                    (tagged-value
                     (cond
                       ((null initial-value) 0)  ; nil = 0
                       ((numberp initial-value) (ash initial-value 4))  ; tag as fixnum
                       ((eq initial-value t) (ash 1 4))  ; t = 1 << 4
                       (t (error "defvar only supports constant fixnum/nil/t values for now: ~S"
                                initial-value)))))
               (funcall set-val-fn sym tagged-value)
               (format t "; defvar ~A = ~A -> symbol ~X~%" name initial-value sym)))))

       ;; Return 0 as a placeholder (defvar doesn't produce runtime value)
       (make-expr :type 'fixnum :value 0)))

    ((and (consp form) (eq (first form) 'defmacro))
     ;; Special form: (defmacro name (params) body)
     ;; Store macro definition in global table
     ;; Macros are expanded at compile-time, not runtime
     (let ((name (second form))
           (params (third form))
           (body (fourth form)))
       (setf (gethash name *macro-table*) (cons params body))
       ;; ALSO define a function in SBCL that delegates to our macro expander
       ;; This allows nested macro calls in macro bodies to work
       (eval `(defun ,name (&rest args)
                (expand-macros-in-form (cons ',name args))))
       ;; Return 0 as a placeholder
       (make-expr :type 'fixnum :value 0)))

    ((and (consp form) (eq (first form) 'funcall))
     ;; Special form: (funcall fn-expr arg1 arg2 ...)
     ;; Two cases:
     ;;   1. (funcall 'name ...) - runtime function call via symbol
     ;;   2. (funcall expr ...) - call closure value
     (let* ((fn-expr (second form))
            (args (cddr form)))
       (if (and (consp fn-expr) (eq (first fn-expr) 'quote))
           ;; Case 1: Quoted function name - runtime-call
           (let ((fn-name (second fn-expr))
                 (fn-def (gethash (second fn-expr) *function-table*)))
             (unless fn-def
               (error "Undefined function: ~S" fn-name))
             ;; Create runtime-call IR node
             (make-expr :type 'runtime-call
                        :value fn-name
                        :args (mapcar #'parse args)))
           ;; Case 2: Expression that evaluates to closure - funcall
           (make-expr :type 'funcall
                      :value (parse fn-expr)
                      :args (mapcar #'parse args)))))

    ((and (consp form) (eq (first form) 'symbol-value))
     ;; Special form: (symbol-value 'name)
     ;; Look up the symbol's value slot and embed it as a constant
     (let* ((sym-name-expr (second form))
            ;; Extract the symbol name from the quoted symbol
            (sym-name (if (and (consp sym-name-expr)
                              (eq (first sym-name-expr) 'quote))
                         (second sym-name-expr)
                         (error "symbol-value requires quoted symbol name: ~S" sym-name-expr))))
       ;; Look up the symbol and get its value
       (if (find-package :habu-runtime)
           (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
                 (get-val-fn (find-symbol "RUNTIME-SYMBOL-VALUE" :habu-runtime)))
             (if (and intern-fn get-val-fn)
                 (let* ((name-str (string sym-name))
                        (sym (funcall intern-fn name-str))
                        (tagged-value (funcall get-val-fn sym)))
                   ;; Embed the value as a fixnum constant
                   ;; The value is already tagged, so untag it
                   (make-expr :type 'fixnum :value (ash tagged-value -4)))
                 (error "Runtime functions not available")))
           (error "Runtime not loaded"))))

    ((and (consp form) (eq (first form) 'set))
     ;; Special form: (set 'name value)
     ;; Set a global variable's value at compile-time
     (let* ((sym-name-expr (second form))
            (value-expr (third form))
            ;; Extract the symbol name from the quoted symbol
            (sym-name (if (and (consp sym-name-expr)
                              (eq (first sym-name-expr) 'quote))
                         (second sym-name-expr)
                         (error "set requires quoted symbol name: ~S" sym-name-expr))))
       ;; Compile the value expression first
       (let ((value-ir (parse value-expr)))
         ;; For now, only support constant values (like defvar)
         (unless (eq (expr-type value-ir) 'fixnum)
           (error "set currently only supports constant fixnum values"))

         ;; Set the symbol's value in the runtime
         (when (find-package :habu-runtime)
           (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
                 (set-val-fn (find-symbol "SET-SYMBOL-VALUE" :habu-runtime)))
             (when (and intern-fn set-val-fn)
               (let* ((name-str (string sym-name))
                      (sym (funcall intern-fn name-str))
                      (tagged-value (ash (expr-value value-ir) 4)))
                 (funcall set-val-fn sym tagged-value)
                 (format t "; set ~A = ~D -> symbol ~X~%"
                         sym-name (expr-value value-ir) sym)))))

         ;; Return the value as the result
         value-ir)))

    ((and (consp form) (eq (first form) 'setq))
     ;; Special form: (setq var value)
     ;; Mutate a lexical variable
     (let ((var (second form))
           (value (third form)))
       (make-expr :type 'setq
                  :value var  ; Variable name
                  :args (list (parse value)))))  ; Value expression

    ((and (consp form) (eq (first form) 'incf))
     ;; Macro: (incf var [delta]) -> (setq var (+ var delta))
     (let ((var (second form))
           (delta (if (third form) (third form) 1)))
       (parse `(setq ,var (+ ,var ,delta)))))

    ((and (consp form) (eq (first form) 'decf))
     ;; Macro: (decf var [delta]) -> (setq var (- var delta))
     (let ((var (second form))
           (delta (if (third form) (third form) 1)))
       (parse `(setq ,var (- ,var ,delta)))))

    ((and (consp form) (eq (first form) 'equal))
     ;; Alias for = (for compatibility)
     (parse `(= ,@(rest form))))

    ((and (consp form) (eq (first form) 'null))
     ;; Predicate: (null x) - check if x is 0/nil
     ;; Alias for zerop
     (parse `(zerop ,@(rest form))))

    ((and (consp form) (eq (first form) 'identity))
     ;; Function: (identity x) - returns its argument
     (parse (second form)))

    ((and (consp form) (eq (first form) 'logandc1))
     ;; Function: (logandc1 x y) => (logand (lognot x) y)
     (parse `(logand (lognot ,(second form)) ,(third form))))

    ((and (consp form) (eq (first form) 'logandc2))
     ;; Function: (logandc2 x y) => (logand x (lognot y))
     (parse `(logand ,(second form) (lognot ,(third form)))))

    ((and (consp form) (eq (first form) 'logorc1))
     ;; Function: (logorc1 x y) => (logior (lognot x) y)
     (parse `(logior (lognot ,(second form)) ,(third form))))

    ((and (consp form) (eq (first form) 'logorc2))
     ;; Function: (logorc2 x y) => (logior x (lognot y))
     (parse `(logior ,(second form) (lognot ,(third form)))))

    ((and (consp form) (eq (first form) 'square))
     ;; Function: (square x) => (* x x)
     (let ((x (second form)))
       (if (and (consp x) (not (eq (first x) 'quote)))
           ;; Complex expression: bind to temp var to avoid double evaluation
           (let ((temp (gensym "SQ")))
             (parse `(let ((,temp ,x)) (* ,temp ,temp))))
           ;; Simple expression: safe to duplicate
           (parse `(* ,x ,x)))))

    ((and (consp form) (eq (first form) 'clamp))
     ;; Function: (clamp x low high) => (max low (min high x))
     (parse `(max ,(third form) (min ,(fourth form) ,(second form)))))

    ((and (consp form) (eq (first form) 'between))
     ;; Predicate: (between x low high) => (and (>= x low) (<= x high))
     (parse `(and (>= ,(second form) ,(third form))
                  (<= ,(second form) ,(fourth form)))))

    ((and (consp form) (eq (first form) 'neg))
     ;; Function: (neg x) => (- x) - unary negation
     (parse `(- ,(second form))))

    ((and (consp form) (eq (first form) 'let1))
     ;; Macro: (let1 var val body) => (let ((var val)) body)
     (parse `(let ((,(second form) ,(third form))) ,(fourth form))))

    ;; Scheme-style predicate aliases
    ((and (consp form) (eq (first form) 'zero?))
     ;; Alias for zerop
     (parse `(zerop ,(second form))))

    ((and (consp form) (eq (first form) 'positive?))
     ;; Alias for plusp
     (parse `(plusp ,(second form))))

    ((and (consp form) (eq (first form) 'negative?))
     ;; Alias for minusp
     (parse `(minusp ,(second form))))

    ((and (consp form) (eq (first form) 'even?))
     ;; Alias for evenp
     (parse `(evenp ,(second form))))

    ((and (consp form) (eq (first form) 'odd?))
     ;; Alias for oddp
     (parse `(oddp ,(second form))))

    ((and (consp form) (eq (first form) 'number?))
     ;; Alias for numberp
     (parse `(numberp ,(second form))))

    ;; Power-of-2 utilities
    ((and (consp form) (eq (first form) 'power-of-2?))
     ;; Predicate: Check if x is a power of 2
     ;; A number is a power of 2 if x > 0 and (x & (x-1)) == 0
     (let ((x (second form)))
       (if (and (consp x) (not (eq (first x) 'quote)))
           ;; Complex expression: bind to temp var to avoid double evaluation
           (let ((temp (gensym "POW2")))
             (parse `(let ((,temp ,x))
                       (and (> ,temp 0) (zerop (logand ,temp (1- ,temp)))))))
           ;; Simple expression: safe to duplicate
           (parse `(and (> ,x 0) (zerop (logand ,x (1- ,x))))))))

    ((and (consp form) (eq (first form) 'log2))
     ;; Function: Integer log base 2 (position of highest set bit)
     ;; log2(x) = integer-length(x) - 1
     (parse `(1- (integer-length ,(second form)))))

    ;; Alignment utilities (boundary must be power of 2)
    ((and (consp form) (eq (first form) 'align-up))
     ;; Function: Align value up to boundary
     ;; align-up(x, b) = (x + b - 1) & ~(b - 1)
     (let ((x (second form))
           (boundary (third form)))
       (if (or (and (consp x) (not (eq (first x) 'quote)))
               (and (consp boundary) (not (eq (first boundary) 'quote))))
           ;; Complex expression: bind to temp vars
           (let ((tx (gensym "ALIGN-X"))
                 (tb (gensym "ALIGN-B")))
             (parse `(let ((,tx ,x) (,tb ,boundary))
                       (logand (+ ,tx (1- ,tb)) (lognot (1- ,tb))))))
           ;; Simple expressions: safe to duplicate
           (parse `(logand (+ ,x (1- ,boundary)) (lognot (1- ,boundary)))))))

    ((and (consp form) (eq (first form) 'align-down))
     ;; Function: Align value down to boundary
     ;; align-down(x, b) = x & ~(b - 1)
     (let ((x (second form))
           (boundary (third form)))
       (if (or (and (consp x) (not (eq (first x) 'quote)))
               (and (consp boundary) (not (eq (first boundary) 'quote))))
           ;; Complex expression: bind to temp vars
           (let ((tx (gensym "ALIGN-X"))
                 (tb (gensym "ALIGN-B")))
             (parse `(let ((,tx ,x) (,tb ,boundary))
                       (logand ,tx (lognot (1- ,tb))))))
           ;; Simple expressions: safe to duplicate
           (parse `(logand ,x (lognot (1- ,boundary)))))))

    ((and (consp form) (eq (first form) 'aligned?))
     ;; Predicate: Check if value is aligned to boundary
     ;; aligned?(x, b) = (x & (b - 1)) == 0
     (let ((x (second form))
           (boundary (third form)))
       (if (or (and (consp x) (not (eq (first x) 'quote)))
               (and (consp boundary) (not (eq (first boundary) 'quote))))
           ;; Complex expression: bind to temp vars
           (let ((tx (gensym "ALIGN-X"))
                 (tb (gensym "ALIGN-B")))
             (parse `(let ((,tx ,x) (,tb ,boundary))
                       (zerop (logand ,tx (1- ,tb))))))
           ;; Simple expressions: safe to duplicate
           (parse `(zerop (logand ,x (1- ,boundary)))))))

    ;; Additional utility functions
    ((and (consp form) (eq (first form) 'cube))
     ;; Function: (cube x) => (* x x x)
     (let ((x (second form)))
       (if (and (consp x) (not (eq (first x) 'quote)))
           ;; Complex expression: bind to temp var
           (let ((temp (gensym "CUBE")))
             (parse `(let ((,temp ,x)) (* ,temp ,temp ,temp))))
           ;; Simple expression: safe to duplicate
           (parse `(* ,x ,x ,x)))))

    ((and (consp form) (eq (first form) 'double))
     ;; Function: (double x) => (* x 2)
     (parse `(* ,(second form) 2)))

    ((and (consp form) (eq (first form) 'half))
     ;; Function: (half x) => (/ x 2)
     (parse `(/ ,(second form) 2)))

    ((and (consp form) (eq (first form) 'avg))
     ;; Function: (avg x y) => (/ (+ x y) 2)
     (parse `(/ (+ ,(second form) ,(third form)) 2)))

    ((and (consp form) (eq (first form) 'range))
     ;; Function: (range x y) => (- y x) - difference/range
     (parse `(- ,(third form) ,(second form))))

    ;; Bit manipulation utilities
    ((and (consp form) (eq (first form) 'set-bit))
     ;; Function: (set-bit x n) - set bit n in x
     ;; => (logior x (ash 1 n))
     (parse `(logior ,(second form) (ash 1 ,(third form)))))

    ((and (consp form) (eq (first form) 'clear-bit))
     ;; Function: (clear-bit x n) - clear bit n in x
     ;; => (logand x (lognot (ash 1 n)))
     (parse `(logand ,(second form) (lognot (ash 1 ,(third form))))))

    ((and (consp form) (eq (first form) 'toggle-bit))
     ;; Function: (toggle-bit x n) - flip bit n in x
     ;; => (logxor x (ash 1 n))
     (parse `(logxor ,(second form) (ash 1 ,(third form)))))

    ((and (consp form) (eq (first form) 'mask))
     ;; Function: (mask n) - create n-bit mask (2^n - 1)
     ;; => (1- (ash 1 n))
     (parse `(1- (ash 1 ,(second form)))))

    ((and (consp form) (eq (first form) 'low-bits))
     ;; Function: (low-bits x n) - extract low n bits
     ;; => (logand x (mask n))
     (parse `(logand ,(second form) (1- (ash 1 ,(third form))))))

    ((and (consp form) (eq (first form) 'high-bit?))
     ;; Predicate: Check if highest bit is set (negative in 2's complement)
     ;; => (minusp x)
     (parse `(minusp ,(second form))))

    ;; Comparison and range utilities
    ((and (consp form) (eq (first form) 'min3))
     ;; Function: Minimum of three values
     (parse `(min ,(second form) (min ,(third form) ,(fourth form)))))

    ((and (consp form) (eq (first form) 'max3))
     ;; Function: Maximum of three values
     (parse `(max ,(second form) (max ,(third form) ,(fourth form)))))

    ((and (consp form) (eq (first form) 'within?))
     ;; Predicate: Check if value is within range (inclusive)
     ;; Same as between but more descriptive name
     (parse `(and (>= ,(second form) ,(third form))
                  (<= ,(second form) ,(fourth form)))))

    ((and (consp form) (eq (first form) 'outside?))
     ;; Predicate: Check if value is outside range (exclusive)
     (parse `(or (< ,(second form) ,(third form))
                 (> ,(second form) ,(fourth form)))))

    ((and (consp form) (eq (first form) 'sign))
     ;; Function: Return sign of number (-1, 0, or 1)
     ;; Same as signum
     (parse `(signum ,(second form))))

    ((and (consp form) (eq (first form) 'same-sign?))
     ;; Predicate: Check if two numbers have same sign
     (let ((x (second form))
           (y (third form)))
       (if (or (and (consp x) (not (eq (first x) 'quote)))
               (and (consp y) (not (eq (first y) 'quote))))
           ;; Complex expression: bind to temp vars
           (let ((tx (gensym "SIGN-X"))
                 (ty (gensym "SIGN-Y")))
             (parse `(let ((,tx ,x) (,ty ,y))
                       (or (and (zerop ,tx) (zerop ,ty))
                           (and (plusp ,tx) (plusp ,ty))
                           (and (minusp ,tx) (minusp ,ty))))))
           ;; Simple expressions: safe to duplicate
           (parse `(or (and (zerop ,x) (zerop ,y))
                       (and (plusp ,x) (plusp ,y))
                       (and (minusp ,x) (minusp ,y)))))))

    ;; Bit rotation (circular shift)
    ((and (consp form) (eq (first form) 'rotl))
     ;; Function: Rotate left
     ;; rotl(x, n, width) = (logior (ash (logand x (mask width)) n)
     ;;                              (ash (logand x (mask width)) (- n width)))
     ;; Simplified for fixnums: just use logior of left and right shifts
     (let ((x (second form))
           (n (third form))
           (width (or (fourth form) 32))) ; default to 32-bit rotation
       (parse `(let ((#1=#:val (logand ,x (mask ,width)))
                     (#2=#:shift (mod ,n ,width)))
                 (logior (logand (ash #1# #2#) (mask ,width))
                         (ash #1# (- #2# ,width)))))))

    ((and (consp form) (eq (first form) 'rotr))
     ;; Function: Rotate right
     ;; Same as rotl with negative shift
     (let ((x (second form))
           (n (third form))
           (width (or (fourth form) 32)))
       (parse `(rotl ,x (- ,width (mod ,n ,width)) ,width))))

    ;; Conditional expressions
    ((and (consp form) (eq (first form) 'if-let))
     ;; Macro: (if-let var test then else)
     ;; Bind var to test result, execute then if true, else otherwise
     (let ((var (second form))
           (test (third form))
           (then-expr (fourth form))
           (else-expr (fifth form)))
       (parse `(let ((,var ,test))
                 (if ,var ,then-expr ,else-expr)))))

    ((and (consp form) (eq (first form) 'when-let))
     ;; Macro: (when-let var test body...)
     ;; Bind var to test result, execute body if true
     (let ((var (second form))
           (test (third form))
           (body (cdddr form)))
       (parse `(let ((,var ,test))
                 (when ,var (progn ,@body))))))

    ;; Additional predicates and utilities
    ((and (consp form) (eq (first form) 'nonzero?))
     ;; Predicate: Check if value is non-zero
     ;; Opposite of zerop
     (parse `(not (zerop ,(second form)))))

    ((and (consp form) (eq (first form) 'divisible?))
     ;; Predicate: Check if x is divisible by y
     ;; (divisible? x y) => (zerop (mod x y))
     (parse `(zerop (mod ,(second form) ,(third form)))))

    ((and (consp form) (eq (first form) 'multiple-of?))
     ;; Predicate: Same as divisible?
     (parse `(zerop (mod ,(second form) ,(third form)))))

    ((and (consp form) (eq (first form) 'quot))
     ;; Function: Quotient (same as /)
     ;; Common Lisp style alias
     (parse `(/ ,(second form) ,(third form))))

    ((and (consp form) (eq (first form) 'reciprocal))
     ;; Function: Reciprocal (1/x)
     (parse `(/ 1 ,(second form))))

    ((and (consp form) (eq (first form) 'sqr))
     ;; Function: Square (alias for square)
     (parse `(square ,(second form))))

    ;; Bit field operations
    ((and (consp form) (eq (first form) 'bit-field))
     ;; Function: Extract bit field from position with width
     ;; (bit-field x pos width) => extract width bits starting at pos
     ;; Formula: (logand (ash x (- pos)) (mask width))
     (parse `(logand (ash ,(second form) (- ,(third form))) (mask ,(fourth form)))))

    ((and (consp form) (eq (first form) 'bit-field-set))
     ;; Function: Set bit field at position with width to value
     ;; Clear the field, then OR in the new value
     (let ((x (second form))
           (pos (third form))
           (width (fourth form))
           (val (fifth form)))
       (parse `(logior (logand ,x (lognot (ash (mask ,width) ,pos)))
                       (ash (logand ,val (mask ,width)) ,pos)))))

    ;; Additional math utilities
    ((and (consp form) (eq (first form) 'divides?))
     ;; Predicate: Check if x divides y (opposite of divisible?)
     ;; (divides? x y) => (zerop (mod y x))
     (parse `(zerop (mod ,(third form) ,(second form)))))

    ((and (consp form) (eq (first form) 'coprime?))
     ;; Predicate: Check if x and y are coprime (gcd = 1)
     (parse `(= (gcd ,(second form) ,(third form)) 1)))

    ((and (consp form) (eq (first form) 'lerp))
     ;; Function: Linear interpolation
     ;; lerp(a, b, t) = a + t * (b - a)
     ;; For integer arithmetic: a + (t * (b - a)) / 100 (assuming t is 0-100)
     (parse `(+ ,(second form)
                (/ (* ,(fourth form) (- ,(third form) ,(second form))) 100))))

    ((and (consp form) (eq (first form) 'median3))
     ;; Function: Median of three values
     ;; median3(a,b,c) = max(min(a,b), min(max(a,b), c))
     (let ((a (second form))
           (b (third form))
           (c (fourth form)))
       (parse `(max (min ,a ,b) (min (max ,a ,b) ,c)))))

    ((and (consp form) (eq (first form) 'constrain))
     ;; Function: Alias for clamp
     (parse `(clamp ,(second form) ,(third form) ,(fourth form))))

    ((and (consp form) (eq (first form) 'map-range))
     ;; Function: Map value from one range to another
     ;; map-range(x, in-min, in-max, out-min, out-max)
     ;; = out-min + (x - in-min) * (out-max - out-min) / (in-max - in-min)
     (let ((x (second form))
           (in-min (third form))
           (in-max (fourth form))
           (out-min (fifth form))
           (out-max (sixth form)))
       (parse `(+ ,out-min
                  (/ (* (- ,x ,in-min) (- ,out-max ,out-min))
                     (- ,in-max ,in-min))))))

    ;; Additional comparison and numeric utilities
    ((and (consp form) (eq (first form) 'positive-or-zero?))
     ;; Predicate: Check if number is >= 0
     (parse `(>= ,(second form) 0)))

    ((and (consp form) (eq (first form) 'negative-or-zero?))
     ;; Predicate: Check if number is <= 0
     (parse `(<= ,(second form) 0)))

    ((and (consp form) (eq (first form) 'strictly-between?))
     ;; Predicate: Check if value is strictly between bounds (exclusive)
     (parse `(and (> ,(second form) ,(third form))
                  (< ,(second form) ,(fourth form)))))

    ((and (consp form) (eq (first form) 'approximately?))
     ;; Predicate: Check if two values are within tolerance
     ;; (approximately? a b tolerance) => (< (abs (- a b)) tolerance)
     (parse `(< (abs (- ,(second form) ,(third form))) ,(fourth form))))

    ((and (consp form) (eq (first form) 'nearest-multiple))
     ;; Function: Round to nearest multiple of n
     ;; nearest-multiple(x, n) = (* (/ (+ x (/ n 2)) n) n)
     (let ((x (second form))
           (n (third form)))
       (parse `(* (/ (+ ,x (/ ,n 2)) ,n) ,n))))

    ((and (consp form) (eq (first form) 'round-up-to))
     ;; Function: Round up to nearest multiple of n
     ;; Same as align-up but more descriptive name for general use
     (parse `(align-up ,(second form) ,(third form))))

    ((and (consp form) (eq (first form) 'round-down-to))
     ;; Function: Round down to nearest multiple of n
     ;; Same as align-down but more descriptive name
     (parse `(align-down ,(second form) ,(third form))))

    ;; Bit manipulation - population count variations
    ((and (consp form) (eq (first form) 'hamming-distance))
     ;; Function: Count differing bits between two values
     ;; hamming-distance(a, b) = logcount(logxor(a, b))
     (parse `(logcount (logxor ,(second form) ,(third form)))))

    ((and (consp form) (eq (first form) 'parity))
     ;; Function: Calculate parity (1 if odd number of bits, 0 if even)
     ;; parity(x) = mod(logcount(x), 2)
     (parse `(mod (logcount ,(second form)) 2)))

    ((and (consp form) (eq (first form) 'reverse-bits))
     ;; Function: Reverse bits in n-bit value (default 8 bits)
     ;; This is a simplified version for small bit widths
     ;; Note: Full implementation would need loops - placeholder just returns value
     (parse (second form)))

    ;; Mathematical sequences and patterns
    ((and (consp form) (eq (first form) 'factorial))
     ;; Function: Factorial (iterative for small values)
     ;; This needs recursion or loops - placeholder for now
     (let ((n (second form)))
       ;; For compile-time constants, we could compute it
       (if (and (consp n) (eq (car n) 'quote) (integerp (cadr n)))
           (let ((val (cadr n)))
             (if (<= val 12) ; Factorial up to 12 fits in fixnum range
                 (parse (labels ((fact (n) (if (<= n 1) 1 (* n (fact (1- n))))))
                         (fact val)))
                 (error "Factorial too large for fixnum")))
           ;; Runtime value - would need loops/recursion
           (error "Factorial of runtime values not yet implemented"))))

    ((and (consp form) (eq (first form) 'fibonacci))
     ;; Function: Fibonacci number (for compile-time constants)
     (let ((n (second form)))
       (if (and (consp n) (eq (car n) 'quote) (integerp (cadr n)))
           (let ((val (cadr n)))
             (if (<= val 20) ; Reasonable range
                 (parse (labels ((fib (n)
                                   (if (<= n 1)
                                       n
                                       (+ (fib (- n 1)) (fib (- n 2))))))
                         (fib val)))
                 (error "Fibonacci index too large")))
           (error "Fibonacci of runtime values not yet implemented"))))

    ((and (consp form) (eq (first form) 'triangle-number))
     ;; Function: Triangular number (sum of first n natural numbers)
     ;; triangle(n) = n * (n + 1) / 2
     (parse `(/ (* ,(second form) (1+ ,(second form))) 2)))

    ((and (consp form) (eq (first form) 'square-number?))
     ;; Predicate: Check if number is a perfect square
     ;; Check if isqrt(n)^2 == n
     (let ((n (second form)))
       (if (and (consp n) (not (eq (first n) 'quote)))
           (let ((temp (gensym "SQR"))
                 (root (gensym "ROOT")))
             (parse `(let ((,temp ,n))
                       (let ((,root (isqrt ,temp)))
                         (= (* ,root ,root) ,temp)))))
           (let ((root (gensym "ROOT")))
             (parse `(let ((,root (isqrt ,n)))
                       (= (* ,root ,root) ,n)))))))

    ;; Additional utility predicates and functions
    ((and (consp form) (eq (first form) 'bool->int))
     ;; Function: Convert boolean to integer (0 or 1)
     (parse `(if ,(second form) 1 0)))

    ((and (consp form) (eq (first form) 'int->bool))
     ;; Function: Convert integer to boolean (nonzero is true)
     (parse `(not (zerop ,(second form)))))

    ((and (consp form) (eq (first form) 'negate-if))
     ;; Function: Negate value if condition is true
     ;; (negate-if condition value) => (if condition (- value) value)
     (let ((condition (second form))
           (x (third form)))
       (if (and (consp x) (not (eq (first x) 'quote)))
           (let ((temp (gensym "NEG")))
             (parse `(let ((,temp ,x))
                       (if ,condition (- ,temp) ,temp))))
           (parse `(if ,condition (- ,x) ,x)))))

    ((and (consp form) (eq (first form) 'select))
     ;; Function: Select one of two values based on condition
     ;; (select cond then-val else-val) => (if cond then-val else-val)
     (parse `(if ,(second form) ,(third form) ,(fourth form))))

    ((and (consp form) (eq (first form) 'swap-if))
     ;; Macro: Conditionally swap two variables
     ;; (swap-if cond a b) => (if cond (progn (setq temp a) (setq a b) (setq b temp)))
     (let ((cond (second form))
           (a (third form))
           (b (fourth form))
           (temp (gensym "SWAP")))
       (parse `(when ,cond
                 (let ((,temp ,a))
                   (setq ,a ,b)
                   (setq ,b ,temp))))))

    ;; More bit manipulation utilities
    ((and (consp form) (eq (first form) 'count-leading-zeros))
     ;; Function: Count leading zeros in integer
     ;; clz(x) = 64 - integer-length(x) for 64-bit values
     ;; For tagged fixnums, we use 60 bits
     (parse `(- 60 (integer-length ,(second form)))))

    ((and (consp form) (eq (first form) 'count-trailing-zeros))
     ;; Function: Count trailing zeros
     ;; Find position of lowest set bit
     ;; ctz(x) = integer-length(x & -x) - 1
     (let ((x (second form))
           (val (gensym "CTZ")))
       (parse `(if (zerop ,x)
                   60
                   (let ((,val ,x))
                     (1- (integer-length (logand ,val (- 0 ,val)))))))))

    ((and (consp form) (eq (first form) 'next-power-of-2))
     ;; Function: Find next power of 2 >= n
     ;; Algorithm: Set all bits after highest bit, then add 1
     (let ((n (second form))
           (v (gensym "POW")))
       (if (and (consp n) (not (eq (first n) 'quote)))
           (let ((temp (gensym "N")))
             (parse `(let ((,temp (1- ,n)))
                       (let ((,v ,temp))
                         (setq ,v (logior ,v (ash ,v -1)))
                         (setq ,v (logior ,v (ash ,v -2)))
                         (setq ,v (logior ,v (ash ,v -4)))
                         (setq ,v (logior ,v (ash ,v -8)))
                         (setq ,v (logior ,v (ash ,v -16)))
                         (setq ,v (logior ,v (ash ,v -32)))
                         (1+ ,v)))))
           (parse `(let ((,v (1- ,n)))
                     (setq ,v (logior ,v (ash ,v -1)))
                     (setq ,v (logior ,v (ash ,v -2)))
                     (setq ,v (logior ,v (ash ,v -4)))
                     (setq ,v (logior ,v (ash ,v -8)))
                     (setq ,v (logior ,v (ash ,v -16)))
                     (setq ,v (logior ,v (ash ,v -32)))
                     (1+ ,v))))))

    ((and (consp form) (eq (first form) 'prev-power-of-2))
     ;; Function: Find previous power of 2 <= n
     ;; Just shift right by (integer-length - 1)
     (parse `(ash 1 (1- (integer-length ,(second form))))))

    ;; Mathematical predicates and utilities
    ((and (consp form) (eq (first form) 'in-range?))
     ;; Alias for within?
     (parse `(within? ,(second form) ,(third form) ,(fourth form))))

    ((and (consp form) (eq (first form) 'out-of-range?))
     ;; Alias for outside?
     (parse `(outside? ,(second form) ,(third form) ,(fourth form))))

    ((and (consp form) (eq (first form) 'wrap))
     ;; Function: Wrap value to range [0, max)
     ;; wrap(x, max) = mod(x, max) with proper handling of negatives
     (parse `(mod (+ (mod ,(second form) ,(third form)) ,(third form)) ,(third form))))

    ((and (consp form) (eq (first form) 'wrap-range))
     ;; Function: Wrap value to range [min, max)
     ;; wrap-range(x, min, max) = min + wrap(x - min, max - min)
     (let ((x (second form))
           (min-val (third form))
           (max-val (fourth form)))
       (parse `(+ ,min-val (wrap (- ,x ,min-val) (- ,max-val ,min-val))))))

    ;; Additional bit utilities
    ((and (consp form) (eq (first form) 'bit-width))
     ;; Function: Number of bits needed to represent value
     ;; bit-width(x) = integer-length(x)
     (parse `(integer-length ,(second form))))

    ((and (consp form) (eq (first form) 'msb-position))
     ;; Function: Position of most significant bit (0-indexed from right)
     ;; msb-position(x) = integer-length(x) - 1
     (parse `(1- (integer-length ,(second form)))))

    ((and (consp form) (eq (first form) 'lsb-position))
     ;; Function: Position of least significant bit (0-indexed from right)
     ;; lsb-position(x) = count-trailing-zeros(x)
     (parse `(count-trailing-zeros ,(second form))))

    ;; Arithmetic aliases
    ((and (consp form) (eq (first form) 'inc))
     ;; Function: Increment by 1 (alias for 1+)
     (parse `(1+ ,(second form))))

    ((and (consp form) (eq (first form) 'dec))
     ;; Function: Decrement by 1 (alias for 1-)
     (parse `(1- ,(second form))))

    ;; Comparison utilities
    ((and (consp form) (eq (first form) 'compare))
     ;; Function: Three-way comparison (spaceship operator)
     ;; Returns -1 if a < b, 0 if a = b, 1 if a > b
     (let ((a (second form))
           (b (third form)))
       (if (or (and (consp a) (not (eq (first a) 'quote)))
               (and (consp b) (not (eq (first b) 'quote))))
           (let ((temp-a (gensym "CMP-A"))
                 (temp-b (gensym "CMP-B")))
             (parse `(let ((,temp-a ,a))
                       (let ((,temp-b ,b))
                         (cond ((< ,temp-a ,temp-b) -1)
                               ((> ,temp-a ,temp-b) 1)
                               (t 0))))))
           (parse `(cond ((< ,a ,b) -1)
                         ((> ,a ,b) 1)
                         (t 0))))))

    ((and (consp form) (eq (first form) 'clamp-01))
     ;; Function: Clamp value to [0, 1]
     (parse `(clamp ,(second form) 0 1)))

    ;; Logical operators (for boolean logic)
    ((and (consp form) (eq (first form) 'implies))
     ;; Function: Logical implication (a => b)
     ;; a => b is equivalent to (not a) or b
     (parse `(or (not ,(second form)) ,(third form))))

    ((and (consp form) (eq (first form) 'xnor))
     ;; Function: Logical equivalence (XNOR)
     ;; Returns true if both arguments have the same truth value
     (parse `(not (logxor ,(second form) ,(third form)))))

    ((and (consp form) (eq (first form) 'nand))
     ;; Function: Logical NAND (not and)
     (parse `(not (and ,(second form) ,(third form)))))

    ((and (consp form) (eq (first form) 'nor))
     ;; Function: Logical NOR (not or)
     (parse `(not (or ,(second form) ,(third form)))))

    ;; Number theory predicates
    ((and (consp form) (eq (first form) 'triangular-number?))
     ;; Predicate: Check if number is a triangular number
     ;; n is triangular if (8n + 1) is a perfect square
     (let ((n (second form))
           (test (gensym "TRI")))
       (if (and (consp n) (not (eq (first n) 'quote)))
           (let ((temp (gensym "N")))
             (parse `(let ((,temp ,n))
                       (let ((,test (1+ (* 8 ,temp))))
                         (square-number? ,test)))))
           (parse `(let ((,test (1+ (* 8 ,n))))
                     (square-number? ,test))))))

    ((and (consp form) (eq (first form) 'pentagonal-number?))
     ;; Predicate: Check if number is a pentagonal number
     ;; n is pentagonal if (24n + 1) is a perfect square
     (let ((n (second form))
           (test (gensym "PENT")))
       (if (and (consp n) (not (eq (first n) 'quote)))
           (let ((temp (gensym "N")))
             (parse `(let ((,temp ,n))
                       (let ((,test (1+ (* 24 ,temp))))
                         (square-number? ,test)))))
           (parse `(let ((,test (1+ (* 24 ,n))))
                     (square-number? ,test))))))

    ((and (consp form) (eq (first form) 'hexagonal-number?))
     ;; Predicate: Check if number is a hexagonal number
     ;; All hexagonal numbers are also triangular
     ;; n is hexagonal if (8n + 1) is a perfect square AND the root is odd
     (let ((n (second form))
           (test (gensym "HEX"))
           (root (gensym "ROOT")))
       (if (and (consp n) (not (eq (first n) 'quote)))
           (let ((temp (gensym "N")))
             (parse `(let ((,temp ,n))
                       (let ((,test (1+ (* 8 ,temp))))
                         (let ((,root (isqrt ,test)))
                           (and (= (* ,root ,root) ,test)
                                (oddp ,root)))))))
           (parse `(let ((,test (1+ (* 8 ,n))))
                     (let ((,root (isqrt ,test)))
                       (and (= (* ,root ,root) ,test)
                            (oddp ,root))))))))

    ;; Additional sequence functions
    ((and (consp form) (eq (first form) 'pentagonal-number))
     ;; Function: Calculate nth pentagonal number
     ;; P(n) = n(3n - 1) / 2
     (let ((n (second form)))
       (if (numberp n)
           ;; Compile-time evaluation
           (parse (/ (* n (- (* 3 n) 1)) 2))
           (parse `(/ (* ,n (- (* 3 ,n) 1)) 2)))))

    ((and (consp form) (eq (first form) 'hexagonal-number))
     ;; Function: Calculate nth hexagonal number
     ;; H(n) = n(2n - 1)
     (let ((n (second form)))
       (if (numberp n)
           ;; Compile-time evaluation
           (parse (* n (- (* 2 n) 1)))
           (parse `(* ,n (- (* 2 ,n) 1))))))

    ;; More utility predicates
    ((and (consp form) (eq (first form) 'one?))
     ;; Predicate: Check if value equals 1
     (parse `(= ,(second form) 1)))

    ((and (consp form) (eq (first form) 'negative-one?))
     ;; Predicate: Check if value equals -1
     (parse `(= ,(second form) -1)))

    ((and (consp form) (eq (first form) 'positive-power-of-2?))
     ;; Predicate: Check if value is a positive power of 2
     (parse `(and (plusp ,(second form)) (power-of-2? ,(second form)))))

    ;; Additional conditional macros
    ((and (consp form) (eq (first form) 'if-not))
     ;; Macro: if-not condition then else
     ;; Inverted if - runs then branch when condition is false
     (parse `(if (not ,(second form)) ,(third form) ,(fourth form))))

    ((and (consp form) (eq (first form) 'when-not))
     ;; Macro: when-not condition body...
     ;; Runs body when condition is false
     (parse `(when (not ,(second form)) ,@(cddr form))))

    ((and (consp form) (eq (first form) 'unless-let))
     ;; Macro: unless-let (var value) body...
     ;; Bind var to value and execute body if value is falsy
     (let ((binding (second form))
           (body (cddr form)))
       (parse `(let ((,(first binding) ,(second binding)))
                 (unless ,(first binding) ,@body)))))

    ((and (consp form) (eq (first form) 'cond-let))
     ;; Macro: cond-let var (test1 val1) (test2 val2) ...
     ;; Like cond but binds result to var in each branch
     (let ((var (second form))
           (clauses (cddr form)))
       (parse `(cond ,@(mapcar (lambda (clause)
                                 `(,(first clause)
                                   (let ((,var ,(second clause)))
                                     ,var)))
                               clauses)))))

    ;; List and cons aliases
    ((and (consp form) (eq (first form) 'pair?))
     ;; Predicate: Alias for consp
     (parse `(consp ,(second form))))

    ((and (consp form) (eq (first form) 'empty?))
     ;; Predicate: Alias for null
     (parse `(null ,(second form))))

    ((and (consp form) (eq (first form) 'first))
     ;; Function: Alias for car
     (parse `(car ,(second form))))

    ((and (consp form) (eq (first form) 'rest))
     ;; Function: Alias for cdr
     (parse `(cdr ,(second form))))

    ((and (consp form) (eq (first form) 'second))
     ;; Function: Alias for cadr
     (parse `(cadr ,(second form))))

    ((and (consp form) (eq (first form) 'third))
     ;; Function: Alias for caddr
     (parse `(caddr ,(second form))))

    ((and (consp form) (eq (first form) 'fourth))
     ;; Function: Alias for cadddr
     (parse `(cadddr ,(second form))))

    ;; More numeric utilities
    ((and (consp form) (eq (first form) 'abs-diff))
     ;; Function: Absolute difference |a - b|
     (parse `(abs (- ,(second form) ,(third form)))))

    ((and (consp form) (eq (first form) 'distance))
     ;; Function: Alias for abs-diff (1D distance)
     (parse `(abs-diff ,(second form) ,(third form))))

    ((and (consp form) (eq (first form) 'pow2))
     ;; Function: Raise 2 to the power of n
     (let ((n (second form)))
       (if (numberp n)
           ;; Compile-time evaluation
           (parse (expt 2 n))
           (parse `(expt 2 ,n)))))

    ((and (consp form) (eq (first form) 'pow10))
     ;; Function: Raise 10 to the power of n
     (let ((n (second form)))
       (if (numberp n)
           ;; Compile-time evaluation
           (parse (expt 10 n))
           (parse `(expt 10 ,n)))))

    ;; Bit manipulation aliases and utilities
    ((and (consp form) (eq (first form) 'bit-set?))
     ;; Predicate: Check if bit is set (alias for logbitp)
     (parse `(logbitp ,(second form) ,(third form))))

    ((and (consp form) (eq (first form) 'bit-clear?))
     ;; Predicate: Check if bit is clear (not set)
     (parse `(not (logbitp ,(second form) ,(third form)))))

    ((and (consp form) (eq (first form) 'test-bit))
     ;; Function: Alias for logbitp
     (parse `(logbitp ,(second form) ,(third form))))

    ;; More range predicates
    ((and (consp form) (eq (first form) 'in-open-range?))
     ;; Predicate: Check if value in open range (min, max) - exclusive
     (parse `(strictly-between? ,(second form) ,(third form) ,(fourth form))))

    ((and (consp form) (eq (first form) 'in-closed-range?))
     ;; Predicate: Check if value in closed range [min, max] - inclusive
     (parse `(within? ,(second form) ,(third form) ,(fourth form))))

    ;; More sequence numbers
    ((and (consp form) (eq (first form) 'lucas-number))
     ;; Function: Calculate nth Lucas number
     ;; L(0) = 2, L(1) = 1, L(n) = L(n-1) + L(n-2)
     ;; For small constants, compute at compile time
     (let ((n (second form)))
       (if (and (numberp n) (<= n 20))
           ;; Compile-time evaluation for small n
           (labels ((lucas (k)
                      (cond ((= k 0) 2)
                            ((= k 1) 1)
                            (t (+ (lucas (- k 1)) (lucas (- k 2)))))))
             (parse (lucas n)))
           ;; Runtime computation
           (parse `(cond ((= ,n 0) 2)
                         ((= ,n 1) 1)
                         (t (+ (lucas-number (- ,n 1))
                               (lucas-number (- ,n 2)))))))))

    ;; More utility functions
    ((and (consp form) (eq (first form) 'toggle))
     ;; Function: Toggle between 0 and 1
     ;; toggle(x) = 1 - x for 0/1 values
     (parse `(- 1 ,(second form))))

    ((and (consp form) (eq (first form) 'flip))
     ;; Function: Alias for toggle
     (parse `(toggle ,(second form))))

    ((and (consp form) (eq (first form) 'normalize))
     ;; Function: Normalize value to [0, 1] range given min and max
     ;; normalize(x, min, max) = (x - min) / (max - min)
     (let ((x (second form))
           (min-val (third form))
           (max-val (fourth form)))
       (parse `(/ (- ,x ,min-val) (- ,max-val ,min-val)))))

    ((and (consp form) (eq (first form) 'denormalize))
     ;; Function: Denormalize value from [0, 1] to [min, max]
     ;; denormalize(x, min, max) = min + x * (max - min)
     (let ((x (second form))
           (min-val (third form))
           (max-val (fourth form)))
       (parse `(+ ,min-val (* ,x (- ,max-val ,min-val))))))

    ;; Prime checking (compile-time only for constants)
    ((and (consp form) (eq (first form) 'prime?))
     ;; Predicate: Check if number is prime (compile-time for constants)
     (let ((n (second form)))
       (if (numberp n)
           ;; Compile-time prime check
           (labels ((is-prime (num)
                      (cond ((<= num 1) nil)
                            ((<= num 3) t)
                            ((or (zerop (mod num 2)) (zerop (mod num 3))) nil)
                            (t (loop for i from 5 to (isqrt num) by 6
                                     never (or (zerop (mod num i))
                                               (zerop (mod num (+ i 2)))))))))
             (parse (if (is-prime n) 1 0)))
           ;; Runtime: Not supported - would be too slow
           (error "prime? only supports compile-time constants"))))

    ((and (consp form) (eq (first form) 'composite?))
     ;; Predicate: Check if number is composite (not prime, > 1)
     (let ((n (second form)))
       (if (numberp n)
           (labels ((is-prime (num)
                      (cond ((<= num 1) nil)
                            ((<= num 3) t)
                            ((or (zerop (mod num 2)) (zerop (mod num 3))) nil)
                            (t (loop for i from 5 to (isqrt num) by 6
                                     never (or (zerop (mod num i))
                                               (zerop (mod num (+ i 2)))))))))
             (parse (if (and (> n 1) (not (is-prime n))) 1 0)))
           (error "composite? only supports compile-time constants"))))

    ;; Additional mathematical operations
    ((and (consp form) (eq (first form) 'min*))
     ;; Function: Minimum of multiple values (varargs)
     (let ((args (rest form)))
       (cond ((null args) (error "min* requires at least one argument"))
             ((null (cdr args)) (parse (first args)))
             ((null (cddr args)) (parse `(min ,(first args) ,(second args))))
             (t (parse `(min ,(first args) (min* ,@(cdr args))))))))

    ((and (consp form) (eq (first form) 'max*))
     ;; Function: Maximum of multiple values (varargs)
     (let ((args (rest form)))
       (cond ((null args) (error "max* requires at least one argument"))
             ((null (cdr args)) (parse (first args)))
             ((null (cddr args)) (parse `(max ,(first args) ,(second args))))
             (t (parse `(max ,(first args) (max* ,@(cdr args))))))))

    ((and (consp form) (eq (first form) 'sum))
     ;; Function: Sum of multiple values
     (let ((args (rest form)))
       (cond ((null args) (parse 0))
             ((null (cdr args)) (parse (first args)))
             (t (parse `(+ ,@args))))))

    ((and (consp form) (eq (first form) 'product))
     ;; Function: Product of multiple values
     (let ((args (rest form)))
       (cond ((null args) (parse 1))
             ((null (cdr args)) (parse (first args)))
             (t (parse `(* ,@args))))))

    ((and (consp form) (eq (first form) 'negate))
     ;; Function: Negate value (unary minus)
     (parse `(- ,(second form))))

    ((and (consp form) (eq (first form) 'sqr-diff))
     ;; Function: Square of difference (a - b)^2
     (let ((a (second form))
           (b (third form))
           (diff (gensym "DIFF")))
       (if (or (and (consp a) (not (eq (first a) 'quote)))
               (and (consp b) (not (eq (first b) 'quote))))
           (let ((temp-a (gensym "A"))
                 (temp-b (gensym "B")))
             (parse `(let ((,temp-a ,a))
                       (let ((,temp-b ,b))
                         (let ((,diff (- ,temp-a ,temp-b)))
                           (* ,diff ,diff))))))
           (parse `(let ((,diff (- ,a ,b)))
                     (* ,diff ,diff))))))

    ;; More predicates
    ((and (consp form) (eq (first form) 'negative?))
     ;; Predicate: Is value negative? (Scheme-style alias)
     (parse `(minusp ,(second form))))

    ((and (consp form) (eq (first form) 'nonnegative?))
     ;; Predicate: Is value >= 0?
     (parse `(not (minusp ,(second form)))))

    ((and (consp form) (eq (first form) 'nonpositive?))
     ;; Predicate: Is value <= 0?
     (parse `(not (plusp ,(second form)))))

    ((and (consp form) (eq (first form) 'exact-power-of-2?))
     ;; Predicate: Is value exactly 2^n for some n >= 0?
     (parse `(and (plusp ,(second form)) (power-of-2? ,(second form)))))

    ((and (consp form) (eq (first form) 'multiple?))
     ;; Predicate: Is a a multiple of b? (same as divisible?)
     (parse `(divisible? ,(second form) ,(third form))))

    ((and (consp form) (eq (first form) 'factor?))
     ;; Predicate: Is a a factor of b? (same as divides?)
     (parse `(divides? ,(second form) ,(third form))))

    ;; Conditional expressions
    ((and (consp form) (eq (first form) 'and-let*))
     ;; Macro: Sequential binding with short-circuit
     ;; (and-let* ((x expr1) (y expr2)) body) - stops if any binding is falsy
     (let ((bindings (second form))
           (body (cddr form)))
       (if (null bindings)
           (parse `(progn ,@body))
           (let ((first-binding (first bindings))
                 (rest-bindings (rest bindings)))
             (parse `(let ((,(first first-binding) ,(second first-binding)))
                       (when ,(first first-binding)
                         (and-let* ,rest-bindings ,@body))))))))

    ((and (consp form) (eq (first form) 'or-let))
     ;; Macro: Bind and return first truthy value
     ;; (or-let (x val1) (y val2)) - returns first truthy binding
     (let ((bindings (rest form)))
       (if (null bindings)
           (parse 0)  ; Return 0 (falsy) if no bindings
           (let ((binding (first bindings)))
             (parse `(let ((,(first binding) ,(second binding)))
                       (if ,(first binding)
                           ,(first binding)
                           (or-let ,@(rest bindings)))))))))

    ((and (consp form) (eq (first form) 'dotimes))
     ;; Macro: Execute body n times with counter
     ;; (dotimes (i n) body...)
     (let ((var (first (second form)))
           (count (second (second form)))
           (body (cddr form))
           (counter (gensym "CNT"))
           (limit (gensym "LIM")))
       (parse `(let ((,limit ,count))
                 (let ((,counter 0))
                   (labels ((loop-fn ()
                              (when (< ,counter ,limit)
                                (let ((,var ,counter))
                                  ,@body
                                  (setq ,counter (1+ ,counter))
                                  (loop-fn)))))
                     (loop-fn)))))))

    ;; More bitwise utilities
    ((and (consp form) (eq (first form) 'bit-count))
     ;; Function: Alias for logcount (population count)
     (parse `(logcount ,(second form))))

    ((and (consp form) (eq (first form) 'popcount))
     ;; Function: Alias for logcount (population count)
     (parse `(logcount ,(second form))))

    ((and (consp form) (eq (first form) 'all-bits-set?))
     ;; Predicate: Check if all bits in mask are set in value
     ;; (value & mask) == mask
     (let ((value (second form))
           (mask (third form)))
       (if (or (and (consp value) (not (eq (first value) 'quote)))
               (and (consp mask) (not (eq (first mask) 'quote))))
           (let ((temp-v (gensym "VAL"))
                 (temp-m (gensym "MSK")))
             (parse `(let ((,temp-v ,value))
                       (let ((,temp-m ,mask))
                         (= (logand ,temp-v ,temp-m) ,temp-m)))))
           (parse `(= (logand ,value ,mask) ,mask)))))

    ((and (consp form) (eq (first form) 'any-bits-set?))
     ;; Predicate: Check if any bits in mask are set in value
     ;; (value & mask) != 0
     (parse `(logtest ,(third form) ,(second form))))

    ((and (consp form) (eq (first form) 'no-bits-set?))
     ;; Predicate: Check if no bits in mask are set in value
     ;; (value & mask) == 0
     (parse `(not (logtest ,(third form) ,(second form)))))

    ;; More range utilities
    ((and (consp form) (eq (first form) 'clamp-positive))
     ;; Function: Clamp to positive values (max 0)
     (parse `(max 0 ,(second form))))

    ((and (consp form) (eq (first form) 'clamp-negative))
     ;; Function: Clamp to negative values (min 0)
     (parse `(min 0 ,(second form))))

    ((and (consp form) (eq (first form) 'saturate))
     ;; Function: Saturate to [min, max] - alias for clamp
     (parse `(clamp ,(second form) ,(third form) ,(fourth form))))

    ;; Misc utilities
    ((and (consp form) (eq (first form) 'identity?))
     ;; Predicate: Check if two values are identical
     (parse `(= ,(second form) ,(third form))))

    ((and (consp form) (eq (first form) 'different?))
     ;; Predicate: Check if two values are different
     (parse `(/= ,(second form) ,(third form))))

    ((and (consp form) (eq (first form) 'max-of-3))
     ;; Function: Alias for max3
     (parse `(max3 ,(second form) ,(third form) ,(fourth form))))

    ((and (consp form) (eq (first form) 'min-of-3))
     ;; Function: Alias for min3
     (parse `(min3 ,(second form) ,(third form) ,(fourth form))))

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
       ;; First check if this is a c[ad]r combination (cadr, caddr, etc.)
       (let ((expansion (expand-cadr op (first args))))
         (if expansion
             (parse expansion)
             ;; Not a c[ad]r, check if this is a macro (macros expand at compile-time)
             (let ((macro-def (gethash op *macro-table*)))
               (if macro-def
                   ;; Macro: expand and re-parse
                   ;; Use expand-macros-in-form to handle nested macros properly
                   (let* ((expanded (expand-macros-in-form form)))
                     ;; Re-parse the expanded form
                     (parse expanded))
                   ;; Not a macro, check if this is a user-defined function
                   (let ((fn-def (gethash op *function-table*)))
                     (if fn-def
                         ;; User-defined function: transform to ((lambda params body) args...)
                         (let ((params (car fn-def))
                               (body (cdr fn-def)))
                           (parse `((lambda ,params ,body) ,@args)))
                         ;; Primitive operator
                         (make-expr :type 'call
                                    :value op
                                    :args (mapcar #'parse args))))))))))

    (t
     (error "Cannot parse form: ~S" form))))

;;; Algebraic simplification for mixed constant/variable expressions
(defun simplify-algebraic (op args)
  "Apply algebraic simplifications like (* x 0) => 0, (+ x 0) => x"
  (when (and args (= (length args) 2))
    (let ((arg1 (first args))
          (arg2 (second args)))
      (case op
        ;; Multiplication simplifications
        (*
         (cond
           ;; (* x 0) => 0 or (* 0 x) => 0
           ((and (eq (expr-type arg1) 'fixnum) (zerop (expr-value arg1)))
            (make-expr :type 'fixnum :value 0))
           ((and (eq (expr-type arg2) 'fixnum) (zerop (expr-value arg2)))
            (make-expr :type 'fixnum :value 0))
           ;; (* x 1) => x
           ((and (eq (expr-type arg2) 'fixnum) (= (expr-value arg2) 1))
            arg1)
           ;; (* 1 x) => x
           ((and (eq (expr-type arg1) 'fixnum) (= (expr-value arg1) 1))
            arg2)
           (t nil)))

        ;; Addition simplifications
        (+
         (cond
           ;; (+ x 0) => x
           ((and (eq (expr-type arg2) 'fixnum) (zerop (expr-value arg2)))
            arg1)
           ;; (+ 0 x) => x
           ((and (eq (expr-type arg1) 'fixnum) (zerop (expr-value arg1)))
            arg2)
           (t nil)))

        ;; Subtraction simplifications
        (-
         (cond
           ;; (- x 0) => x
           ((and (eq (expr-type arg2) 'fixnum) (zerop (expr-value arg2)))
            arg1)
           ;; (- 0 x) => (- x) but needs negation, skip for now
           (t nil)))

        ;; Division simplifications
        (/
         (cond
           ;; (/ x 1) => x
           ((and (eq (expr-type arg2) 'fixnum) (= (expr-value arg2) 1))
            arg1)
           ;; (/ 0 x) => 0 (when x != 0)
           ((and (eq (expr-type arg1) 'fixnum) (zerop (expr-value arg1)))
            (make-expr :type 'fixnum :value 0))
           (t nil)))

        ;; Bitwise AND simplifications
        (logand
         (cond
           ;; (logand x 0) => 0
           ((and (eq (expr-type arg2) 'fixnum) (zerop (expr-value arg2)))
            (make-expr :type 'fixnum :value 0))
           ;; (logand 0 x) => 0
           ((and (eq (expr-type arg1) 'fixnum) (zerop (expr-value arg1)))
            (make-expr :type 'fixnum :value 0))
           ;; (logand x -1) => x
           ((and (eq (expr-type arg2) 'fixnum) (= (expr-value arg2) -1))
            arg1)
           ;; (logand -1 x) => x
           ((and (eq (expr-type arg1) 'fixnum) (= (expr-value arg1) -1))
            arg2)
           (t nil)))

        ;; Bitwise OR simplifications
        (logior
         (cond
           ;; (logior x 0) => x
           ((and (eq (expr-type arg2) 'fixnum) (zerop (expr-value arg2)))
            arg1)
           ;; (logior 0 x) => x
           ((and (eq (expr-type arg1) 'fixnum) (zerop (expr-value arg1)))
            arg2)
           (t nil)))

        ;; Bitwise XOR simplifications
        (logxor
         (cond
           ;; (logxor x 0) => x
           ((and (eq (expr-type arg2) 'fixnum) (zerop (expr-value arg2)))
            arg1)
           ;; (logxor 0 x) => x
           ((and (eq (expr-type arg1) 'fixnum) (zerop (expr-value arg1)))
            arg2)
           (t nil)))

        (t nil)))))

;;; Constant folding optimization
(defun constant-fold (expr)
  "Optimize expression by evaluating constant operations at compile time"
  (case (expr-type expr)
    (fixnum expr) ; Already a constant

    (variable expr) ; Variables can't be folded

    (quote expr) ; Quoted forms are already constant

    (lambda
     ; Fold lambda body but keep lambda structure
     (let ((params (expr-value expr))
           (body (expr-args expr)))
       (make-expr :type 'lambda
                  :value params
                  :args (mapcar #'constant-fold body))))

    (if
     ; Optimize if expressions
     (let ((condition (constant-fold (first (expr-args expr))))
           (then-expr (constant-fold (second (expr-args expr))))
           (else-expr (constant-fold (third (expr-args expr)))))
       (if (and (eq (expr-type condition) 'fixnum))
           ; Constant condition - evaluate at compile time
           (if (zerop (expr-value condition))
               else-expr
               then-expr)
           ; Non-constant condition - keep the if
           (make-expr :type 'if :value nil
                      :args (list condition then-expr else-expr)))))

    (call
     ; Optimize arithmetic operations on constants
     (let* ((op (expr-value expr))
            (args (mapcar #'constant-fold (expr-args expr))))
       (if (and args (every (lambda (arg) (eq (expr-type arg) 'fixnum)) args))
           ; All arguments are constants - evaluate at compile time
           (let ((values (mapcar #'expr-value args)))
             (make-expr :type 'fixnum
                        :value
                        (case op
                          (+ (apply #'+ values))
                          (- (apply #'- values))
                          (* (apply #'* values))
                          (/ (if (zerop (second values))
                                 (return-from constant-fold
                                   (make-expr :type 'call :value op :args args))
                                 (truncate (first values) (second values))))
                          (mod (mod (first values) (second values)))
                          (rem (rem (first values) (second values)))
                          (< (if (< (first values) (second values)) 1 0))
                          (> (if (> (first values) (second values)) 1 0))
                          (= (if (= (first values) (second values)) 1 0))
                          (<= (if (<= (first values) (second values)) 1 0))
                          (>= (if (>= (first values) (second values)) 1 0))
                          (/= (if (/= (first values) (second values)) 1 0))
                          (logand (apply #'logand values))
                          (logior (apply #'logior values))
                          (logxor (apply #'logxor values))
                          (lognot (lognot (first values)))
                          (ash (ash (first values) (second values)))
                          (min (apply #'min values))
                          (max (apply #'max values))
                          (abs (abs (first values)))
                          (1+ (1+ (first values)))
                          (1- (1- (first values)))
                          (gcd (apply #'gcd values))
                          (lcm (apply #'lcm values))
                          (t ; Non-foldable operation
                           (return-from constant-fold
                             (make-expr :type 'call :value op :args args))))))
           ; Not all constants - apply algebraic simplifications
           (let ((simplified (simplify-algebraic op args)))
             (if simplified
                 simplified
                 (make-expr :type 'call :value op :args args))))))

    (progn
     ; Optimize progn - fold each expression
     (let ((folded-args (mapcar #'constant-fold (expr-args expr))))
       (make-expr :type 'progn :value nil :args folded-args)))

    (let
     ; Optimize let - fold body only (bindings are in raw form, not parsed)
     (let ((bindings (expr-value expr))
           (body (expr-args expr)))
       (make-expr :type 'let
                  :value bindings
                  :args (mapcar #'constant-fold body))))

    (named-let
     ; Optimize named-let - fold body only
     (let ((name-and-bindings (expr-value expr))
           (body (expr-args expr)))
       (make-expr :type 'named-let
                  :value name-and-bindings
                  :args (mapcar #'constant-fold body))))

    (t
     ; Unknown type - don't optimize
     expr)))

;;; Helper function to detect recursive calls in named-let
(defun find-recursive-calls (name expr)
  "Check if expr contains any calls to name"
  (cond
    ((null expr) nil)
    ((atom expr) nil)
    ((and (eq (expr-type expr) 'call)
          (eq (expr-value expr) name))
     t)
    ((expr-args expr)
     (some (lambda (arg) (find-recursive-calls name arg))
           (expr-args expr)))
    (t nil)))

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

    (string
     ;; Create string on heap at compile time and load pointer into RAX
     ;; mov rax, imm64
     (let* ((lisp-string (expr-value expr))
            (make-string-fn (find-symbol "RUNTIME-MAKE-STRING" :habu-runtime))
            (string-ptr (funcall make-string-fn lisp-string)))
       (append '(#x48 #xB8)               ; REX.W + mov rax prefix
               (int-to-bytes string-ptr 8))))

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

    (setq
     ;; Compile (setq var value) - mutate a lexical variable
     (let* ((var-name (expr-value expr))
            (value-expr (first (expr-args expr)))
            (binding (assoc var-name env)))
       (if binding
           (let ((offset (cdr binding)))
             (append
              ;; First, evaluate the value expression into RAX
              (emit-x86_64 value-expr env)
              ;; Then store RAX to the variable's stack location
              (if (zerop offset)
                  (list #x48 #x89 #x04 #x24)  ; mov [rsp], rax
                  (append (list #x48 #x89 #x84 #x24)  ; mov [rsp + disp32], rax
                          (int-to-bytes offset 4)))))
           (error "Cannot setq unbound variable: ~S" var-name))))

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

    (closure
     ;; Create a heap-allocated closure object as a first-class value
     ;; Phase 1: Use runtime make-closure-N trampolines with eval'd wrapper
     (let* ((params (expr-value expr))
            (body (first (expr-args expr)))
            (free-vars (second (expr-args expr)))
            (original-body (third (expr-args expr)))  ; Original Lisp form
            (num-free (length free-vars))
            (arity (length params))
            (closure-name (gensym "CLOSURE"))
            (wrapper-params (append free-vars params)))

       ;; Phase 1 limitation: only support 0-3 captured variables
       (when (> num-free 3)
         (error "Phase 1 only supports closures with up to 3 captured variables, got ~D" num-free))

       ;; Create wrapper function via eval: (lambda (free-vars... params...) body)
       ;; This will be called with captured vars as first args, then regular args
       (let ((callable-name (intern (format nil "HABU-CLOSURE-~A" (string-upcase (string closure-name)))
                                    (find-package :habu-compiler))))
         ;; Create the Lisp function that implements the closure body
         (eval `(defun ,callable-name ,wrapper-params ,original-body))

         ;; Create alien-callable wrapper (SBCL-specific for Phase 1)
         #+sbcl
         (let ((num-wrapper-params (length wrapper-params)))
           (cond
             ((<= num-wrapper-params 0)
              (eval `(sb-alien:define-alien-callable ,callable-name
                         sb-alien:unsigned-long ()
                       (,callable-name))))
             ((<= num-wrapper-params 1)
              (eval `(sb-alien:define-alien-callable ,callable-name
                         sb-alien:unsigned-long ((,(first wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params)))))
             ((<= num-wrapper-params 2)
              (eval `(sb-alien:define-alien-callable ,callable-name
                         sb-alien:unsigned-long ((,(first wrapper-params) sb-alien:unsigned-long)
                                                 (,(second wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params) ,(second wrapper-params)))))
             ((<= num-wrapper-params 3)
              (eval `(sb-alien:define-alien-callable ,callable-name
                         sb-alien:unsigned-long ((,(first wrapper-params) sb-alien:unsigned-long)
                                                 (,(second wrapper-params) sb-alien:unsigned-long)
                                                 (,(third wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params) ,(second wrapper-params) ,(third wrapper-params)))))
             ((<= num-wrapper-params 4)
              (eval `(sb-alien:define-alien-callable ,callable-name
                         sb-alien:unsigned-long ((,(first wrapper-params) sb-alien:unsigned-long)
                                                 (,(second wrapper-params) sb-alien:unsigned-long)
                                                 (,(third wrapper-params) sb-alien:unsigned-long)
                                                 (,(fourth wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params) ,(second wrapper-params) ,(third wrapper-params) ,(fourth wrapper-params)))))
             (t (error "Wrapper function with ~D parameters not supported" num-wrapper-params))))

         ;; Store wrapper function pointer in runtime symbol table
         (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
               (set-fn-fn (find-symbol "SET-SYMBOL-FUNCTION" :habu-runtime)))
           (when (and intern-fn set-fn-fn)
             (let* ((name-str (string closure-name))
                    (sym (funcall intern-fn name-str))
                    (fn-ptr (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:alien-callable-function callable-name)))))
               (funcall set-fn-fn sym fn-ptr))))

         ;; Now generate code to call make-closure-N trampoline
         (let ((trampoline-addr
                (case num-free
                  (0 *runtime-make-closure-0-addr*)
                  (1 *runtime-make-closure-1-addr*)
                  (2 *runtime-make-closure-2-addr*)
                  (3 *runtime-make-closure-3-addr*)
                  (t (error "Unsupported number of captured vars: ~D" num-free)))))
           (unless trampoline-addr
             (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
           (let ((func-addr (sb-sys:sap-int trampoline-addr))
                 (intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime)))
             (unless intern-fn
               (error "Runtime not initialized"))

             ;; Get symbol address for wrapper function
             (let ((sym-addr (funcall intern-fn (string closure-name))))
               (append
                ;; Setup arguments for make-closure-N call
                ;; Arg1 (RDI): code pointer from symbol-function slot
                (list #x48 #xB8)                    ; movabs rax, imm64 (symbol addr)
                (int-to-bytes sym-addr 8)
                (list #x48 #x8B #x78 #x18)          ; mov rdi, [rax + 24] (load function ptr to RDI)

                ;; Arg2 (RSI): arity (number of declared parameters, not including captured vars)
                (list #x48 #xBE)                    ; movabs rsi, imm64 (arity)
                (int-to-bytes arity 8)

                ;; Args 3-5 (RDX, RCX, R8): captured variable values
                (case num-free
                  (0
                   ;; No captured vars, just call make-closure-0(code-ptr, arity)
                   nil)
                  (1
                   ;; Arg3 (RDX): captured var 1
                   (append
                    (emit-x86_64 (make-expr :type 'variable :value (first free-vars)) env)
                    (list #x48 #x89 #xC2)))         ; mov rdx, rax
                  (2
                   ;; Arg3 (RDX): captured var 1, Arg4 (RCX): captured var 2
                   (append
                    (emit-x86_64 (make-expr :type 'variable :value (first free-vars)) env)
                    (list #x50)                     ; push rax (save var1)
                    (emit-x86_64 (make-expr :type 'variable :value (second free-vars)) env)
                    (list #x48 #x89 #xC1)           ; mov rcx, rax (var2 -> RCX)
                    (list #x5A)))                   ; pop rdx (var1 -> RDX)
                  (3
                   ;; Arg3 (RDX): captured var 1, Arg4 (RCX): captured var 2, Arg5 (R8): captured var 3
                   (append
                    (emit-x86_64 (make-expr :type 'variable :value (first free-vars)) env)
                    (list #x50)                     ; push rax (save var1)
                    (emit-x86_64 (make-expr :type 'variable :value (second free-vars)) env)
                    (list #x50)                     ; push rax (save var2)
                    (emit-x86_64 (make-expr :type 'variable :value (third free-vars)) env)
                    (list #x49 #x89 #xC0)           ; mov r8, rax (var3 -> R8)
                    (list #x59)                     ; pop rcx (var2 -> RCX)
                    (list #x5A)))                   ; pop rdx (var1 -> RDX)
                  (t (error "Unsupported num-free: ~D" num-free)))

                ;; Call make-closure-N trampoline
                (list #x48 #xB8)                    ; movabs rax, imm64
                (int-to-bytes func-addr 8)
                (list #xFF #xD0))))))))

    (named-let
     ;; Compile (let name ((var val) ...) body)
     ;; For now, compile as regular let - recursive calls will need TCO support
     (let* ((name-and-bindings (expr-value expr))
            (loop-name (first name-and-bindings))
            (bindings (second name-and-bindings))
            (body (first (expr-args expr)))
            (vars (mapcar #'first bindings))
            (num-bindings (length bindings)))
       ;; Warn about recursive calls (they won't work without TCO)
       (when (find-recursive-calls loop-name body)
         (warn "Named-let '~A' contains recursive calls. Tail-call optimization not yet implemented. Recursive calls will cause errors." loop-name))
       ;; Compile as regular let for now
       (let ((binding-code nil)
             (new-env env))
         ;; Evaluate and push each binding
         (dolist (binding bindings)
           (let ((var (first binding))
                 (val (second binding)))
             (setf binding-code
                   (append binding-code
                           (emit-x86_64 (parse val) env)
                           (list #x50)))))  ; push rax
         ;; Build environment with variable offsets
         (let ((offset 0))
           (dolist (var (reverse vars))
             (setf new-env (cons (cons var offset) new-env))
             (setf offset (+ offset 8))))
         ;; Compile body with new environment
         (let ((body-code (emit-x86_64 body new-env)))
           (append binding-code
                   body-code
                   ;; Clean up stack
                   (if (<= num-bindings 255)
                       (list #x48 #x83 #xC4 (* num-bindings 8))  ; add rsp, imm8
                       (append (list #x48 #x81 #xC4)  ; add rsp, imm32
                               (int-to-bytes (* num-bindings 8) 4))))))))

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

    (when
     ;; Compile (when test body...)
     ;; Transform to (if test (progn body...) 0)
     (let* ((test-expr (first (expr-args expr)))
            (body-exprs (rest (expr-args expr)))
            (test-code (emit-x86_64 test-expr env))
            (body-code (if (null body-exprs)
                           (emit-x86_64 (make-expr :type 'fixnum :value 0) env)
                           (if (= (length body-exprs) 1)
                               (emit-x86_64 (first body-exprs) env)
                               (emit-x86_64 (make-expr :type 'progn :args body-exprs) env))))
            (body-size (length body-code)))
       (append test-code
               (list #x48 #x85 #xC0)           ; test rax, rax
               (list #x0F #x84)                ; jz (skip body if test is false)
               (int-to-bytes body-size 4)
               body-code)))

    (unless
     ;; Compile (unless test body...)
     ;; Transform to (if (not test) (progn body...) 0)
     (let* ((test-expr (first (expr-args expr)))
            (body-exprs (rest (expr-args expr)))
            (test-code (emit-x86_64 test-expr env))
            (body-code (if (null body-exprs)
                           (emit-x86_64 (make-expr :type 'fixnum :value 0) env)
                           (if (= (length body-exprs) 1)
                               (emit-x86_64 (first body-exprs) env)
                               (emit-x86_64 (make-expr :type 'progn :args body-exprs) env))))
            (body-size (length body-code)))
       (append test-code
               (list #x48 #x85 #xC0)           ; test rax, rax
               (list #x0F #x85)                ; jnz (skip body if test is true)
               (int-to-bytes body-size 4)
               body-code)))

    (case
     ;; Compile (case keyform (key1 result1) (key2 result2) (t default))
     ;; Transform to (let ((#:key keyform)) (cond ((eql #:key key1) result1) ...))
     (let* ((keyform (first (expr-args expr)))
            (clauses (expr-value expr))
            (key-var (gensym "CASE-KEY")))
       ;; Evaluate keyform once and bind to temporary variable
       (let ((key-code (emit-x86_64 keyform env))
             (new-env (cons (cons key-var 0) env)))
         (labels ((compile-case-clauses (clauses)
                    (if (null clauses)
                        (emit-x86_64 (make-expr :type 'fixnum :value 0) new-env)
                        (let* ((clause (first clauses))
                               (keys (first clause))
                               (result (second clause))
                               (rest-clauses (rest clauses)))
                          (if (or (eq keys t) (null rest-clauses))
                              ;; Default clause or last clause
                              (emit-x86_64 (parse result) new-env)
                              ;; Test clause: (eql key-var key)
                              (let* ((key-value (if (listp keys) (first keys) keys))
                                     (test-code (append
                                                 ;; Load key-var from stack
                                                 (list #x48 #x8B #x04 #x24)  ; mov rax, [rsp]
                                                 ;; Compare with key
                                                 (list #x48 #xBD)            ; mov rbp, imm64
                                                 (int-to-bytes (* key-value 16) 8)
                                                 (list #x48 #x39 #xE8)       ; cmp rax, rbp
                                                 (list #x0F #x94 #xC0)       ; sete al
                                                 (list #x48 #x0F #xB6 #xC0)  ; movzx rax, al
                                                 (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4
                                     (then-code (emit-x86_64 (parse result) new-env))
                                     (else-code (compile-case-clauses rest-clauses))
                                     (then-size (length then-code))
                                     (else-size (length else-code)))
                                (append test-code
                                        (list #x48 #x85 #xC0)  ; test rax, rax
                                        (list #x0F #x84)       ; jz to else
                                        (int-to-bytes (+ then-size 5) 4)
                                        then-code
                                        (list #xE9)  ; jmp to end
                                        (int-to-bytes else-size 4)
                                        else-code)))))))
           (append key-code
                   (list #x50)  ; push rax (save key)
                   (compile-case-clauses clauses)
                   (list #x48 #x83 #xC4 #x08)))))) ; pop stack

    (funcall
     ;; Compile function calls: ((lambda ...) args) or (closure-value args)
     (let* ((fn-expr (expr-value expr))
            (arg-exprs (expr-args expr)))
       (if (or (eq (expr-type fn-expr) 'lambda)
               (eq (expr-type fn-expr) 'closure))
           ;; Inline lambda/closure call
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
                                      '(#x50)))  ; push rax
                        ;; Add parameter to environment
                        (push (cons param (* offset 8)) new-env)))
             ;; Compile body with parameters bound
             (let ((body-code (emit-x86_64 body (reverse new-env))))
               (append binding-code
                       body-code
                       ;; Clean up stack
                       (if (<= (* num-params 8) 127)
                           '(#x48 #x83 #xC4)
                           (append '(#x48 #x81 #xC4)
                                   (int-to-bytes (* num-params 8) 4)))
                       (int-to-bytes (* num-params 8) (if (<= (* num-params 8) 127) 1 4)))))
           ;; Calling a closure value (not inline)
           ;; Evaluate fn-expr to get closure pointer, then call it
           (let ((num-args (length arg-exprs)))
             (append
              ;; Evaluate closure expression into RAX
              (emit-x86_64 fn-expr env)
              ;; Save closure pointer on stack
              '(#x50)                                  ; push rax

              ;; Verify it's a closure (tag 0x7)
              '(#x48 #x89 #xC1)                        ; mov rcx, rax
              '(#x48 #x83 #xE1 #x0F)                   ; and rcx, 0xF
              '(#x48 #x83 #xF9 #x07)                   ; cmp rcx, 7
              ;; TODO: Add error handling for non-closure

              ;; Get closure pointer back from stack (don't pop yet)
              '(#x48 #x8B #x04 #x24)                   ; mov rax, [rsp]

              ;; Extract arity from [rax + 16] and verify
              '(#x48 #x8B #x48 #x10)                   ; mov rcx, [rax + 16]
              '(#x48 #xC1 #xE9 #x04)                   ; shr rcx, 4 (untag)
              ;; Compare with num-args
              '(#x48 #x83 #xF9)                        ; cmp rcx, imm8
              (list num-args)
              ;; TODO: Add error handling for arity mismatch

              ;; Extract env-size from [rax + 24]
              '(#x48 #x8B #x50 #x18)                   ; mov rdx, [rax + 24]
              '(#x48 #xC1 #xEA #x04)                   ; shr rdx, 4 (untag env-size)

              ;; Push captured variables onto stack
              ;; For i from env-size-1 down to 0: push [rax + 32 + i*8]
              '(#x48 #x85 #xD2)                        ; test rdx, rdx
              '(#x74 #x0E)                             ; jz skip-push-env (14 bytes ahead)
              ;; rdx = env-size, rax = closure ptr (from [rsp])
              '(#x4C #x8D #x04 #xD5 #x00 #x00 #x00 #x00) ; lea r8, [rdx*8 + 0] (total bytes)
              '(#x49 #x83 #xE8 #x08)                   ; sub r8, 8 (start from last)
              ;; Loop: push [rax + 32 + r8]
              ;; .loop:
              '(#x4A #xFF #x74 #x00 #x20)              ; push qword [rax + r8 + 32]
              '(#x49 #x83 #xE8 #x08)                   ; sub r8, 8
              '(#x4D #x83 #xF8 #xFF)                   ; cmp r8, -1
              '(#x75 #xF3)                             ; jne .loop (back 13 bytes)
              ;; skip-push-env:

              ;; Now evaluate and push arguments (in order)
              (apply #'append
                     (loop for arg-expr in arg-exprs
                           collect (append
                                    (emit-x86_64 arg-expr env)
                                    '(#x50))))        ; push rax

              ;; Get closure pointer from bottom of our pushed values
              ;; Stack: [closure-ptr][env-vars...][args...]
              ;; Calculate offset: (env-size + num-args) * 8
              '(#x48 #x8B #x04 #x24)                   ; mov rax, [rsp] (just to get it, will recalc)
              ;; Actually, closure ptr is at rsp + (env-size + num-args) * 8
              ;; Let's use a different approach: save env-size and use it
              '(#x52)                                  ; push rdx (save env-size)

              ;; Recalculate stack offset to closure ptr
              ;; closure is at: rsp + 8 + (env-size + num-args) * 8
              '(#x48 #x8D #x04 #xD5)                   ; lea rax, [rdx*8 + ...
              (int-to-bytes (+ 8 (* num-args 8)) 4)    ; ... + 8 + num-args*8]
              '(#x48 #x03 #x04 #x24)                   ; add rax, [rsp] (add saved rdx*8)
              '(#x48 #x8B #x04 #x04)                   ; mov rax, [rsp + rax]

              ;; Extract code pointer from [rax + 8]
              '(#x48 #x8B #x48 #x08)                   ; mov rcx, [rax + 8]

              ;; Pop saved env-size
              '(#x5A)                                  ; pop rdx

              ;; Calculate total stack cleanup: (env-size + num-args + 1) * 8
              '(#x48 #x8D #x44 #x15)                   ; lea rax, [rdx + rdx*1 + ...
              (int-to-bytes (+ num-args 1) 1)          ; ... + num-args + 1]
              '(#x48 #xC1 #xE0 #x03)                   ; shl rax, 3 (multiply by 8)
              '(#x50)                                  ; push rax (save cleanup amount)

              ;; Call the code pointer
              '(#xFF #xD1)                             ; call rcx

              ;; Clean up stack: pop cleanup amount, then adjust rsp
              '(#x59)                                  ; pop rcx (cleanup amount)
              '(#x48 #x01 #xCC)                        ; add rsp, rcx
              )))))

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

         ((eq op 'rem)
          ;; Compile (rem a b) - remainder operation (same as mod for positive numbers)
          ;; For x86_64, idiv gives remainder in rdx (same as mod implementation)
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

         ((eq op '/=)
          ;; Compile (/= a b) - not equal
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x95 #xC0)         ; setne al (set if not equal)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'eql)
          ;; Compile (eql a b) - object identity
          ;; For fixnums, same as =
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x94 #xC0)         ; sete al (set if equal)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'eq)
          ;; Compile (eq a b) - pointer equality
          ;; For fixnums, same as =
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x94 #xC0)         ; sete al (set if equal)
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

         ((eq op 'logand)
          ;; Compile (logand a b) - bitwise AND
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x21 #xD8)         ; and rax, rbx
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'logior)
          ;; Compile (logior a b) - bitwise OR
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x09 #xD8)         ; or rax, rbx
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'logxor)
          ;; Compile (logxor a b) - bitwise XOR
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x31 #xD8)         ; xor rax, rbx
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'lognot)
          ;; Compile (lognot a) - bitwise NOT
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xF7 #xD0)))       ; not rax

         ((eq op 'ash)
          ;; Compile (ash a b) - arithmetic shift
          ;; Positive b: left shift, negative b: right shift
          (append (emit-x86_64 (second args) env)  ; shift count in rax
                  (list #x50)                     ; push rax
                  (emit-x86_64 (first args) env)   ; value in rax
                  (list #x48 #x8B #x0C #x24)      ; mov rcx, [rsp] (shift count)
                  (list #x48 #xC1 #xF9 #x04)      ; sar rcx, 4 (untag)
                  (list #x48 #x85 #xC9)           ; test rcx, rcx
                  (list #x78)                     ; js (jump if negative)
                  (list 6)                        ; offset to right shift
                  ;; Left shift
                  (list #x48 #xD3 #xE0)           ; shl rax, cl
                  (list #xEB)                     ; jmp
                  (list 2)                        ; offset to end
                  ;; Right shift
                  (list #x48 #xD3 #xF8)           ; sar rax, cl
                  (list #x48 #x83 #xC4 #x08)))   ; add rsp, 8

         ;; Numeric operators
         ((eq op 'min)
          ;; Compile (min a b) - return smaller value
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x4C #xC3)         ; cmovl rax, rbx (move if less)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'max)
          ;; Compile (max a b) - return larger value
          (append (emit-x86_64 (first args) env)
                  (list #x50)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x39 #xC3)         ; cmp rbx, rax
                  (list #x0F #x4F #xC3)         ; cmovg rax, rbx (move if greater)
                  (list #x48 #x83 #xC4 #x08)))

         ((eq op 'abs)
          ;; Compile (abs a) - absolute value
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax
                  (list #x48 #xC1 #xFB #x3F)    ; sar rbx, 63 (sign bit)
                  (list #x48 #x31 #xD8)         ; xor rax, rbx
                  (list #x48 #x29 #xD8)))       ; sub rax, rbx

         ((eq op '1+)
          ;; Compile (1+ a) - increment by 1
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x83 #xC0 #x10))) ; add rax, 16 (1 << 4)

         ((eq op '1-)
          ;; Compile (1- a) - decrement by 1
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x83 #xE8 #x10))) ; sub rax, 16 (1 << 4)

         ;; Predicates
         ((eq op 'zerop)
          ;; Compile (zerop a) - test if zero
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x0F #x94 #xC0)         ; setz al
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4

         ((eq op 'plusp)
          ;; Compile (plusp a) - test if positive
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x0F #x9F #xC0)         ; setg al
                  (list #x48 #x0F #xB6 #xC0)
                  (list #x48 #xC1 #xE0 #x04)))

         ((eq op 'minusp)
          ;; Compile (minusp a) - test if negative
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x0F #x9C #xC0)         ; setl al
                  (list #x48 #x0F #xB6 #xC0)
                  (list #x48 #xC1 #xE0 #x04)))

         ((eq op 'evenp)
          ;; Compile (evenp a) - test if even
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #x83 #xE0 #x01)    ; and rax, 1 (get low bit)
                  (list #x48 #x83 #xF0 #x01)    ; xor rax, 1 (invert)
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag)

         ((eq op 'oddp)
          ;; Compile (oddp a) - test if odd
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #x83 #xE0 #x01)    ; and rax, 1 (get low bit)
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag)

         ;; Type predicates (for fixnum-only system)
         ((eq op 'numberp)
          ;; numberp always returns true for fixnums
          (append (emit-x86_64 (first args) env)  ; Evaluate arg (for side effects)
                  (list #x48 #xC7 #xC0 #x10 #x00 #x00 #x00))) ; mov rax, 16 (tagged 1)

         ((eq op 'integerp)
          ;; integerp always returns true for fixnums
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC7 #xC0 #x10 #x00 #x00 #x00))) ; mov rax, 16 (tagged 1)

         ((eq op 'atom)
          ;; atom always returns true for fixnums (not conses)
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC7 #xC0 #x10 #x00 #x00 #x00))) ; mov rax, 16 (tagged 1)

         ((eq op 'listp)
          ;; listp always returns false for fixnums
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x31 #xC0))) ; xor rax, rax (tagged 0)

         ((eq op 'consp)
          ;; consp always returns false for fixnums
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x31 #xC0))) ; xor rax, rax (tagged 0)

         ((eq op 'symbolp)
          ;; symbolp always returns false for fixnums
          (append (emit-x86_64 (first args) env)
                  (list #x48 #x31 #xC0))) ; xor rax, rax (tagged 0)

         ((eq op 'signum)
          ;; Compile (signum a) - return -1, 0, or 1 based on sign
          ;; Algorithm: (if (< a 0) -1 (if (> a 0) 1 0))
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  ;; Check if zero
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x74 #x0E)              ; jz +14 (zero case)
                  ;; Not zero: check sign
                  (list #x48 #x31 #xDB)         ; xor rbx, rbx
                  (list #x48 #x0F #x9E #xC3)    ; setle bl (1 if rax <= 0)
                  (list #x48 #xD1 #xE3)         ; shl rbx, 1 (multiply by 2)
                  (list #x48 #xFF #xCB)         ; dec rbx (2 -> 1, 0 -> -1)
                  (list #x48 #x89 #xD8)         ; mov rax, rbx
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #xEB #x05)              ; jmp +5 (skip zero case)
                  ;; Zero case:
                  (list #x48 #x31 #xC0)         ; xor rax, rax (rax = 0)
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag to 0)

         ((eq op 'logcount)
          ;; Compile (logcount a) - count number of set bits (population count)
          ;; Uses Brian Kernighan's algorithm: repeatedly clear lowest set bit
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #x31 #xDB)         ; xor rbx, rbx (counter = 0)
                  ;; Loop: while (rax != 0)
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x74 #x0D)              ; jz +13 (exit loop)
                  (list #x48 #xFF #xC3)         ; inc rbx (counter++)
                  (list #x48 #x89 #xC1)         ; mov rcx, rax
                  (list #x48 #xFF #xC9)         ; dec rcx
                  (list #x48 #x21 #xC8)         ; and rax, rcx (clear lowest set bit)
                  (list #xEB #xF1)              ; jmp -15 (back to test)
                  ;; Exit: rbx has count
                  (list #x48 #x89 #xD8)         ; mov rax, rbx
                  (list #x48 #xC1 #xE0 #x04))) ; shl rax, 4 (retag)

         ((eq op 'logtest)
          ;; Compile (logtest a b) - test if any bits are set in both args
          ;; Returns 1 if (logand a b) != 0, else 0
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag first)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag second)
                  (list #x48 #x21 #xD8)         ; and rax, rbx
                  (list #x48 #x0F #x95 #xC0)    ; setnz al (1 if result != 0)
                  (list #x48 #x0F #xB6 #xC0)    ; movzx rax, al
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'logbitp)
          ;; Compile (logbitp position integer) - test if bit at position is set
          ;; Returns 1 if bit is set, 0 otherwise
          (append (emit-x86_64 (first args) env)   ; position
                  (list #x50)                        ; push rax
                  (emit-x86_64 (second args) env)    ; integer
                  (list #x48 #x8B #x0C #x24)         ; mov rcx, [rsp] (position)
                  (list #x48 #xC1 #xF9 #x04)         ; sar rcx, 4 (untag position)
                  (list #x48 #xC1 #xF8 #x04)         ; sar rax, 4 (untag integer)
                  (list #x48 #xD3 #xF8)              ; sar rax, cl (shift right by position)
                  (list #x48 #x83 #xE0 #x01)         ; and rax, 1 (get bit)
                  (list #x48 #xC1 #xE0 #x04)         ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08)))       ; add rsp, 8

         ((eq op 'lognand)
          ;; Compile (lognand a b) - bitwise NAND: ~(a & b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x21 #xD8)         ; and rax, rbx
                  (list #x48 #xF7 #xD0)         ; not rax
                  (list #x48 #x83 #xE0 #xF0)    ; and rax, ~0xF (keep only data bits, preserve tag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'lognor)
          ;; Compile (lognor a b) - bitwise NOR: ~(a | b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x09 #xD8)         ; or rax, rbx
                  (list #x48 #xF7 #xD0)         ; not rax
                  (list #x48 #x83 #xE0 #xF0)    ; and rax, ~0xF (keep only data bits)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'logeqv)
          ;; Compile (logeqv a b) - bitwise equivalence: ~(a ^ b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x8B #x1C #x24)    ; mov rbx, [rsp]
                  (list #x48 #x31 #xD8)         ; xor rax, rbx
                  (list #x48 #xF7 #xD0)         ; not rax
                  (list #x48 #x83 #xE0 #xF0)    ; and rax, ~0xF (keep only data bits)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'gcd)
          ;; Compile (gcd a b) - greatest common divisor using Euclidean algorithm
          ;; Algorithm: gcd(a,0) = |a|, gcd(a,b) = gcd(b, a mod b)
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (second arg)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (first arg)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag)
                  ;; Get absolute value of rax (a = abs(a))
                  (list #x48 #x89 #xC1)         ; mov rcx, rax
                  (list #x48 #xC1 #xF9 #x3F)    ; sar rcx, 63 (sign bit)
                  (list #x48 #x31 #xC8)         ; xor rax, rcx
                  (list #x48 #x29 #xC8)         ; sub rax, rcx
                  ;; Get absolute value of rbx (b = abs(b))
                  (list #x48 #x89 #xD9)         ; mov rcx, rbx
                  (list #x48 #xC1 #xF9 #x3F)    ; sar rcx, 63
                  (list #x48 #x31 #xD9)         ; xor rbx, rcx
                  (list #x48 #x29 #xD9)         ; sub rbx, rcx
                  ;; GCD loop: while (b != 0) { temp = a % b; a = b; b = temp; }
                  (list #x48 #x85 #xDB)         ; test rbx, rbx
                  (list #x74 #x0D)              ; jz +13 (done, skip to retag)
                  (list #x48 #x99)              ; cqo (sign extend rax to rdx:rax)
                  (list #x48 #xF7 #xFB)         ; idiv rbx (rdx = remainder)
                  (list #x48 #x89 #xD8)         ; mov rax, rbx (a = b)
                  (list #x48 #x89 #xD3)         ; mov rbx, rdx (b = remainder)
                  (list #xEB #xEF)              ; jmp -17 (back to test)
                  ;; Done: rax contains GCD
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'lcm)
          ;; Compile (lcm a b) - least common multiple
          ;; Formula: lcm(a,b) = |a*b| / gcd(a,b), with lcm(a,0) = 0
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax (first arg)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (second arg)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (first arg)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag first)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag second)
                  ;; Check for zero: if either is 0, return 0
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x74 #x52)              ; jz +82 (return 0)
                  (list #x48 #x85 #xDB)         ; test rbx, rbx
                  (list #x74 #x4E)              ; jz +78 (return 0)
                  ;; Get absolute value of rax
                  (list #x48 #x89 #xC1)         ; mov rcx, rax
                  (list #x48 #xC1 #xF9 #x3F)    ; sar rcx, 63
                  (list #x48 #x31 #xC8)         ; xor rax, rcx
                  (list #x48 #x29 #xC8)         ; sub rax, rcx
                  (list #x50)                   ; push rax (save |a|)
                  ;; Get absolute value of rbx
                  (list #x48 #x89 #xD9)         ; mov rcx, rbx
                  (list #x48 #xC1 #xF9 #x3F)    ; sar rcx, 63
                  (list #x48 #x31 #xD9)         ; xor rbx, rcx
                  (list #x48 #x29 #xD9)         ; sub rbx, rcx
                  (list #x53)                   ; push rbx (save |b|)
                  ;; Compute product |a| * |b|
                  (list #x48 #x0F #xAF #xC3)    ; imul rax, rbx
                  (list #x50)                   ; push rax (save product)
                  ;; Compute GCD of |a| and |b|
                  (list #x48 #x8B #x44 #x24 #x10) ; mov rax, [rsp+16] (|a|)
                  (list #x48 #x8B #x5C #x24 #x08) ; mov rbx, [rsp+8] (|b|)
                  ;; GCD loop
                  (list #x48 #x85 #xDB)         ; test rbx, rbx
                  (list #x74 #x0D)              ; jz +13
                  (list #x48 #x99)              ; cqo
                  (list #x48 #xF7 #xFB)         ; idiv rbx
                  (list #x48 #x89 #xD8)         ; mov rax, rbx
                  (list #x48 #x89 #xD3)         ; mov rbx, rdx
                  (list #xEB #xEF)              ; jmp -17
                  ;; rax = gcd, compute product/gcd
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (gcd)
                  (list #x48 #x58)              ; pop rax (product)
                  (list #x48 #x99)              ; cqo
                  (list #x48 #xF7 #xFB)         ; idiv rbx (product / gcd)
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x18)    ; add rsp, 24 (clean all pushes)
                  (list #xEB #x05)              ; jmp +5 (skip zero case)
                  ;; Zero case
                  (list #x48 #x31 #xC0)         ; xor rax, rax
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'isqrt)
          ;; Compile (isqrt n) - integer square root using Newton's method
          ;; Algorithm: x_new = (x + n/x) / 2, iterate until convergence
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  ;; Handle special cases
                  (list #x48 #x83 #xF8 #x01)    ; cmp rax, 1
                  (list #x76 #x1E)              ; jbe +30 (return rax if <= 1)
                  ;; Initialize: x = n/2
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (save n in rbx)
                  (list #x48 #xD1 #xE8)         ; shr rax, 1 (x = n/2)
                  ;; Newton loop: while (true)
                  (list #x48 #x89 #xC1)         ; mov rcx, rax (save old x)
                  (list #x48 #x89 #xD8)         ; mov rax, rbx (n)
                  (list #x48 #x99)              ; cqo
                  (list #x48 #xF7 #xF9)         ; idiv rcx (n/x)
                  (list #x48 #x01 #xC8)         ; add rax, rcx (n/x + x)
                  (list #x48 #xD1 #xE8)         ; shr rax, 1 ((n/x + x)/2)
                  (list #x48 #x39 #xC1)         ; cmp rcx, rax
                  (list #x7F #x02)              ; jg +2 (if old > new, continue)
                  (list #xEB #x05)              ; jmp +5 (converged, use old value)
                  (list #xEB #xE9)              ; jmp -23 (back to loop start)
                  (list #x48 #x89 #xC8)         ; mov rax, rcx (use old x)
                  ;; Retag and return
                  (list #x48 #xC1 #xE0 #x04)))  ; shl rax, 4 (retag)

         ((eq op 'integer-length)
          ;; Compile (integer-length n) - number of bits needed to represent n
          ;; For n >= 0: position of highest 1 bit + 1
          ;; For n < 0: integer-length(NOT n)
          (append (emit-x86_64 (first args) env)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag)
                  ;; Check if negative
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x79 #x06)              ; jns +6 (skip to positive case)
                  ;; Negative: compute NOT n = -n - 1
                  (list #x48 #xF7 #xD8)         ; neg rax
                  (list #x48 #xFF #xC8)         ; dec rax
                  (list #xEB #x00)              ; jmp +0 (nop, continue)
                  ;; Positive or converted: use BSR to find highest bit
                  (list #x48 #x85 #xC0)         ; test rax, rax
                  (list #x74 #x0A)              ; jz +10 (zero case, return 0)
                  (list #x48 #x0F #xBD #xC8)    ; bsr rcx, rax (find highest set bit)
                  (list #x48 #xFF #xC1)         ; inc rcx (position + 1)
                  (list #x48 #x89 #xC8)         ; mov rax, rcx
                  (list #xEB #x02)              ; jmp +2 (skip zero case)
                  (list #x48 #x31 #xC0)         ; xor rax, rax (return 0)
                  (list #x48 #xC1 #xE0 #x04)))  ; shl rax, 4 (retag)

         ((eq op 'expt)
          ;; Compile (expt base exponent) - integer exponentiation
          ;; Algorithm: repeated multiplication, result = base^exponent
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax (base)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC1)         ; mov rcx, rax (exponent)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (base)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag base)
                  (list #x48 #xC1 #xF9 #x04)    ; sar rcx, 4 (untag exponent)
                  ;; Handle special cases
                  (list #x48 #x85 #xC9)         ; test rcx, rcx
                  (list #x7C #x25)              ; jl +37 (negative exponent = 0)
                  (list #x74 #x1C)              ; jz +28 (exponent = 0, return 1)
                  (list #x48 #x83 #xF9 #x01)    ; cmp rcx, 1
                  (list #x74 #x1A)              ; jz +26 (exponent = 1, return base)
                  ;; Initialize result = 1, save base in rbx
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (save base)
                  (list #x48 #xC7 #xC0 #x01 #x00 #x00 #x00) ; mov rax, 1
                  ;; Loop: while (rcx > 0)
                  (list #x48 #x85 #xC9)         ; test rcx, rcx
                  (list #x74 #x09)              ; jz +9 (done)
                  (list #x48 #x0F #xAF #xC3)    ; imul rax, rbx (result *= base)
                  (list #x48 #xFF #xC9)         ; dec rcx
                  (list #xEB #xF3)              ; jmp -13 (loop back)
                  ;; Done: rax has result
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08)    ; add rsp, 8
                  (list #xEB #x0C)              ; jmp +12 (skip special cases)
                  ;; Exponent = 1: return base
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4
                  (list #x48 #x83 #xC4 #x08)    ; add rsp, 8
                  (list #xEB #x05)              ; jmp +5
                  ;; Exponent = 0: return 1
                  (list #x48 #xC7 #xC0 #x10 #x00 #x00 #x00) ; mov rax, 16 (tagged 1)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ;; Rounding functions
         ;; Since we're working with fixnums (already integers), these are identity operations
         ((eq op 'floor)
          ;; floor(n) for integer n returns n
          (emit-x86_64 (first args) env))

         ((eq op 'ceiling)
          ;; ceiling(n) for integer n returns n
          (emit-x86_64 (first args) env))

         ((eq op 'truncate)
          ;; truncate(n) for integer n returns n
          (emit-x86_64 (first args) env))

         ((eq op 'round)
          ;; round(n) for integer n returns n
          (emit-x86_64 (first args) env))

         ;; Two-argument rounding division operators
         ((eq op 'ffloor)
          ;; ffloor(a, b) = floor(a/b) - rounds toward negative infinity
          ;; For integers: if remainder != 0 and signs differ, subtract 1 from quotient
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax (dividend)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (divisor)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (dividend)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag dividend)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag divisor)
                  (list #x48 #x99)              ; cqo (sign extend)
                  (list #x48 #xF7 #xFB)         ; idiv rbx (rax = quotient, rdx = remainder)
                  ;; Check if we need to adjust: remainder != 0 and signs differ
                  (list #x48 #x85 #xD2)         ; test rdx, rdx (check remainder)
                  (list #x74 #x0F)              ; jz +15 (skip adjustment if rem = 0)
                  ;; Check if signs differ: (a ^ b) < 0
                  (list #x48 #x8B #x0C #x24)    ; mov rcx, [rsp] (original dividend, tagged)
                  (list #x48 #x31 #xD9)         ; xor rcx, rbx (signs differ if MSB set)
                  (list #x48 #x85 #xC9)         ; test rcx, rcx
                  (list #x79 #x03)              ; jns +3 (skip if same sign)
                  ;; Different signs and remainder: subtract 1
                  (list #x48 #xFF #xC8)         ; dec rax
                  ;; Retag and clean up
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'fceiling)
          ;; fceiling(a, b) = ceiling(a/b) - rounds toward positive infinity
          ;; For integers: if remainder != 0 and signs are same, add 1 to quotient
          (append (emit-x86_64 (first args) env)
                  (list #x50)                   ; push rax (dividend)
                  (emit-x86_64 (second args) env)
                  (list #x48 #x89 #xC3)         ; mov rbx, rax (divisor)
                  (list #x48 #x8B #x04 #x24)    ; mov rax, [rsp] (dividend)
                  (list #x48 #xC1 #xF8 #x04)    ; sar rax, 4 (untag dividend)
                  (list #x48 #xC1 #xFB #x04)    ; sar rbx, 4 (untag divisor)
                  (list #x48 #x99)              ; cqo (sign extend)
                  (list #x48 #xF7 #xFB)         ; idiv rbx (rax = quotient, rdx = remainder)
                  ;; Check if we need to adjust: remainder != 0 and signs same
                  (list #x48 #x85 #xD2)         ; test rdx, rdx (check remainder)
                  (list #x74 #x0F)              ; jz +15 (skip adjustment if rem = 0)
                  ;; Check if signs same: (a ^ b) >= 0
                  (list #x48 #x8B #x0C #x24)    ; mov rcx, [rsp] (original dividend, tagged)
                  (list #x48 #x31 #xD9)         ; xor rcx, rbx (signs same if MSB not set)
                  (list #x48 #x85 #xC9)         ; test rcx, rcx
                  (list #x78 #x03)              ; js +3 (skip if different signs)
                  ;; Same signs and remainder: add 1
                  (list #x48 #xFF #xC0)         ; inc rax
                  ;; Retag and clean up
                  (list #x48 #xC1 #xE0 #x04)    ; shl rax, 4 (retag)
                  (list #x48 #x83 #xC4 #x08))) ; add rsp, 8

         ((eq op 'ftruncate)
          ;; ftruncate(a, b) = truncate(a/b) - rounds toward zero
          ;; For integers, this is the same as regular division
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

         ((eq op 'fround)
          ;; fround(a, b) = round(a/b) - rounds to nearest integer
          ;; For integers: if remainder > b/2, round up; if < b/2, round down
          ;; If exactly b/2, round to even (banker's rounding)
          ;; Simplified: just use truncate for integers
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

         ;; List operations - integrated with runtime heap via FFI trampolines
         ((eq op 'cons)
          ;; (cons car cdr) - allocate cons cell on heap
          ;; Call runtime-cons trampoline following System V AMD64 ABI:
          ;; Args: RDI (car), RSI (cdr), Return: RAX
          (unless *runtime-cons-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-cons-addr*)))
            (append
             ;; Evaluate car into RAX
             (emit-x86_64 (first args) env)
             ;; Save car on stack
             (list #x50)                          ; push rax
             ;; Evaluate cdr into RAX
             (emit-x86_64 (second args) env)
             ;; Setup call: RDI=car, RSI=cdr
             (list #x48 #x89 #xC6)                ; mov rsi, rax (cdr in RSI)
             (list #x48 #x8B #x3C #x24)           ; mov rdi, [rsp] (car in RDI)
             (list #x48 #x83 #xC4 #x08)           ; add rsp, 8 (pop car)
             ;; Load function address and call
             (list #x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             (list #xFF #xD0))))                  ; call rax

         ((eq op 'car)
          ;; (car cons-ptr) - read car field from cons cell
          ;; Call runtime-car trampoline: Arg: RDI (cons-ptr), Return: RAX
          (unless *runtime-car-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-car-addr*)))
            (append
             ;; Evaluate cons expression into RAX
             (emit-x86_64 (first args) env)
             ;; Setup call: RDI=cons-ptr
             (list #x48 #x89 #xC7)                ; mov rdi, rax
             ;; Load function address and call
             (list #x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             (list #xFF #xD0))))                  ; call rax

         ((eq op 'cdr)
          ;; (cdr cons-ptr) - read cdr field from cons cell
          ;; Call runtime-cdr trampoline: Arg: RDI (cons-ptr), Return: RAX
          (unless *runtime-cdr-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-cdr-addr*)))
            (append
             ;; Evaluate cons expression into RAX
             (emit-x86_64 (first args) env)
             ;; Setup call: RDI=cons-ptr
             (list #x48 #x89 #xC7)                ; mov rdi, rax
             ;; Load function address and call
             (list #x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             (list #xFF #xD0))))                  ; call rax

         ((eq op 'list)
          ;; (list a b c ...) - build list by repeated cons
          ;; Build from right to left using parsed cons operations
          (if (null args)
              ;; Empty list is 0 (nil)
              (list #x48 #x31 #xC0)              ; xor rax, rax
              ;; Build nested cons expressions with already-parsed args
              (let ((cons-expr (reduce (lambda (rest-expr elem-expr)
                                         (make-expr :type 'call
                                                    :value 'cons
                                                    :args (list elem-expr rest-expr)))
                                       (reverse args)
                                       :initial-value (make-expr :type 'fixnum
                                                                 :value 0
                                                                 :args nil))))
                (emit-x86_64 cons-expr env))))


         ((eq op 'length)
          ;; (length list-ptr) - count elements in list
          ;; Call runtime-length trampoline: Arg: RDI (list-ptr), Return: RAX
          (unless *runtime-length-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-length-addr*)))
            (append
             ;; Evaluate list expression into RAX
             (emit-x86_64 (first args) env)
             ;; Setup call: RDI=list-ptr
             (list #x48 #x89 #xC7)                ; mov rdi, rax
             ;; Load function address and call
             (list #x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             (list #xFF #xD0))))                  ; call rax

         ((eq op 'nth)
          ;; (nth n list-ptr) - get nth element
          ;; Call runtime-nth trampoline: Args: RDI (n), RSI (list-ptr), Return: RAX
          (unless *runtime-nth-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-nth-addr*)))
            (append
             ;; Evaluate n into RAX
             (emit-x86_64 (first args) env)
             ;; Save n on stack
             (list #x50)                          ; push rax
             ;; Evaluate list into RAX
             (emit-x86_64 (second args) env)
             ;; Setup call: RDI=n, RSI=list
             (list #x48 #x89 #xC6)                ; mov rsi, rax (list in RSI)
             (list #x48 #x8B #x3C #x24)           ; mov rdi, [rsp] (n in RDI)
             (list #x48 #x83 #xC4 #x08)           ; add rsp, 8 (pop n)
             ;; Load function address and call
             (list #x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             (list #xFF #xD0))))                  ; call rax

         ((eq op 'append)
          ;; (append list1 list2) - concatenate lists
          ;; Call runtime-append trampoline: Args: RDI (list1), RSI (list2), Return: RAX
          (unless *runtime-append-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-append-addr*)))
            (append
             ;; Evaluate list1 into RAX
             (emit-x86_64 (first args) env)
             ;; Save list1 on stack
             (list #x50)                          ; push rax
             ;; Evaluate list2 into RAX
             (emit-x86_64 (second args) env)
             ;; Setup call: RDI=list1, RSI=list2
             (list #x48 #x89 #xC6)                ; mov rsi, rax (list2 in RSI)
             (list #x48 #x8B #x3C #x24)           ; mov rdi, [rsp] (list1 in RDI)
             (list #x48 #x83 #xC4 #x08)           ; add rsp, 8 (pop list1)
             ;; Load function address and call
             (list #x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             (list #xFF #xD0))))                  ; call rax

         ((eq op 'reverse)
          ;; (reverse list-ptr) - reverse a list
          ;; Call runtime-reverse trampoline: Arg: RDI (list-ptr), Return: RAX
          (unless *runtime-reverse-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-reverse-addr*)))
            (append
             ;; Evaluate list expression into RAX
             (emit-x86_64 (first args) env)
             ;; Setup call: RDI=list-ptr
             (list #x48 #x89 #xC7)                ; mov rdi, rax
             ;; Load function address and call
             (list #x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             (list #xFF #xD0))))                  ; call rax

         ;; String operations
         ((eq op 'string-length)
          ;; (string-length str-ptr) - get length of string
          ;; Call runtime-string-length trampoline: Arg: RDI (str-ptr), Return: RAX (raw length)
          ;; Need to tag result as fixnum
          (unless *runtime-string-length-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-string-length-addr*)))
            (append
             ;; Evaluate string expression into RAX
             (emit-x86_64 (first args) env)
             ;; Setup call: RDI=str-ptr
             '(#x48 #x89 #xC7)                ; mov rdi, rax
             ;; Load function address and call
             '(#x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             '(#xFF #xD0)                     ; call rax
             ;; Tag result as fixnum
             '(#x48 #xC1 #xE0 #x04))))        ; shl rax, 4

         ((eq op 'string-concat)
          ;; (string-concat str1 str2) - concatenate two strings
          ;; Call runtime-string-concat trampoline: Args: RDI (str1), RSI (str2), Return: RAX
          (unless *runtime-string-concat-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-string-concat-addr*)))
            (append
             ;; Evaluate str1 into RAX
             (emit-x86_64 (first args) env)
             ;; Save str1 on stack
             '(#x50)                          ; push rax
             ;; Evaluate str2 into RAX
             (emit-x86_64 (second args) env)
             ;; Setup call: RDI=str1, RSI=str2
             '(#x48 #x89 #xC6)                ; mov rsi, rax (str2 in RSI)
             '(#x48 #x8B #x3C #x24)           ; mov rdi, [rsp] (str1 in RDI)
             '(#x48 #x83 #xC4 #x08)           ; add rsp, 8 (pop str1)
             ;; Load function address and call
             '(#x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             '(#xFF #xD0))))                  ; call rax

         ((eq op 'string-equal)
          ;; (string-equal str1 str2) - compare two strings for equality
          ;; Call runtime-string-equal trampoline: Args: RDI (str1), RSI (str2), Return: RAX (tagged boolean)
          (unless *runtime-string-equal-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-string-equal-addr*)))
            (append
             ;; Evaluate str1 into RAX
             (emit-x86_64 (first args) env)
             ;; Save str1 on stack
             '(#x50)                          ; push rax
             ;; Evaluate str2 into RAX
             (emit-x86_64 (second args) env)
             ;; Setup call: RDI=str1, RSI=str2
             '(#x48 #x89 #xC6)                ; mov rsi, rax (str2 in RSI)
             '(#x48 #x8B #x3C #x24)           ; mov rdi, [rsp] (str1 in RDI)
             '(#x48 #x83 #xC4 #x08)           ; add rsp, 8 (pop str1)
             ;; Load function address and call
             '(#x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             '(#xFF #xD0))))                  ; call rax (returns tagged boolean)

         ((eq op 'string-substring)
          ;; (string-substring str start end) - extract substring
          ;; Call runtime-string-substring trampoline: Args: RDI (str), RSI (start), RDX (end), Return: RAX
          ;; start and end are tagged fixnums, need to untag them
          (unless *runtime-string-substring-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-string-substring-addr*)))
            (append
             ;; Evaluate str into RAX
             (emit-x86_64 (first args) env)
             ;; Save str on stack
             '(#x50)                          ; push rax
             ;; Evaluate start into RAX (tagged fixnum)
             (emit-x86_64 (second args) env)
             '(#x48 #xC1 #xF8 #x04)           ; sar rax, 4 (untag start)
             ;; Save start on stack
             '(#x50)                          ; push rax
             ;; Evaluate end into RAX (tagged fixnum)
             (emit-x86_64 (third args) env)
             '(#x48 #xC1 #xF8 #x04)           ; sar rax, 4 (untag end)
             ;; Setup call: RDI=str, RSI=start, RDX=end
             '(#x48 #x89 #xC2)                ; mov rdx, rax (end in RDX)
             '(#x48 #x8B #x34 #x24)           ; mov rsi, [rsp] (start in RSI)
             '(#x48 #x8B #x7C #x24 #x08)      ; mov rdi, [rsp + 8] (str in RDI)
             '(#x48 #x83 #xC4 #x10)           ; add rsp, 16 (pop start and str)
             ;; Load function address and call
             '(#x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             '(#xFF #xD0))))                  ; call rax

         ;; Reader/Printer operations
         ((eq op 'read)
          ;; (read str-ptr) - read S-expression from string
          ;; Call runtime-read-from-string trampoline: Arg: RDI (str-ptr), Return: RAX
          (unless *runtime-read-from-string-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-read-from-string-addr*)))
            (append
             ;; Evaluate string expression into RAX
             (emit-x86_64 (first args) env)
             ;; Setup call: RDI=str-ptr
             '(#x48 #x89 #xC7)                ; mov rdi, rax
             ;; Load function address and call
             '(#x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             '(#xFF #xD0))))                  ; call rax

         ((eq op 'print)
          ;; (print value) - print value to string
          ;; Call runtime-print-to-string trampoline: Arg: RDI (value), Return: RAX
          (unless *runtime-print-to-string-addr*
            (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
          (let ((func-addr (sb-sys:sap-int *runtime-print-to-string-addr*)))
            (append
             ;; Evaluate value expression into RAX
             (emit-x86_64 (first args) env)
             ;; Setup call: RDI=value
             '(#x48 #x89 #xC7)                ; mov rdi, rax
             ;; Load function address and call
             '(#x48 #xB8)                     ; movabs rax, imm64
             (int-to-bytes func-addr 8)
             '(#xFF #xD0))))                  ; call rax

         (t
          (error "Unknown operator: ~S" op)))))

    (runtime-call
     ;; Generate code to call function via symbol-function slot
     ;; (funcall 'name arg1 arg2 ...)
     (let* ((fn-name (expr-value expr))
            (args (expr-args expr))
            (num-args (length args)))
       ;; Phase 1: Only support 0-3 arguments (matching defun limitation)
       (when (> num-args 3)
         (error "Runtime funcall currently supports up to 3 arguments"))

       ;; Get symbol address at compile time
       (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
             (sym-addr 0))
         (unless intern-fn
           (error "Runtime not initialized"))
         (unless (symbolp fn-name)
           (error "fn-name should be a symbol, got ~S (type ~S, value ~S)"
                  fn-name (type-of fn-name) (expr-value expr)))
         (setf sym-addr (funcall intern-fn (string fn-name)))

         ;; Generate code:
         ;; 1. Load symbol address
         ;; 2. Read symbol-function slot (offset 24)
         ;; 3. Push function pointer to stack
         ;; 4. Evaluate arguments and setup registers
         ;; 5. Call function pointer
         (append
          ;; Load symbol address into RAX
          (list #x48 #xB8)                      ; movabs rax, imm64
          (int-to-bytes sym-addr 8)
          ;; Read symbol-function slot [rax + 24] into RAX
          (list #x48 #x8B #x40 #x18)            ; mov rax, [rax + 24]
          ;; Save function pointer on stack
          (list #x50)                           ; push rax

          ;; Evaluate arguments and setup registers
          ;; System V AMD64 ABI: RDI, RSI, RDX, RCX, R8, R9
          (cond
            ((= num-args 0)
             ;; No arguments, just call
             nil)

            ((= num-args 1)
             ;; Evaluate arg into RAX, move to RDI
             (append
              (emit-x86_64 (first args) env)
              (list #x48 #x89 #xC7)))           ; mov rdi, rax

            ((= num-args 2)
             ;; Eval arg1 -> RDI, arg2 -> RSI
             (append
              (emit-x86_64 (first args) env)
              (list #x50)                       ; push rax (save arg1)
              (emit-x86_64 (second args) env)
              (list #x48 #x89 #xC6)             ; mov rsi, rax (arg2 -> RSI)
              (list #x48 #x8B #x3C #x24)        ; mov rdi, [rsp] (arg1 -> RDI)
              (list #x48 #x83 #xC4 #x08)))      ; add rsp, 8 (pop arg1)

            ((= num-args 3)
             ;; Eval arg1 -> RDI, arg2 -> RSI, arg3 -> RDX
             (append
              (emit-x86_64 (first args) env)
              (list #x50)                       ; push rax (save arg1)
              (emit-x86_64 (second args) env)
              (list #x50)                       ; push rax (save arg2)
              (emit-x86_64 (third args) env)
              (list #x48 #x89 #xC2)             ; mov rdx, rax (arg3 -> RDX)
              (list #x48 #x8B #x34 #x24)        ; mov rsi, [rsp] (arg2 -> RSI)
              (list #x48 #x8B #x7C #x24 #x08)   ; mov rdi, [rsp + 8] (arg1 -> RDI)
              (list #x48 #x83 #xC4 #x10)))      ; add rsp, 16 (pop arg2 and arg1)

            (t (error "Unsupported number of arguments: ~D" num-args)))

          ;; Pop function pointer from stack to R11 and call
          (list #x49 #x8B #x1C #x24)            ; mov r11, [rsp]
          (list #x48 #x83 #xC4 #x08)            ; add rsp, 8 (pop fn ptr)
          (list #x41 #xFF #xD3)))))))           ; call r11

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

    (string
     ;; Create string on heap at compile time and load pointer into X0
     ;; Load 64-bit pointer using MOVZ + MOVK sequence
     (let* ((lisp-string (expr-value expr))
            (make-string-fn (find-symbol "RUNTIME-MAKE-STRING" :habu-runtime))
            (ptr (funcall make-string-fn lisp-string)))
       (append
        ;; MOVZ X0, #(ptr[15:0])
        (int-to-bytes (logior #xD2800000
                              (ash (logand ptr #xFFFF) 5))
                      4)
        ;; MOVK X0, #(ptr[31:16]), LSL#16
        (int-to-bytes (logior #xF2A00000
                              (ash (logand (ash ptr -16) #xFFFF) 5))
                      4)
        ;; MOVK X0, #(ptr[47:32]), LSL#32
        (int-to-bytes (logior #xF2C00000
                              (ash (logand (ash ptr -32) #xFFFF) 5))
                      4)
        ;; MOVK X0, #(ptr[63:48]), LSL#48
        (int-to-bytes (logior #xF2E00000
                              (ash (logand (ash ptr -48) #xFFFF) 5))
                      4))))

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

    (setq
     ;; Compile (setq var value) - mutate a lexical variable
     (let* ((var-name (expr-value expr))
            (value-expr (first (expr-args expr)))
            (binding (assoc var-name env)))
       (if binding
           (let ((offset (cdr binding)))
             (if (< offset 256)
                 (append
                  ;; First, evaluate the value expression into X0
                  (emit-arm64 value-expr env)
                  ;; Then store X0 to the variable's stack location
                  ;; str x0, [sp, #offset]
                  (int-to-bytes (logior #xF9000000  ; str x0, [sp, #imm]
                                        (ash (/ offset 8) 10))  ; offset in bits [21:10]
                                4))
                 (error "Variable offset too large: ~D" offset)))
           (error "Cannot setq unbound variable: ~S" var-name))))

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

    (closure
     ;; Create a heap-allocated closure object as a first-class value (ARM64)
     ;; Phase 1: Use runtime make-closure-N trampolines with eval'd wrapper
     (let* ((params (expr-value expr))
            (body (first (expr-args expr)))
            (free-vars (second (expr-args expr)))
            (original-body (third (expr-args expr)))  ; Original Lisp form
            (num-free (length free-vars))
            (arity (length params))
            (closure-name (gensym "CLOSURE"))
            (wrapper-params (append free-vars params)))

       ;; Phase 1 limitation: only support 0-3 captured variables
       (when (> num-free 3)
         (error "Phase 1 only supports closures with up to 3 captured variables, got ~D" num-free))

       ;; Create wrapper function via eval (same as x86_64)
       (let ((callable-name (intern (format nil "HABU-CLOSURE-~A" (string-upcase (string closure-name)))
                                    (find-package :habu-compiler))))
         (eval `(defun ,callable-name ,wrapper-params ,original-body))

         #+sbcl
         (let ((num-wrapper-params (length wrapper-params)))
           (cond
             ((<= num-wrapper-params 0)
              (eval `(sb-alien:define-alien-callable ,callable-name sb-alien:unsigned-long () (,callable-name))))
             ((<= num-wrapper-params 1)
              (eval `(sb-alien:define-alien-callable ,callable-name sb-alien:unsigned-long
                       ((,(first wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params)))))
             ((<= num-wrapper-params 2)
              (eval `(sb-alien:define-alien-callable ,callable-name sb-alien:unsigned-long
                       ((,(first wrapper-params) sb-alien:unsigned-long)
                        (,(second wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params) ,(second wrapper-params)))))
             ((<= num-wrapper-params 3)
              (eval `(sb-alien:define-alien-callable ,callable-name sb-alien:unsigned-long
                       ((,(first wrapper-params) sb-alien:unsigned-long)
                        (,(second wrapper-params) sb-alien:unsigned-long)
                        (,(third wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params) ,(second wrapper-params) ,(third wrapper-params)))))
             ((<= num-wrapper-params 4)
              (eval `(sb-alien:define-alien-callable ,callable-name sb-alien:unsigned-long
                       ((,(first wrapper-params) sb-alien:unsigned-long)
                        (,(second wrapper-params) sb-alien:unsigned-long)
                        (,(third wrapper-params) sb-alien:unsigned-long)
                        (,(fourth wrapper-params) sb-alien:unsigned-long))
                       (,callable-name ,(first wrapper-params) ,(second wrapper-params) ,(third wrapper-params) ,(fourth wrapper-params)))))
             (t (error "Wrapper function with ~D parameters not supported" num-wrapper-params))))

         ;; Store wrapper function pointer in runtime symbol table
         (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
               (set-fn-fn (find-symbol "SET-SYMBOL-FUNCTION" :habu-runtime)))
           (when (and intern-fn set-fn-fn)
             (let* ((name-str (string closure-name))
                    (sym (funcall intern-fn name-str))
                    (fn-ptr (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:alien-callable-function callable-name)))))
               (funcall set-fn-fn sym fn-ptr))))

         ;; Generate ARM64 code to call make-closure-N trampoline
         (let ((trampoline-addr
                (case num-free
                  (0 *runtime-make-closure-0-addr*)
                  (1 *runtime-make-closure-1-addr*)
                  (2 *runtime-make-closure-2-addr*)
                  (3 *runtime-make-closure-3-addr*)
                  (t (error "Unsupported number of captured vars: ~D" num-free)))))
           (unless trampoline-addr
             (error "Runtime not initialized. Call (initialize-runtime-integration) first."))
           (let ((func-addr (sb-sys:sap-int trampoline-addr))
                 (intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime)))
             (unless intern-fn
               (error "Runtime not initialized"))

             ;; Get symbol address for wrapper function
             (let ((sym-addr (funcall intern-fn (string closure-name))))
               ;; ARM64 calling convention: X0-X7 for first 8 args
               ;; X0 = code pointer, X1 = arity, X2-X4 = captured vars
               (append
                ;; Load symbol address into X9 (temp register)
                '(#xE9 #x01 #x00 #x58)             ; ldr x9, [pc + offset to data]
                '(#x03 #x00 #x00 #x14)             ; b skip_data (3 instructions)
                (int-to-bytes sym-addr 8)          ; .data: symbol address

                ;; skip_data:
                ;; Load function pointer from symbol [x9 + 24] into X0
                '(#x20 #x61 #x40 #xF9)             ; ldr x0, [x9, #24*8]

                ;; Load arity into X1
                (int-to-arm64-mov-imm 'x1 arity)

                ;; Load captured variables into X2, X3, X4
                (case num-free
                  (0 nil)
                  (1 (append (emit-arm64 (make-expr :type 'variable :value (first free-vars)) env)
                             '(#xE2 #x03 #x00 #xAA)))  ; mov x2, x0
                  (2 (append (emit-arm64 (make-expr :type 'variable :value (first free-vars)) env)
                             '(#xF0 #x03 #x00 #xF8)    ; str x0, [sp, #-16]!
                             (emit-arm64 (make-expr :type 'variable :value (second free-vars)) env)
                             '(#xE3 #x03 #x00 #xAA)    ; mov x3, x0
                             '(#xF0 #x07 #x40 #xF8)))  ; ldr x2, [sp], #16
                  (3 (append (emit-arm64 (make-expr :type 'variable :value (first free-vars)) env)
                             '(#xF0 #x03 #x00 #xF8)    ; str x0, [sp, #-16]!
                             (emit-arm64 (make-expr :type 'variable :value (second free-vars)) env)
                             '(#xF0 #x03 #x00 #xF8)    ; str x0, [sp, #-16]!
                             (emit-arm64 (make-expr :type 'variable :value (third free-vars)) env)
                             '(#xE4 #x03 #x00 #xAA)    ; mov x4, x0
                             '(#xF0 #x07 #x40 #xF8)    ; ldr x3, [sp], #16
                             '(#xF0 #x07 #x40 #xF8)))  ; ldr x2, [sp], #16
                  (t (error "Unsupported num-free: ~D" num-free)))

                ;; Load trampoline address and call
                '(#xE9 #x01 #x00 #x58)             ; ldr x9, [pc + offset]
                '(#x03 #x00 #x00 #x14)             ; b skip_data2
                (int-to-bytes func-addr 8)         ; .data: trampoline address
                ;; skip_data2:
                '(#x20 #x01 #x3F #xD6))))))))      ; blr x9

    (named-let
     ;; Compile (let name ((var val) ...) body) for ARM64
     ;; For now, compile as regular let - recursive calls will need TCO support
     (let* ((name-and-bindings (expr-value expr))
            (loop-name (first name-and-bindings))
            (bindings (second name-and-bindings))
            (body (first (expr-args expr)))
            (vars (mapcar #'first bindings))
            (num-bindings (length bindings)))
       ;; Warn about recursive calls (they won't work without TCO)
       (when (find-recursive-calls loop-name body)
         (warn "Named-let '~A' contains recursive calls. Tail-call optimization not yet implemented. Recursive calls will cause errors." loop-name))
       ;; Compile as regular let for now
       (let ((binding-code nil)
             (new-env env))
         ;; Evaluate and push each binding
         (dolist (binding bindings)
           (let ((var (first binding))
                 (val (second binding)))
             (setf binding-code
                   (append binding-code
                           (emit-arm64 (parse val) env)
                           (list #xFD #x7B #xBF #xA9)))))  ; stp x29, x30, [sp, #-16]!
         ;; Build environment with variable offsets
         (let ((offset 0))
           (dolist (var (reverse vars))
             (setf new-env (cons (cons var offset) new-env))
             (setf offset (+ offset 16))))  ; ARM64 uses 16-byte alignment
         ;; Compile body with new environment
         (let ((body-code (emit-arm64 body new-env)))
           (append binding-code
                   body-code
                   ;; Clean up stack
                   (loop repeat num-bindings
                         append (list #xFD #x7B #xC1 #xA8)))))))  ; ldp x29, x30, [sp], #16

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

    (when
     ;; Compile (when test body...) for ARM64
     (let* ((test-expr (first (expr-args expr)))
            (body-exprs (rest (expr-args expr)))
            (test-code (emit-arm64 test-expr env))
            (body-code (if (null body-exprs)
                           (emit-arm64 (make-expr :type 'fixnum :value 0) env)
                           (if (= (length body-exprs) 1)
                               (emit-arm64 (first body-exprs) env)
                               (emit-arm64 (make-expr :type 'progn :args body-exprs) env))))
            (body-size (length body-code)))
       (append test-code
               (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
               ;; b.eq (skip body if test is false)
               (let ((offset-bytes body-size))
                 (list #x40  ; condition code 0 = eq
                       (logand (ash offset-bytes -2) #xFF)
                       (logand (ash offset-bytes -10) #xFF)
                       #x54))
               body-code)))

    (unless
     ;; Compile (unless test body...) for ARM64
     (let* ((test-expr (first (expr-args expr)))
            (body-exprs (rest (expr-args expr)))
            (test-code (emit-arm64 test-expr env))
            (body-code (if (null body-exprs)
                           (emit-arm64 (make-expr :type 'fixnum :value 0) env)
                           (if (= (length body-exprs) 1)
                               (emit-arm64 (first body-exprs) env)
                               (emit-arm64 (make-expr :type 'progn :args body-exprs) env))))
            (body-size (length body-code)))
       (append test-code
               (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
               ;; b.ne (skip body if test is true)
               (let ((offset-bytes body-size))
                 (list #x41  ; condition code 1 = ne
                       (logand (ash offset-bytes -2) #xFF)
                       (logand (ash offset-bytes -10) #xFF)
                       #x54))
               body-code)))

    (case
     ;; Compile (case keyform (key1 result1) (key2 result2) (t default)) for ARM64
     (let* ((keyform (first (expr-args expr)))
            (clauses (expr-value expr))
            (key-var (gensym "CASE-KEY")))
       (let ((key-code (emit-arm64 keyform env))
             (new-env (cons (cons key-var 0) env)))
         (labels ((compile-case-clauses (clauses)
                    (if (null clauses)
                        (emit-arm64 (make-expr :type 'fixnum :value 0) new-env)
                        (let* ((clause (first clauses))
                               (keys (first clause))
                               (result (second clause))
                               (rest-clauses (rest clauses)))
                          (if (or (eq keys t) (null rest-clauses))
                              (emit-arm64 (parse result) new-env)
                              (let* ((key-value (if (listp keys) (first keys) keys))
                                     ;; Generate test code: load key from stack, compare with key-value
                                     (test-code (append
                                                 (list #xE1 #x03 #x40 #xF9)  ; ldr x1, [sp]
                                                 ;; Compare with key-value
                                                 (let ((tagged-key (* key-value 16)))
                                                   (if (< tagged-key 4096)
                                                       (list (logand tagged-key #xFF)
                                                             (logand (ash tagged-key -8) #xFF)
                                                             #x00 #x91)  ; add x0, xzr, #tagged-key
                                                       (append (list #xE0 #x03 #x1F #xAA)  ; mov x0, xzr
                                                               (list (logand tagged-key #xFF)
                                                                     (logand (ash tagged-key -8) #x1F)
                                                                     #x80 #xD2))))  ; mov x0, #tagged-key
                                                 (list #x3F #x00 #x00 #xEB)   ; cmp x1, x0
                                                 (list #xE0 #x07 #x9F #x9A))) ; cset x0, eq
                                     (then-code (emit-arm64 (parse result) new-env))
                                     (else-code (compile-case-clauses rest-clauses))
                                     (then-size (length then-code))
                                     (else-size (length else-code)))
                                (append test-code
                                        (list #x1F #x00 #x00 #xF1)  ; cmp x0, #0
                                        (let ((offset-bytes (+ then-size 4)))
                                          (list #x40  ; b.eq
                                                (logand (ash offset-bytes -2) #xFF)
                                                (logand (ash offset-bytes -10) #xFF)
                                                #x54))
                                        then-code
                                        (let ((offset-bytes else-size))
                                          (list (logand (ash offset-bytes -2) #xFF)
                                                (logand (ash offset-bytes -10) #xFF)
                                                (logand (ash offset-bytes -18) #xFF)
                                                #x14))
                                        else-code)))))))
           (append key-code
                   (list #xE0 #x0F #x1F #xF8)  ; str x0, [sp, #-8]!
                   (compile-case-clauses clauses)
                   (list #xFF #x07 #x00 #x91)))))) ; add sp, sp, #8

    (funcall
     ;; Compile ((lambda (params) body) args) or ((closure ...) args) for ARM64
     (let* ((fn-expr (expr-value expr))
            (arg-exprs (expr-args expr)))
       (if (or (eq (expr-type fn-expr) 'lambda)
               (eq (expr-type fn-expr) 'closure))
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
             ;; For closures, free variables should already be in env
             (let ((body-code (emit-arm64 body (reverse new-env))))
               (append binding-code
                       body-code
                       ;; Clean up stack
                       (if (<= (* num-params 8) 4095)
                           (int-to-bytes (logior #x910003E0
                                                 (ash (* num-params 8) 10))
                                         4)
                           (error "Too many parameters for immediate encoding")))))
           (error "Can only call lambda/closure expressions for now"))))

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

         ((eq op 'rem)
          ;; Compile (rem a b) for ARM64 - remainder operation
          ;; Same as mod (ARM64 sdiv gives truncating division, same as rem)
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save dividend)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (divisor to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (dividend back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag dividend)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag divisor)
                  (list #xE3 #x0C #xC1 #x9A)       ; sdiv x3, x0, x1 (quotient in x3)
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

         ((eq op '/=)
          ;; Compile (/= a b) - not equal for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #x17 #x9F #x9A)       ; cset x0, ne (not equal)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))

         ((eq op 'eql)
          ;; Compile (eql a b) - object identity for ARM64
          ;; For fixnums, same as =
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #x07 #x9F #x9A)       ; cset x0, eq (equal)
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))

         ((eq op 'eq)
          ;; Compile (eq a b) - pointer equality for ARM64
          ;; For fixnums, same as =
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0
                  (list #x5F #x00 #x01 #xEB)       ; cmp x2, x1
                  (list #xE0 #x07 #x9F #x9A)       ; cset x0, eq (equal)
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

         ((eq op 'logand)
          ;; Compile (logand a b) for ARM64 - bitwise AND
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)      ; str x0, [sp, #-8]!
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)      ; ldr x1, [sp]
                  (list #x00 #x00 #x01 #x8A)      ; and x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))    ; add sp, sp, #8

         ((eq op 'logior)
          ;; Compile (logior a b) for ARM64 - bitwise OR
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)
                  (list #x00 #x00 #x01 #xAA)      ; orr x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))

         ((eq op 'logxor)
          ;; Compile (logxor a b) for ARM64 - bitwise XOR
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)
                  (list #x00 #x00 #x01 #xCA)      ; eor x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))

         ((eq op 'lognot)
          ;; Compile (lognot a) for ARM64 - bitwise NOT
          (append (emit-arm64 (first args) env)
                  (list #x00 #x00 #x20 #xAA)))    ; mvn x0, x0

         ((eq op 'ash)
          ;; Compile (ash a b) for ARM64 - arithmetic shift
          (append (emit-arm64 (second args) env)  ; shift count in x0
                  (list #xE0 #x0F #x1F #xF8)      ; str x0, [sp, #-8]!
                  (emit-arm64 (first args) env)   ; value in x0
                  (list #xE1 #x03 #x40 #xF9)      ; ldr x1, [sp] (shift count)
                  (list #x21 #x10 #x40 #xD3)      ; lsr x1, x1, #4 (untag)
                  (list #x3F #x00 #x00 #xF1)      ; cmp x1, #0
                  ;; b.ge to left shift (skip right shift)
                  (list #x42 #x00 #x00 #x54)      ; b.ge #8
                  ;; Right shift (negative count)
                  (list #x21 #x00 #x00 #xCB)      ; neg x1, x1
                  (list #x00 #xFC #xC1 #x9A)      ; asr x0, x0, x1
                  (list #x01 #x00 #x00 #x14)      ; b #4 (skip left shift)
                  ;; Left shift
                  (list #x00 #x20 #xC1 #x9A)      ; lsl x0, x0, x1
                  (list #xFF #x07 #x00 #x91)))    ; add sp, sp, #8

         ;; Numeric operators
         ((eq op 'min)
          ;; Compile (min a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)      ; str x0, [sp, #-8]!
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)      ; ldr x1, [sp]
                  (list #x3F #x00 #x00 #xEB)      ; cmp x1, x0
                  (list #x20 #xD0 #x81 #x9A)      ; csel x0, x1, x1, le (select x1 if x1 <= x0)
                  (list #xFF #x07 #x00 #x91)))    ; add sp, sp, #8

         ((eq op 'max)
          ;; Compile (max a b) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x0F #x1F #xF8)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x40 #xF9)
                  (list #x3F #x00 #x00 #xEB)      ; cmp x1, x0
                  (list #x20 #xC0 #x81 #x9A)      ; csel x0, x1, x1, gt (select x1 if x1 > x0)
                  (list #xFF #x07 #x00 #x91)))

         ((eq op 'abs)
          ;; Compile (abs a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x01 #xFC #x7F #xD3)      ; lsr x1, x0, #63 (sign bit)
                  (list #x00 #x00 #x01 #xCA)      ; eor x0, x0, x1
                  (list #x00 #x00 #x01 #xCB)))    ; sub x0, x0, x1

         ((eq op '1+)
          ;; Compile (1+ a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x40 #x00 #x91)))    ; add x0, x0, #16

         ((eq op '1-)
          ;; Compile (1- a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x40 #x00 #xD1)))    ; sub x0, x0, #16

         ;; Predicates
         ((eq op 'zerop)
          ;; Compile (zerop a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE0 #x07 #x9F #x9A)      ; cset x0, eq
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4

         ((eq op 'plusp)
          ;; Compile (plusp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE0 #xC7 #x9F #x9A)      ; cset x0, gt
                  (list #x00 #x10 #x00 #xD3)))

         ((eq op 'minusp)
          ;; Compile (minusp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE0 #xB7 #x9F #x9A)      ; cset x0, lt
                  (list #x00 #x10 #x00 #xD3)))

         ((eq op 'evenp)
          ;; Compile (evenp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x40 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x00 #x04 #x00 #x92)      ; and x0, x0, #1
                  (list #x00 #x04 #x00 #xD2)      ; eor x0, x0, #1 (invert)
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4

         ((eq op 'oddp)
          ;; Compile (oddp a) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x40 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x00 #x04 #x00 #x92)      ; and x0, x0, #1
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4

         ;; Type predicates (for fixnum-only system)
         ((eq op 'numberp)
          ;; numberp always returns true for fixnums
          (append (emit-arm64 (first args) env)
                  (list #x20 #x02 #x80 #xD2)))    ; mov x0, #16 (tagged 1)

         ((eq op 'integerp)
          ;; integerp always returns true for fixnums
          (append (emit-arm64 (first args) env)
                  (list #x20 #x02 #x80 #xD2)))    ; mov x0, #16 (tagged 1)

         ((eq op 'atom)
          ;; atom always returns true for fixnums (not conses)
          (append (emit-arm64 (first args) env)
                  (list #x20 #x02 #x80 #xD2)))    ; mov x0, #16 (tagged 1)

         ((eq op 'listp)
          ;; listp always returns false for fixnums
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x03 #x1F #xAA)))    ; mov x0, xzr (tagged 0)

         ((eq op 'consp)
          ;; consp always returns false for fixnums
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x03 #x1F #xAA)))    ; mov x0, xzr (tagged 0)

         ((eq op 'symbolp)
          ;; symbolp always returns false for fixnums
          (append (emit-arm64 (first args) env)
                  (list #xE0 #x03 #x1F #xAA)))    ; mov x0, xzr (tagged 0)

         ((eq op 'signum)
          ;; Compile (signum a) - return -1, 0, or 1 based on sign
          ;; Use conditional select: x < 0 ? -1 : (x > 0 ? 1 : 0)
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x44 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #xE1 #xB3 #x9F #x1A)      ; csetm x1, lt (x1 = -1 if neg else 0)
                  (list #xE2 #xC7 #x9A #x9A)      ; cset x2, gt (x2 = 1 if pos else 0)
                  (list #x00 #x00 #x82 #x8B)      ; add x0, x0, x2 (combine)
                  (list #x00 #x00 #x01 #x8B)      ; add x0, x0, x1
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4 (retag)

         ((eq op 'logcount)
          ;; Compile (logcount a) - count number of set bits
          ;; Uses loop to count bits (ARM64 has no single instruction for this in base ISA)
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x44 #xD3)      ; lsr x0, x0, #4 (untag)
                  (list #x01 #x00 #x80 #xD2)      ; mov x1, #0 (counter)
                  ;; Loop start
                  (list #x1F #x00 #x00 #xF1)      ; cmp x0, #0
                  (list #x60 #x00 #x00 #x54)      ; b.eq +12 (exit if zero)
                  (list #x21 #x04 #x00 #x91)      ; add x1, x1, #1 (counter++)
                  (list #x02 #x00 #x00 #xD1)      ; sub x2, x0, #1
                  (list #x00 #x00 #x02 #x8A)      ; and x0, x0, x2 (clear lowest bit)
                  (list #xE0 #xFF #xFF #x17)      ; b -8 (loop back)
                  ;; Exit
                  (list #x00 #x00 #x01 #xAA)      ; mov x0, x1 (result = counter)
                  (list #x00 #x10 #x00 #xD3)))    ; lsl x0, x0, #4 (retag)

         ((eq op 'logtest)
          ;; Compile (logtest a b) - test if any bits are set in both
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)      ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)      ; mov x2, x0 (save first)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)      ; mov x1, x0 (second to x1)
                  (list #x40 #x10 #x44 #xD3)      ; lsr x0, x2, #4 (untag first)
                  (list #x21 #x10 #x44 #xD3)      ; lsr x1, x1, #4 (untag second)
                  (list #x00 #x00 #x01 #x8A)      ; and x0, x0, x1
                  (list #xE0 #x17 #x9F #x9A)      ; cset x0, ne (1 if result != 0)
                  (list #x00 #x10 #x00 #xD3)      ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)))    ; ldp x29, x30, [sp], #16

         ((eq op 'logbitp)
          ;; Compile (logbitp position integer) for ARM64
          (append (emit-arm64 (first args) env)       ; position
                  (list #xFD #x7B #xBF #xA9)           ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)           ; mov x2, x0 (save position)
                  (emit-arm64 (second args) env)       ; integer
                  (list #x42 #x10 #x44 #xD3)           ; lsr x2, x2, #4 (untag position)
                  (list #x00 #x10 #x44 #xD3)           ; lsr x0, x0, #4 (untag integer)
                  (list #x00 #x24 #xC2 #x9A)           ; lsr x0, x0, x2 (shift right by position)
                  (list #x00 #x04 #x00 #x92)           ; and x0, x0, #1 (get bit)
                  (list #x00 #x10 #x00 #xD3)           ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)))         ; ldp x29, x30, [sp], #16

         ((eq op 'lognand)
          ;; Compile (lognand a b) for ARM64 - bitwise NAND
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)      ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)      ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)      ; mov x1, x0
                  (list #xE0 #x03 #x02 #xAA)      ; mov x0, x2
                  (list #x00 #x00 #x01 #x8A)      ; and x0, x0, x1
                  (list #x00 #x00 #x20 #xAA)      ; mvn x0, x0 (bitwise not)
                  (list #x00 #x3C #x00 #x92)      ; and x0, x0, #~0xF (keep only data bits)
                  (list #xFD #x7B #xC1 #xA8)))    ; ldp x29, x30, [sp], #16

         ((eq op 'lognor)
          ;; Compile (lognor a b) for ARM64 - bitwise NOR
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)      ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)      ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)      ; mov x1, x0
                  (list #xE0 #x03 #x02 #xAA)      ; mov x0, x2
                  (list #x00 #x00 #x01 #xAA)      ; orr x0, x0, x1
                  (list #x00 #x00 #x20 #xAA)      ; mvn x0, x0 (bitwise not)
                  (list #x00 #x3C #x00 #x92)      ; and x0, x0, #~0xF (keep only data bits)
                  (list #xFD #x7B #xC1 #xA8)))    ; ldp x29, x30, [sp], #16

         ((eq op 'logeqv)
          ;; Compile (logeqv a b) for ARM64 - bitwise equivalence
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)      ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)      ; mov x2, x0
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)      ; mov x1, x0
                  (list #xE0 #x03 #x02 #xAA)      ; mov x0, x2
                  (list #x00 #x00 #x01 #xCA)      ; eor x0, x0, x1
                  (list #x00 #x00 #x20 #xAA)      ; mvn x0, x0 (bitwise not)
                  (list #x00 #x3C #x00 #x92)      ; and x0, x0, #~0xF (keep only data bits)
                  (list #xFD #x7B #xC1 #xA8)))    ; ldp x29, x30, [sp], #16

         ((eq op 'gcd)
          ;; Compile (gcd a b) for ARM64 - greatest common divisor
          ;; Using Euclidean algorithm with loop
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save first)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (second to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (first back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag)
                  ;; abs(x0): x0 = (x0 XOR (x0>>63)) - (x0>>63)
                  (list #x02 #xFC #x47 #x93)       ; asr x2, x0, #63 (sign extend)
                  (list #x00 #x00 #x02 #xCA)       ; eor x0, x0, x2
                  (list #x00 #x00 #x02 #xCB)       ; sub x0, x0, x2
                  ;; abs(x1): x1 = (x1 XOR (x1>>63)) - (x1>>63)
                  (list #x22 #xFC #x47 #x93)       ; asr x2, x1, #63
                  (list #x21 #x00 #x02 #xCA)       ; eor x1, x1, x2
                  (list #x21 #x00 #x02 #xCB)       ; sub x1, x1, x2
                  ;; GCD loop: while x1 != 0
                  ;; Check if x1 == 0, use cmp and conditional select approach
                  (list #x3F #x00 #x01 #xEB)       ; cmp x1, #0
                  (list #x03 #x00 #x00 #x54)       ; b.eq +6 (to done, skip 6 instructions)
                  ;; Compute remainder: x3 = x0 - (x0/x1)*x1
                  (list #xE2 #x0C #xC1 #x9A)       ; sdiv x2, x0, x1 (quotient)
                  (list #x03 #x7C #x01 #x9B)       ; msub x3, x0, x1, x2 (remainder)
                  (list #xE0 #x03 #x01 #xAA)       ; mov x0, x1 (a = b)
                  (list #xE1 #x03 #x03 #xAA)       ; mov x1, x3 (b = remainder)
                  (list #xFA #xFF #xFF #x17)       ; b -6 (back to cmp)
                  ;; Done: x0 has GCD
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op 'lcm)
          ;; Compile (lcm a b) for ARM64 - least common multiple
          ;; Formula: lcm(a,b) = |a*b| / gcd(a,b)
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save first)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (second to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (first back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag)
                  ;; Check for zero
                  (list #x1F #x00 #x00 #xF1)       ; cmp x0, #0
                  (list #xE0 #x01 #x00 #x54)       ; b.eq +15 (return 0)
                  (list #x3F #x00 #x00 #xF1)       ; cmp x1, #0
                  (list #xC0 #x01 #x00 #x54)       ; b.eq +14 (return 0)
                  ;; abs(x0)
                  (list #x02 #xFC #x47 #x93)       ; asr x2, x0, #63
                  (list #x00 #x00 #x02 #xCA)       ; eor x0, x0, x2
                  (list #x00 #x00 #x02 #xCB)       ; sub x0, x0, x2
                  (list #xE3 #x03 #x00 #xAA)       ; mov x3, x0 (save |a|)
                  ;; abs(x1)
                  (list #x02 #xFC #x47 #x93)       ; asr x2, x1, #63
                  (list #x21 #x00 #x02 #xCA)       ; eor x1, x1, x2
                  (list #x21 #x00 #x02 #xCB)       ; sub x1, x1, x2
                  (list #xE4 #x03 #x01 #xAA)       ; mov x4, x1 (save |b|)
                  ;; Compute product |a| * |b|
                  (list #x60 #x7C #x01 #x9B)       ; mul x0, x3, x1 (x0 = |a| * |b|)
                  (list #xE5 #x03 #x00 #xAA)       ; mov x5, x0 (save product)
                  ;; Compute GCD(|a|, |b|) - x3=|a|, x4=|b|
                  (list #xE0 #x03 #x03 #xAA)       ; mov x0, x3 (|a|)
                  (list #xE1 #x03 #x04 #xAA)       ; mov x1, x4 (|b|)
                  ;; GCD loop
                  (list #x3F #x00 #x00 #xF1)       ; cmp x1, #0
                  (list #x03 #x00 #x00 #x54)       ; b.eq +6
                  (list #xE2 #x0C #xC1 #x9A)       ; sdiv x2, x0, x1
                  (list #x03 #x7C #x01 #x9B)       ; msub x3, x0, x1, x2
                  (list #xE0 #x03 #x01 #xAA)       ; mov x0, x1
                  (list #xE1 #x03 #x03 #xAA)       ; mov x1, x3
                  (list #xFA #xFF #xFF #x17)       ; b -6
                  ;; x0 = gcd, compute product/gcd
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (gcd)
                  (list #xE0 #x03 #x05 #xAA)       ; mov x0, x5 (product)
                  (list #xE0 #x0C #xC1 #x9A)       ; sdiv x0, x0, x1
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)       ; ldp x29, x30, [sp], #16
                  (list #x00 #x08 #x00 #x14)       ; b +8 (skip zero case)
                  ;; Zero case
                  (list #x00 #x00 #x80 #xD2)       ; mov x0, #0
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op 'isqrt)
          ;; Compile (isqrt n) for ARM64 - integer square root using Newton's method
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag)
                  ;; Handle special cases: if n <= 1, return n
                  (list #x1F #x08 #x00 #xF1)       ; cmp x0, #2
                  (list #xC3 #x00 #x00 #x54)       ; b.lo +6 (skip to retag if < 2)
                  ;; Initialize: x1 = n, x2 = n/2 (initial guess)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (save n)
                  (list #x02 #x08 #x40 #xD3)       ; lsr x2, x0, #1 (x2 = n/2)
                  ;; Newton loop
                  (list #xE3 #x03 #x02 #xAA)       ; mov x3, x2 (save old guess)
                  (list #xE0 #x03 #x01 #xAA)       ; mov x0, x1 (n)
                  (list #xE0 #x0C #xC3 #x9A)       ; sdiv x0, x0, x3 (n/x)
                  (list #x00 #x00 #x03 #x8B)       ; add x0, x0, x3 (n/x + x)
                  (list #x02 #x08 #x40 #xD3)       ; lsr x2, x0, #1 ((n/x + x)/2)
                  ;; Check convergence: if old >= new, done
                  (list #x7F #x00 #x02 #xEB)       ; cmp x3, x2
                  (list #x42 #x00 #x00 #x54)       ; b.hs +2 (if old >= new, use old)
                  (list #xF9 #xFF #xFF #x17)       ; b -7 (back to loop)
                  ;; Use old value (converged)
                  (list #xE0 #x03 #x03 #xAA)       ; mov x0, x3 (result)
                  ;; Retag and return
                  (list #x00 #x10 #x00 #xD3)))     ; lsl x0, x0, #4 (retag)

         ((eq op 'integer-length)
          ;; Compile (integer-length n) for ARM64
          ;; Returns number of bits needed to represent n
          (append (emit-arm64 (first args) env)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag)
                  ;; Check if negative
                  (list #x1F #x00 #x00 #xF1)       ; cmp x0, #0
                  (list #x4A #x00 #x00 #x54)       ; b.ge +2 (if positive, skip)
                  ;; Negative: compute -n - 1
                  (list #x00 #x00 #x00 #xCB)       ; neg x0, x0
                  (list #x00 #x04 #x00 #xD1)       ; sub x0, x0, #1
                  ;; Check for zero
                  (list #x1F #x00 #x00 #xF1)       ; cmp x0, #0
                  (list #x60 #x00 #x00 #x54)       ; b.eq +3 (return 0 if zero)
                  ;; Use CLZ then compute 64 - clz
                  (list #xE1 #x10 #xC0 #xDA)       ; clz x1, x0 (count leading zeros)
                  (list #x00 #x00 #x80 #xD2)       ; mov x0, #64
                  (list #x00 #x00 #x01 #xCB)       ; sub x0, x0, x1 (64 - clz = bit position + 1)
                  ;; Retag and return
                  (list #x00 #x10 #x00 #xD3)))     ; lsl x0, x0, #4 (retag)

         ((eq op 'expt)
          ;; Compile (expt base exponent) for ARM64
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save base)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (exponent to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (base back to x0)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag base)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag exponent)
                  ;; Check for special cases
                  (list #x3F #x00 #x00 #xF1)       ; cmp x1, #0
                  (list #x0A #x00 #x00 #x54)       ; b.lt +1 (negative exp)
                  (list #x60 #x01 #x00 #x54)       ; b.eq +11 (exp = 0, return 1)
                  (list #x3F #x04 #x00 #xF1)       ; cmp x1, #1
                  (list #x80 #x01 #x00 #x54)       ; b.eq +12 (exp = 1, return base)
                  ;; Initialize: x2 = result = 1, x3 = base
                  (list #xE3 #x03 #x00 #xAA)       ; mov x3, x0 (save base)
                  (list #x22 #x00 #x80 #xD2)       ; mov x2, #1 (result = 1)
                  ;; Loop: while x1 > 0
                  (list #x3F #x00 #x00 #xF1)       ; cmp x1, #0
                  (list #x60 #x00 #x00 #x54)       ; b.eq +3 (done)
                  (list #x42 #x7C #x03 #x9B)       ; mul x2, x2, x3 (result *= base)
                  (list #x21 #x04 #x00 #xD1)       ; sub x1, x1, #1
                  (list #xFD #xFF #xFF #x17)       ; b -3
                  ;; Done: x2 has result
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)       ; ldp x29, x30, [sp], #16
                  (list #x03 #x08 #x00 #x14)       ; b +3 (skip special cases)
                  ;; Exponent = 1: return base
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4
                  (list #xFD #x7B #xC1 #xA8)       ; ldp x29, x30, [sp], #16
                  (list #xC0 #x03 #x5F #xD6)       ; ret
                  ;; Exponent = 0: return 1
                  (list #x20 #x02 #x80 #xD2)       ; mov x0, #16 (tagged 1)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ;; Rounding functions
         ;; Since we're working with fixnums (already integers), these are identity operations
         ((eq op 'floor)
          ;; floor(n) for integer n returns n
          (emit-arm64 (first args) env))

         ((eq op 'ceiling)
          ;; ceiling(n) for integer n returns n
          (emit-arm64 (first args) env))

         ((eq op 'truncate)
          ;; truncate(n) for integer n returns n
          (emit-arm64 (first args) env))

         ((eq op 'round)
          ;; round(n) for integer n returns n
          (emit-arm64 (first args) env))

         ;; Two-argument rounding division operators
         ((eq op 'ffloor)
          ;; ffloor(a, b) = floor(a/b) - rounds toward negative infinity
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save dividend)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (divisor to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (dividend back to x0)
                  ;; Save original values for sign check
                  (list #xE4 #x03 #x00 #xAA)       ; mov x4, x0 (save original dividend)
                  (list #xE5 #x03 #x01 #xAA)       ; mov x5, x1 (save original divisor)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag dividend)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag divisor)
                  ;; Compute quotient and remainder
                  (list #xE3 #x0C #xC1 #x9A)       ; sdiv x3, x0, x1 (quotient in x3)
                  (list #x06 #x80 #x01 #x9B)       ; msub x6, x0, x1, x3 (remainder in x6)
                  ;; Check if adjustment needed: remainder != 0 and signs differ
                  (list #xDF #x00 #x00 #xF1)       ; cmp x6, #0
                  (list #x60 #x00 #x00 #x54)       ; b.eq +12 (skip if rem = 0)
                  ;; Check if signs differ by XOR
                  (list #x84 #x00 #x05 #xCA)       ; eor x4, x4, x5 (signs differ if MSB set)
                  (list #x9F #x00 #x00 #xF1)       ; cmp x4, #0
                  (list #x4A #x00 #x00 #x54)       ; b.ge +8 (skip if same sign)
                  ;; Different signs and remainder: subtract 1
                  (list #x63 #x04 #x00 #xD1)       ; sub x3, x3, #1
                  ;; Return quotient
                  (list #xE0 #x03 #x03 #xAA)       ; mov x0, x3
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op 'fceiling)
          ;; fceiling(a, b) = ceiling(a/b) - rounds toward positive infinity
          (append (emit-arm64 (first args) env)
                  (list #xFD #x7B #xBF #xA9)       ; stp x29, x30, [sp, #-16]!
                  (list #xE2 #x03 #x00 #xAA)       ; mov x2, x0 (save dividend)
                  (emit-arm64 (second args) env)
                  (list #xE1 #x03 #x00 #xAA)       ; mov x1, x0 (divisor to x1)
                  (list #xE0 #x03 #x02 #xAA)       ; mov x0, x2 (dividend back to x0)
                  ;; Save original values for sign check
                  (list #xE4 #x03 #x00 #xAA)       ; mov x4, x0 (save original dividend)
                  (list #xE5 #x03 #x01 #xAA)       ; mov x5, x1 (save original divisor)
                  (list #x00 #x10 #x44 #xD3)       ; lsr x0, x0, #4 (untag dividend)
                  (list #x21 #x10 #x44 #xD3)       ; lsr x1, x1, #4 (untag divisor)
                  ;; Compute quotient and remainder
                  (list #xE3 #x0C #xC1 #x9A)       ; sdiv x3, x0, x1 (quotient in x3)
                  (list #x06 #x80 #x01 #x9B)       ; msub x6, x0, x1, x3 (remainder in x6)
                  ;; Check if adjustment needed: remainder != 0 and signs same
                  (list #xDF #x00 #x00 #xF1)       ; cmp x6, #0
                  (list #x60 #x00 #x00 #x54)       ; b.eq +12 (skip if rem = 0)
                  ;; Check if signs same by XOR
                  (list #x84 #x00 #x05 #xCA)       ; eor x4, x4, x5 (signs same if MSB not set)
                  (list #x9F #x00 #x00 #xF1)       ; cmp x4, #0
                  (list #x0B #x00 #x00 #x54)       ; b.lt +8 (skip if different signs)
                  ;; Same signs and remainder: add 1
                  (list #x63 #x04 #x00 #x91)       ; add x3, x3, #1
                  ;; Return quotient
                  (list #xE0 #x03 #x03 #xAA)       ; mov x0, x3
                  (list #x00 #x10 #x00 #xD3)       ; lsl x0, x0, #4 (retag)
                  (list #xFD #x7B #xC1 #xA8)))     ; ldp x29, x30, [sp], #16

         ((eq op 'ftruncate)
          ;; ftruncate(a, b) = truncate(a/b) - rounds toward zero
          ;; For integers, this is the same as regular division
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

         ((eq op 'fround)
          ;; fround(a, b) = round(a/b) - rounds to nearest integer
          ;; Simplified: just use truncate for integers
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

         ;; List operations - require runtime integration
         ;; These operations need heap allocation and are not yet integrated with compiled code.
         ;; They work in the REPL (interpreted mode) which has access to the runtime heap.
         ;; Future work: Implement FFI or compile runtime functions to machine code.
         ;; See docs/RUNTIME_INTEGRATION.md for implementation plan.
         ((eq op 'cons)
          (error "cons requires runtime heap integration~%~
                  Hint: cons works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'car)
          (error "car requires runtime heap integration~%~
                  Hint: car works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'cdr)
          (error "cdr requires runtime heap integration~%~
                  Hint: cdr works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         ((eq op 'list)
          (error "list requires runtime heap integration~%~
                  Hint: list works in the REPL. For compiled code, runtime integration is needed.~%~
                  See docs/RUNTIME_INTEGRATION.md for details."))

         (t
          (error "Unknown operator: ~S" op)))))

    (runtime-call
     ;; Generate ARM64 code to call function via symbol-function slot
     ;; (funcall 'name arg1 arg2 ...)
     (let* ((fn-name (expr-value expr))
            (args (expr-args expr))
            (num-args (length args)))
       ;; Phase 1: Only support 0-3 arguments (matching defun limitation)
       (when (> num-args 3)
         (error "Runtime funcall currently supports up to 3 arguments"))

       ;; Get symbol address at compile time
       (let ((intern-fn (find-symbol "RUNTIME-INTERN" :habu-runtime))
             (sym-addr 0))
         (unless intern-fn
           (error "Runtime not initialized"))
         (unless (symbolp fn-name)
           (error "fn-name should be a symbol, got ~S (type ~S, value ~S)"
                  fn-name (type-of fn-name) (expr-value expr)))
         (setf sym-addr (funcall intern-fn (string fn-name)))

         ;; Generate code:
         ;; 1. Load symbol address into X9
         ;; 2. Read symbol-function slot [X9 + 24] into X9
         ;; 3. Evaluate arguments into X0-X2
         ;; 4. Call via blr x9
         (append
          ;; Load symbol address into X9 (64-bit immediate)
          ;; movz x9, #(sym-addr & 0xFFFF), lsl #0
          (list #x09 (logand (ash sym-addr 0) #xFF) (logand (ash sym-addr -8) #xFF) #xD2)
          ;; movk x9, #((sym-addr >> 16) & 0xFFFF), lsl #16
          (list #x09 (logand (ash sym-addr -16) #xFF) (logand (ash sym-addr -24) #xFF) #xF2)
          ;; movk x9, #((sym-addr >> 32) & 0xFFFF), lsl #32
          (list #x09 (logand (ash sym-addr -32) #xFF) (logand (ash sym-addr -40) #xFF) #xF2)
          ;; movk x9, #((sym-addr >> 48) & 0xFFFF), lsl #48
          (list #x09 (logand (ash sym-addr -48) #xFF) (logand (ash sym-addr -56) #xFF) #xF2)

          ;; Read symbol-function slot: ldr x9, [x9, #24]
          (list #x29 #x31 #x40 #xF9)            ; ldr x9, [x9, #24]

          ;; Evaluate arguments and setup registers (X0, X1, X2)
          (cond
            ((= num-args 0)
             ;; No arguments, just call
             nil)

            ((= num-args 1)
             ;; Evaluate arg into X0 (already there by default)
             (emit-arm64 (first args) env))

            ((= num-args 2)
             ;; Eval arg1 -> X0, then save; eval arg2 -> X0, move to X1; restore X0
             (append
              (emit-arm64 (first args) env)
              (list #xEA #x03 #x00 #xAA)        ; mov x10, x0 (save arg1)
              (emit-arm64 (second args) env)
              (list #xE1 #x03 #x00 #xAA)        ; mov x1, x0 (arg2 -> X1)
              (list #xE0 #x03 #x0A #xAA)))      ; mov x0, x10 (arg1 -> X0)

            ((= num-args 3)
             ;; Eval arg1 -> X0, save; arg2 -> X0, save; arg3 -> X0, move to X2; restore X1, X0
             (append
              (emit-arm64 (first args) env)
              (list #xEA #x03 #x00 #xAA)        ; mov x10, x0 (save arg1)
              (emit-arm64 (second args) env)
              (list #xEB #x03 #x00 #xAA)        ; mov x11, x0 (save arg2)
              (emit-arm64 (third args) env)
              (list #xE2 #x03 #x00 #xAA)        ; mov x2, x0 (arg3 -> X2)
              (list #xE1 #x03 #x0B #xAA)        ; mov x1, x11 (arg2 -> X1)
              (list #xE0 #x03 #x0A #xAA)))      ; mov x0, x10 (arg1 -> X0)

            (t (error "Unsupported number of arguments: ~D" num-args)))

          ;; Call function pointer: blr x9
          (list #x20 #x01 #x3F #xD6)))))))      ; blr x9

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
           (optimized-ir (constant-fold ir))
           (code (ecase arch
                   (:x86_64 (emit-x86_64 optimized-ir))
                   (:arm64 (emit-arm64 optimized-ir)))))
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
