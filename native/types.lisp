;;; types.lisp - Type system for habu0 self-hosting
;;;
;;; Simplified version of typed/types.lisp compatible with habu0 primitives.
;;; Provides deftype for ADTs and match for pattern matching.
;;;
;;; Key differences from typed/types.lisp:
;;; - Uses alists instead of hash-tables
;;; - No compile-time exhaustiveness checking (runtime error instead)
;;; - No format (uses string-concat for messages)
;;; - Simpler symbol handling

;;; For SBCL bootstrap testing, provide shims for habu0 primitives
#+sbcl
(progn
  (defun make-symbol-from-string (str)
    (make-symbol str))
  (defun make-keyword-from-string (str)
    (intern str :keyword))
  (defun string-concat (s1 s2)
    (concatenate 'string s1 s2))
  (defun make-vector (n)
    (make-array n :element-type 'character))
  (defun vector-set (vec idx val)
    (setf (aref vec idx) (code-char val)))
  (defun make-string-from-vector (vec)
    (coerce vec 'string))
  (defun setcdr (cons new-cdr)
    (setf (cdr cons) new-cdr)
    cons)
  (defun set-cdr (cons new-cdr)
    (setf (cdr cons) new-cdr)))

;;; Type registry - alist of (type-name . type-info-plist)
(defvar *habu-type-registry* nil)

;;; Gensym counter for unique symbol generation
(defvar *habu-gensym-counter* 0)

;;; Generate a unique uninterned symbol
(defun habu-gensym (prefix)
  (setq *habu-gensym-counter* (+ *habu-gensym-counter* 1))
  (make-symbol-from-string
   (string-concat prefix
                  (string-concat "-" (number-to-string *habu-gensym-counter*)))))

;;; Convert number to string (simple implementation for gensym)
(defun number-to-string (n)
  (if (< n 0)
      (string-concat "-" (number-to-string (- 0 n)))
      (if (< n 10)
          (make-string-from-char-code (+ 48 n))  ; 48 = ASCII '0'
          (string-concat (number-to-string (/ n 10))
                        (number-to-string (mod n 10))))))

;;; Make a single-character string from a character code
(defun make-string-from-char-code (code)
  (let ((v (make-vector 1)))
    (vector-set v 0 code)
    (make-string-from-vector v)))

;;; Type registry access
(defun type-info (name)
  "Get type info plist for NAME"
  (let ((entry (assoc name *habu-type-registry*)))
    (if entry (cdr entry) nil)))

(defun register-type (name info)
  "Register type NAME with info plist INFO"
  (let ((existing (assoc name *habu-type-registry*)))
    (if existing
        (setcdr existing info)
        (setq *habu-type-registry* (cons (cons name info) *habu-type-registry*)))))

;;; Plist helpers (since habu0 doesn't have getf/putf)
(defun plist-get (plist key)
  "Get value for KEY in PLIST"
  (if (null plist)
      nil
      (if (eq (car plist) key)
          (cadr plist)
          (plist-get (cddr plist) key))))

(defun plist-put (plist key value)
  "Set KEY to VALUE in PLIST, returns new plist"
  (if (null plist)
      (list key value)
      (if (eq (car plist) key)
          (cons key (cons value (cddr plist)))
          (cons (car plist) (cons (cadr plist)
                                  (plist-put (cddr plist) key value))))))

;;; Convert symbol to keyword (for variant tags)
(defun symbol-to-keyword (sym)
  "Convert symbol to keyword"
  (make-keyword-from-string (symbol-name sym)))

;;; Build error messages without format
(defun type-error-msg (type-name msg details)
  "Build error message string"
  (string-concat (string-concat (symbol-name type-name) ": ")
                (string-concat msg
                              (if details
                                  (string-concat " " (symbol-name details))
                                  ""))))

;;; ==========================================================
;;; DEFTYPE macro - define algebraic data types
;;; ==========================================================

(defmacro habu-deftype (name &rest spec)
  "Define a type. Supports sum types with optional :prefix.

   Sum type:
     (deftype ir-node
       (ir-lit value)
       (ir-var offset)
       (ir-add left right))

   Sum type with prefix:
     (deftype ir-node :prefix ir
       (lit value)          ; generates ir-lit constructor
       (var offset)         ; generates ir-var constructor
       (add left right))    ; generates ir-add constructor

   Types are represented as tagged lists: (:ir-lit 42)"
  (if (eq (car spec) ':prefix)
      ;; Sum type with prefix
      (expand-sum-type name (cddr spec) (cadr spec))
      ;; Sum type without prefix
      (expand-sum-type name spec nil)))

;;; Expand sum type definition
(defun expand-sum-type (name variants prefix)
  "Generate code for sum type definition.
   Each variant becomes a constructor, predicate, and accessors."
  (let* ((short-names (collect-variant-names variants))
         (full-names (if prefix
                        (add-prefix-to-names short-names prefix)
                        short-names)))
    `(progn
       ;; Register type metadata
       (register-type ',name
                     (list ':kind ':sum
                           ':prefix ',prefix
                           ':variants ',full-names
                           ':short-names ',short-names))

       ;; Generate constructor, predicate, and accessors for each variant
       ,@(generate-variant-functions variants full-names)

       ;; Type predicate (checks if x is any variant)
       (defun ,(make-type-predicate-name name) (x)
         (and (consp x)
              (member-keyword (car x) ',full-names)))

       ;; Return type name
       ',name)))

;;; Collect variant names from variant specs
(defun collect-variant-names (variants)
  "Extract just the names from variant specs: ((name fields...) ...) -> (name ...)"
  (if (null variants)
      nil
      (cons (caar variants)
            (collect-variant-names (cdr variants)))))

;;; Add prefix to all names
(defun add-prefix-to-names (names prefix)
  "Prepend prefix- to each name"
  (if (null names)
      nil
      (cons (make-prefixed-name prefix (car names))
            (add-prefix-to-names (cdr names) prefix))))

;;; Create prefixed symbol name
(defun make-prefixed-name (prefix name)
  "Make symbol PREFIX-NAME"
  (intern (string-concat (string-concat (symbol-name prefix) "-")
                        (symbol-name name))))

;;; Generate all functions for variants
(defun generate-variant-functions (variants full-names)
  "Generate constructor, predicate, and accessor functions for each variant"
  (if (null variants)
      nil
      (let* ((variant (car variants))
             (full-name (car full-names))
             (fields (cdr variant)))
        (append (generate-variant-constructor full-name fields)
                (list (generate-variant-predicate full-name))
                (generate-variant-accessors full-name fields)
                (generate-variant-functions (cdr variants) (cdr full-names))))))

;;; Generate constructor function
(defun generate-variant-constructor (name fields)
  "Generate: (defun NAME (fields...) (list :NAME fields...))"
  (list `(defun ,name ,fields
           (cons ,(symbol-to-keyword name) (list ,@fields)))))

;;; Generate predicate function
(defun generate-variant-predicate (name)
  "Generate: (defun NAME-P (x) (and (consp x) (eq (car x) :NAME)))"
  (let ((pred-name (make-predicate-name name)))
    `(defun ,pred-name (x)
       (and (consp x)
            (eq (car x) ,(symbol-to-keyword name))))))

;;; Generate accessor functions
(defun generate-variant-accessors (name fields)
  "Generate: (defun NAME-FIELD (x) (nth N (cdr x))) for each field"
  (generate-accessors-loop name fields 1))

(defun generate-accessors-loop (name fields index)
  (if (null fields)
      nil
      (cons `(defun ,(make-accessor-name name (car fields)) (x)
               (nth ,(- index 1) (cdr x)))
            (generate-accessors-loop name (cdr fields) (+ index 1)))))

;;; Name construction helpers
(defun make-predicate-name (name)
  "Make NAME-P"
  (intern (string-concat (symbol-name name) "-P")))

(defun make-type-predicate-name (name)
  "Make NAME-P"
  (intern (string-concat (symbol-name name) "-P")))

(defun make-accessor-name (variant-name field-name)
  "Make VARIANT-FIELD"
  (intern (string-concat (string-concat (symbol-name variant-name) "-")
                        (symbol-name field-name))))

;;; Helper for member with keyword comparison
(defun member-keyword (kw name-list)
  "Check if keyword KW matches symbol-to-keyword of any name in list"
  (if (null name-list)
      nil
      (if (eq kw (symbol-to-keyword (car name-list)))
          t
          (member-keyword kw (cdr name-list)))))

;;; ==========================================================
;;; MATCH macro - pattern matching with runtime dispatch
;;; ==========================================================

(defmacro habu-match (type-name expr &rest clauses)
  "Pattern match on sum type with runtime exhaustiveness checking.

   (match ir-node my-node
     (lit (value) ...)
     (var (offset) ...)
     (add (left right) ...))

   If type has :prefix, use short names in patterns."
  (let* ((info (type-info type-name))
         (val-sym (habu-gensym "VAL")))
    (if (null info)
        (error (type-error-msg type-name "unknown type" nil))
        `(let ((,val-sym ,expr))
           ,(expand-match-cases type-name info val-sym clauses)))))

;;; Expand match cases into nested if/eq chain
(defun expand-match-cases (type-name info val-sym clauses)
  "Generate if/eq dispatch chain for match clauses"
  (let* ((variants (plist-get info ':variants))
         (short-names (plist-get info ':short-names))
         (prefix (plist-get info ':prefix)))
    ;; TODO: Runtime exhaustiveness check would go here
    ;; For now, generate the dispatch
    (expand-case-chain val-sym clauses variants short-names prefix)))

;;; Generate if/eq dispatch chain
(defun expand-case-chain (val-sym clauses variants short-names prefix)
  "Generate: (if (eq (car val) :VAR1) ... (if (eq (car val) :VAR2) ... (error)))"
  (if (null clauses)
      ;; No more clauses - error
      `(error "match: unhandled case")
      (let* ((clause (car clauses))
             (clause-name (car clause))
             (fields (cadr clause))
             (body (cddr clause))
             ;; Resolve short name to full name if prefix exists
             (full-name (if prefix
                           (resolve-variant-name clause-name short-names variants)
                           clause-name)))
        `(if (eq (car ,val-sym) ,(symbol-to-keyword full-name))
             (destructuring-bind-list ,fields (cdr ,val-sym)
               ,@body)
             ,(expand-case-chain val-sym (cdr clauses) variants short-names prefix)))))

;;; Resolve short name to full name using parallel lists
(defun resolve-variant-name (name short-names full-names)
  "Find NAME in short-names and return corresponding full-name"
  (if (null short-names)
      name  ; Not found, assume it's already a full name
      (if (eq name (car short-names))
          (car full-names)
          (resolve-variant-name name (cdr short-names) (cdr full-names)))))

;;; Simplified destructuring-bind for lists (habu0 doesn't have full destructuring-bind)
(defmacro destructuring-bind-list (pattern list-expr &rest body)
  "Bind PATTERN variables to elements of list produced by LIST-EXPR"
  (let ((list-sym (habu-gensym "LST")))
    `(let ((,list-sym ,list-expr))
       ,(expand-destructuring-bindings pattern list-sym body))))

;;; Expand destructuring bindings into nested lets
(defun expand-destructuring-bindings (pattern list-sym body)
  "Generate nested let forms to bind pattern variables"
  (if (null pattern)
      `(progn ,@body)
      `(let ((,(car pattern) (car ,list-sym)))
         ,(expand-destructuring-bindings (cdr pattern) `(cdr ,list-sym) body))))

;;; ==========================================================
;;; NTH function (needed for accessors) - only for native habu0
;;; ==========================================================

#-sbcl
(defun nth (n lst)
  "Return the Nth element of LST (0-indexed)"
  (if (null lst)
      nil
      (if (<= n 0)
          (car lst)
          (nth (- n 1) (cdr lst)))))

;;; ==========================================================
;;; SETCDR primitive (needed for plist-put) - only for native habu0
;;; ==========================================================

#-sbcl
(defun setcdr (cons new-cdr)
  "Destructively set the cdr of CONS to NEW-CDR"
  (set-cdr cons new-cdr)
  cons)

;;; Export type registry for inspection
;;; (In a module system, this would be explicit exports)
