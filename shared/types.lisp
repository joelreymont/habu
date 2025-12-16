;;;; Unified Type System for Habu
;;;;
;;;; One macro to rule them all: deftype
;;;;
;;;; Supports:
;;;; - Sum types (variants)    : (deftype name (ctor1 fields) (ctor2 fields) ...)
;;;; - Sum types with prefix   : (deftype name :prefix pfx (ctor1 fields) ...) -> pfx-ctor1
;;;; - Product types (records) : (deftype name :record (field1) (field2) ...)
;;;; - Enumerations           : (deftype name :enum :val1 :val2 :val3)
;;;; - Type aliases           : (deftype name := other-type)
;;;;
;;;; All types get:
;;;; - Exhaustiveness-checked match macro
;;;; - Type predicates
;;;; - Constructors
;;;; - Accessors
;;;;
;;;; With :prefix, match patterns use short names:
;;;;   (deftype ir-node :prefix ir (lit value) (add left right))
;;;;   (match ir-node x (lit (v) ...) (add (l r) ...))  ; not ir-lit, ir-add

(defpackage :habu.types
  (:use :cl)
  (:shadow :deftype)
  (:export :deftype :match :*type-registry* :*variant-to-type*
           :type-info :type-kind :type-variants :type-docstring
           ;; Code marker ADT
           :code-marker :code-marker-p
           :marker-call-fn :marker-call-fn-p :marker-call-fn-name
           :marker-tail-call-fn :marker-tail-call-fn-p :marker-tail-call-fn-name
           :marker-extern-call :marker-extern-call-p :marker-extern-call-name
           :marker-branch :marker-branch-p :marker-branch-label
           :marker-branch-ne :marker-branch-ne-p :marker-branch-ne-label
           :marker-branch-eq :marker-branch-eq-p :marker-branch-eq-label
           :marker-lambda-ref :marker-lambda-ref-p :marker-lambda-ref-name
           :marker-fn-label :marker-fn-label-p :marker-fn-label-name
           :marker-loop-start :marker-loop-start-p :marker-loop-start-id
           :marker-loop-continue :marker-loop-continue-p :marker-loop-continue-id
           :marker-block-start :marker-block-start-p :marker-block-start-name
           :marker-block-end :marker-block-end-p :marker-block-end-name
           :marker-return-from :marker-return-from-p :marker-return-from-name
           :marker-tco-branch :marker-tco-branch-p :marker-tco-branch-target
           :marker-funcall-marker :marker-funcall-marker-p :marker-funcall-marker-arity
           :marker-heap-alloc :marker-heap-alloc-p :marker-heap-alloc-size
           ;; Unresolved tracking
           :*unresolved-markers* :record-unresolved :clear-unresolved :report-unresolved))

(in-package :habu.types)

(defvar *type-registry* (make-hash-table)
  "Maps type-name -> type-info plist")

(defun type-info (name)
  (gethash name *type-registry*))

(defun type-kind (name)
  (getf (type-info name) :kind))

(defun type-variants (name)
  (getf (type-info name) :variants))

(defun type-docstring (name)
  "Get the docstring for a type, or nil if none."
  (getf (type-info name) :docstring))

;;; Helper to make keyword from symbol
(defun symbol-to-keyword (sym)
  (intern (symbol-name sym) :keyword))

;;; Expansion functions for each type kind

(defun expand-type-alias (name target)
  `(progn
     (setf (gethash ',name *type-registry*)
           '(:kind :alias :target ,target))
     ;; Predicate delegates to target
     (defun ,(intern (format nil "~A-P" name)) (x)
       (,(intern (format nil "~A-P" target)) x))
     ',name))

(defun expand-enum-type (name values &optional docstring)
  `(progn
     (setf (gethash ',name *type-registry*)
           '(:kind :enum :values ,values :docstring ,docstring))
     ;; Predicate
     (defun ,(intern (format nil "~A-P" name)) (x)
       (member x ',values))
     ;; Constructor (identity, validates)
     (defun ,name (x)
       (assert (member x ',values) () "~A: invalid value ~S" ',name x)
       x)
     ',name))

(defun expand-record-type (name fields &optional docstring)
  "Expand record type using defstruct internally.
   Fields can be: field-name or (field-name default)"
  (let* ((parsed-fields (mapcar (lambda (f)
                                  (if (consp f)
                                      (list (car f) (cadr f))
                                      (list f nil)))
                                fields))
         (field-names (mapcar #'car parsed-fields))
         (make-name (intern (format nil "MAKE-~A" name)))
         (copy-name (intern (format nil "COPY-~A" name)))
         (pred-name (intern (format nil "~A-P" name))))
    `(progn
       ;; Use defstruct for efficient implementation
       (cl:defstruct (,name
                      (:constructor ,make-name)
                      (:copier ,copy-name)
                      (:predicate ,pred-name))
         ,@(when docstring (list docstring))
         ,@parsed-fields)

       ;; Register in type system for match integration
       (setf (gethash ',name *type-registry*)
             '(:kind :record
               :fields ,field-names
               :constructor ,make-name
               :predicate ,pred-name
               :docstring ,docstring))
       ',name)))

(defvar *variant-to-type* (make-hash-table)
  "Maps variant keyword -> (type-name . short-name) for type inference in match")

(defun expand-sum-type (name variants &optional prefix docstring)
  "Expand sum type definition.
   If PREFIX is given, variant names become prefix-variant (e.g., ir-lit).
   Short names are stored for match pattern resolution.
   Values are self-describing: (type-keyword variant-keyword . fields)
   DOCSTRING is optional documentation for the type."
  (let* ((short-names (mapcar #'car variants))
         (full-names (if prefix
                         (mapcar (lambda (v)
                                   (intern (format nil "~A-~A" prefix v)))
                                 short-names)
                         short-names))
         (type-kw (symbol-to-keyword name))
         (variant-kws (mapcar #'symbol-to-keyword short-names))
         ;; Map short name -> full name for match resolution
         (name-map (mapcar #'cons short-names full-names)))
    `(progn
       (setf (gethash ',name *type-registry*)
             '(:kind :sum
               :prefix ,prefix
               :variants ,full-names
               :short-names ,short-names
               :name-map ,name-map
               :type-keyword ,type-kw
               :variant-keywords ,variant-kws
               :docstring ,docstring))

       ;; Register reverse mapping: variant-kw -> (type-name . short-name)
       ,@(loop for short in short-names
               for var-kw in variant-kws
               collect `(setf (gethash ,var-kw *variant-to-type*)
                              '(,name . ,short)))

       ;; For each variant: constructor, predicate, accessors
       ,@(loop for variant in variants
               for short-name = (car variant)
               for full-name in full-names
               for var-kw in variant-kws
               for fields = (cdr variant)
               nconc
               (append
                ;; Constructor: (type-kw variant-kw . fields) - self-describing
                (list `(defun ,full-name ,fields
                         (list ,type-kw ,var-kw ,@fields)))
                ;; Predicate: check type AND variant
                (list `(defun ,(intern (format nil "~A-P" full-name)) (x)
                         (and (consp x)
                              (eq (car x) ,type-kw)
                              (eq (cadr x) ,var-kw))))
                ;; Accessors at offset 2+ (type-kw at 0, variant-kw at 1)
                (loop for field in fields
                      for i from 2
                      collect
                      `(defun ,(intern (format nil "~A-~A" full-name field)) (x)
                         (nth ,i x)))))

       ;; Type predicate (any variant of this type)
       (defun ,(intern (format nil "~A-P" name)) (x)
         (and (consp x)
              (eq (car x) ,type-kw)))
       ',name)))

;;; Main macro

(defmacro deftype (name &body spec)
  "Define a type. Structure determines kind:

   ;; Sum type (multiple constructors)
   (deftype ir-node
     (ir-lit value)
     (ir-var offset)
     (ir-add left right))

   ;; Sum type with prefix (DRY variant names)
   (deftype ir-node :prefix ir
     (lit value)          ; generates ir-lit
     (var offset)         ; generates ir-var
     (add left right))    ; generates ir-add

   ;; Sum type with docstring
   (deftype ir-node :prefix ir
     \"IR nodes for the compiler.\"
     (lit value)
     (var offset))

   ;; Record type (single product)
   (deftype point :record
     (x integer)
     (y integer))

   ;; Enumeration
   (deftype color :enum
     :red :green :blue)

   ;; Alias
   (deftype tagged-int := integer)"
  (cond
    ;; Type alias: (deftype foo := bar)
    ((and (>= (length spec) 2) (eq (first spec) ':=))
     (expand-type-alias name (second spec)))

    ;; Enumeration: (deftype color :enum [docstring] :red :green :blue)
    ((eq (first spec) ':enum)
     (let* ((rest (cdr spec))
            (docstring (when (stringp (car rest)) (car rest)))
            (values (if docstring (cdr rest) rest)))
       (expand-enum-type name values docstring)))

    ;; Record: (deftype point :record [docstring] (x) (y))
    ((eq (first spec) ':record)
     (let* ((rest (cdr spec))
            (docstring (when (stringp (car rest)) (car rest)))
            (fields (if docstring (cdr rest) rest)))
       (expand-record-type name fields docstring)))

    ;; Sum type with prefix: (deftype ir-node :prefix ir [docstring] (lit v) ...)
    ((eq (first spec) ':prefix)
     (let* ((prefix (second spec))
            (rest (cddr spec))
            ;; Check if first element after prefix is a docstring
            (docstring (when (stringp (car rest)) (car rest)))
            (variants (if docstring (cdr rest) rest)))
       (expand-sum-type name variants prefix docstring)))

    ;; Sum type (default): (deftype ir [docstring] (ir-lit v) (ir-var o) ...)
    (t
     (let* (;; Check if first element is a docstring
            (docstring (when (stringp (car spec)) (car spec)))
            (variants (if docstring (cdr spec) spec)))
       (expand-sum-type name variants nil docstring)))))

;;;; Pattern Matching

(defun resolve-variant-name (clause-name info)
  "Resolve a clause name to a full variant name.
   If type has :prefix, short names are accepted and mapped to full names.
   Returns the full name or nil if not found."
  (let ((prefix (getf info :prefix))
        (name-map (getf info :name-map))
        (full-names (getf info :variants)))
    (cond
      ;; No prefix - clause name should be a full name
      ((null prefix)
       (if (member clause-name full-names :test #'string= :key #'symbol-name)
           clause-name
           nil))
      ;; Has prefix - check short names first, then full names
      (t
       (let ((mapped (cdr (assoc clause-name name-map
                                 :test (lambda (a b)
                                         (string= (symbol-name a) (symbol-name b)))))))
         (or mapped
             ;; Also accept full names directly
             (if (member clause-name full-names :test #'string= :key #'symbol-name)
                 clause-name
                 nil)))))))

(defun wildcard-clause-p (clause)
  "Check if clause is a wildcard: (_ body...) or (otherwise body...)"
  (let ((name (car clause)))
    (or (eq name '_)
        (eq name 'otherwise)
        (string= (symbol-name name) "_")
        (string= (symbol-name name) "OTHERWISE"))))

(defun expand-sum-match (type-name info expr clauses)
  "Expand match for sum types. Value format: (type-kw variant-kw . fields)
   Supports wildcard pattern: (_ body...) or (otherwise body...) catches all unhandled.
   Wildcards make match exhaustive without listing every variant explicitly."
  (let* ((full-variants (getf info :variants))
         (short-names (getf info :short-names))
         (variant-kws (getf info :variant-keywords))
         (prefix (getf info :prefix))
         ;; Separate wildcard from regular clauses
         (wildcard-clause (find-if #'wildcard-clause-p clauses))
         (regular-clauses (remove-if #'wildcard-clause-p clauses))
         ;; Resolve each regular clause name to (full-name . variant-kw)
         (resolved-clauses
           (loop for (cname fields . body) in regular-clauses
                 for full-name = (resolve-variant-name cname info)
                 for idx = (position full-name full-variants :test #'string= :key #'symbol-name)
                 for var-kw = (and idx (nth idx variant-kws))
                 do (unless full-name
                      (error "match ~A: unknown variant ~S~@[ (valid: ~{~A~^, ~})~]"
                             type-name cname (or short-names full-variants)))
                 collect (list* var-kw fields body)))
         ;; Check exhaustiveness (only if no wildcard)
         (resolved-kws (mapcar #'car resolved-clauses))
         (missing (set-difference variant-kws resolved-kws))
         (duplicates (loop for kw in resolved-kws
                           when (> (count kw resolved-kws) 1)
                           collect kw)))
    ;; Only error on missing if no wildcard
    (when (and missing (not wildcard-clause))
      (error "match ~A: MISSING variants ~S~@[ (use short names: ~{~A~^, ~})~]~%  Hint: use (_ ...) to handle all remaining cases"
             type-name missing (when prefix short-names)))
    (when duplicates
      (error "match ~A: DUPLICATE variants ~S" type-name (remove-duplicates duplicates)))
    (let ((val (gensym "VAL")))
      (if wildcard-clause
          ;; With wildcard: use case with t default
          `(let ((,val ,expr))
             (case (cadr ,val)  ; variant-kw at position 1
               ,@(loop for (var-kw fields . body) in resolved-clauses
                       collect `(,var-kw
                                 (destructuring-bind ,fields (cddr ,val)
                                   ,@body)))
               (t ,@(cdr wildcard-clause))))  ; wildcard body
          ;; Without wildcard: use ecase (runtime error on unknown)
          `(let ((,val ,expr))
             (ecase (cadr ,val)  ; variant-kw at position 1
               ,@(loop for (var-kw fields . body) in resolved-clauses
                       collect `(,var-kw
                                 (destructuring-bind ,fields (cddr ,val)
                                   ,@body)))))))))

(defun expand-enum-match (type-name info expr clauses)
  (let* ((values (getf info :values))
         (clause-vals (mapcar #'car clauses))
         (missing (set-difference values clause-vals))
         (extra (set-difference clause-vals values)))
    (when missing
      (error "match ~A: MISSING values ~S" type-name missing))
    (when extra
      (error "match ~A: UNKNOWN values ~S" type-name extra))
    `(ecase ,expr
       ,@(loop for (val . body) in clauses
               collect `(,val ,@body)))))

(defun expand-record-match (type-name info expr clauses)
  "Expand match for record/struct types.
   Binds fields using struct accessors."
  (unless (= (length clauses) 1)
    (error "match ~A: record types have exactly one pattern" type-name))
  (let* ((clause (car clauses))
         (pattern-name (first clause))
         (pattern-fields (second clause))
         (body (cddr clause))
         (registered-fields (getf info :fields))
         (obj (gensym "OBJ")))
    (declare (ignore pattern-name))
    ;; Verify field count matches
    (unless (= (length pattern-fields) (length registered-fields))
      (error "match ~A: expected ~D fields, got ~D"
             type-name (length registered-fields) (length pattern-fields)))
    ;; Generate accessor bindings
    `(let* ((,obj ,expr)
            ,@(loop for pat-field in pattern-fields
                    for reg-field in registered-fields
                    collect `(,pat-field (,(intern (format nil "~A-~A" type-name reg-field))
                                          ,obj))))
       ,@body)))

(defun infer-type-from-clauses (clauses)
  "Try to infer ADT type from clause variant names using *variant-to-type* registry.
   Returns type-name or nil if can't infer."
  (let ((first-clause-name (caar clauses)))
    (when (symbolp first-clause-name)
      (let ((entry (gethash (symbol-to-keyword first-clause-name) *variant-to-type*)))
        (when entry (car entry))))))

(defmacro match (type-name-or-expr &body clauses-or-expr-and-clauses)
  "Pattern match with exhaustiveness checking.

   Explicit type (compile-time exhaustiveness):
   (match ir-node my-ir
     (lit (value) body...)
     (var (offset) body...))

   Inferred type (runtime exhaustiveness via self-describing values):
   (match my-ir
     (lit (value) body...)
     (var (offset) body...))

   For inferred match, values must be created with new-style constructors
   that embed type: (type-kw variant-kw . fields)

   COMPILE-TIME ERROR if any variant missing (explicit type)
   RUNTIME ERROR if any variant missing (inferred type)"
  ;; Check if first arg is a known type name
  (let ((info (and (symbolp type-name-or-expr)
                   (gethash type-name-or-expr *type-registry*))))
    (if info
        ;; Explicit type: (match type-name expr clauses...)
        (let ((expr (car clauses-or-expr-and-clauses))
              (clauses (cdr clauses-or-expr-and-clauses))
              (kind (getf info :kind)))
          (ecase kind
            (:sum    (expand-sum-match type-name-or-expr info expr clauses))
            (:enum   (expand-enum-match type-name-or-expr info expr clauses))
            (:record (expand-record-match type-name-or-expr info expr clauses))))
        ;; Inferred type: (match expr clauses...)
        ;; Try to infer type from clause names for compile-time checking
        (let* ((expr type-name-or-expr)
               (clauses clauses-or-expr-and-clauses)
               (inferred-type (infer-type-from-clauses clauses))
               (inferred-info (and inferred-type (gethash inferred-type *type-registry*))))
          (if inferred-info
              ;; Found type - use compile-time exhaustiveness
              (expand-sum-match inferred-type inferred-info expr clauses)
              ;; Can't infer - generate runtime dispatch with runtime check
              (let ((val (gensym "VAL")))
                `(let ((,val ,expr))
                   (case (cadr ,val)  ; variant-kw is at position 1
                     ,@(loop for (vname fields . body) in clauses
                             collect `(,(symbol-to-keyword vname)
                                       (destructuring-bind ,fields (cddr ,val)
                                         ,@body)))
                     (t (error "match: unhandled variant ~S in ~S" (cadr ,val) ,val))))))))))


;;;; ============================================================
;;;; Code Marker ADT
;;;; ============================================================
;;;;
;;;; Markers are placeholders in generated code that get resolved
;;;; during linking. Using an ADT ensures exhaustive handling.

(deftype code-marker :prefix marker
  ;; Call markers - need linker resolution
  (call-fn name)              ; BL to internal function
  (tail-call-fn name)         ; B to internal function (TCO)
  (extern-call name)          ; BL to external C function

  ;; Branch markers - need offset resolution
  (branch label)              ; unconditional branch
  (branch-ne label)           ; branch if not equal
  (branch-eq label)           ; branch if equal

  ;; Reference markers
  (lambda-ref name)           ; reference to lambda (closure)
  (fn-label name)             ; function entry point label

  ;; Loop markers
  (loop-start id)             ; loop entry point
  (loop-continue id)          ; continue to loop start

  ;; Block markers (for return-from)
  (block-start name)          ; block entry
  (block-end name)            ; block exit
  (return-from name)          ; non-local return

  ;; Other
  (tco-branch target)         ; tail call optimization branch
  (funcall-marker arity)      ; dynamic funcall
  (heap-alloc size))          ; heap allocation point

;;; Unresolved marker tracking
(defvar *unresolved-markers* nil
  "List of (offset marker) pairs that couldn't be resolved")

(defun record-unresolved (offset marker)
  "Record an unresolved marker. Call this during linking."
  (push (list offset marker) *unresolved-markers*))

(defun clear-unresolved ()
  "Clear unresolved marker list before new compilation."
  (setf *unresolved-markers* nil))

(defun report-unresolved ()
  "Report all unresolved markers. Errors if any exist."
  (when *unresolved-markers*
    (format *error-output* "~%=== UNRESOLVED MARKERS ===~%")
    (let ((by-type (make-hash-table)))
      ;; Group by marker type
      (dolist (entry *unresolved-markers*)
        (let* ((marker (second entry))
               (type (if (consp marker) (car marker) marker)))
          (push entry (gethash type by-type))))
      ;; Report each type
      (maphash (lambda (type entries)
                 (format *error-output* "~%~A (~D occurrences):~%" type (length entries))
                 (dolist (e (subseq entries 0 (min 5 (length entries))))
                   (format *error-output* "  offset ~X: ~S~%" (first e) (second e)))
                 (when (> (length entries) 5)
                   (format *error-output* "  ... and ~D more~%" (- (length entries) 5))))
               by-type))
    (error "~D unresolved markers - implementation incomplete"
           (length *unresolved-markers*))))
