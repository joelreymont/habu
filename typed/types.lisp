;;;; Unified Type System for Habu
;;;;
;;;; One macro to rule them all: deftype
;;;;
;;;; Supports:
;;;; - Sum types (variants)    : (deftype name (ctor1 fields) (ctor2 fields) ...)
;;;; - Product types (records) : (deftype name :record (field1) (field2) ...)
;;;; - Enumerations           : (deftype name :enum :val1 :val2 :val3)
;;;; - Type aliases           : (deftype name := other-type)
;;;;
;;;; All types get:
;;;; - Exhaustiveness-checked match macro
;;;; - Type predicates
;;;; - Constructors
;;;; - Accessors

(defpackage :habu.types
  (:use :cl)
  (:shadow :deftype)
  (:export :deftype :match :match* :*type-registry*
           :type-info :type-kind :type-variants))

(in-package :habu.types)

(defvar *type-registry* (make-hash-table)
  "Maps type-name -> type-info plist")

(defun type-info (name)
  (gethash name *type-registry*))

(defun type-kind (name)
  (getf (type-info name) :kind))

(defun type-variants (name)
  (getf (type-info name) :variants))

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

(defun expand-enum-type (name values)
  `(progn
     (setf (gethash ',name *type-registry*)
           '(:kind :enum :values ,values))
     ;; Predicate
     (defun ,(intern (format nil "~A-P" name)) (x)
       (member x ',values))
     ;; Constructor (identity, validates)
     (defun ,name (x)
       (assert (member x ',values) () "~A: invalid value ~S" ',name x)
       x)
     ',name))

(defun expand-record-type (name fields)
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
         ,@parsed-fields)

       ;; Register in type system for match integration
       (setf (gethash ',name *type-registry*)
             '(:kind :record
               :fields ,field-names
               :constructor ,make-name
               :predicate ,pred-name))
       ',name)))

(defun expand-sum-type (name variants)
  (let ((variant-names (mapcar #'car variants)))
    `(progn
       (setf (gethash ',name *type-registry*)
             '(:kind :sum :variants ,variant-names))

       ;; For each variant: constructor, predicate, accessors
       ,@(loop for variant in variants
               for vname = (car variant)
               for fields = (cdr variant)
               nconc
               (append
                ;; Constructor
                (list `(defun ,vname ,fields
                         (list ,(symbol-to-keyword vname) ,@fields)))
                ;; Predicate
                (list `(defun ,(intern (format nil "~A-P" vname)) (x)
                         (and (consp x)
                              (eq (car x) ,(symbol-to-keyword vname)))))
                ;; Accessors
                (loop for field in fields
                      for i from 1
                      collect
                      `(defun ,(intern (format nil "~A-~A" vname field)) (x)
                         (nth ,i x)))))

       ;; Type predicate (any variant)
       (defun ,(intern (format nil "~A-P" name)) (x)
         (and (consp x)
              (member (car x)
                      ',(mapcar #'symbol-to-keyword variant-names))))
       ',name)))

;;; Main macro

(defmacro deftype (name &body spec)
  "Define a type. Structure determines kind:

   ;; Sum type (multiple constructors)
   (deftype ir-node
     (ir-lit value)
     (ir-var offset)
     (ir-add left right))

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

    ;; Enumeration: (deftype color :enum :red :green :blue)
    ((eq (first spec) ':enum)
     (expand-enum-type name (rest spec)))

    ;; Record: (deftype point :record (x) (y))
    ((eq (first spec) ':record)
     (expand-record-type name (rest spec)))

    ;; Sum type (default): (deftype ir (ir-lit v) (ir-var o) ...)
    (t
     (expand-sum-type name spec))))

;;;; Pattern Matching

(defun expand-sum-match (type-name info expr clauses)
  (let* ((variants (getf info :variants))
         (clause-names (mapcar #'car clauses))
         ;; Compare by symbol-name for cross-package compatibility
         (variant-names (mapcar #'symbol-name variants))
         (clause-name-strs (mapcar #'symbol-name clause-names))
         (missing-strs (set-difference variant-names clause-name-strs :test #'string=))
         (extra-strs (set-difference clause-name-strs variant-names :test #'string=)))
    (when missing-strs
      (error "match ~A: MISSING variants ~S" type-name missing-strs))
    (when extra-strs
      (error "match ~A: UNKNOWN variants ~S" type-name extra-strs))
    (let ((val (gensym "VAL")))
      `(let ((,val ,expr))
         (ecase (car ,val)
           ,@(loop for (vname fields . body) in clauses
                   collect `(,(symbol-to-keyword vname)
                             (destructuring-bind ,fields (cdr ,val)
                               ,@body))))))))

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

(defmacro match (type-name expr &body clauses)
  "Pattern match with exhaustiveness checking.

   (match ir-node my-ir
     (ir-lit (value) body...)
     (ir-var (offset) body...)
     ...)

   COMPILE-TIME ERROR if any variant missing!"
  (let* ((info (gethash type-name *type-registry*))
         (kind (getf info :kind)))
    (ecase kind
      (:sum    (expand-sum-match type-name info expr clauses))
      (:enum   (expand-enum-match type-name info expr clauses))
      (:record (expand-record-match type-name info expr clauses))
      ((nil)   (error "match: unknown type ~A" type-name)))))

;;;; Partial Match (escape hatch)

(defmacro match* (type-name expr &body clauses)
  "Like match but no exhaustiveness check. Use sparingly!"
  (declare (ignore type-name))
  (let ((val (gensym "VAL")))
    `(let ((,val ,expr))
       (case (car ,val)
         ,@(loop for (vname fields . body) in clauses
                 collect `(,(symbol-to-keyword vname)
                           (destructuring-bind ,fields (cdr ,val)
                             ,@body)))
         (t (error "match*: unhandled ~S" (car ,val)))))))
