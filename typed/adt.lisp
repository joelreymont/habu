;;;; adt.lisp - Algebraic Data Types for Lisp
;;;;
;;;; Provides compile-time exhaustiveness checking via match macro.
;;;; Types are tagged lists: (:ir-lit 42), (:ir-add left right)

(defpackage :adt
  (:use :cl)
  (:export #:defadt #:match #:match* #:type-variants))
(in-package :adt)

;;; Type registry - maps type-name -> list of variant names
(defvar *type-registry* (make-hash-table :test 'eq))

(defun type-variants (type-name)
  "Get list of variant names for a type"
  (gethash type-name *type-registry*))

(defun make-tag (sym)
  "Convert symbol to keyword tag"
  (intern (symbol-name sym) :keyword))

;;; defadt macro - define algebraic data type
(defmacro defadt (type-name &body variants)
  "Define an algebraic data type with variants.
   Each variant: (name field1 field2 ...)

   Creates:
   - Constructor: (name f1 f2) -> (:name f1 f2)
   - Predicate: (name-p x) -> t/nil
   - Accessors: (name-field x) -> value
   - Type predicate: (type-name-p x) -> t/nil"
  (let* ((variant-names (mapcar #'car variants))
         (tag-keywords (mapcar #'make-tag variant-names))
         (type-pred-name (intern (format nil "~A-P" type-name))))
    `(progn
       ;; Register type
       (setf (gethash ',type-name *type-registry*) ',variant-names)

       ;; Generate variant definitions
       ,@(mapcan
          (lambda (variant tag)
            (let* ((vname (car variant))
                   (fields (cdr variant))
                   (pred-name (intern (format nil "~A-P" vname))))
              (append
               ;; Constructor
               (list `(defun ,vname ,fields
                        (list ,tag ,@fields)))
               ;; Predicate
               (list `(defun ,pred-name (x)
                        (and (consp x) (eq (car x) ,tag))))
               ;; Accessors
               (loop for field in fields
                     for i from 1
                     collect `(defun ,(intern (format nil "~A-~A" vname field)) (x)
                                (nth ,i x))))))
          variants tag-keywords)

       ;; Type predicate (any variant)
       (defun ,type-pred-name (x)
         (and (consp x) (member (car x) ',tag-keywords)))

       ',type-name)))

;;; match macro with exhaustiveness checking
(defmacro match (type-name expr &body clauses)
  "Pattern match with COMPILE-TIME exhaustiveness checking.

   (match ir my-expr
     (ir-lit (v) (process v))
     (ir-var (off) (lookup off))
     (ir-add (l r) (+ l r)))

   ERROR if any variant of type-name is missing!"
  (let* ((variants (gethash type-name *type-registry*))
         (clause-names (mapcar #'car clauses))
         (missing (set-difference variants clause-names))
         (extra (set-difference clause-names variants)))
    ;; Compile-time checks
    (when (null variants)
      (error "match: unknown type ~A" type-name))
    (when missing
      (error "match ~A: MISSING variants ~S" type-name missing))
    (when extra
      (error "match ~A: UNKNOWN variants ~S" type-name extra))
    ;; Generate code
    (let ((val (gensym "VAL")))
      `(let ((,val ,expr))
         (ecase (car ,val)
           ,@(loop for clause in clauses
                   for vname = (car clause)
                   for fields = (cadr clause)
                   for body = (cddr clause)
                   for tag = (make-tag vname)
                   collect
                   `(,tag
                     (let ,(loop for f in fields
                                 for i from 1
                                 collect `(,f (nth ,i ,val)))
                       ,@body))))))))

;;; match* - partial match (escape hatch, no exhaustiveness)
(defmacro match* (type-name expr &body clauses)
  "Like match but allows partial matching. Use sparingly!"
  (declare (ignore type-name))
  (let ((val (gensym "VAL")))
    `(let ((,val ,expr))
       (case (car ,val)
         ,@(loop for clause in clauses
                 for vname = (car clause)
                 for fields = (cadr clause)
                 for body = (cddr clause)
                 for tag = (make-tag vname)
                 collect
                 `(,tag
                   (let ,(loop for f in fields
                               for i from 1
                               collect `(,f (nth ,i ,val)))
                     ,@body)))
         (t (error "match*: unhandled variant ~S" (car ,val)))))))
