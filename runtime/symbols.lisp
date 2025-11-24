;;;; Habu Runtime - Symbol Table and Interning
;;;; Symbol management for Habu Lisp

(in-package :habu-runtime)

;;; Export symbols
(export '(runtime-intern
          runtime-make-symbol
          runtime-symbol-name
          runtime-symbol->print-name
          runtime-symbol-value
          runtime-symbol-function
          runtime-symbol-plist
          set-symbol-value
          set-symbol-function
          set-symbol-plist
          runtime-gensym
          *symbol-table*
          clear-symbol-table
          runtime-find-symbol
          runtime-make-package
          runtime-in-package
          runtime-use-package
          runtime-export-symbols))

;;; Symbol structure (on heap)
;;; Layout: header(8) + name-ptr(8) + value(8) + function(8) + plist(8) = 40 bytes
;;; name-ptr: pointer to string (or fixnum for now)
;;; value: current value binding (or unbound marker)
;;; function: function definition (or unbound marker)
;;; plist: property list

;;; Global symbol table (name -> symbol pointer)
(defvar *symbol-table* (make-hash-table :test 'equal)
  "Global symbol table for interning (keyed by (pkg . name))")

(defvar *habu-gensym-counter* 0
  "Counter for gensym unique symbols")

(defstruct (habu-package (:conc-name pkg-))
  name
  (use '("HABU-RUNTIME"))
  (symbols (make-hash-table :test 'equal))
  (exports (make-hash-table :test 'equal)))

(defvar *packages* (make-hash-table :test 'equal) "Name -> package struct")
(defvar *current-package* "HABU-USER")

;;; Symbol allocation
(defun allocate-symbol (name-ptr)
  "Allocate a symbol on the heap"
  (unless *heap*
    (error "Runtime not initialized - call (initialize-runtime)"))
  (let* ((ptr (heap-allocate *heap* 32 +tag-symbol+))  ; 4 * 8 bytes
         (header-addr (logand ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    ;; Write symbol fields: name, value, function, plist
    (write-u64 *heap* data-addr name-ptr)            ; name
    (write-u64 *heap* (+ data-addr 8) +unbound+)     ; value (unbound)
    (write-u64 *heap* (+ data-addr 16) +unbound+)    ; function (unbound)
    (write-u64 *heap* (+ data-addr 24) 0)            ; plist (nil)
    ptr))

;;; Symbol accessors
(defun symbol-name-ptr (sym-ptr)
  "Read name pointer from symbol"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (read-u64 *heap* data-addr)))

(defun runtime-symbol-value (sym-ptr)
  "Read value from symbol"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (value (read-u64 *heap* (+ data-addr 8))))
    (when (= value +unbound+)
      (error "Unbound variable: symbol at ~X" sym-ptr))
    value))

(defun runtime-symbol-function (sym-ptr)
  "Read function from symbol"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (data-addr (+ header-addr 8))
         (fn (read-u64 *heap* (+ data-addr 16))))
    (when (= fn +unbound+)
      (error "Undefined function: symbol at ~X" sym-ptr))
    fn))

(defun runtime-symbol-plist (sym-ptr)
  "Read property list from symbol"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (read-u64 *heap* (+ data-addr 24))))

;;; Symbol setters
(defun set-symbol-value (sym-ptr value)
  "Set value of symbol"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (write-u64 *heap* (+ data-addr 8) value))
  value)

(defun set-symbol-function (sym-ptr fn)
  "Set function of symbol"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (write-u64 *heap* (+ data-addr 16) fn))
  fn)

(defun set-symbol-plist (sym-ptr plist)
  "Set property list of symbol"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (data-addr (+ header-addr 8)))
    (write-u64 *heap* (+ data-addr 24) plist))
  plist)

;;; Interning
(defun runtime-intern (name &optional package-name)
  "Intern a symbol by NAME in PACKAGE-NAME (default *current-package*)."
  (unless *heap*
    (error "Runtime not initialized - call (initialize-runtime)"))

  (let* ((pkg-name (string-upcase (or package-name *current-package*)))
         (pkg (package-or-create pkg-name))
         (key (string-upcase name))
         (syms (pkg-symbols pkg))
         (exports (pkg-exports pkg))
         (existing (or (gethash key syms)
                       (gethash key exports))))
    (if existing
        existing
        (let* ((name-str (runtime-make-string key))
               (sym (allocate-symbol name-str)))
          (setf (gethash key syms) sym)
          (setf (gethash (cons pkg-name key) *symbol-table*) sym)
          sym))))

(defun runtime-make-symbol (name)
  "Create an uninterned symbol"
  (unless *heap*
    (error "Runtime not initialized - call (initialize-runtime)"))
  (let ((name-str (runtime-make-string name)))
    (allocate-symbol name-str)))

(defun runtime-gensym (&optional (prefix "G"))
  "Generate a unique uninterned symbol"
  (let* ((name (format nil "~A~D" prefix *habu-gensym-counter*))
         (sym (runtime-make-symbol name)))
    (incf *habu-gensym-counter*)
    sym))

(defun clear-symbol-table ()
  "Clear the symbol table"
  (clrhash *symbol-table*)
  (setf *habu-gensym-counter* 0)
  (clrhash *packages*)
  (setf *current-package* "HABU-USER"))

;;; Symbol name lookup (reverse lookup for debugging)
(defun runtime-symbol-name (sym-ptr)
  "Get the Common Lisp name of a symbol (for debugging)"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (error "Not a symbol: ~X" sym-ptr))
  (runtime-string->lisp (symbol-name-ptr sym-ptr)))

(defun find-symbol-package-name (sym-ptr key)
  "Return package name containing SYM-PTR (symbols/exports), else NIL."
  (block found
    (maphash (lambda (_ pkg)
               (declare (ignore _))
               (when (or (eq sym-ptr (gethash key (pkg-symbols pkg)))
                         (eq sym-ptr (gethash key (pkg-exports pkg))))
                 (return-from found (pkg-name pkg))))
             *packages*)
    nil))

(defun runtime-symbol->print-name (sym-ptr)
  "Produce a display name for SYM-PTR with package prefix when needed."
  (let* ((name (runtime-symbol-name sym-ptr))
         (key (string-upcase name))
         (pkg (find-symbol-package-name sym-ptr key)))
    (cond
      ((null pkg) name)
      ((or (string= pkg *current-package*)
           (string= pkg "HABU-RUNTIME"))
       name)
      (t (format nil "~A::~A" pkg name)))))

;;; Packages (minimal)
(defun package-or-create (name)
  (or (gethash name *packages*)
      (let ((pkg (make-habu-package :name name)))
        (setf (gethash name *packages*) pkg)
        pkg)))

(defun runtime-make-package (name)
  (package-or-create name)
  0)

(defun runtime-in-package (name)
  (setf *current-package* (string-upcase name))
  (package-or-create *current-package*)
  0)

(defun runtime-use-package (name)
  (let* ((pkg (package-or-create *current-package*))
         (target (string-upcase name)))
    (pushnew target (pkg-use pkg) :test #'string=))
  0)

(defun runtime-export-symbols (names package-name)
  (let* ((pkg (package-or-create (or package-name *current-package*)))
         (exports (pkg-exports pkg)))
    (dolist (n names)
      (let ((sym (runtime-find-symbol n (pkg-name pkg))))
        (setf (gethash (string-upcase n) exports) sym))))
  0)

(defun runtime-find-symbol (name &optional package-name)
  (let* ((pkg (package-or-create (or package-name *current-package*)))
         (exports (pkg-exports pkg))
         (syms (pkg-symbols pkg))
         (key (string-upcase name))
         (sym (or (gethash key syms)
                  (gethash key exports)
                  (find-in-used-packages key (pkg-use pkg)))))
    (or sym
        (runtime-intern name (pkg-name pkg)))))

(defun find-in-used-packages (key use-list)
  (dolist (pkg-name use-list)
    (let* ((pkg (gethash pkg-name *packages*))
           (hit (and pkg (gethash key (pkg-exports pkg)))))
      (when hit (return hit)))))

;;; GC support for symbols
(defun gc-mark-symbol (heap sym-ptr)
  "Mark a symbol and its reachable objects"
  (unless (= (logand sym-ptr #xF) +tag-symbol+)
    (return-from gc-mark-symbol))

  (let* ((header-addr (logand sym-ptr (lognot #xF)))
         (header (read-u64 heap header-addr)))

    ;; Already marked?
    (when (header-marked-p header)
      (return-from gc-mark-symbol))

    ;; Mark symbol
    (write-u64 heap header-addr (header-set-mark header))

    ;; Mark symbol's fields
    (let ((data-addr (+ header-addr 8)))
      ;; Mark name (if it's a string pointer, not a fixnum)
      (let ((name-ptr (read-u64 heap data-addr)))
        (when (and (not (zerop name-ptr))
                   (= (logand name-ptr #xF) +tag-string+))
          (gc-mark-object heap name-ptr)))

      ;; Mark value (if bound and is a pointer)
      (let ((value (read-u64 heap (+ data-addr 8))))
        (when (and (not (= value +unbound+))
                   (not (zerop value))
                   (not (= (logand value #xF) +tag-fixnum+)))
          (gc-mark-object heap value)))

      ;; Mark function (if bound and is a pointer)
      (let ((fn (read-u64 heap (+ data-addr 16))))
        (when (and (not (= fn +unbound+))
                   (not (zerop fn))
                   (not (= (logand fn #xF) +tag-fixnum+)))
          (gc-mark-object heap fn)))

      ;; Mark plist
      (let ((plist (read-u64 heap (+ data-addr 24))))
        (when (and (not (zerop plist))
                   (= (logand plist #xF) +tag-cons+))
          (gc-mark-object heap plist))))))

;;; Pretty printing
(defun print-symbol (sym-ptr)
  "Print symbol information"
  (format t "Symbol ~X:~%" sym-ptr)
  (format t "  Name:     ~A~%" (runtime-symbol-name sym-ptr))
  (format t "  Value:    ~:[~X~;UNBOUND~]~%"
          (= (read-u64 *heap* (+ (logand sym-ptr (lognot #xF)) 8 8)) +unbound+)
          (read-u64 *heap* (+ (logand sym-ptr (lognot #xF)) 8 8)))
  (format t "  Function: ~:[~X~;UNBOUND~]~%"
          (= (read-u64 *heap* (+ (logand sym-ptr (lognot #xF)) 8 16)) +unbound+)
          (read-u64 *heap* (+ (logand sym-ptr (lognot #xF)) 8 16)))
  (format t "  Plist:    ~X~%"
          (read-u64 *heap* (+ (logand sym-ptr (lognot #xF)) 8 24))))
