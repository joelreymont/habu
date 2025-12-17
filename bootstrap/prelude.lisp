;;; Habu Prelude - CL Functions for Native Compiler
;;;
;;; These functions are needed by the Habu compiler but aren't
;;; built-in primitives. They're compiled normally.

(in-package :habu)

;;; Symbol comparison for native Habu
;;; Native Habu doesn't intern symbols properly, so eq fails across reads.
;;; This function compares symbols by name using string-equal.
(defun sym-eq (a b)
  "Compare two symbols by name. Works even when symbols aren't interned properly."
  (if (and (symbolp a) (symbolp b))
      (string-equal (symbol-name a) (symbol-name b))
      (eq a b)))

;;; sym-case macro - like case but uses sym-eq for symbol comparison
;;; This is needed because native Habu creates different symbol objects
;;; than those embedded in SBCL-compiled code.
(defmacro sym-case (keyform &rest clauses)
  "Like CASE but uses sym-eq for symbol comparison.
   Works across different symbol tables (interpreter vs compiled)."
  (let ((key-var (gensym "KEY")))
    `(let ((,key-var ,keyform))
       (cond
         ,@(mapcar (lambda (clause)
                     (let ((keys (car clause))
                           (body (cdr clause)))
                       (cond
                         ;; Default clause
                         ((or (eq keys 't) (eq keys 'otherwise))
                          `(t ,@body))
                         ;; Single key
                         ((atom keys)
                          `((sym-eq ,key-var ',keys) ,@body))
                         ;; Multiple keys
                         (t
                          `((or ,@(mapcar (lambda (k) `(sym-eq ,key-var ',k)) keys))
                            ,@body)))))
                   clauses)))))

;;; Type predicates
(defun zerop (x) (= x 0))
(defun integerp (x)
  ;; In Habu, fixnums have bit 0 = 1 (tagged fixnum representation)
  (= (logand x 1) 1))
(defun listp (x)
  ;; nil or cons
  (or (null x) (consp x)))

;;; Arithmetic
(defun 1+ (n)
  (+ n 1))

(defun 1- (n)
  (- n 1))

(defun ceiling (n d)
  ;; Ceiling division: (ceiling 7 3) = 3
  (let ((q (truncate n d)))
    (if (= (* q d) n)
        q
        (+ q 1))))

(defun truncate (n d)
  ;; Integer division (toward zero)
  (/ n d))

;;; Bitwise
(defun lognot (x)
  ;; Bitwise NOT - XOR with -1
  (logxor x -1))

;;; List functions
(defun last (list)
  (if (null (cdr list))
      list
      (last (cdr list))))

(defun butlast (list)
  (if (null (cdr list))
      nil
      (cons (car list) (butlast (cdr list)))))

;;; list* - fixed arity versions (no &rest)
(defun list*-1 (a) a)
(defun list*-2 (a b) (cons a b))
(defun list*-3 (a b c) (cons a (cons b c)))
(defun list*-4 (a b c d) (cons a (cons b (cons c d))))

;;; Append-all: append a list of lists (replacement for (apply #'append ...))
(defun append-all (lists)
  (if (null lists)
      nil
      (append (car lists) (append-all (cdr lists)))))

;;; Limited APPLY: only works for 0-8 arguments
;;; Used for (apply #'fn args-list)
(defun apply (fn args)
  (let ((len (length args)))
    (cond
      ((= len 0) (funcall fn))
      ((= len 1) (funcall fn (nth 0 args)))
      ((= len 2) (funcall fn (nth 0 args) (nth 1 args)))
      ((= len 3) (funcall fn (nth 0 args) (nth 1 args) (nth 2 args)))
      ((= len 4) (funcall fn (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args)))
      ((= len 5) (funcall fn (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args)))
      ((= len 6) (funcall fn (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args) (nth 5 args)))
      ((= len 7) (funcall fn (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args) (nth 5 args) (nth 6 args)))
      ((= len 8) (funcall fn (nth 0 args) (nth 1 args) (nth 2 args) (nth 3 args) (nth 4 args) (nth 5 args) (nth 6 args) (nth 7 args)))
      (t (error "apply: too many arguments")))))

;;; Higher-order functions
(defun some (pred list)
  (if (null list)
      nil
      (if (funcall pred (car list))
          t
          (some pred (cdr list)))))

;;; Equality
(defun eql (a b)
  ;; EQL: same as EQ for most types, compares numbers by value
  (eq a b))

(defun equal (a b)
  (cond
    ((eq a b) t)
    ((and (consp a) (consp b))
     (and (equal (car a) (car b))
          (equal (cdr a) (cdr b))))
    ((and (stringp a) (stringp b))
     (string-equal a b))
    (t nil)))

;;; Character comparison
(defun char= (a b)
  (= (char-code a) (char-code b)))

;;; String operations
(defun string-concat (s1 s2)
  "Concatenate two strings"
  (let* ((len1 (string-length s1))
         (len2 (string-length s2))
         (result (make-string (+ len1 len2))))
    (dotimes (i len1)
      (string-set! result i (char-at s1 i)))
    (dotimes (i len2)
      (string-set! result (+ len1 i) (char-at s2 i)))
    result))

(defun concatenate-strings (strings)
  "Concatenate a list of strings"
  (if (null strings)
      ""
      (if (null (cdr strings))
          (car strings)
          (string-concat (car strings) (concatenate-strings (cdr strings))))))

(defun concatenate (type s1 s2)
  "Concatenate two strings (simplified - ignores type)"
  (string-concat s1 s2))

;;; Character access
(defun char (string index)
  ;; Alias for char-at
  (char-at string index))

;;; I/O
(defun write-string (str)
  (sys-write 1 str (string-length str)))

(defun write-char (ch)
  (sys-write-char 1 ch))

;;; Error handling (simple version)
(defun error (msg)
  (write-string "ERROR: ")
  (write-string msg)
  (write-char #\Newline)
  (sys-exit 1))

;;; File I/O for executables
;;; O_WRONLY | O_CREAT | O_TRUNC = 1 + 512 + 1024 = 1537
;;; Mode 0755 = 493 decimal
(defun native-write-executable (path content)
  "Write executable file with +x permission (native version)"
  (let ((fd (sys-open path 1537 493)))
    (if (>= fd 0)
        (let* ((len (string-length content))
               (written (sys-write fd content len)))
          (sys-close fd)
          written)
        -1)))

;;; Symbol interning - ensures all symbols with the same name are eq
;;; This is critical for self-hosting: (eq 'defun (car form)) must work
;;; The intern table is stored at [x27+0], same as the reader uses
(defun intern-symbol (name)
  "Intern a symbol by name string. Returns the canonical symbol.
   If a symbol with this name already exists, returns it.
   Otherwise creates a new symbol and adds it to the intern table."
  (labels ((find-in-table (lst)
             (if (null lst)
                 nil
                 (if (string-equal name (symbol-name (cdr (car lst))))
                     (cdr (car lst))  ;; Return the symbol
                     (find-in-table (cdr lst))))))
    (let* ((table (get-intern-table))
           (existing (find-in-table table)))
      (if existing
          existing
          ;; Create new symbol and add to table
          (let ((sym (make-symbol-from-string name)))
            (set-intern-table (cons (cons name sym) table))
            sym)))))
