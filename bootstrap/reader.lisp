;;; Habu Lisp Reader - reads Lisp source into S-expressions
;;; This file MUST have no SBCL dependencies - it's used by Stage 1+

;;; For SBCL, we just use the package system
#+sbcl (in-package :habu)

;;; Native entry point - MUST be first function for Stage 1 binary
;;; This is a forward reference that calls the real main in compiler.lisp
(defun stage1-entry ()
  "Entry point for Stage 1 compiler. Calls the real main after all code is loaded."
  ;; Note: This function gets compiled first but shouldn't be _main
  ;; The linker should look for a function named MAIN instead
  0)

;;; SBCL version of while - native version is in compiler.lisp
#+sbcl
(defmacro while (test &body body)
  "SBCL version of while loop"
  `(loop while ,test do (progn ,@body)))

;;; Global intern table for native mode
(defvar *intern-table* nil)
(defvar *lambda-counter* 0)

;;; Package system globals
(defvar *packages* nil)          ; list of known package names
(defvar *current-package* nil)   ; current package name (nil = no prefix)

;;; SBCL version - symbol interning works automatically
#+sbcl
(defun ensure-symbols-registered ()
  "In SBCL, symbol interning works correctly - nothing to do."
  nil)

;;; Native version - build symbol table incrementally to avoid deep nesting
(defun make-special-forms ()
  "Build list of special form entries"
  (cons (cons "DEFUN" 'defun)
  (cons (cons "PROGN" 'progn)
  (cons (cons "IF" 'if)
  (cons (cons "LET" 'let)
  (cons (cons "LET*" 'let*)
  (cons (cons "QUOTE" 'quote)
  (cons (cons "LAMBDA" 'lambda)
  (cons (cons "FUNCALL" 'funcall)
  (cons (cons "LABELS" 'labels)
  (cons (cons "FUNCTION" 'function)
  (cons (cons "COND" 'cond)
  (cons (cons "WHEN" 'when)
  (cons (cons "UNLESS" 'unless)
  (cons (cons "AND" 'and)
  (cons (cons "OR" 'or)
  (cons (cons "NOT" 'not)
  (cons (cons "SETQ" 'setq)
        nil))))))))))))))))))

(defun make-arithmetic ()
  "Build list of arithmetic entries"
  (cons (cons "+" '+)
  (cons (cons "-" '-)
  (cons (cons "*" '*)
  (cons (cons "/" '/)
  (cons (cons "MOD" 'mod)
  (cons (cons "=" '=)
  (cons (cons "<" '<)
  (cons (cons ">" '>)
  (cons (cons "<=" '<=)
  (cons (cons ">=" '>=)
  (cons (cons "/=" '/=)
  (cons (cons "EQ" 'eq)
  (cons (cons "LOGAND" 'logand)
  (cons (cons "LOGIOR" 'logior)
  (cons (cons "LOGXOR" 'logxor)
  (cons (cons "ASH" 'ash)
        nil)))))))))))))))))

(defun make-list-ops ()
  "Build list of list operation entries"
  (cons (cons "CONS" 'cons)
  (cons (cons "CAR" 'car)
  (cons (cons "CDR" 'cdr)
  (cons (cons "CADR" 'cadr)
  (cons (cons "CADDR" 'caddr)
  (cons (cons "CDDR" 'cddr)
  (cons (cons "CDDDR" 'cdddr)
  (cons (cons "CADDDR" 'cadddr)
  (cons (cons "NTH" 'nth)
  (cons (cons "LIST" 'list)
  (cons (cons "LENGTH" 'length)
  (cons (cons "REVERSE" 'reverse)
  (cons (cons "SETCAR" 'setcar)
  (cons (cons "SETCDR" 'setcdr)
        nil)))))))))))))))

(defun make-predicates ()
  "Build list of predicate entries"
  (cons (cons "NULL" 'null)
  (cons (cons "CONSP" 'consp)
  (cons (cons "NUMBERP" 'numberp)
  (cons (cons "SYMBOLP" 'symbolp)
  (cons (cons "STRINGP" 'stringp)
  (cons (cons "VECTORP" 'vectorp)
        nil)))))))

(defun make-string-ops ()
  "Build list of string operation entries"
  (cons (cons "STRING-LENGTH" 'string-length)
  (cons (cons "STRING-REF" 'string-ref)
  (cons (cons "STRING-CONCAT" 'string-concat)
  (cons (cons "STRING-EQUAL" 'string-equal)
        nil)))))

(defun make-vector-ops ()
  "Build list of vector operation entries"
  (cons (cons "MAKE-VECTOR" 'make-vector)
  (cons (cons "VECTOR-REF" 'vector-ref)
  (cons (cons "VECTOR-SET" 'vector-set)
  (cons (cons "VECTOR-LENGTH" 'vector-length)
  (cons (cons "MAKE-STRING-FROM-VECTOR" 'make-string-from-vector)
  (cons (cons "BUFFER-TO-STRING" 'buffer-to-string)
        nil)))))))

(defun make-symbol-ops ()
  "Build list of symbol operation entries"
  (cons (cons "SYMBOL-NAME" 'symbol-name)
  (cons (cons "MAKE-SYMBOL-FROM-STRING" 'make-symbol-from-string)
        nil)))

(defun make-system-ops ()
  "Build list of system operation entries"
  (cons (cons "SYS-EXIT" 'sys-exit)
  (cons (cons "SYS-OPEN" 'sys-open)
  (cons (cons "SYS-READ" 'sys-read)
  (cons (cons "SYS-WRITE" 'sys-write)
  (cons (cons "SYS-WRITE-CHAR" 'sys-write-char)
  (cons (cons "SYS-READ-BYTE" 'sys-read-byte)
  (cons (cons "SYS-CLOSE" 'sys-close)
  (cons (cons "NATIVE-READ-FILE" 'native-read-file)
  (cons (cons "GET-INTERN-TABLE" 'get-intern-table)
  (cons (cons "SET-INTERN-TABLE" 'set-intern-table)
  (cons (cons "GET-LAMBDA-COUNTER" 'get-lambda-counter)
  (cons (cons "SET-LAMBDA-COUNTER" 'set-lambda-counter)
  (cons (cons "IN-PACKAGE" 'in-package)
  (cons (cons "DEFPACKAGE" 'defpackage)
  (cons (cons "NIL" 'nil)
  (cons (cons "T" 't)
        nil)))))))))))))))))

(defun make-ir-basic ()
  "Build list of basic IR tag entries"
  (cons (cons "LIT" 'lit)
  (cons (cons "VAR" 'var)
  (cons (cons "SYM-LIT" 'sym-lit)
  (cons (cons "STR-LIT" 'str-lit)
  (cons (cons "NIL-IR" 'nil-ir)
  (cons (cons "ADD" 'add)
  (cons (cons "SUB" 'sub)
  (cons (cons "MUL" 'mul)
  (cons (cons "DIV" 'div)
  (cons (cons "CMP-EQ" 'cmp-eq)
  (cons (cons "CMP-LT" 'cmp-lt)
  (cons (cons "CMP-GT" 'cmp-gt)
  (cons (cons "CMP-LE" 'cmp-le)
  (cons (cons "CMP-GE" 'cmp-ge)
        nil)))))))))))))))

(defun make-ir-cons ()
  "Build list of cons IR tag entries"
  (cons (cons "CONS-IR" 'cons-ir)
  (cons (cons "CAR-IR" 'car-ir)
  (cons (cons "CDR-IR" 'cdr-ir)
  (cons (cons "SETCAR-IR" 'setcar-ir)
  (cons (cons "SETCDR-IR" 'setcdr-ir)
        nil))))))

(defun make-ir-control ()
  "Build list of control flow IR tag entries"
  (cons (cons "IF-IR" 'if-ir)
  (cons (cons "LET-IR" 'let-ir)
  (cons (cons "LET*-IR" 'let*-ir)
  (cons (cons "PROGN-IR" 'progn-ir)
  (cons (cons "OR-IR" 'or-ir)
  (cons (cons "AND-IR" 'and-ir)
        nil)))))))

(defun make-ir-functions ()
  "Build list of function IR tag entries"
  (cons (cons "CALL-FN" 'call-fn)
  (cons (cons "FUNCALL-IR" 'funcall-ir)
  (cons (cons "LAMBDA-IR" 'lambda-ir)
  (cons (cons "FN-REF-IR" 'fn-ref-ir)
  (cons (cons "LABELS-IR" 'labels-ir)
        nil))))))

(defun make-ir-syscalls ()
  "Build list of syscall IR tag entries"
  (cons (cons "SYS-EXIT-IR" 'sys-exit-ir)
  (cons (cons "SYS-OPEN-IR" 'sys-open-ir)
  (cons (cons "SYS-READ-IR" 'sys-read-ir)
  (cons (cons "SYS-WRITE-IR" 'sys-write-ir)
  (cons (cons "SYS-WRITE-CHAR-IR" 'sys-write-char-ir)
  (cons (cons "SYS-READ-BYTE-IR" 'sys-read-byte-ir)
  (cons (cons "SYS-CLOSE-IR" 'sys-close-ir)
  (cons (cons "SETQ-IR" 'setq-ir)
        nil)))))))))

(defun make-ir-predicates ()
  "Build list of predicate IR tag entries"
  (cons (cons "GET-TAG" 'get-tag)
  (cons (cons "QUOTE-IR" 'quote-ir)
  (cons (cons "NULL-IR" 'null-ir)
  (cons (cons "LIST-IR" 'list-ir)
        nil)))))

(defun make-ir-strings ()
  "Build list of string IR tag entries"
  (cons (cons "STRING-LENGTH-IR" 'string-length-ir)
  (cons (cons "STRING-REF-IR" 'string-ref-ir)
  (cons (cons "STRING-CONCAT-IR" 'string-concat-ir)
  (cons (cons "SYMBOL-NAME-IR" 'symbol-name-ir)
  (cons (cons "MAKE-SYMBOL-IR" 'make-symbol-ir)
        nil))))))

(defun make-ir-vectors ()
  "Build list of vector IR tag entries"
  (cons (cons "MAKE-VECTOR-IR" 'make-vector-ir)
  (cons (cons "VECTOR-REF-IR" 'vector-ref-ir)
  (cons (cons "VECTOR-SET-IR" 'vector-set-ir)
  (cons (cons "VECTOR-LENGTH-IR" 'vector-length-ir)
  (cons (cons "MAKE-STRING-FROM-VECTOR-IR" 'make-string-from-vector-ir)
  (cons (cons "BUFFER-TO-STRING-IR" 'buffer-to-string-ir)
        nil)))))))

(defun make-ir-intern ()
  "Build list of intern table IR tag entries"
  (cons (cons "GET-INTERN-TABLE-IR" 'get-intern-table-ir)
  (cons (cons "SET-INTERN-TABLE-IR" 'set-intern-table-ir)
  (cons (cons "GET-LAMBDA-COUNTER-IR" 'get-lambda-counter-ir)
  (cons (cons "SET-LAMBDA-COUNTER-IR" 'set-lambda-counter-ir)
  (cons (cons "NATIVE-READ-FILE-IR" 'native-read-file-ir)
        nil))))))

(defun make-ir-lambda ()
  "Build list of lambda IR tag entries"
  (cons (cons "LIFTED-LAMBDA-IR" 'lifted-lambda-ir)
  (cons (cons "LAMBDA-REF" 'lambda-ref)
  (cons (cons ":CALL" ':call)
  (cons (cons ":EXTERN-CALL" ':extern-call)
        nil)))))

(defun append-lists (a b)
  "Append list b to end of list a"
  (if (null a)
      b
      (cons (car a) (append-lists (cdr a) b))))

#-sbcl
(defun ensure-symbols-registered ()
  "Register compiler symbols in the intern table if not already done.
   Uses SYS-EXIT-IR (an IR tag) to detect if already initialized."
  (if (find-interned "SYS-EXIT-IR" (get-intern-table))
      nil
      (set-intern-table
       (append-lists (make-special-forms)
       (append-lists (make-arithmetic)
       (append-lists (make-list-ops)
       (append-lists (make-predicates)
       (append-lists (make-string-ops)
       (append-lists (make-vector-ops)
       (append-lists (make-symbol-ops)
       (append-lists (make-system-ops)
       (append-lists (make-ir-basic)
       (append-lists (make-ir-cons)
       (append-lists (make-ir-control)
       (append-lists (make-ir-functions)
       (append-lists (make-ir-syscalls)
       (append-lists (make-ir-predicates)
       (append-lists (make-ir-strings)
       (append-lists (make-ir-vectors)
       (append-lists (make-ir-intern)
                     (make-ir-lambda)))))))))))))))))))))

;;; Core utilities (must be defined before reader functions)
;;; These are also defined in compiler-pure.lisp but reader needs them first

(defun map-list (fn lst)
  "Map function over list - iterative"
  (let ((current lst)
        (result nil))
    (while (not (null current))
      (setq result (cons (funcall fn (car current)) result))
      (setq current (cdr current)))
    (reverse result)))

(defun assoc-get (key alist)
  "Get value for key in alist - iterative"
  (let ((current alist)
        (found nil)
        (result nil))
    (while (and (not found) (not (null current)))
      (if (eq key (car (car current)))
          (progn
            (setq found t)
            (setq result (cdr (car current))))
          (setq current (cdr current))))
    result))

;;; String comparison
#-sbcl
(defun string= (s1 s2)
  "Compare two strings for equality - iterative"
  (if (or (null s1) (null s2))
      (and (null s1) (null s2))  ; nil = nil, nil != string
      (let ((len1 (string-length s1))
            (len2 (string-length s2)))
        (if (= len1 len2)
            (let ((i 0)
                  (equal t))
              (while (and equal (< i len1))
                (if (= (string-ref s1 i) (string-ref s2 i))
                    (setq i (+ i 1))
                    (setq equal nil)))
              equal)
            nil))))

(defun find-interned (name table)
  "Find symbol with NAME in intern TABLE (alist of (name . symbol)) - iterative"
  (let ((current table)
        (found nil)
        (result nil))
    (while (and (not found) (not (null current)))
      (if (string= name (car (car current)))
          (progn
            (setq found t)
            (setq result (cdr (car current))))
          (setq current (cdr current))))
    result))

#-sbcl
(defun intern (name)
  "Intern a string as a symbol. Returns existing symbol if found, else creates new.
   Preserves package prefix if present (ARM64:MOVZ stays ARM64:MOVZ).
   Adds current package prefix for unqualified names."
  (let ((qname (if (and (contains-colon name)
                        (> (string-length name) 0)
                        (not (= (string-ref name 0) #x3A)))  ; not a keyword
                   ;; Already package-qualified - preserve it (upcase for CL convention)
                   (string-upcase name)
                   ;; No package prefix - qualify with current package
                   (qualify-symbol-name name))))
    (let ((existing (find-interned qname (get-intern-table))))
      (if existing
          existing
          (let ((sym (make-symbol-from-string qname)))
            (set-intern-table (cons (cons qname sym) (get-intern-table)))
            sym)))))

;;; Global state accessors (implemented in codegen for native)
(defun get-intern-table () *intern-table*)

(defun set-intern-table (table)
  (setq *intern-table* table))

(defun get-lambda-counter () *lambda-counter*)

(defun set-lambda-counter (n)
  (setq *lambda-counter* n))

;;; Package system accessors
(defun get-current-package () *current-package*)

(defun set-current-package (pkg)
  (setq *current-package* pkg))

(defun get-packages () *packages*)

(defun add-package (name)
  "Register a new package name"
  (if (not (member-string name *packages*))
      (setq *packages* (cons name *packages*))))

(defun member-string (s lst)
  "Check if string s is in list lst"
  (let ((current lst)
        (found nil))
    (while (and (not found) (not (null current)))
      (if (string= s (car current))
          (setq found t)
          (setq current (cdr current))))
    found))

(defun contains-colon (name)
  "Check if string contains a colon (for package-qualified symbols)"
  (let ((len (string-length name))
        (i 0)
        (found nil))
    (while (and (< i len) (not found))
      (if (= (string-ref name i) #x3A)  ; :
          (setq found t)
          (setq i (+ i 1))))
    found))

(defun strip-package-prefix (name)
  "Strip package prefix from symbol name. ARM64:ENCODE -> ENCODE.
   Keywords (:FOO) are returned unchanged.
   Names without colon are returned unchanged."
  (let ((len (string-length name)))
    (if (= len 0)
        name
        (if (= (string-ref name 0) #x3A)  ; keyword starting with :
            name
            ;; Find last colon position
            (let ((i (- len 1))
                  (colon-pos -1))
              (while (>= i 0)
                (if (= (string-ref name i) #x3A)
                    (progn
                      (setq colon-pos i)
                      (setq i -1))  ; stop searching
                    (setq i (- i 1))))
              (if (< colon-pos 0)
                  name  ; no colon found
                  (substring name (+ colon-pos 1) len)))))))

(defun qualify-symbol-name (name)
  "Add current package prefix if name doesn't have one and package is set.
   Names starting with : are keywords, leave unchanged.
   Names already containing : are package-qualified, leave unchanged."
  (if (null *current-package*)
      name
      (if (= (string-length name) 0)
          name
          (if (= (string-ref name 0) #x3A)  ; keyword starting with :
              name
              (if (contains-colon name)
                  name  ; already qualified
                  ;; Add package prefix: PKG:NAME
                  (string-concat (string-concat *current-package* ":") name))))))

;;; Character predicates

(defun whitespace? (ch)
  (or (= ch #x20)   ; space
      (= ch #x09)   ; tab
      (= ch #x0A)   ; newline
      (= ch #x0D))) ; return

(defun digit? (ch)
  (and (>= ch #x30) (<= ch #x39)))

#-sbcl
(defun digit-val (ch)
  (- ch #x30))

(defun alpha? (ch)
  (or (and (>= ch #x41) (<= ch #x5A))   ; A-Z
      (and (>= ch #x61) (<= ch #x7A)))) ; a-z

(defun symbol-char? (ch)
  (or (alpha? ch)
      (digit? ch)
      (= ch #x2D)   ; -
      (= ch #x5F)   ; _
      (= ch #x2B)   ; +
      (= ch #x2A)   ; *
      (= ch #x2F)   ; /
      (= ch #x3D)   ; =
      (= ch #x3C)   ; <
      (= ch #x3E)   ; >
      (= ch #x21)   ; !
      (= ch #x3F)   ; ?
      (= ch #x26)   ; &
      (= ch #x3A)   ; :
      (= ch #x25))) ; %

;;; Get character at position (0 if beyond end)
#-sbcl
(defun char-at (str pos)
  (if (>= pos (string-length str))
      #x0
      (string-ref str pos)))

;;; Skip whitespace and comments - iterative with inlined predicates
#-sbcl
(defun skip-line (source pos)
  "Skip to end of line - iterative"
  (let ((current-pos pos))
    (while (let ((ch (char-at source current-pos)))
             (and (not (= ch #x0A)) (not (= ch #x0))))
      (setq current-pos (+ current-pos 1)))
    (+ current-pos 1)))

#-sbcl
(defun skip-ws (source pos)
  "Skip whitespace and comments - iterative with inlined whitespace check"
  (let ((current-pos pos)
        (done nil))
    (while (not done)
      (let ((ch (char-at source current-pos)))
        (cond
          ;; Inline whitespace?: space(32), tab(9), newline(10), return(13)
          ((or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D))
           (setq current-pos (+ current-pos 1)))
          ((= ch #x3B)  ; semicolon - line comment
           (setq current-pos (skip-line source (+ current-pos 1))))
          (t (setq done t)))))
    current-pos))

;;; Read digits - helper for read-int - iterative with inlined digit check
#-sbcl
(defun read-digits (source pos n)
  "Read decimal digits iteratively with inlined predicates"
  (let ((current-pos pos)
        (current-n n))
    (while (let ((ch (char-at source current-pos)))
             ;; Inline digit?: ch >= '0' (48) and ch <= '9' (57)
             (and (>= ch #x30) (<= ch #x39)))
      (let ((ch (char-at source current-pos)))
        ;; Inline digit-val: ch - '0'
        (setq current-n (+ (* current-n 10) (- ch #x30))))
      (setq current-pos (+ current-pos 1)))
    (cons current-n current-pos)))

;;; Read integer - returns (value . new-pos)
#-sbcl
(defun read-int (source pos)
  (let ((ch (char-at source pos)))
    (cond ((= ch #x2D) ; minus
           (let ((result (read-digits source (+ pos #x1) #x0)))
             (cons (- #x0 (car result)) (cdr result))))
          ((= ch #x2B) ; plus
           (read-digits source (+ pos #x1) #x0))
          (t (read-digits source pos #x0)))))

;;; Read hex integer - returns (value . new-pos)
(defun hex-digit-val (ch)
  (cond ((and (>= ch #x30) (<= ch #x39)) (- ch #x30))
        ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA))
        ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA))
        (t #x0)))

(defun hex-digit? (ch)
  (or (and (>= ch #x30) (<= ch #x39))
      (and (>= ch #x41) (<= ch #x46))
      (and (>= ch #x61) (<= ch #x66))))

#-sbcl
(defun read-hex-digits (source pos n)
  "Read hexadecimal digits iteratively with inlined predicates"
  (let ((current-pos pos)
        (current-n n))
    (while (let ((ch (char-at source current-pos)))
             ;; Inline hex-digit?: 0-9, A-F, a-f
             (or (and (>= ch #x30) (<= ch #x39))
                 (and (>= ch #x41) (<= ch #x46))
                 (and (>= ch #x61) (<= ch #x66))))
      (let ((ch (char-at source current-pos)))
        ;; Inline hex-digit-val
        (setq current-n (+ (* current-n 16)
                           (cond ((and (>= ch #x30) (<= ch #x39)) (- ch #x30))
                                 ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) 10))
                                 (t (+ (- ch #x61) 10))))))
      (setq current-pos (+ current-pos 1)))
    (cons current-n current-pos)))

;;; Helper: reverse list and convert to vector
(defun list-to-vector-rev (lst len)
  "Convert reversed list to vector of given length"
  (let ((vec (make-vector len))
        (i (- len 1)))
    (while (>= i 0)
      (vector-set vec i (car lst))
      (setq lst (cdr lst))
      (setq i (- i 1)))
    vec))

;;; Read string literal - returns (string . new-pos)
;;; Uses list accumulator to avoid O(n^2) string allocations
#-sbcl
(defun read-str-chars (source pos acc)
  "Read string characters using list accumulator (O(n) allocation)"
  (let ((current-pos pos)
        (char-list nil)  ; reversed list of chars
        (char-count 0)
        (done nil))
    (while (not done)
      (let ((ch (char-at source current-pos)))
        (cond
          ((= ch #x22) ; closing quote
           (setq done t)
           (setq current-pos (+ current-pos 1)))
          ((= ch #x5C) ; backslash escape
           (let ((next-ch (char-at source (+ current-pos 1))))
             (cond
               ((= next-ch #x6E) ; \n
                (setq char-list (cons #x0A char-list))
                (setq char-count (+ char-count 1)))
               ((= next-ch #x74) ; \t
                (setq char-list (cons #x09 char-list))
                (setq char-count (+ char-count 1)))
               ((= next-ch #x22) ; \"
                (setq char-list (cons #x22 char-list))
                (setq char-count (+ char-count 1)))
               ((= next-ch #x5C) ; \\
                (setq char-list (cons #x5C char-list))
                (setq char-count (+ char-count 1)))
               (t
                (setq char-list (cons next-ch char-list))
                (setq char-count (+ char-count 1))))
             (setq current-pos (+ current-pos 2))))
          ((= ch #x0) ; EOF
           (setq done t))
          (t
           (setq char-list (cons ch char-list))
           (setq char-count (+ char-count 1))
           (setq current-pos (+ current-pos 1))))))
    ;; Convert list to string
    (if (= char-count 0)
        (cons acc current-pos)
        (let ((vec (list-to-vector-rev char-list char-count)))
          (let ((new-str (make-string-from-vector vec)))
            (cons (if (= (string-length acc) 0)
                      new-str
                      (string-concat acc new-str))
                  current-pos))))))

#-sbcl
(defun read-str (source pos)
  (read-str-chars source (+ pos #x1) ""))

;;; Read symbol - returns (symbol-or-number . new-pos)
;;; Uses vector accumulator to avoid O(n^2) string allocations
#-sbcl
(defun read-sym-chars (source pos acc)
  "Read symbol characters using vector accumulator (O(n) allocation).
   Inlines symbol-char? check to avoid function call overhead."
  (let ((start-pos pos)
        (current-pos pos)
        (source-len (string-length source)))
    ;; First pass: count characters with inlined symbol-char? check
    (while (let ((ch (if (>= current-pos source-len)
                         #x0
                         (string-ref source current-pos))))
             (or (and (>= ch #x41) (<= ch #x5A))   ; A-Z
                 (and (>= ch #x61) (<= ch #x7A))   ; a-z
                 (and (>= ch #x30) (<= ch #x39))   ; 0-9
                 (= ch #x2D)   ; -
                 (= ch #x5F)   ; _
                 (= ch #x2B)   ; +
                 (= ch #x2A)   ; *
                 (= ch #x2F)   ; /
                 (= ch #x3D)   ; =
                 (= ch #x3C)   ; <
                 (= ch #x3E)   ; >
                 (= ch #x21)   ; !
                 (= ch #x3F)   ; ?
                 (= ch #x26)   ; &
                 (= ch #x3A)   ; :
                 (= ch #x25))) ; %
      (setq current-pos (+ current-pos 1)))
    ;; Calculate length
    (let ((len (- current-pos start-pos)))
      (if (= len 0)
          (cons acc current-pos)
          ;; Allocate vector of exact size, fill it, convert once
          (let ((vec (make-vector len))
                (i 0))
            (while (< i len)
              (vector-set vec i (char-at source (+ start-pos i)))
              (setq i (+ i 1)))
            ;; Convert vector to string and concat with acc
            (let ((new-str (make-string-from-vector vec)))
              (cons (if (= (string-length acc) 0)
                        new-str
                        (string-concat acc new-str))
                    current-pos)))))))

(defun upcase-char (ch)
  (if (and (>= ch #x61) (<= ch #x7A))
      (- ch #x20)
      ch))

(defun upcase-string (s)
  "Upcase string using vector accumulator (O(n) allocation)"
  (let ((len (string-length s)))
    (if (= len 0)
        s
        (let ((vec (make-vector len))
              (i 0))
          (while (< i len)
            (vector-set vec i (upcase-char (string-ref s i)))
            (setq i (+ i 1)))
          (make-string-from-vector vec)))))

#-sbcl
(defun read-sym (source pos)
  (let ((result (read-sym-chars source pos "")))
    (let ((name (car result))
          (end-pos (cdr result)))
      ;; Handle empty symbol name - skip unrecognized character
      (if (= (string-length name) 0)
          (cons nil (+ pos 1))
          ;; Upcase the name
          (let ((uname (upcase-string name)))
            ;; Check for numeric or symbol
            (let ((first-ch (string-ref name #x0)))
              (cond ((digit? first-ch)
                 ;; Numeric - parse as integer
                 (read-int source pos))
                ((and (= first-ch #x2D)  ; starts with -
                      (> (string-length name) #x1)
                      (digit? (string-ref name #x1)))
                 ;; Negative number
                 (read-int source pos))
                ((and (= first-ch #x2B)  ; starts with +
                      (> (string-length name) #x1)
                      (digit? (string-ref name #x1)))
                 ;; Positive number
                 (read-int source pos))
                ((string= uname "NIL") (cons nil end-pos))
                ((string= uname "T") (cons t end-pos))
                (t (cons (intern uname) end-pos)))))))))

;;; habu-read returns (value . new-pos)
(defun habu-read (source pos)
  (labels
      ;; Read list elements iteratively using while loop
      ((read-list-elems (start-pos)
         (let ((current-pos start-pos)
               (acc nil)
               (done nil)
               (final-cdr nil))
           (while (not done)
             (let ((pos2 (skip-ws source current-pos)))
               (setq current-pos pos2)
               (let ((ch (char-at source current-pos)))
                 (cond
                   ((= ch #x29)  ; )
                    (setq done t)
                    (setq current-pos (+ current-pos 1)))
                   ((= ch #x2E)  ; dot for improper list
                    (let ((result (read-one (+ current-pos 1))))
                      (setq final-cdr (car result))
                      (setq current-pos (cdr result)))
                    (setq current-pos (+ (skip-ws source current-pos) 1))  ; skip )
                    (setq done t))
                   ((= ch #x0)  ; EOF
                    (setq done t))
                   (t
                    (let ((elem-result (read-one current-pos)))
                      (setq acc (cons (car elem-result) acc))
                      (setq current-pos (cdr elem-result))))))))
           ;; Reverse acc and attach final-cdr as tail
           (let ((result final-cdr))
             (while acc
               (setq result (cons (car acc) result))
               (setq acc (cdr acc)))
             (cons result current-pos))))

       ;; Read list
       (read-list (pos)
         (read-list-elems (+ pos #x1)))  ; skip opening (

       ;; Feature check for native execution:
       ;; :habu is always present, :sbcl is always absent
       ;; (native Habu reader never sees :sbcl as present)
       (feature-present? (sym)
         (if (symbolp sym)
             (let ((name (symbol-name sym)))
               (or (string= name "HABU")
                   (string= name "habu")))
             nil))

       ;; Read # macros
       (read-sharp (pos)
         (let ((ch (char-at source (+ pos #x1))))
           (cond
             ;; Hex number #xNNN
             ((= ch #x78)  ; x
              (read-hex-digits source (+ pos #x2) #x0))
             ;; Character literal #\x
             ((= ch #x5C)  ; backslash
              (let ((ch2 (char-at source (+ pos #x2))))
                ;; Check for named characters
                (if (alpha? ch2)
                    (let ((result (read-sym-chars source (+ pos #x2) "")))
                      (let ((name (car result)))
                        (cons (cond ((string= name "newline") #x0A)
                                    ((string= name "space") #x20)
                                    ((string= name "tab") #x09)
                                    (t ch2))
                              (cdr result))))
                    (cons ch2 (+ pos #x3)))))
             ;; #+ feature - read form only if feature present
             ((= ch #x2B)  ; +
              (let ((feat-result (read-one (+ pos #x2))))
                (let ((feature (car feat-result))
                      (pos3 (cdr feat-result)))
                  (let ((form-result (read-one pos3)))
                    (let ((form (car form-result))
                          (pos4 (cdr form-result)))
                      (if (feature-present? feature)
                          (cons form pos4)
                          (read-one pos4)))))))  ; Skip this form, read next
             ;; #- feature - read form only if feature absent
             ((= ch #x2D)  ; -
              (let ((feat-result (read-one (+ pos #x2))))
                (let ((feature (car feat-result))
                      (pos3 (cdr feat-result)))
                  (let ((form-result (read-one pos3)))
                    (let ((form (car form-result))
                          (pos4 (cdr form-result)))
                      (if (not (feature-present? feature))
                          (cons form pos4)
                          (read-one pos4)))))))  ; Skip this form, read next
             ;; Unknown
             (t (cons nil (+ pos #x2))))))

       ;; Main read dispatcher
       (read-one (pos)
         (let ((pos2 (skip-ws source pos)))
           (if (>= pos2 (string-length source))
               (cons nil pos2)  ; EOF
               (let ((ch (char-at source pos2)))
                 (cond
                   ;; String
                   ((= ch #x22) (read-str source pos2))
                   ;; List
                   ((= ch #x28) (read-list pos2))
                   ;; Quote
                   ((= ch #x27)
                    (let ((result (read-one (+ pos2 #x1))))
                      (cons (list 'quote (car result)) (cdr result))))
                   ;; Backquote
                   ((= ch #x60)
                    (let ((result (read-one (+ pos2 #x1))))
                      (cons (list 'quote (car result)) (cdr result))))
                   ;; Comma (unquote) - just read next form
                   ((= ch #x2C)
                    (let ((pos3 (+ pos2 #x1)))
                      ;; Check for ,@ (unquote-splicing)
                      (if (= (char-at source pos3) #x40)  ; @
                          (let ((result (read-one (+ pos3 #x1))))
                            (cons (list 'unquote-splicing (car result)) (cdr result)))
                          (let ((result (read-one pos3)))
                            (cons (list 'unquote (car result)) (cdr result))))))
                   ;; Sharp macros
                   ((= ch #x23) (read-sharp pos2))
                   ;; Pipe-quoted symbol |sym|
                   ((= ch #x7C)  ; |
                    (let ((result (read-pipe-symbol source (+ pos2 #x1) "")))
                      (let ((name (car result))
                            (end-pos (cdr result)))
                        (cons (intern name) end-pos))))
                   ;; Symbol or number
                   (t (read-sym source pos2))))))))

    (read-one pos)))

;; Helper for pipe-quoted symbols - uses two-pass to avoid O(n^2)
(defun read-pipe-symbol (source pos acc)
  "Read pipe-quoted symbol using vector accumulator (O(n) allocation)"
  (let ((start-pos pos)
        (current-pos pos))
    ;; First pass: find closing | or EOF
    (while (and (not (= (char-at source current-pos) #x7C))
                (not (= (char-at source current-pos) #x0)))
      (setq current-pos (+ current-pos 1)))
    ;; Calculate length
    (let ((len (- current-pos start-pos)))
      (if (= len 0)
          (cons acc (if (= (char-at source current-pos) #x7C)
                        (+ current-pos 1)
                        current-pos))
          ;; Allocate vector and fill
          (let ((vec (make-vector len))
                (i 0))
            (while (< i len)
              (vector-set vec i (char-at source (+ start-pos i)))
              (setq i (+ i 1)))
            (let ((new-str (make-string-from-vector vec)))
              (cons (if (= (string-length acc) 0)
                        new-str
                        (string-concat acc new-str))
                    (if (= (char-at source current-pos) #x7C)
                        (+ current-pos 1)
                        current-pos))))))))

;;; Package form processing helpers
(defun keyword-to-string (kw)
  "Convert a keyword symbol to its package name string.
   :FOO -> FOO, :foo -> FOO"
  (let ((name (symbol-name kw)))
    (if (and (> (string-length name) 0)
             (= (string-ref name 0) #x3A))  ; starts with :
        ;; Strip leading colon and upcase
        (upcase-string (substring name 1 (string-length name)))
        (upcase-string name))))

(defun substring (s start end)
  "Extract substring from start to end"
  (let ((len (- end start)))
    (if (<= len 0)
        ""
        (let ((vec (make-vector len))
              (i 0))
          (while (< i len)
            (vector-set vec i (string-ref s (+ start i)))
            (setq i (+ i 1)))
          (make-string-from-vector vec)))))

(defun process-package-form (form)
  "Process defpackage or in-package form, updating reader state.
   Returns t if form was processed, nil otherwise."
  (if (and (consp form) (symbolp (car form)))
      (let ((head-name (symbol-name (car form))))
        (cond
          ;; (in-package :pkg) or (in-package :pkg)
          ((string= head-name "IN-PACKAGE")
           (if (and (cdr form) (symbolp (cadr form)))
               (let ((pkg-name (keyword-to-string (cadr form))))
                 (set-current-package pkg-name)
                 t)
               nil))
          ;; (defpackage :pkg ...) - just register the package name
          ((string= head-name "DEFPACKAGE")
           (if (and (cdr form) (symbolp (cadr form)))
               (let ((pkg-name (keyword-to-string (cadr form))))
                 (add-package pkg-name)
                 t)
               nil))
          (t nil)))
      nil))

;;; Read all forms from source string - iterative
#-sbcl
(defun read-all (source)
  "Read all forms from source string - iterative.
   Processes defpackage and in-package forms to update reader state."
  (let ((pos 0)
        (acc nil)
        (source-len (string-length source)))
    (while (< pos source-len)
      (setq pos (skip-ws source pos))
      (if (< pos source-len)
          (let ((result (habu-read source pos)))
            (let ((form (car result)))
              ;; Process package forms to update reader state
              (process-package-form form)
              ;; Always accumulate the form (defpackage/in-package are kept)
              (setq acc (cons form acc)))
            (setq pos (cdr result)))))
    (reverse acc)))

#-sbcl
(defun reverse (lst)
  "Reverse list - iterative"
  (let ((current lst)
        (acc nil))
    (while (not (null current))
      (setq acc (cons (car current) acc))
      (setq current (cdr current)))
    acc))
