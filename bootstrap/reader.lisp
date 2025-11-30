;;; Habu Lisp Reader - reads Lisp source into S-expressions
;;; This file MUST have no SBCL dependencies - it's used by Stage 1+

;;; For SBCL, we just use the package system
#+sbcl (in-package :habu)

;;; Global intern table for native mode
(defvar *intern-table* nil)
(defvar *lambda-counter* 0)

;;; SBCL version - symbol interning works automatically
#+sbcl
(defun ensure-symbols-registered ()
  "In SBCL, symbol interning works correctly - nothing to do."
  nil)

;;; Native version - build symbol table incrementally to avoid deep nesting
#-sbcl
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

#-sbcl
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

#-sbcl
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

#-sbcl
(defun make-predicates ()
  "Build list of predicate entries"
  (cons (cons "NULL" 'null)
  (cons (cons "CONSP" 'consp)
  (cons (cons "NUMBERP" 'numberp)
  (cons (cons "SYMBOLP" 'symbolp)
  (cons (cons "STRINGP" 'stringp)
  (cons (cons "VECTORP" 'vectorp)
        nil)))))))

#-sbcl
(defun make-string-ops ()
  "Build list of string operation entries"
  (cons (cons "STRING-LENGTH" 'string-length)
  (cons (cons "STRING-REF" 'string-ref)
  (cons (cons "STRING-CONCAT" 'string-concat)
  (cons (cons "STRING-EQUAL" 'string-equal)
        nil)))))

#-sbcl
(defun make-vector-ops ()
  "Build list of vector operation entries"
  (cons (cons "MAKE-VECTOR" 'make-vector)
  (cons (cons "VECTOR-REF" 'vector-ref)
  (cons (cons "VECTOR-SET" 'vector-set)
  (cons (cons "VECTOR-LENGTH" 'vector-length)
  (cons (cons "MAKE-STRING-FROM-VECTOR" 'make-string-from-vector)
  (cons (cons "BUFFER-TO-STRING" 'buffer-to-string)
        nil)))))))

#-sbcl
(defun make-symbol-ops ()
  "Build list of symbol operation entries"
  (cons (cons "SYMBOL-NAME" 'symbol-name)
  (cons (cons "MAKE-SYMBOL-FROM-STRING" 'make-symbol-from-string)
        nil)))

#-sbcl
(defun make-system-ops ()
  "Build list of system operation entries"
  (cons (cons "SYS-EXIT" 'sys-exit)
  (cons (cons "SYS-OPEN" 'sys-open)
  (cons (cons "SYS-READ" 'sys-read)
  (cons (cons "SYS-WRITE" 'sys-write)
  (cons (cons "SYS-CLOSE" 'sys-close)
  (cons (cons "NATIVE-READ-FILE" 'native-read-file)
  (cons (cons "GET-INTERN-TABLE" 'get-intern-table)
  (cons (cons "SET-INTERN-TABLE" 'set-intern-table)
  (cons (cons "GET-LAMBDA-COUNTER" 'get-lambda-counter)
  (cons (cons "SET-LAMBDA-COUNTER" 'set-lambda-counter)
  (cons (cons "NIL" 'nil)
  (cons (cons "T" 't)
        nil)))))))))))))

#-sbcl
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

#-sbcl
(defun make-ir-cons ()
  "Build list of cons IR tag entries"
  (cons (cons "CONS-IR" 'cons-ir)
  (cons (cons "CAR-IR" 'car-ir)
  (cons (cons "CDR-IR" 'cdr-ir)
  (cons (cons "SETCAR-IR" 'setcar-ir)
  (cons (cons "SETCDR-IR" 'setcdr-ir)
        nil))))))

#-sbcl
(defun make-ir-control ()
  "Build list of control flow IR tag entries"
  (cons (cons "IF-IR" 'if-ir)
  (cons (cons "LET-IR" 'let-ir)
  (cons (cons "LET*-IR" 'let*-ir)
  (cons (cons "PROGN-IR" 'progn-ir)
  (cons (cons "OR-IR" 'or-ir)
  (cons (cons "AND-IR" 'and-ir)
        nil)))))))

#-sbcl
(defun make-ir-functions ()
  "Build list of function IR tag entries"
  (cons (cons "CALL-FN" 'call-fn)
  (cons (cons "FUNCALL-IR" 'funcall-ir)
  (cons (cons "LAMBDA-IR" 'lambda-ir)
  (cons (cons "FN-REF-IR" 'fn-ref-ir)
  (cons (cons "LABELS-IR" 'labels-ir)
        nil))))))

#-sbcl
(defun make-ir-syscalls ()
  "Build list of syscall IR tag entries"
  (cons (cons "SYS-EXIT-IR" 'sys-exit-ir)
  (cons (cons "SYS-OPEN-IR" 'sys-open-ir)
  (cons (cons "SYS-READ-IR" 'sys-read-ir)
  (cons (cons "SYS-WRITE-IR" 'sys-write-ir)
  (cons (cons "SYS-CLOSE-IR" 'sys-close-ir)
  (cons (cons "SETQ-IR" 'setq-ir)
        nil)))))))

#-sbcl
(defun make-ir-predicates ()
  "Build list of predicate IR tag entries"
  (cons (cons "GET-TAG" 'get-tag)
  (cons (cons "QUOTE-IR" 'quote-ir)
  (cons (cons "NULL-IR" 'null-ir)
  (cons (cons "LIST-IR" 'list-ir)
        nil)))))

#-sbcl
(defun make-ir-strings ()
  "Build list of string IR tag entries"
  (cons (cons "STRING-LENGTH-IR" 'string-length-ir)
  (cons (cons "STRING-REF-IR" 'string-ref-ir)
  (cons (cons "STRING-CONCAT-IR" 'string-concat-ir)
  (cons (cons "SYMBOL-NAME-IR" 'symbol-name-ir)
  (cons (cons "MAKE-SYMBOL-IR" 'make-symbol-ir)
        nil))))))

#-sbcl
(defun make-ir-vectors ()
  "Build list of vector IR tag entries"
  (cons (cons "MAKE-VECTOR-IR" 'make-vector-ir)
  (cons (cons "VECTOR-REF-IR" 'vector-ref-ir)
  (cons (cons "VECTOR-SET-IR" 'vector-set-ir)
  (cons (cons "VECTOR-LENGTH-IR" 'vector-length-ir)
  (cons (cons "MAKE-STRING-FROM-VECTOR-IR" 'make-string-from-vector-ir)
  (cons (cons "BUFFER-TO-STRING-IR" 'buffer-to-string-ir)
        nil)))))))

#-sbcl
(defun make-ir-intern ()
  "Build list of intern table IR tag entries"
  (cons (cons "GET-INTERN-TABLE-IR" 'get-intern-table-ir)
  (cons (cons "SET-INTERN-TABLE-IR" 'set-intern-table-ir)
  (cons (cons "GET-LAMBDA-COUNTER-IR" 'get-lambda-counter-ir)
  (cons (cons "SET-LAMBDA-COUNTER-IR" 'set-lambda-counter-ir)
  (cons (cons "NATIVE-READ-FILE-IR" 'native-read-file-ir)
        nil))))))

#-sbcl
(defun make-ir-lambda ()
  "Build list of lambda IR tag entries"
  (cons (cons "LIFTED-LAMBDA-IR" 'lifted-lambda-ir)
  (cons (cons "LAMBDA-REF" 'lambda-ref)
  (cons (cons ":CALL" ':call)
  (cons (cons ":EXTERN-CALL" ':extern-call)
        nil)))))

#-sbcl
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

#-sbcl
(defun map-list (fn lst)
  "Map function over list"
  (if (null lst)
      nil
      (cons (funcall fn (car lst)) (map-list fn (cdr lst)))))

#-sbcl
(defun assoc-get (key alist)
  "Get value for key in alist"
  (if (null alist)
      nil
      (if (eq key (car (car alist)))
          (cdr (car alist))
          (assoc-get key (cdr alist)))))

;;; String comparison

#-sbcl
(defun string=-iter (s1 s2 i len)
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string=-iter s1 s2 (+ i 1) len)
          nil)))

#-sbcl
(defun string= (s1 s2)
  "Compare two strings for equality"
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (string=-iter s1 s2 0 len1)
        nil)))

(defun find-interned (name table)
  "Find symbol with NAME in intern TABLE (alist of (name . symbol))"
  (if (null table)
      nil
      (if (string= name (car (car table)))
          (cdr (car table))
          (find-interned name (cdr table)))))

#-sbcl
(defun intern (name)
  "Intern a string as a symbol. Returns existing symbol if found, else creates new."
  (let ((existing (find-interned name (get-intern-table))))
    (if existing
        existing
        (let ((sym (make-symbol-from-string name)))
          (set-intern-table (cons (cons name sym) (get-intern-table)))
          sym))))

;;; Global state accessors (implemented in codegen for native)
#-sbcl
(defun get-intern-table () *intern-table*)

#-sbcl
(defun set-intern-table (table)
  (setq *intern-table* table))

#-sbcl
(defun get-lambda-counter () *lambda-counter*)

#-sbcl
(defun set-lambda-counter (n)
  (setq *lambda-counter* n))

;;; Character predicates

(defun whitespace? (ch)
  (or (= ch #x20)   ; space
      (= ch #x09)   ; tab
      (= ch #x0A)   ; newline
      (= ch #x0D))) ; return

(defun digit? (ch)
  (and (>= ch #x30) (<= ch #x39)))

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
(defun char-at (str pos)
  (if (>= pos (string-length str))
      #x0
      (string-ref str pos)))

;;; Skip whitespace and comments
(defun skip-line (source pos)
  (let ((ch (char-at source pos)))
    (if (or (= ch #x0A) (= ch #x0))
        (+ pos #x1)
        (skip-line source (+ pos #x1)))))

(defun skip-ws (source pos)
  (let ((ch (char-at source pos)))
    (cond
      ((whitespace? ch) (skip-ws source (+ pos #x1)))
      ((= ch #x3B)  ; semicolon - line comment
       (skip-ws source (skip-line source (+ pos #x1))))
      (t pos))))

;;; Read digits - helper for read-int
(defun read-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (read-digits source (+ pos #x1) (+ (* n #xA) (digit-val ch)))
        (cons n pos))))

;;; Read integer - returns (value . new-pos)
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

(defun read-hex-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (hex-digit? ch)
        (read-hex-digits source (+ pos #x1) (+ (* n #x10) (hex-digit-val ch)))
        (cons n pos))))

;;; Read string literal - returns (string . new-pos)
(defun read-str-chars (source pos acc)
  (let ((ch (char-at source pos)))
    (cond
      ((= ch #x22) ; closing quote
       (cons acc (+ pos #x1)))
      ((= ch #x5C) ; backslash escape
       (let ((next-ch (char-at source (+ pos #x1))))
         (cond
           ((= next-ch #x6E) ; \n
            (read-str-chars source (+ pos #x2) (string-concat acc (make-string-from-vector (cons #x0A nil)))))
           ((= next-ch #x74) ; \t
            (read-str-chars source (+ pos #x2) (string-concat acc (make-string-from-vector (cons #x09 nil)))))
           ((= next-ch #x22) ; \"
            (read-str-chars source (+ pos #x2) (string-concat acc (make-string-from-vector (cons #x22 nil)))))
           ((= next-ch #x5C) ; \\
            (read-str-chars source (+ pos #x2) (string-concat acc (make-string-from-vector (cons #x5C nil)))))
           (t
            (read-str-chars source (+ pos #x2) (string-concat acc (make-string-from-vector (cons next-ch nil))))))))
      ((= ch #x0) ; EOF
       (cons acc pos))
      (t
       (read-str-chars source (+ pos #x1) (string-concat acc (make-string-from-vector (cons ch nil))))))))

(defun read-str (source pos)
  (read-str-chars source (+ pos #x1) ""))

;;; Read symbol - returns (symbol-or-number . new-pos)
(defun read-sym-chars (source pos acc)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (read-sym-chars source (+ pos #x1)
                        (string-concat acc (make-string-from-vector (cons ch nil))))
        (cons acc pos))))

(defun upcase-char (ch)
  (if (and (>= ch #x61) (<= ch #x7A))
      (- ch #x20)
      ch))

(defun upcase-string-iter (s i len acc)
  (if (>= i len)
      acc
      (upcase-string-iter s (+ i 1) len
                          (string-concat acc (make-string-from-vector (cons (upcase-char (string-ref s i)) nil))))))

(defun upcase-string (s)
  (upcase-string-iter s 0 (string-length s) ""))

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
                #+sbcl (t (cons (intern uname (find-package :habu)) end-pos))
                #-sbcl (t (cons (intern uname) end-pos)))))))))

;;; habu-read returns (value . new-pos)
(defun habu-read (source pos)
  (labels
      ;; Read list elements
      ((read-list-elems (pos)
         (let ((pos2 (skip-ws source pos)))
           (let ((ch (char-at source pos2)))
             (cond
               ((= ch #x29) (cons nil (+ pos2 #x1)))  ; )
               ((= ch #x2E)  ; dot for improper list
                (let ((result (read-one (+ pos2 #x1))))
                  (let ((cdr-val (car result))
                        (pos3 (cdr result)))
                    (let ((pos4 (skip-ws source pos3)))
                      (cons cdr-val (+ pos4 #x1))))))  ; skip )
               ((= ch #x0) (cons nil pos2))  ; EOF
               (t
                (let ((elem-result (read-one pos2)))
                  (let ((elem (car elem-result))
                        (pos3 (cdr elem-result)))
                    (let ((rest-result (read-list-elems pos3)))
                      (cons (cons elem (car rest-result))
                            (cdr rest-result))))))))))

       ;; Read list
       (read-list (pos)
         (read-list-elems (+ pos #x1)))  ; skip opening (

       ;; Feature check: :habu is always present, :sbcl is absent
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
                        (cons #+sbcl (intern name :habu)
                              #-sbcl (intern name)
                              end-pos))))
                   ;; Symbol or number
                   (t (read-sym source pos2))))))))

    (read-one pos)))

;; Helper for pipe-quoted symbols
(defun read-pipe-symbol (source pos acc)
  (let ((ch (char-at source pos)))
    (if (= ch #x7C)  ; closing |
        (cons acc (+ pos #x1))
        (if (= ch #x0)  ; EOF
            (cons acc pos)
            (read-pipe-symbol source (+ pos #x1)
                              (string-concat acc (make-string-from-vector (cons ch nil))))))))

;;; Read all forms from source string
(defun read-all-iter (source pos acc)
  (let ((pos2 (skip-ws source pos)))
    (if (>= pos2 (string-length source))
        (reverse acc)
        (let ((result (habu-read source pos2)))
          (let ((form (car result))
                (new-pos (cdr result)))
            (if (>= new-pos (string-length source))
                (reverse (cons form acc))
                (read-all-iter source new-pos (cons form acc))))))))

(defun read-all (source)
  "Read all forms from source string"
  (read-all-iter source #x0 nil))

#-sbcl
(defun reverse (lst)
  (reverse-iter lst nil))

#-sbcl
(defun reverse-iter (lst acc)
  (if (null lst)
      acc
      (reverse-iter (cdr lst) (cons (car lst) acc))))
