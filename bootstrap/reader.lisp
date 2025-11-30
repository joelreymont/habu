;;; Pure Habu Reader - For native compilation (no SBCL dependencies)
;;;
;;; This reader uses only pure Habu primitives (no dotimes, no string-upcase)
;;; Reads Lisp source code and produces S-expressions.

#+sbcl (in-package :habu)

;;; Core utilities (must be defined before reader functions)
;;; These are also defined in compiler-pure.lisp but reader needs them first

#-sbcl
(defun reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

#-sbcl
(defun length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

;; String comparison helper - no closures to avoid labels/closure bugs
#-sbcl
(defun string=-iter (s1 s2 i len)
  "Internal: compare strings starting at index i"
  (if (>= i len)
      t
      (if (= (string-ref s1 i) (string-ref s2 i))
          (string=-iter s1 s2 (+ i 1) len)
          nil)))

#-sbcl
(defun string= (s1 s2)
  "Compare two strings character by character"
  (let ((len1 (string-length s1))
        (len2 (string-length s2)))
    (if (= len1 len2)
        (string=-iter s1 s2 0 len1)
        nil)))

#-sbcl
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

;;; Character predicates
(defun whitespace? (ch)
  (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))

(defun digit? (ch)
  (and (>= ch #x30) (<= ch #x39)))

(defun hex-digit? (ch)
  (or (digit? ch)
      (and (>= ch #x41) (<= ch #x46))
      (and (>= ch #x61) (<= ch #x66))))

(defun alpha? (ch)
  (or (and (>= ch #x41) (<= ch #x5A))
      (and (>= ch #x61) (<= ch #x7A))))

(defun symbol-char? (ch)
  (or (alpha? ch) (digit? ch)
      (= ch #x2D) (= ch #x5F) (= ch #x2B) (= ch #x2A)
      (= ch #x2F) (= ch #x3D) (= ch #x3C) (= ch #x3E)
      (= ch #x21) (= ch #x3F) (= ch #x26) (= ch #x25) (= ch #x3A)))

;;; Character case conversion (inline, no string-upcase needed)
#-sbcl
(defun char-upcase (ch)
  (if (and (>= ch #x61) (<= ch #x7A))
      (- ch #x20)
      ch))

;;; Low-level access
#+sbcl
(defun char-at (source pos)
  (if (< pos (length source))
      (char-code (char source pos))
      0))

#-sbcl
(defun char-at (source pos)
  (if (< pos (string-length source))
      (string-ref source pos)
      0))

(defun digit-val (ch) (- ch #x30))

(defun hex-val (ch)
  (cond ((digit? ch) (- ch #x30))
        ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA))
        ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA))
        (t #x0)))

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

;;; Read hex digits - helper for read-hex
(defun read-hex-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (hex-digit? ch)
        (read-hex-digits source (+ pos #x1) (+ (* n #x10) (hex-val ch)))
        (cons n pos))))

;;; Read hex - returns (value . new-pos)
(defun read-hex (source pos)
  (read-hex-digits source pos #x0))

;;; Helper to convert char list to string (using labels, no dotimes)
(defun chars-to-string (chars)
  (let ((len (length chars)))
    (let ((vec (make-vector len)))
      (labels ((fill-vec (cs i)
                 (if (null cs)
                     vec
                     (progn
                       (vector-set vec i (car cs))
                       (fill-vec (cdr cs) (+ i 1))))))
        (fill-vec chars 0))
      (make-string-from-vector vec))))

;;; Helper to upcase chars while building string (inline char-upcase)
;;; chars is a list of char codes (integers)
(defun chars-to-string-upcase (chars)
  (let ((len (length chars)))
    (let ((vec (make-vector len)))
      (labels ((fill-vec (cs i)
                 (if (null cs)
                     vec
                     (progn
                       ;; Convert to char, upcase, back to code
                       #+sbcl (vector-set vec i (char-code (cl:char-upcase (code-char (car cs)))))
                       #-sbcl (vector-set vec i (char-upcase (car cs)))
                       (fill-vec (cdr cs) (+ i 1))))))
        (fill-vec chars 0))
      (make-string-from-vector vec))))

;;; Read symbol chars - helper for read-sym
(defun read-sym-chars (source pos chars)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (read-sym-chars source (+ pos #x1) (cons ch chars))
        (cons chars pos))))

;;; Read symbol - returns (symbol . new-pos)
;;; Symbols are upcased inline (no string-upcase function needed)
;;; Interns into HABU package for consistent symbol comparison
(defun read-sym (source pos)
  (let ((result (read-sym-chars source pos nil)))
    (let ((chars (car result))
          (end (cdr result)))
      ;; Build upcased string directly from chars
      (let ((uname (chars-to-string-upcase (reverse chars))))
        (cons (cond ((string= uname "NIL") nil)
                    ((string= uname "T") t)
                    ;; Intern into HABU package for consistency
                    #+sbcl (t (intern uname (find-package :habu)))
                    ;; In native Habu, use regular intern
                    #-sbcl (t (intern uname)))
              end)))))

;;; Read string chars - helper for read-str
(defun read-str-chars (source pos chars)
  (let ((ch (char-at source pos)))
    (cond
      ((= ch #x22) (cons chars (+ pos #x1)))  ; closing "
      ((= ch #x5C)  ; backslash
       (let ((esc (char-at source (+ pos #x1))))
         (let ((esc-char (cond ((= esc #x6E) #x0A)
                               ((= esc #x74) #x09)
                               ((= esc #x72) #x0D)
                               (t esc))))
           (read-str-chars source (+ pos #x2) (cons esc-char chars)))))
      ((= ch #x0) (cons chars pos))  ; EOF
      (t (read-str-chars source (+ pos #x1) (cons ch chars))))))

;;; Read string - returns (string . new-pos)
(defun read-str (source pos)
  (let ((result (read-str-chars source (+ pos #x1) nil)))  ; skip opening "
    (let ((chars (car result))
          (end (cdr result)))
      (cons (chars-to-string (reverse chars)) end))))

;;; Main reader - uses labels for mutual recursion
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
             ;; #x hex
             ((or (= ch #x78) (= ch #x58))
              (read-hex source (+ pos #x2)))
             ;; #' function
             ((= ch #x27)
              (let ((result (read-one (+ pos #x2))))
                (cons (list 'function (car result)) (cdr result))))
             ;; #\ character
             ((= ch #x5C)
              (let ((ch2 (char-at source (+ pos #x2))))
                (if (alpha? (char-at source (+ pos #x3)))
                    ;; Named char like #\newline
                    (let ((result (read-sym-chars source (+ pos #x2) nil)))
                      (let ((name (chars-to-string (reverse (car result)))))
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
                      (cons (list 'quasiquote (car result)) (cdr result))))
                   ;; Comma
                   ((= ch #x2C)
                    (if (= (char-at source (+ pos2 #x1)) #x40)
                        (let ((result (read-one (+ pos2 #x2))))
                          (cons (list 'unquote-splicing (car result)) (cdr result)))
                        (let ((result (read-one (+ pos2 #x1))))
                          (cons (list 'unquote (car result)) (cdr result)))))
                   ;; Sharp macro
                   ((= ch #x23) (read-sharp pos2))
                   ;; Number
                   ((or (digit? ch)
                        (and (or (= ch #x2D) (= ch #x2B))
                             (digit? (char-at source (+ pos2 #x1)))))
                    (read-int source pos2))
                   ;; Symbol
                   ((symbol-char? ch) (read-sym source pos2))
                   ;; Pipe-quoted symbol |...|
                   ((= ch #x7C)
                    (labels ((read-pipe-chars (p acc)
                               (let ((c (char-at source p)))
                                 (if (= c #x7C)
                                     (cons acc (+ p 1))  ; return chars and pos after closing |
                                     (read-pipe-chars (+ p 1) (cons c acc))))))
                      (let ((result (read-pipe-chars (+ pos2 1) nil)))
                        (let ((name (chars-to-string-upcase (reverse (car result)))))
                          (cons #+sbcl (intern name :habu)
                                #-sbcl (intern name)
                                (cdr result))))))
                   ;; Close paren
                   ((= ch #x29) (cons nil (+ pos2 #x1)))
                   ;; Skip unknown
                   (t (read-one (+ pos2 #x1)))))))))
    ;; Entry point
    (read-one pos)))

;;; Public interface
#-sbcl
(defun read-from-string (source)
  (car (habu-read source #x0)))

(defun read-all (source)
  (let ((len (string-length source)))
    (labels ((read-all-iter (pos acc)
               (let ((pos2 (skip-ws source pos)))
                 (if (>= pos2 len)
                     (reverse acc)
                     (let ((result (habu-read source pos2)))
                       (read-all-iter (cdr result) (cons (car result) acc)))))))
      (read-all-iter #x0 nil))))
