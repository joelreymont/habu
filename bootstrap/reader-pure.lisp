;;; Pure Habu Reader - For native compilation (no SBCL dependencies)
;;;
;;; This reader uses only pure Habu primitives (no dotimes, no string-upcase)
;;; Reads Lisp source code and produces S-expressions.

#+sbcl (in-package :habu)

;;; Core utilities (must be defined before reader functions)
;;; These are also defined in compiler-pure.lisp but reader needs them first

(defun pure-reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

(defun pure-length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

(defun pure-string= (s1 s2)
  "Compare two strings character by character"
  (labels ((cmp (i len1 len2)
             (cond
               ((/= len1 len2) nil)
               ((>= i len1) t)
               ((= (string-ref s1 i) (string-ref s2 i))
                (cmp (+ i 1) len1 len2))
               (t nil))))
    (cmp 0 (string-length s1) (string-length s2))))

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
(defun pure-char-upcase (ch)
  (if (and (>= ch #x61) (<= ch #x7A))
      (- ch #x20)
      ch))

;;; Low-level access
(defun char-at (source pos)
  (if (< pos (string-length source))
      (string-ref source pos)
      #x0))

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
  (let ((len (pure-length chars)))
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
(defun chars-to-string-upcase (chars)
  (let ((len (pure-length chars)))
    (let ((vec (make-vector len)))
      (labels ((fill-vec (cs i)
                 (if (null cs)
                     vec
                     (progn
                       (vector-set vec i (pure-char-upcase (car cs)))
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
      (let ((uname (chars-to-string-upcase (pure-reverse chars))))
        (cons (cond ((pure-string= uname "NIL") nil)
                    ((pure-string= uname "T") t)
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
      (cons (chars-to-string (pure-reverse chars)) end))))

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
               (or (pure-string= name "HABU")
                   (pure-string= name "habu")))
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
                      (let ((name (chars-to-string (pure-reverse (car result)))))
                        (cons (cond ((pure-string= name "newline") #x0A)
                                    ((pure-string= name "space") #x20)
                                    ((pure-string= name "tab") #x09)
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
                   ;; Close paren
                   ((= ch #x29) (cons nil (+ pos2 #x1)))
                   ;; Skip unknown
                   (t (read-one (+ pos2 #x1)))))))))
    ;; Entry point
    (read-one pos)))

;;; Public interface
(defun pure-read-from-string (source)
  (car (habu-read source #x0)))

(defun pure-read-all (source)
  (let ((len (string-length source)))
    (labels ((read-all-iter (pos acc)
               (let ((pos2 (skip-ws source pos)))
                 (if (>= pos2 len)
                     (pure-reverse acc)
                     (let ((result (habu-read source pos2)))
                       (read-all-iter (cdr result) (cons (car result) acc)))))))
      (read-all-iter #x0 nil))))
