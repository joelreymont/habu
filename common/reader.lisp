;;; Habu Reader - Pure Lisp implementation for self-hosting
;;;
;;; Reads Lisp source code and produces S-expressions.
;;; Supports: numbers, hex (#x), symbols, strings, lists, quote, function quote (#')
;;;
;;; Functional style: functions take (source pos) and return (value . new-pos)
;;; Uses labels for mutually recursive functions (habu-read, read-list-elems, etc.)
;;; NOTE: Helper functions must be defined BEFORE callers (no forward references)

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

;;; Read digits - helper for read-int (must be defined first)
(defun read-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (let ((new-pos (+ pos #x1))
              (new-n (+ (* n #xA) (digit-val ch))))
          (read-digits source new-pos new-n))
        (cons n pos))))

;;; Read integer - returns (value . new-pos)
(defun read-int (source pos)
  (let ((neg nil)
        (start pos))
    (let ((ch (char-at source pos)))
      (cond ((= ch #x2D) (setf neg t) (setf start (+ pos #x1)))
            ((= ch #x2B) (setf start (+ pos #x1)))))
    (let ((result (read-digits source start #x0)))
      (let ((val (car result))
            (end (cdr result)))
        (cons (if neg (- #x0 val) val) end)))))

;;; Read hex digits - helper for read-hex (must be defined first)
(defun read-hex-digits (source pos n)
  (let ((ch (char-at source pos)))
    (if (hex-digit? ch)
        (let ((new-pos (+ pos #x1))
              (new-n (+ (* n #x10) (hex-val ch))))
          (read-hex-digits source new-pos new-n))
        (cons n pos))))

;;; Read hex - returns (value . new-pos)
(defun read-hex (source pos)
  (read-hex-digits source pos #x0))

;;; Helper to convert char list to string
(defun chars-to-string (chars)
  (let* ((len (length chars))
         (vec (make-vector len)))
    (dotimes (i len)
      (vector-set vec i (nth i chars)))
    (make-string-from-vector vec)))

;;; Read symbol chars - helper for read-sym (must be defined first)
(defun read-sym-chars (source pos chars)
  (let ((ch (char-at source pos)))
    (if (symbol-char? ch)
        (let ((new-pos (+ pos #x1))
              (new-chars (cons ch chars)))
          (read-sym-chars source new-pos new-chars))
        (cons chars pos))))

;;; Read symbol - returns (symbol . new-pos)
(defun read-sym (source pos)
  (let ((result (read-sym-chars source pos nil)))
    (let ((chars (car result))
          (end (cdr result)))
      (let ((name (chars-to-string (reverse chars))))
        (cons (cond ((string= name "nil") nil)
                    ((string= name "t") t)
                    (t (intern name)))
              end)))))

;;; Read string chars - helper for read-str (must be defined first)
(defun read-str-chars (source pos chars)
  (let ((ch (char-at source pos)))
    (cond
      ((= ch #x22) (cons chars (+ pos #x1)))  ; closing "
      ((= ch #x5C)  ; backslash
       (let* ((esc (char-at source (+ pos #x1)))
              (new-pos (+ pos #x2))
              (esc-char (cond ((= esc #x6E) #x0A)
                              ((= esc #x74) #x09)
                              ((= esc #x72) #x0D)
                              (t esc)))
              (new-chars (cons esc-char chars)))
         (read-str-chars source new-pos new-chars)))
      ((= ch #x0) (cons chars pos))  ; EOF
      (t (let ((new-pos (+ pos #x1))
               (new-chars (cons ch chars)))
           (read-str-chars source new-pos new-chars))))))

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
(defun read-from-string (source)
  (car (habu-read source #x0)))

(defun read-all-from-string (source)
  (let ((len (string-length source)))
    (labels ((read-all (pos acc)
               (let ((pos2 (skip-ws source pos)))
                 (if (>= pos2 len)
                     (reverse acc)
                     (let ((result (habu-read source pos2)))
                       (read-all (cdr result) (cons (car result) acc)))))))
      (read-all #x0 nil))))

(defun read-source-file (path)
  (let ((content (read-file path)))
    (if content
        (read-all-from-string content)
        nil)))
