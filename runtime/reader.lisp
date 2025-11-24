;;;; Habu Runtime - S-expression Reader
;;;; Read Habu S-expressions from strings

(in-package :habu-runtime)

(export '(runtime-read-from-string
          runtime-print-to-string))

;;; Tokenizer

(defstruct token
  type   ; :lparen :rparen :symbol :number :string :quote :dot
  value) ; String or number value

(defun whitespace-p (char)
  "Check if character is whitespace"
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun delimiter-p (char)
  "Check if character is a delimiter"
  (or (whitespace-p char)
      (member char '(#\( #\) #\" #\;))))

(defun tokenize (string)
  "Tokenize a string into a list of tokens"
  (let ((tokens nil)
        (i 0)
        (len (length string)))
    (loop while (< i len) do
      (let ((ch (char string i)))
        (cond
          ;; Skip whitespace
          ((whitespace-p ch)
           (incf i))

          ;; Skip comments (from ; to end of line)
          ((char= ch #\;)
           (loop while (and (< i len) (not (char= (char string i) #\Newline)))
                 do (incf i))
           (when (< i len) (incf i)))

          ;; Left paren
          ((char= ch #\()
           (push (make-token :type :lparen :value "(") tokens)
           (incf i))

          ;; Right paren
          ((char= ch #\))
           (push (make-token :type :rparen :value ")") tokens)
           (incf i))

          ;; Quote
          ((char= ch #\')
           (push (make-token :type :quote :value "'") tokens)
           (incf i))

          ;; Dot (for dotted pairs)
          ((and (char= ch #\.)
                (or (>= (1+ i) len)
                    (delimiter-p (char string (1+ i)))))
           (push (make-token :type :dot :value ".") tokens)
           (incf i))

          ;; String literal
          ((char= ch #\")
           (let ((start (1+ i))
                 (chars nil))
             (incf i) ; skip opening quote
             (loop while (and (< i len) (not (char= (char string i) #\"))) do
               (if (char= (char string i) #\\)
                   ;; Escape sequence
                   (progn
                     (incf i)
                     (when (< i len)
                       (let ((escaped (char string i)))
                         (push (case escaped
                                 (#\n #\Newline)
                                 (#\t #\Tab)
                                 (#\r #\Return)
                                 (t escaped))
                               chars))
                       (incf i)))
                   ;; Regular character
                   (progn
                     (push (char string i) chars)
                     (incf i))))
             ;; Check for unterminated string
             (if (>= i len)
                 (error "Unterminated string literal starting at position ~A" start)
                 (incf i)) ; skip closing quote
             (push (make-token :type :string
                              :value (coerce (nreverse chars) 'string))
                   tokens)))

          ;; Number or symbol
          (t
           (let ((start i))
             (loop while (and (< i len) (not (delimiter-p (char string i))))
                   do (incf i))
             (let* ((text (subseq string start i))
                    (num (parse-integer text :junk-allowed t)))
               (if (and num (= (length (write-to-string num)) (length text)))
                   ;; It's a number
                   (push (make-token :type :number :value num) tokens)
                   ;; It's a symbol
                   (push (make-token :type :symbol :value text) tokens))))))))
    (nreverse tokens)))

;;; Parser

(defun parse-tokens (tokens)
  "Parse tokens into S-expressions, returns (expr . remaining-tokens)"
  (when (null tokens)
    (error "Unexpected end of input"))

  (let ((tok (first tokens)))
    (case (token-type tok)
      (:lparen
       ;; Parse list
       (let ((elems nil)
             (rest-tokens (rest tokens))
             (dotted nil)
             (cdr-expr nil))
         (loop while (and rest-tokens
                         (not (eq (token-type (first rest-tokens)) :rparen)))
               do
               (when (eq (token-type (first rest-tokens)) :dot)
                 ;; Dotted pair
                 (setf dotted t)
                 (setf rest-tokens (rest rest-tokens))
                 (multiple-value-bind (expr new-rest)
                     (parse-tokens rest-tokens)
                   (setf cdr-expr expr)
                   (setf rest-tokens new-rest))
                 (return))
               (multiple-value-bind (expr new-rest)
                   (parse-tokens rest-tokens)
                 (push expr elems)
                 (setf rest-tokens new-rest)))

         ;; Check for closing paren
         (unless (and rest-tokens (eq (token-type (first rest-tokens)) :rparen))
           (error "Missing closing parenthesis"))

         (values (if dotted
                     ;; Dotted list: (a b c . d)
                     ;; Build full prefix list, then set cdr-expr as final cdr
                     (let ((reversed-elems (nreverse elems)))
                       (if (null reversed-elems)
                           cdr-expr  ; Just `. x)` => x
                           ;; Build list from right to left
                           (reduce (lambda (acc elem) (cons elem acc))
                                   reversed-elems
                                   :from-end t
                                   :initial-value cdr-expr)))
                     ;; Regular list
                     (nreverse elems))
                 (rest rest-tokens))))

      (:rparen
       (error "Unexpected closing parenthesis"))

      (:quote
       ;; Quote: 'x => (quote x)
       (multiple-value-bind (expr rest-tokens)
           (parse-tokens (rest tokens))
         (values (list 'quote expr) rest-tokens)))

      (:number
       (values (token-value tok) (rest tokens)))

      (:string
       (values (token-value tok) (rest tokens)))

      (:symbol
       (values (token-value tok) (rest tokens)))

      (t
       (error "Unexpected token: ~A" tok)))))

(defun runtime-read-from-string (str-ptr)
  "Read one S-expression from a Habu string, return as Habu runtime value"
  (unless (= (logand str-ptr #xF) +tag-string+)
    (error "Not a string: ~X" str-ptr))

  ;; Convert Habu string to Lisp string
  (let* ((lisp-str (runtime-string->lisp str-ptr))
         (tokens (tokenize lisp-str)))
    (when (null tokens)
      (error "No expression to read"))

    ;; Parse first expression
    (multiple-value-bind (expr rest-tokens)
        (parse-tokens tokens)
      ;; Convert Lisp expression to Habu runtime value
      (lisp-to-habu expr))))

;;; Printer

(defun runtime-print-to-string (habu-value)
  "Print a Habu runtime value to a Habu string"
  (let ((lisp-str (habu-to-string habu-value)))
    (runtime-lisp->string lisp-str)))

(defun habu-to-string (value)
  "Convert Habu runtime value to Lisp string representation"
  (cond
    ;; Nil (0) - check first before tag
    ((zerop value)
     "NIL")

    (t
     (let ((tag (logand value #xF)))
       (cond
         ;; Fixnum (tag 0x0)
         ((= tag 0)
          (format nil "~D" (ash value -4)))

         ;; Cons cell (tag 0x1)
         ((= tag +tag-cons+)
          (format nil "(~A)" (cons-to-string value)))

         ;; Symbol (tag 0x2)
         ((= tag +tag-symbol+)
          (runtime-symbol->print-name value))

         ;; String (tag 0x3)
         ((= tag +tag-string+)
          (format nil "\"~A\"" (escape-string (runtime-string->lisp value))))

         ;; Closure (tag 0x5)
         ((= tag +tag-closure+)
          (format nil "#<CLOSURE:~X>" value))

         (t
          (format nil "#<UNKNOWN:~X>" value)))))))

(defun cons-to-string (cons-ptr)
  "Convert cons cell to string (without outer parens)"
  (let ((car-val (runtime-car cons-ptr))
        (cdr-val (runtime-cdr cons-ptr)))
    (cond
      ;; Proper list: (a b c)
      ((zerop cdr-val)
       (habu-to-string car-val))

      ;; Continue list
      ((= (logand cdr-val #xF) +tag-cons+)
       (format nil "~A ~A"
               (habu-to-string car-val)
               (cons-to-string cdr-val)))

      ;; Dotted pair: (a . b)
      (t
       (format nil "~A . ~A"
               (habu-to-string car-val)
               (habu-to-string cdr-val))))))

(defun escape-string (str)
  "Escape special characters in string for printing"
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\Newline (write-string "\\n" out))
        (#\Tab (write-string "\\t" out))
        (#\Return (write-string "\\r" out))
        (#\\ (write-string "\\\\" out))
        (#\" (write-string "\\\"" out))
        (t (write-char ch out))))))

;;; Helper: Convert Lisp expression to Habu runtime value

(defun lisp-to-habu (expr)
  "Convert a Lisp expression to a Habu runtime value"
  (cond
    ;; Number -> fixnum
    ((integerp expr)
     (ash expr 4)) ; Tag as fixnum

    ;; String -> Habu string
    ((stringp expr)
     (runtime-lisp->string expr))

    ;; NIL -> 0
    ((null expr)
     0)

    ;; List -> cons cells
    ((consp expr)
     (runtime-cons (lisp-to-habu (car expr))
                   (lisp-to-habu (cdr expr))))

    ;; Symbol -> intern and return symbol pointer
    ((symbolp expr)
     (let ((pkg (or (symbol-package expr)
                    (find-package *current-package*))))
       (runtime-find-symbol (symbol-name expr)
                            (when pkg (package-name pkg)))))

    (t
     (error "Cannot convert to Habu value: ~S" expr))))
