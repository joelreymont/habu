;;;; Reader - Parse S-expressions from strings
;;;; Written in Habu Lisp - NO C dependencies except primitives

;;; Character utilities (characters are fixnums)

(defun char-code (ch-str)
  "Get character code from single-char string"
  (string-ref ch-str (quote 0)))

(defun is-whitespace? (ch)
  "Check if character is whitespace"
  (if (= ch (quote 32))  ; space
      (quote 1)
      (if (= ch (quote 10))  ; newline
          (quote 1)
          (if (= ch (quote 9))  ; tab
              (quote 1)
              (if (= ch (quote 13))  ; carriage return
                  (quote 1)
                  (quote nil))))))

(defun is-digit? (ch)
  "Check if character is a digit 0-9"
  (if (>= ch (quote 48))  ; '0'
      (<= ch (quote 57))  ; '9'
      (quote nil)))

(defun is-alpha? (ch)
  "Check if character is alphabetic"
  (if (>= ch (quote 65))  ; 'A'
      (if (<= ch (quote 90))  ; 'Z'
          (quote 1)
          (if (>= ch (quote 97))  ; 'a'
              (<= ch (quote 122))  ; 'z'
              (quote nil)))
      (quote nil)))

(defun is-special-symbol-char? (ch)
  "Check if character can be in a symbol"
  (if (= ch (quote 43)) (quote 1)  ; +
  (if (= ch (quote 45)) (quote 1)  ; -
  (if (= ch (quote 42)) (quote 1)  ; *
  (if (= ch (quote 47)) (quote 1)  ; /
  (if (= ch (quote 61)) (quote 1)  ; =
  (if (= ch (quote 60)) (quote 1)  ; <
  (if (= ch (quote 62)) (quote 1)  ; >
  (if (= ch (quote 63)) (quote 1)  ; ?
  (if (= ch (quote 33)) (quote 1)  ; !
      (quote nil)))))))))))

(defun is-symbol-char? (ch)
  "Check if character can be part of a symbol"
  (if (is-alpha? ch)
      (quote 1)
      (if (is-digit? ch)
          (quote 1)
          (if (is-special-symbol-char? ch)
              (quote 1)
              (quote nil)))))

(defun is-paren? (ch)
  "Check if character is parenthesis"
  (if (= ch (quote 40))  ; '('
      (quote 1)
      (if (= ch (quote 41))  ; ')'
          (quote 1)
          (quote nil))))

(defun digit-to-int (ch)
  "Convert digit character to integer"
  (- ch (quote 48)))

;;; String builder (using cons list of chars, then convert)

(defun build-string-from-chars (chars)
  "Build string from list of character codes"
  (if (nil? chars)
      (make-string-from-cstr (quote ""))
      (let ((len (length-helper chars (quote 0))))
        (build-string-iter chars (quote 0) len (make-vector len)))))

(defun length-helper (lst acc)
  (if (nil? lst)
      acc
      (length-helper (cdr lst) (+ acc (quote 1)))))

(defun build-string-iter (chars idx len vec)
  (if (>= idx len)
      (vector-to-string vec len)
      (progn
        (vector-set vec idx (car chars))
        (build-string-iter (cdr chars) (+ idx (quote 1)) len vec))))

(defun vector-to-string (vec len)
  "Convert vector of chars to string - PLACEHOLDER"
  (make-string-from-cstr (quote "symbol")))

;;; Reader state: (string . index)

(defun make-reader-state (str)
  (cons str (quote 0)))

(defun reader-string (state)
  (car state))

(defun reader-index (state)
  (cdr state))

(defun reader-at-end? (state)
  (let ((str (car state)))
    (let ((idx (cdr state)))
      (>= idx (string-length str)))))

(defun reader-peek (state)
  "Get current character without advancing"
  (if (reader-at-end? state)
      (quote nil)
      (let ((str (car state)))
        (string-ref str (cdr state)))))

(defun reader-advance (state)
  "Move to next character"
  (cons (car state) (+ (cdr state) (quote 1))))

;;; Skip whitespace

(defun skip-whitespace (state)
  (if (reader-at-end? state)
      state
      (let ((ch (reader-peek state)))
        (if (is-whitespace? ch)
            (skip-whitespace (reader-advance state))
            state))))

;;; Parse number

(defun parse-number-digits (state acc)
  (if (reader-at-end? state)
      (cons acc state)
      (let ((ch (reader-peek state)))
        (if (is-digit? ch)
            (parse-number-digits
              (reader-advance state)
              (+ (* acc (quote 10)) (digit-to-int ch)))
            (cons acc state)))))

(defun parse-number (state)
  (parse-number-digits state (quote 0)))

;;; Parse symbol

(defun collect-symbol-chars (state chars)
  (if (reader-at-end? state)
      (cons chars state)
      (let ((ch (reader-peek state)))
        (if (is-symbol-char? ch)
            (collect-symbol-chars
              (reader-advance state)
              (cons ch chars))
            (cons chars state)))))

(defun reverse-list (lst)
  (reverse-helper lst (quote nil)))

(defun reverse-helper (lst acc)
  (if (nil? lst)
      acc
      (reverse-helper (cdr lst) (cons (car lst) acc))))

(defun parse-symbol (state)
  "Parse symbol and return as symbol value"
  (let ((result (collect-symbol-chars state (quote nil))))
    (let ((chars (reverse-list (car result))))
      (let ((new-state (cdr result)))
        (let ((sym-str (build-string-from-chars chars)))
          (cons (make-symbol sym-str) new-state))))))

;;; Parse list

(defun parse-list-elements (state)
  (let ((state2 (skip-whitespace state)))
    (if (reader-at-end? state2)
        (cons (quote nil) state2)
        (let ((ch (reader-peek state2)))
          (if (= ch (quote 41))  ; ')'
              (cons (quote nil) (reader-advance state2))
              (let ((elem-result (read-one state2)))
                (let ((elem (car elem-result)))
                  (let ((state3 (cdr elem-result)))
                    (let ((rest-result (parse-list-elements state3)))
                      (cons (cons elem (car rest-result))
                            (cdr rest-result)))))))))))

(defun parse-list (state)
  "Parse list starting with '('"
  (let ((state2 (reader-advance state)))  ; skip '('
    (parse-list-elements state2)))

;;; Main reader

(defun read-one (state)
  "Read one S-expression from state"
  (let ((state2 (skip-whitespace state)))
    (if (reader-at-end? state2)
        (cons (quote nil) state2)
        (let ((ch (reader-peek state2)))
          (if (= ch (quote 40))  ; '('
              (parse-list state2)
              (if (is-digit? ch)
                  (parse-number state2)
                  (if (is-symbol-char? ch)
                      (parse-symbol state2)
                      (cons (quote nil) state2))))))))

(defun read-from-string (str)
  "Parse one S-expression from string"
  (let ((state (make-reader-state str)))
    (car (read-one state))))
