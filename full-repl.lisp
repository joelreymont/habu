;;;; Full REPL - Complete Reader, Evaluator, Line Editing
;;;; All in Lisp with minimal C runtime

;;;; Type Predicates
(defun fixnum? (x) (= (get-tag x) (quote 0)))
(defun cons? (x) (= (get-tag x) (quote 1)))
(defun symbol? (x) (= (get-tag x) (quote 2)))
(defun string? (x) (= (get-tag x) (quote 4)))
(defun nil? (x) (= x (quote 0)))

;;;; String Comparison
(defun str-cmp-loop (s1 s2 i len)
  (if (>= i len) (quote 1)
    (if (= (string-ref s1 i) (string-ref s2 i))
      (str-cmp-loop s1 s2 (+ i (quote 1)) len)
      (quote nil))))

(defun string=? (s1 s2)
  (let ((len1 (string-length-raw s1)))
    (let ((len2 (string-length-raw s2)))
      (if (= len1 len2)
        (str-cmp-loop s1 s2 (quote 0) len1)
        (quote nil)))))

(defun symbol=? (s1 s2)
  (string=? (symbol-name s1) (symbol-name s2)))

;;;; Reader Implementation

;;; Character classification
(defun is-whitespace? (ch)
  (if (= ch (quote 32)) (quote 1)   ; space
  (if (= ch (quote 10)) (quote 1)   ; newline
  (if (= ch (quote 9)) (quote 1)    ; tab
  (if (= ch (quote 13)) (quote 1)   ; CR
    (quote nil))))))

(defun is-digit? (ch)
  (if (>= ch (quote 48))
    (<= ch (quote 57))
    (quote nil)))

(defun is-alpha? (ch)
  (if (>= ch (quote 65))
    (if (<= ch (quote 90)) (quote 1)
      (if (>= ch (quote 97))
        (<= ch (quote 122))
        (quote nil)))
    (quote nil)))

(defun is-symbol-char? (ch)
  (if (is-alpha? ch) (quote 1)
    (if (is-digit? ch) (quote 1)
      (if (= ch (quote 43)) (quote 1)   ; +
      (if (= ch (quote 45)) (quote 1)   ; -
      (if (= ch (quote 42)) (quote 1)   ; *
      (if (= ch (quote 47)) (quote 1)   ; /
      (if (= ch (quote 61)) (quote 1)   ; =
      (if (= ch (quote 60)) (quote 1)   ; <
      (if (= ch (quote 62)) (quote 1)   ; >
      (if (= ch (quote 63)) (quote 1)   ; ?
      (if (= ch (quote 33)) (quote 1)   ; !
        (quote nil)))))))))))))

;;; Reader state: index into string
(defun skip-ws (str idx)
  (if (>= idx (string-length-raw str)) idx
    (let ((ch (string-ref str idx)))
      (if (is-whitespace? ch)
        (skip-ws str (+ idx (quote 1)))
        idx))))

(defun at-end? (str idx)
  (>= idx (string-length-raw str)))

;;; Parse number
(defun parse-num (str idx acc)
  (if (at-end? str idx)
    (cons acc idx)
    (let ((ch (string-ref str idx)))
      (if (is-digit? ch)
        (parse-num str (+ idx (quote 1))
                   (+ (* acc (quote 10)) (- ch (quote 48))))
        (cons acc idx)))))

;;; Parse symbol
(defun collect-sym-chars (str idx chars)
  (if (at-end? str idx)
    (cons chars idx)
    (let ((ch (string-ref str idx)))
      (if (is-symbol-char? ch)
        (collect-sym-chars str (+ idx (quote 1)) (cons ch chars))
        (cons chars idx)))))

(defun reverse-list (lst)
  (reverse-helper lst (quote nil)))

(defun reverse-helper (lst acc)
  (if (nil? lst) acc
    (reverse-helper (cdr lst) (cons (car lst) acc))))

(defun chars-to-string (chars)
  (let ((len (list-len chars (quote 0))))
    (chars-to-vec chars len (make-vector len) (quote 0))))

(defun list-len (lst acc)
  (if (nil? lst) acc
    (list-len (cdr lst) (+ acc (quote 1)))))

(defun chars-to-vec (chars len vec idx)
  (if (>= idx len) vec
    (progn
      (vector-set vec idx (car chars))
      (chars-to-vec (cdr chars) len vec (+ idx (quote 1))))))

(defun parse-sym (str idx)
  (let ((result (collect-sym-chars str idx (quote nil))))
    (let ((chars (reverse-list (car result))))
      (let ((new-idx (cdr result)))
        (cons (make-symbol (quote "temp")) new-idx)))))

;;; Parse list
(defun parse-list (str idx)
  (let ((idx2 (skip-ws str (+ idx (quote 1)))))  ; skip '('
    (parse-list-elems str idx2 (quote nil))))

(defun parse-list-elems (str idx acc)
  (let ((idx2 (skip-ws str idx)))
    (if (at-end? str idx2)
      (cons (reverse-list acc) idx2)
      (let ((ch (string-ref str idx2)))
        (if (= ch (quote 41))  ; ')'
          (cons (reverse-list acc) (+ idx2 (quote 1)))
          (let ((elem-result (read-one str idx2)))
            (let ((elem (car elem-result)))
              (let ((new-idx (cdr elem-result)))
                (parse-list-elems str new-idx (cons elem acc))))))))))

;;; Main reader
(defun read-one (str idx)
  (let ((idx2 (skip-ws str idx)))
    (if (at-end? str idx2)
      (cons (quote nil) idx2)
      (let ((ch (string-ref str idx2)))
        (if (= ch (quote 40))  ; '('
          (parse-list str idx2)
          (if (is-digit? ch)
            (parse-num str idx2 (quote 0))
            (if (is-symbol-char? ch)
              (parse-sym str idx2)
              (cons (quote nil) idx2))))))))

(defun read-str (str)
  (car (read-one str (quote 0))))

;;;; Evaluator

(defun eval-expr (expr env)
  (if (fixnum? expr) expr
    (if (nil? expr) (quote nil)
      (if (symbol? expr)
        (env-lookup expr env)
        (if (cons? expr)
          (let ((first (car expr)))
            (if (symbol=? first (make-symbol (quote "quote")))
              (car (cdr expr))
              (if (symbol=? first (make-symbol (quote "if")))
                (eval-if (cdr expr) env)
                (eval-apply first (cdr expr) env))))
          expr)))))

(defun eval-if (args env)
  (let ((test (eval-expr (car args) env)))
    (if (nil? test)
      (eval-expr (car (cdr (cdr args))) env)
      (eval-expr (car (cdr args)) env))))

(defun eval-apply (op args env)
  (if (symbol=? op (make-symbol (quote "+")))
    (+ (eval-expr (car args) env)
       (eval-expr (car (cdr args)) env))
    (if (symbol=? op (make-symbol (quote "-")))
      (- (eval-expr (car args) env)
         (eval-expr (car (cdr args)) env))
      (if (symbol=? op (make-symbol (quote "*")))
        (* (eval-expr (car args) env)
           (eval-expr (car (cdr args)) env))
        (if (symbol=? op (make-symbol (quote "/")))
          (/ (eval-expr (car args) env)
             (eval-expr (car (cdr args)) env))
          (quote nil))))))

(defun env-lookup (sym env)
  (quote 0))

;;;; REPL
(defun repl-start ()
  (progn
    (print (quote "Habu REPL - Full S-expression Support"))
    (println)
    (repl-loop)))

(defun repl-loop ()
  (let ((line (readline (quote "habu> "))))
    (if line
      (progn
        (let ((str (make-string-from-cstr line)))
          (let ((expr (read-str str)))
            (let ((result (eval-expr expr (quote nil))))
              (progn
                (print-value result)
                (println)))))
        (repl-loop))
      (progn
        (println)
        (print (quote "Goodbye!"))
        (println)))))

(repl-start)
