;;; Type Predicates using get-tag primitive

(defun fixnum? (x)
  (= (get-tag x) (quote 0)))

(defun cons? (x)
  (= (get-tag x) (quote 1)))

(defun symbol? (x)
  (= (get-tag x) (quote 2)))

(defun vector? (x)
  (= (get-tag x) (quote 3)))

(defun string? (x)
  (= (get-tag x) (quote 4)))

(defun nil? (x)
  (= x (quote 0)))

;;; String comparison using string-ref

(defun string-compare-loop (s1 s2 idx len)
  (if (>= idx len)
      (quote 1)
      (if (= (string-ref s1 idx) (string-ref s2 idx))
          (string-compare-loop s1 s2 (+ idx (quote 1)) len)
          (quote nil))))

(defun string=? (s1 s2)
  (if (string? s1)
      (if (string? s2)
          (let ((len1 (string-length-raw s1)))
            (let ((len2 (string-length-raw s2)))
              (if (= len1 len2)
                  (string-compare-loop s1 s2 (quote 0) len1)
                  (quote nil))))
          (quote nil))
      (quote nil)))

(defun symbol=? (sym1 sym2)
  (if (symbol? sym1)
      (if (symbol? sym2)
          (string=? (symbol-name sym1) (symbol-name sym2))
          (quote nil))
      (quote nil)))

;;;; Habu REPL - Complete implementation in Lisp
;;;; Includes reader, evaluator, and REPL loop

;;;; ============================================================================
;;;; READER - Parse S-expressions from strings
;;;; ============================================================================

;;; Character utilities

(defun is-whitespace? (ch)
  (if (= ch (quote 32))  ; space
      (quote 1)
      (if (= ch (quote 10))  ; newline
          (quote 1)
          (if (= ch (quote 9))  ; tab
              (quote 1)
              (if (= ch (quote 13))  ; CR
                  (quote 1)
                  (quote nil))))))

(defun is-digit? (ch)
  (if (>= ch (quote 48))  ; '0'
      (<= ch (quote 57))  ; '9'
      (quote nil)))

(defun is-alpha? (ch)
  (if (>= ch (quote 65))  ; 'A'
      (if (<= ch (quote 90))  ; 'Z'
          (quote 1)
          (if (>= ch (quote 97))  ; 'a'
              (<= ch (quote 122))  ; 'z'
              (quote nil)))
      (quote nil)))

(defun is-special-char? (ch)
  (if (= ch (quote 43)) (quote 1)  ; +
  (if (= ch (quote 45)) (quote 1)  ; -
  (if (= ch (quote 42)) (quote 1)  ; *
  (if (= ch (quote 47)) (quote 1)  ; /
  (if (= ch (quote 61)) (quote 1)  ; =
  (if (= ch (quote 60)) (quote 1)  ; <
  (if (= ch (quote 62)) (quote 1)  ; >
  (if (= ch (quote 63)) (quote 1)  ; ?
      (quote nil))))))))))

(defun is-symbol-char? (ch)
  (if (is-alpha? ch)
      (quote 1)
      (if (is-digit? ch)
          (quote 1)
          (is-special-char? ch))))

(defun digit-to-int (ch)
  (- ch (quote 48)))

;;; Reader state: (string . index)

(defun reader-at-end? (state)
  (>= (cdr state) (string-length (car state))))

(defun reader-peek (state)
  (if (reader-at-end? state)
      (quote nil)
      (string-ref (car state) (cdr state))))

(defun reader-advance (state)
  (cons (car state) (+ (cdr state) (quote 1))))

(defun skip-ws (state)
  (if (reader-at-end? state)
      state
      (let ((ch (reader-peek state)))
        (if (is-whitespace? ch)
            (skip-ws (reader-advance state))
            state))))

;;; Parse number

(defun parse-num (state acc)
  (if (reader-at-end? state)
      (cons acc state)
      (let ((ch (reader-peek state)))
        (if (is-digit? ch)
            (parse-num (reader-advance state)
                      (+ (* acc (quote 10)) (digit-to-int ch)))
            (cons acc state)))))

;;; Parse list

(defun parse-list-elems (state)
  (let ((s2 (skip-ws state)))
    (if (reader-at-end? s2)
        (cons (quote nil) s2)
        (let ((ch (reader-peek s2)))
          (if (= ch (quote 41))  ; ')'
              (cons (quote nil) (reader-advance s2))
              (let ((elem-result (read-one s2)))
                (let ((rest-result (parse-list-elems (cdr elem-result))))
                  (cons (cons (car elem-result) (car rest-result))
                        (cdr rest-result)))))))))

(defun parse-list (state)
  (parse-list-elems (reader-advance state)))

;;; Main reader

(defun read-one (state)
  (let ((s2 (skip-ws state)))
    (if (reader-at-end? s2)
        (cons (quote nil) s2)
        (let ((ch (reader-peek s2)))
          (if (= ch (quote 40))  ; '('
              (parse-list s2)
              (if (is-digit? ch)
                  (parse-num s2 (quote 0))
                  (cons (quote nil) s2)))))))

(defun read-from-string (str)
  (car (read-one (cons str (quote 0)))))

;;;; ============================================================================
;;;; EVALUATOR - Evaluate S-expressions
;;;; ============================================================================

;;; Environment lookup

(defun env-lookup (sym env)
  (if (nil? env)
      (quote nil)
      (let ((binding (car env)))
        (if (symbol=? (car binding) sym)
            (cdr binding)
            (env-lookup sym (cdr env))))))

;;; Symbol matching

(defun sym-eq? (sym name)
  (if (symbol? sym)
      (string=? (symbol-name sym) name)
      (quote nil)))

;;; Eval list

(defun eval-list (exprs env)
  (if (nil? exprs)
      (quote nil)
      (cons (eval (car exprs) env)
            (eval-list (cdr exprs) env))))

;;; Apply built-ins

(defun apply-plus (args)
  (if (nil? args)
      (quote 0)
      (+ (car args) (apply-plus (cdr args)))))

(defun apply-mult (args)
  (if (nil? args)
      (quote 1)
      (* (car args) (apply-mult (cdr args)))))

(defun apply-builtin (fn args)
  (if (sym-eq? fn (quote "+"))
      (apply-plus args)
  (if (sym-eq? fn (quote "*"))
      (apply-mult args)
  (if (sym-eq? fn (quote "-"))
      (if (nil? (cdr args))
          (- (quote 0) (car args))
          (- (car args) (apply-plus (cdr args))))
  (if (sym-eq? fn (quote "cons"))
      (cons (car args) (car (cdr args)))
  (if (sym-eq? fn (quote "car"))
      (car (car args))
  (if (sym-eq? fn (quote "cdr"))
      (cdr (car args))
  (if (sym-eq? fn (quote "list"))
      args
      (quote nil)))))))))

;;; Main evaluator

(defun eval (expr env)
  (if (fixnum? expr)
      expr
  (if (nil? expr)
      (quote nil)
  (if (cons? expr)
      (let ((first (car expr)))
        (let ((args (cdr expr)))
          (if (sym-eq? first (quote "quote"))
              (car args)
          (if (sym-eq? first (quote "if"))
              (if (nil? (eval (car args) env))
                  (eval (car (cdr (cdr args))) env)
                  (eval (car (cdr args)) env))
          (if (sym-eq? first (quote "progn"))
              (eval-progn args env)
              (apply-builtin first (eval-list args env)))))))
      expr))))

(defun eval-progn (exprs env)
  (if (nil? (cdr exprs))
      (eval (car exprs) env)
      (progn
        (eval (car exprs) env)
        (eval-progn (cdr exprs) env))))

;;;; ============================================================================
;;;; REPL - Read-Eval-Print Loop
;;;; ============================================================================

(defun repl-loop ()
  (progn
    (print (quote "Habu REPL - Written in Lisp!"))
    (println)
    (println)
    (repl-body (quote nil))))

(defun repl-body (env)
  (progn
    (print (quote "habu> "))
    (let ((line (fgets-line)))
      (if line
          (progn
            (if (> (string-length line) (quote 0))
                (let ((str (make-string-from-cstr line)))
                  (let ((expr (read-from-string str)))
                    (let ((result (eval expr env)))
                      (progn
                        (print-value result)
                        (println)))))
                (quote nil))
            (repl-body env))
          (progn
            (println)
            (print (quote "Bye!"))
            (println))))))

;; Start REPL
(repl-loop)
