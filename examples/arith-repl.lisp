;;; Arithmetic Evaluator REPL
;;; Parses and evaluates simple arithmetic expressions

;;; ============================================================
;;; Reader (from common/reader.lisp, simplified)
;;; ============================================================

(defun whitespace? (ch)
  (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))

(defun digit? (ch)
  (and (>= ch #x30) (<= ch #x39)))

(defun alpha? (ch)
  (or (and (>= ch #x41) (<= ch #x5A))
      (and (>= ch #x61) (<= ch #x7A))))

(defun symbol-start? (ch)
  (or (alpha? ch)
      (= ch #x2B)   ;; +
      (= ch #x2D)   ;; -
      (= ch #x2A)   ;; *
      (= ch #x2F)   ;; /
      (= ch #x3D)   ;; =
      (= ch #x3C)   ;; <
      (= ch #x3E))) ;; >

(defun symbol-char? (ch)
  (or (alpha? ch) (digit? ch)
      (= ch #x2D) (= ch #x5F) (= ch #x2B) (= ch #x2A)
      (= ch #x2F) (= ch #x3D) (= ch #x3C) (= ch #x3E)))

(defun char-at (source pos)
  (if (< pos (string-length source))
      (string-ref source pos)
      #x0))

(defun digit-val (ch) (- ch #x30))

(defun skip-ws (source pos)
  (let ((ch (char-at source pos)))
    (if (whitespace? ch)
        (skip-ws source (+ pos 1))
        pos)))

;;; Read a number
(defun read-number (source pos)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (read-number-acc source (+ pos 1) (digit-val ch))
        (cons 0 pos))))

(defun read-number-acc (source pos acc)
  (let ((ch (char-at source pos)))
    (if (digit? ch)
        (read-number-acc source (+ pos 1) (+ (* acc 10) (digit-val ch)))
        (cons acc pos))))

;;; Read a symbol (returns integer ID for now: + = 1, - = 2, * = 3, / = 4)
(defun read-symbol (source pos)
  (let ((ch (char-at source pos)))
    (cond
      ((= ch #x2B) (cons 'ADD (+ pos 1)))      ;; +
      ((= ch #x2D) (cons 'SUB (+ pos 1)))      ;; -
      ((= ch #x2A) (cons 'MUL (+ pos 1)))      ;; *
      ((= ch #x2F) (cons 'DIV (+ pos 1)))      ;; /
      (t (cons 'UNKNOWN (+ pos 1))))))

;;; Read one expression
(defun habu-read (source pos)
  (let ((pos2 (skip-ws source pos)))
    (let ((ch (char-at source pos2)))
      (cond
        ;; End of input
        ((= ch 0) (cons nil pos2))
        ;; Number
        ((digit? ch) (read-number source pos2))
        ;; Open paren - list
        ((= ch #x28) (read-list source (+ pos2 1)))
        ;; Symbol
        ((symbol-start? ch) (read-symbol source pos2))
        ;; Unknown
        (t (cons nil (+ pos2 1)))))))

;;; Read a list
(defun read-list (source pos)
  (let ((pos2 (skip-ws source pos)))
    (let ((ch (char-at source pos2)))
      (if (= ch #x29)  ;; )
          (cons nil (+ pos2 1))
          (let ((first (habu-read source pos2)))
            (let ((rest (read-list-tail source (cdr first))))
              (cons (cons (car first) (car rest)) (cdr rest))))))))

(defun read-list-tail (source pos)
  (let ((pos2 (skip-ws source pos)))
    (let ((ch (char-at source pos2)))
      (if (= ch #x29)  ;; )
          (cons nil (+ pos2 1))
          (let ((elem (habu-read source pos2)))
            (let ((rest (read-list-tail source (cdr elem))))
              (cons (cons (car elem) (car rest)) (cdr rest))))))))

(defun read-from-string (s)
  (car (habu-read s 0)))

;;; ============================================================
;;; Evaluator
;;; ============================================================

(defun eval-expr (expr)
  (cond
    ;; Number - return as is
    ((numberp expr) expr)
    ;; List - function application
    ((consp expr)
     (let ((op (car expr))
           (args (cdr expr)))
       (cond
         ((eq op 'ADD) (+ (eval-expr (car args)) (eval-expr (cadr args))))
         ((eq op 'SUB) (- (eval-expr (car args)) (eval-expr (cadr args))))
         ((eq op 'MUL) (* (eval-expr (car args)) (eval-expr (cadr args))))
         ((eq op 'DIV) (/ (eval-expr (car args)) (eval-expr (cadr args))))
         (t 0))))
    ;; Unknown
    (t 0)))

;;; ============================================================
;;; I/O
;;; ============================================================

(defvar *char-newline* "
")

(defun print-string (s)
  (sys-write 1 s (string-length s)))

(defun print-newline ()
  (print-string *char-newline*))

(defun print-fixnum (n)
  (if (< n 0)
      (progn
        (print-string "-")
        (print-fixnum-positive (- 0 n)))
      (if (= n 0)
          (print-string "0")
          (print-fixnum-positive n))))

(defun print-fixnum-positive (n)
  (if (= n 0)
      nil
      (progn
        (print-fixnum-positive (/ n 10))
        (let ((digit (mod n 10)))
          (let ((s (make-string 1)))
            (string-set! s 0 (code-char (+ 48 digit)))
            (print-string s))))))

;;; Input buffer
(defvar *stdin-buffer* nil)
(defconstant +stdin-buffer-size+ 1024)

(defun read-line-stdin ()
  (if (null *stdin-buffer*)
      (setq *stdin-buffer* (make-vector +stdin-buffer-size+)))
  (read-line-loop 0))

(defun read-line-loop (pos)
  (if (>= pos (- +stdin-buffer-size+ 1))
      (buffer-to-string *stdin-buffer* pos)
      (let ((n (sys-read-byte 0)))
        (if (< n 0)
            (if (= pos 0)
                nil
                (buffer-to-string *stdin-buffer* pos))
            (if (= n 10)
                (buffer-to-string *stdin-buffer* pos)
                (progn
                  (buffer-byte-set *stdin-buffer* pos n)
                  (read-line-loop (+ pos 1))))))))

;;; ============================================================
;;; REPL
;;; ============================================================

(defun eval-repl ()
  (print-string "Arithmetic REPL")
  (print-newline)
  (print-string "Operators: + - * /")
  (print-newline)
  (print-string "Example: (+ 1 2) or (* 3 (+ 1 2))")
  (print-newline)
  (repl-loop))

(defun repl-loop ()
  (print-string "> ")
  (let ((input (read-line-stdin)))
    (if (null input)
        (progn
          (print-newline)
          (print-string "Goodbye.")
          (print-newline))
        (if (= (string-length input) 0)
            (progn
              (print-string "Goodbye.")
              (print-newline))
            (progn
              (let ((expr (read-from-string input)))
                (let ((result (eval-expr expr)))
                  (print-fixnum result)
                  (print-newline)))
              (repl-loop))))))

(eval-repl)
