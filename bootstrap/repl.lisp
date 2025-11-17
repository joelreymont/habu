;;;; Habu REPL - Read-Eval-Print Loop
;;;; Interactive environment for Habu Lisp

(load "compiler.lisp")
(in-package :habu-compiler)

;;; REPL state
(defvar *repl-running* t)
(defvar *repl-history* nil)
(defvar *repl-env* nil)

;;; Simple interpreter for fixnum expressions
(defun interpret-expr (expr env)
  "Interpret an expression in the given environment"
  (ecase (expr-type expr)
    (fixnum
     (expr-value expr))

    (variable
     (let ((binding (assoc (expr-value expr) env)))
       (if binding
           (cdr binding)
           (error "Unbound variable: ~S" (expr-value expr)))))

    (let
     (let* ((bindings (expr-value expr))
            (body (first (expr-args expr)))
            (new-env env))
       ;; Bindings are stored as raw s-expressions, need to parse them first
       (loop for (var val-form) in bindings
             do (let ((val (interpret-expr (parse val-form) env)))
                  (push (cons var val) new-env)))
       (interpret-expr body new-env)))

    (if
     (let* ((args (expr-args expr))
            (condition (first args))
            (then-expr (second args))
            (else-expr (third args))
            (cond-val (interpret-expr condition env)))
       (if (zerop cond-val)
           (interpret-expr else-expr env)
           (interpret-expr then-expr env))))

    (progn
     (let ((args (expr-args expr)))
       (loop for arg in args
             for result = (interpret-expr arg env)
             finally (return result))))

    (lambda
     ;; Return a closure representation
     (list 'closure (expr-value expr) (first (expr-args expr)) env))

    (call
     (let ((op (expr-value expr))
           (args (expr-args expr)))
       (cond
         ;; Arithmetic
         ((eq op '+)
          (+ (interpret-expr (first args) env)
             (interpret-expr (second args) env)))

         ((eq op '-)
          (- (interpret-expr (first args) env)
             (interpret-expr (second args) env)))

         ((eq op '*)
          (* (interpret-expr (first args) env)
             (interpret-expr (second args) env)))

         ((eq op '/)
          (floor (interpret-expr (first args) env)
                 (interpret-expr (second args) env)))

         ((eq op 'mod)
          (mod (interpret-expr (first args) env)
               (interpret-expr (second args) env)))

         ;; Comparison
         ((eq op '<)
          (if (< (interpret-expr (first args) env)
                 (interpret-expr (second args) env))
              1 0))

         ((eq op '>)
          (if (> (interpret-expr (first args) env)
                 (interpret-expr (second args) env))
              1 0))

         ((eq op '=)
          (if (= (interpret-expr (first args) env)
                 (interpret-expr (second args) env))
              1 0))

         ((eq op '<=)
          (if (<= (interpret-expr (first args) env)
                  (interpret-expr (second args) env))
              1 0))

         ((eq op '>=)
          (if (>= (interpret-expr (first args) env)
                  (interpret-expr (second args) env))
              1 0))

         ;; Bitwise
         ((eq op 'logand)
          (logand (interpret-expr (first args) env)
                  (interpret-expr (second args) env)))

         ((eq op 'logior)
          (logior (interpret-expr (first args) env)
                  (interpret-expr (second args) env)))

         ((eq op 'logxor)
          (logxor (interpret-expr (first args) env)
                  (interpret-expr (second args) env)))

         ((eq op 'lognot)
          (lognot (interpret-expr (first args) env)))

         ;; Predicates
         ((eq op 'zerop)
          (if (zerop (interpret-expr (first args) env)) 1 0))

         ((eq op 'plusp)
          (if (plusp (interpret-expr (first args) env)) 1 0))

         ((eq op 'minusp)
          (if (minusp (interpret-expr (first args) env)) 1 0))

         ((eq op 'evenp)
          (if (evenp (interpret-expr (first args) env)) 1 0))

         ((eq op 'oddp)
          (if (oddp (interpret-expr (first args) env)) 1 0))

         ;; Numeric
         ((eq op '1+)
          (1+ (interpret-expr (first args) env)))

         ((eq op '1-)
          (1- (interpret-expr (first args) env)))

         ((eq op 'abs)
          (abs (interpret-expr (first args) env)))

         ((eq op 'min)
          (min (interpret-expr (first args) env)
               (interpret-expr (second args) env)))

         ((eq op 'max)
          (max (interpret-expr (first args) env)
               (interpret-expr (second args) env)))

         (t
          (error "Unknown operator: ~S" op)))))

    (not
     (let ((arg (interpret-expr (first (expr-args expr)) env)))
       (if (zerop arg) 1 0)))

    (and
     (let ((args (expr-args expr)))
       (if (null args)
           1
           (loop for arg in args
                 for val = (interpret-expr arg env)
                 when (zerop val)
                   return 0
                 finally (return val)))))

    (or
     (let ((args (expr-args expr)))
       (if (null args)
           0
           (loop for arg in args
                 for val = (interpret-expr arg env)
                 unless (zerop val)
                   return val
                 finally (return 0)))))))

;;; REPL utilities
(defun repl-prompt ()
  "Display REPL prompt"
  (format t "~%habu> ")
  (force-output))

(defun repl-print-banner ()
  "Print welcome banner"
  (format t "~%")
  (format t "========================================~%")
  (format t "  Habu Lisp REPL v0.1~%")
  (format t "========================================~%")
  (format t "~%")
  (format t "Welcome to Habu Lisp!~%")
  (format t "~%")
  (format t "Features:~%")
  (format t "  - Arithmetic: +, -, *, /, mod~%")
  (format t "  - Comparison: <, >, =, <=, >=~%")
  (format t "  - Logic: and, or, not~%")
  (format t "  - Bitwise: logand, logior, logxor, lognot~%")
  (format t "  - Predicates: zerop, plusp, minusp, evenp, oddp~%")
  (format t "  - Numeric: 1+, 1-, abs, min, max~%")
  (format t "  - Control: if, let, progn~%")
  (format t "  - Macros: defmacro~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "  :quit or :q     - Exit REPL~%")
  (format t "  :help or :h     - Show this help~%")
  (format t "  :clear          - Clear function and macro tables~%")
  (format t "  :macros         - List defined macros~%")
  (format t "~%")
  (format t "Note: This is an interpreter-based REPL (fixnums only).~%")
  (format t "~%"))

(defun repl-eval (form)
  "Evaluate a form by parsing and interpreting it"
  (handler-case
      (let* ((parsed (parse form)))
        (interpret-expr parsed *repl-env*))
    (error (e)
      (format t "Error: ~A~%" e)
      nil)))

(defun repl-print (result)
  "Print evaluation result"
  (when result
    (format t "=> ~D~%" result)))

(defun repl-handle-command (command)
  "Handle REPL commands"
  (cond
    ((or (string= command ":quit") (string= command ":q"))
     (format t "Goodbye!~%")
     (setf *repl-running* nil))

    ((or (string= command ":help") (string= command ":h"))
     (repl-print-banner))

    ((string= command ":clear")
     (clrhash *function-table*)
     (clrhash *macro-table*)
     (setf *repl-env* nil)
     (format t "Cleared function and macro tables.~%"))

    ((string= command ":macros")
     (if (zerop (hash-table-count *macro-table*))
         (format t "No macros defined.~%")
         (progn
           (format t "Defined macros:~%")
           (maphash (lambda (name def)
                      (format t "  ~A ~A~%" name (car def)))
                    *macro-table*))))

    (t
     (format t "Unknown command: ~A~%" command)
     (format t "Type :help for available commands.~%"))))

(defun repl-read-command ()
  "Read a command or expression from user"
  (handler-case
      (let ((input (read-line)))
        (if (and (> (length input) 0)
                 (char= (char input 0) #\:))
            (values :command input)
            (values :expression (read-from-string input))))
    (end-of-file ()
      (values :eof nil))
    (error (e)
      (format t "Read error: ~A~%" e)
      (values :error nil))))

(defun repl ()
  "Main REPL loop"
  (repl-print-banner)

  (loop while *repl-running* do
    (repl-prompt)

    (multiple-value-bind (type value) (repl-read-command)
      (case type
        (:eof
         (format t "~%")
         (setf *repl-running* nil))

        (:command
         (repl-handle-command value))

        (:expression
         (when value
           (push value *repl-history*)
           (let ((result (repl-eval value)))
             (repl-print result))))

        (:error
         ;; Error already printed, continue
         nil)))))

;;; Start REPL
(format t "~%Starting Habu REPL...~%")
(repl)
