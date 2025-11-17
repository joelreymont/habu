;;;; Habu REPL - Read-Eval-Print Loop
;;;; Interactive environment for Habu Lisp

(load "compiler.lisp")
(load "readline.lisp")
(in-package :habu-compiler)

;;; REPL state
(defvar *repl-running* t)
(defvar *repl-history* nil)
(defvar *repl-env* nil)
(defvar *history-file* (merge-pathnames ".habu_history" (user-homedir-pathname)))
(defvar *history-max-size* 1000)

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

         ;; List operations (using CL cons cells for now)
         ((eq op 'cons)
          (cons (interpret-expr (first args) env)
                (interpret-expr (second args) env)))

         ((eq op 'car)
          (let ((val (interpret-expr (first args) env)))
            (if (consp val)
                (car val)
                (error "car: argument is not a cons cell"))))

         ((eq op 'cdr)
          (let ((val (interpret-expr (first args) env)))
            (if (consp val)
                (cdr val)
                (error "cdr: argument is not a cons cell"))))

         ((eq op 'list)
          (mapcar (lambda (arg) (interpret-expr arg env)) args))

         ((eq op 'consp)
          (if (consp (interpret-expr (first args) env)) 1 0))

         ((eq op 'atom)
          (if (atom (interpret-expr (first args) env)) 1 0))

         ((eq op 'null)
          (let ((val (interpret-expr (first args) env)))
            (if (or (null val) (zerop val)) 1 0)))

         ;; List helper functions
         ((eq op 'caar)
          (car (car (interpret-expr (first args) env))))

         ((eq op 'cadr)
          (car (cdr (interpret-expr (first args) env))))

         ((eq op 'cdar)
          (cdr (car (interpret-expr (first args) env))))

         ((eq op 'cddr)
          (cdr (cdr (interpret-expr (first args) env))))

         ((eq op 'caddr)
          (car (cdr (cdr (interpret-expr (first args) env)))))

         ((eq op 'cadddr)
          (car (cdr (cdr (cdr (interpret-expr (first args) env))))))

         ((eq op 'length)
          (cl:length (interpret-expr (first args) env)))

         ((eq op 'reverse)
          (cl:reverse (interpret-expr (first args) env)))

         ((eq op 'append)
          (let ((list1 (interpret-expr (first args) env))
                (list2 (interpret-expr (second args) env)))
            (cl:append list1 list2)))

         ((eq op 'nth)
          (cl:nth (interpret-expr (first args) env)
                  (interpret-expr (second args) env)))

         ;; Convenient aliases
         ((eq op 'first)
          (car (interpret-expr (first args) env)))

         ((eq op 'second)
          (car (cdr (interpret-expr (first args) env))))

         ((eq op 'third)
          (car (cdr (cdr (interpret-expr (first args) env)))))

         ((eq op 'fourth)
          (car (cdr (cdr (cdr (interpret-expr (first args) env))))))

         ((eq op 'rest)
          (cdr (interpret-expr (first args) env)))

         ;; Additional list utilities
         ((eq op 'last)
          (let ((lst (interpret-expr (first args) env)))
            (if (consp lst)
                (cl:last lst)
                (error "last: argument is not a list"))))

         ((eq op 'butlast)
          (cl:butlast (interpret-expr (first args) env)))

         ((eq op 'nthcdr)
          (cl:nthcdr (interpret-expr (first args) env)
                     (interpret-expr (second args) env)))

         ((eq op 'member)
          (let ((item (interpret-expr (first args) env))
                (lst (interpret-expr (second args) env)))
            (if (cl:member item lst :test #'equal)
                1  ; Return 1 for true (found)
                0)))  ; Return 0 for false (not found)

         ;; Type predicates
         ((eq op 'listp)
          (if (listp (interpret-expr (first args) env)) 1 0))

         ((eq op 'numberp)
          (if (numberp (interpret-expr (first args) env)) 1 0))

         ((eq op 'integerp)
          (if (integerp (interpret-expr (first args) env)) 1 0))

         ((eq op 'symbolp)
          (if (symbolp (interpret-expr (first args) env)) 1 0))

         ;; Mathematical functions
         ((eq op 'sqrt)
          (floor (sqrt (interpret-expr (first args) env))))

         ((eq op 'expt)
          (floor (expt (interpret-expr (first args) env)
                       (interpret-expr (second args) env))))

         ;; Association list operations
         ((eq op 'assoc)
          (let ((key (interpret-expr (first args) env))
                (alist (interpret-expr (second args) env)))
            (cl:assoc key alist :test #'equal)))

         ((eq op 'pairlis)
          (let ((keys (interpret-expr (first args) env))
                (values (interpret-expr (second args) env)))
            (cl:pairlis keys values)))

         ;; List utilities
         ((eq op 'make-list)
          (let ((n (interpret-expr (first args) env))
                (init (if (second args)
                          (interpret-expr (second args) env)
                          0)))
            (make-list n :initial-element init)))

         ((eq op 'remove)
          (let ((item (interpret-expr (first args) env))
                (lst (interpret-expr (second args) env)))
            (cl:remove item lst :test #'equal)))

         ((eq op 'find)
          (let ((item (interpret-expr (first args) env))
                (lst (interpret-expr (second args) env)))
            (if (cl:find item lst :test #'equal)
                1  ; Found
                0)))  ; Not found

         ((eq op 'position)
          (let ((item (interpret-expr (first args) env))
                (lst (interpret-expr (second args) env)))
            (or (cl:position item lst :test #'equal) -1)))

         ((eq op 'count)
          (let ((item (interpret-expr (first args) env))
                (lst (interpret-expr (second args) env)))
            (cl:count item lst :test #'equal)))

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

;;; History management
(defun load-history ()
  "Load command history from file"
  (when (probe-file *history-file*)
    (with-open-file (in *history-file* :direction :input :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil nil)
              while line
              collect line into lines
              finally (setf *repl-history* (nreverse lines)))))))

(defun save-history ()
  "Save command history to file"
  (with-open-file (out *history-file* :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    ;; Only save the most recent entries
    (let ((recent (if (> (length *repl-history*) *history-max-size*)
                      (subseq *repl-history* 0 *history-max-size*)
                      *repl-history*)))
      (dolist (entry (reverse recent))
        (when (stringp entry)
          (write-line entry out))))))

(defun add-to-history (expr)
  "Add expression to history"
  (let ((expr-str (prin1-to-string expr)))
    (push expr-str *repl-history*)
    ;; Trim history if too long
    (when (> (length *repl-history*) *history-max-size*)
      (setf *repl-history* (subseq *repl-history* 0 *history-max-size*)))))

;;; Tab completion support
(defun get-completion-candidates ()
  "Get list of symbols available for completion"
  (append
   ;; Operators
   '(+ - * / mod < > = <= >= /= equal
     logand logior logxor lognot ash
     and or not
     zerop plusp minusp evenp oddp null
     1+ 1- abs min max sqrt expt
     if cond case when unless progn begin let let*
     lambda defun defmacro setq incf decf
     quote car cdr cons list consp atom
     caar cadr cdar cddr caddr cadddr
     length reverse append nth
     first second third fourth rest
     last butlast nthcdr member
     listp numberp integerp symbolp
     make-list remove find position count
     assoc pairlis)
   ;; REPL commands
   '(:quit :q :help :h :clear :macros :functions :history)
   ;; User-defined functions
   (loop for name being the hash-keys of *function-table* collect name)
   ;; User-defined macros
   (loop for name being the hash-keys of *macro-table* collect name)))

(defun find-completions (prefix)
  "Find all symbols that start with PREFIX"
  (let ((prefix-str (string-upcase (if (symbolp prefix) (symbol-name prefix) prefix)))
        (candidates (get-completion-candidates)))
    (remove-if-not (lambda (sym)
                     (let ((sym-str (string-upcase (if (symbolp sym) (symbol-name sym) (string sym)))))
                       (and (>= (length sym-str) (length prefix-str))
                            (string= prefix-str sym-str :end2 (length prefix-str)))))
                   candidates)))

(defun show-completions (prefix)
  "Show all possible completions for PREFIX"
  (let ((completions (find-completions prefix)))
    (cond
      ((null completions)
       (format t "No completions found for '~A'~%" prefix))
      ((= (length completions) 1)
       (format t "~A~%" (first completions)))
      (t
       (format t "~%Possible completions:~%")
       (dolist (c completions)
         (format t "  ~A~%" c))))))

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
  (format t "  - Predicates: zerop, plusp, minusp, evenp, oddp, null, consp, atom~%")
  (format t "  - Type checks: listp, numberp, integerp, symbolp~%")
  (format t "  - Numeric: 1+, 1-, abs, min, max, sqrt, expt~%")
  (format t "  - Lists: cons, car, cdr, list, caar, cadr, cdar, cddr, caddr, cadddr~%")
  (format t "  - List ops: length, reverse, append, nth, first, second, third, fourth~%")
  (format t "  - List utils: rest, last, butlast, nthcdr, member~%")
  (format t "  - List search: find, position, count, remove~%")
  (format t "  - List create: make-list~%")
  (format t "  - Assoc lists: assoc, pairlis~%")
  (format t "  - Control: if, let, progn~%")
  (format t "  - Macros: defmacro~%")
  (format t "~%")
  (format t "Commands:~%")
  (format t "  :quit or :q     - Exit REPL~%")
  (format t "  :help or :h     - Show this help~%")
  (format t "  :clear          - Clear function and macro tables~%")
  (format t "  :macros         - List defined macros~%")
  (format t "  :functions      - List defined functions~%")
  (format t "  :history        - Show command history~%")
  (format t "  :complete <sym> - Show completions for symbol~%")
  (format t "~%")
  (format t "Tips:~%")
  (format t "  - Arrow keys for line editing and history navigation~%")
  (format t "  - Tab for completion~%")
  (format t "  - Ctrl-A/E for beginning/end of line~%")
  (format t "  - History saved to: ~A~%" *history-file*)
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
    (if (integerp result)
        (format t "=> ~D~%" result)
        (format t "=> ~S~%" result))))

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

    ((string= command ":functions")
     (if (zerop (hash-table-count *function-table*))
         (format t "No functions defined.~%")
         (progn
           (format t "Defined functions:~%")
           (maphash (lambda (name def)
                      (format t "  ~A ~A~%" name (car def)))
                    *function-table*))))

    ((string= command ":history")
     (if (null *repl-history*)
         (format t "No history.~%")
         (progn
           (format t "Command history (most recent first):~%")
           (loop for i from 0
                 for entry in *repl-history*
                 when (< i 20)  ; Show last 20 entries
                 do (format t "  ~3D: ~A~%" (1+ i) entry)))))

    ;; Handle commands with arguments like ":complete foo"
    ((and (> (length command) 9)
          (string= ":complete" command :end2 9))
     (let ((prefix (string-trim " " (subseq command 9))))
       (if (zerop (length prefix))
           (format t "Usage: :complete <symbol>~%")
           (show-completions prefix))))

    (t
     (format t "Unknown command: ~A~%" command)
     (format t "Type :help for available commands.~%"))))

(defun complete-symbol (prefix)
  "Completion function for readline"
  (find-completions prefix))

(defvar *use-readline* (interactive-stream-p *standard-input*))

(defun repl-read-command ()
  "Read a command or expression from user"
  (handler-case
      (let ((input (if *use-readline*
                       (read-line-with-editing "~%habu> "
                                              :history *repl-history*
                                              :completion-fn #'complete-symbol)
                       (read-line))))
        (unless input
          (return-from repl-read-command (values :eof nil)))
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
  ;; Load history from file
  (load-history)

  ;; Set raw mode if using readline
  (when *use-readline*
    (set-raw-mode))

  (unwind-protect
      (progn
        (repl-print-banner)

        (loop while *repl-running* do
          ;; Only show prompt if not using readline (readline handles it)
          (unless *use-readline*
            (repl-prompt))

          (multiple-value-bind (type value) (repl-read-command)
            (case type
              (:eof
               (format t "~%")
               (setf *repl-running* nil))

              (:command
               (repl-handle-command value))

              (:expression
               (when value
                 (add-to-history value)
                 (let ((result (repl-eval value)))
                   (repl-print result))))

              (:error
               ;; Error already printed, continue
               nil)))))

    ;; Cleanup: restore terminal and save history
    (when *use-readline*
      (restore-cooked-mode))
    (save-history)))

;;; Start REPL
(format t "~%Starting Habu REPL...~%")
(repl)
