;;; habu-repl.lisp - Habu Lisp REPL with compiler capabilities
;;; Provides: eval, compile-file, deliver, profile, trace
;;; Uses cl-readline for line editing and history

(defpackage :habu-repl
  (:use :cl)
  (:export #:main #:repl))

(in-package :habu-repl)

;;; Load readline
(ql:quickload :cl-readline :silent t)

;;; State
(defvar *profile-enabled* (make-hash-table :test 'equal))
(defvar *trace-enabled* (make-hash-table :test 'equal))
(defvar *repl-running* t)
(defvar *history-file* (merge-pathnames ".habu_history" (user-homedir-pathname)))

;;; Prompt
(defparameter *prompt* (format nil "~C> " #\U+1F40D))  ; Snake emoji
(defparameter *continuation-prompt* ".. ")

;;; Utility functions
(defun print-banner ()
  (format t "~%Habu Lisp~%")
  (format t "Type :help for commands, :quit to exit~%~%"))

(defun print-help ()
  (format t "~%Habu REPL Commands:~%")
  (format t "  :help              Show this help~%")
  (format t "  :quit              Exit the REPL~%")
  (format t "  :load <file>       Load and evaluate a Lisp file~%")
  (format t "  :compile <file>    Compile file to native executable~%")
  (format t "  :deliver <src> <out>  Compile source to executable~%")
  (format t "  :profile <fn>      Enable profiling for function~%")
  (format t "  :unprofile <fn>    Disable profiling for function~%")
  (format t "  :trace <fn>        Enable tracing for function~%")
  (format t "  :untrace <fn>      Disable tracing for function~%")
  (format t "  :time <expr>       Time expression evaluation~%")
  (format t "  :disasm <expr>     Show IR for expression~%")
  (format t "~%Keyboard shortcuts (readline):~%")
  (format t "  Up/Down            Navigate command history~%")
  (format t "  Ctrl-A / Ctrl-E    Move to start/end of line~%")
  (format t "  Ctrl-K             Kill to end of line~%")
  (format t "  Ctrl-R             Reverse search history~%")
  (format t "  Tab                Filename completion~%")
  (format t "~%Any other input is evaluated as Lisp code.~%~%"))

;;; Command handlers
(defun handle-load (args)
  (if (null args)
      (format t "Usage: :load <filename>~%")
      (let ((file (first args)))
        (if (probe-file file)
            (progn
              (format t "Loading ~A...~%" file)
              (load file)
              (format t "Loaded.~%"))
            (format t "File not found: ~A~%" file)))))

(defun handle-compile (args)
  (if (null args)
      (format t "Usage: :compile <source.lisp>~%")
      (let* ((src (first args))
             (out (if (second args)
                      (second args)
                      (concatenate 'string
                                   (subseq src 0 (or (position #\. src :from-end t)
                                                    (length src)))))))
        (if (probe-file src)
            (progn
              (format t "Compiling ~A to ~A...~%" src out)
              (handler-case
                  (progn
                    (habu:deliver-file-with-libsystem src out)
                    ;; Sign the executable for macOS
                    (sb-ext:run-program "/usr/bin/codesign"
                                        (list "-s" "-" out)
                                        :output nil :error nil :wait t)
                    (format t "Compiled successfully: ~A~%" out)
                    (format t "Run with: ~A~%" out))
                (error (e)
                  (format t "Compilation error: ~A~%" e))))
            (format t "Source file not found: ~A~%" src)))))

(defun handle-deliver (args)
  (if (< (length args) 2)
      (format t "Usage: :deliver <source.lisp> <output>~%")
      (handle-compile args)))

(defun handle-profile (args)
  (if (null args)
      (progn
        (format t "Profiled functions:~%")
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (format t "  ~A~%" k))
                 *profile-enabled*)
        (when (zerop (hash-table-count *profile-enabled*))
          (format t "  (none)~%")))
      (let ((fn-name (first args)))
        (setf (gethash fn-name *profile-enabled*) t)
        (format t "Profiling enabled for: ~A~%" fn-name))))

(defun handle-unprofile (args)
  (if (null args)
      (progn
        (clrhash *profile-enabled*)
        (format t "All profiling disabled.~%"))
      (let ((fn-name (first args)))
        (remhash fn-name *profile-enabled*)
        (format t "Profiling disabled for: ~A~%" fn-name))))

(defun handle-trace (args)
  (if (null args)
      (progn
        (format t "Traced functions:~%")
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (format t "  ~A~%" k))
                 *trace-enabled*)
        (when (zerop (hash-table-count *trace-enabled*))
          (format t "  (none)~%")))
      (let ((fn-name (first args)))
        (setf (gethash fn-name *trace-enabled*) t)
        ;; Use CL trace
        (eval `(trace ,(intern (string-upcase fn-name))))
        (format t "Tracing enabled for: ~A~%" fn-name))))

(defun handle-untrace (args)
  (if (null args)
      (progn
        (maphash (lambda (k v)
                   (declare (ignore v))
                   (eval `(untrace ,(intern (string-upcase k)))))
                 *trace-enabled*)
        (clrhash *trace-enabled*)
        (format t "All tracing disabled.~%"))
      (let ((fn-name (first args)))
        (eval `(untrace ,(intern (string-upcase fn-name))))
        (remhash fn-name *trace-enabled*)
        (format t "Tracing disabled for: ~A~%" fn-name))))

(defun handle-time (expr-string)
  (if (null expr-string)
      (format t "Usage: :time <expression>~%")
      (handler-case
          (let* ((expr (read-from-string expr-string))
                 (start (get-internal-real-time))
                 (result (eval expr))
                 (end (get-internal-real-time))
                 (elapsed-ms (/ (* 1000.0 (- end start))
                                internal-time-units-per-second)))
            (format t "~S~%" result)
            (format t "Time: ~,3F ms~%" elapsed-ms))
        (error (e)
          (format t "Error: ~A~%" e)))))

(defun handle-disasm (expr-string)
  (if (null expr-string)
      (format t "Usage: :disasm <expression>~%")
      (handler-case
          (let* ((expr (read-from-string expr-string))
                 (ir (habu::nc-compile expr nil nil)))
            (format t "IR: ~S~%" ir))
        (error (e)
          (format t "Error: ~A~%" e)))))

;;; Command parsing
(defun parse-command (line)
  "Parse a REPL command line. Returns (command . args) or NIL for regular expr."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (when (and (> (length trimmed) 0)
               (char= (char trimmed 0) #\:))
      (let* ((space-pos (position #\Space trimmed))
             (cmd (if space-pos
                      (subseq trimmed 1 space-pos)
                      (subseq trimmed 1)))
             (rest (if space-pos
                       (string-trim '(#\Space #\Tab)
                                    (subseq trimmed (1+ space-pos)))
                       "")))
        (cons (string-downcase cmd)
              (if (string= rest "")
                  nil
                  ;; Parse args - handle quoted strings
                  (let ((args nil)
                        (current "")
                        (in-quote nil))
                    (loop for c across rest do
                      (cond
                        ((and (char= c #\") (not in-quote))
                         (setf in-quote t))
                        ((and (char= c #\") in-quote)
                         (setf in-quote nil))
                        ((and (char= c #\Space) (not in-quote))
                         (when (> (length current) 0)
                           (push current args)
                           (setf current "")))
                        (t (setf current (concatenate 'string current (string c))))))
                    (when (> (length current) 0)
                      (push current args))
                    (nreverse args))))))))

(defun process-command (cmd args line)
  "Process a REPL command."
  (cond
    ((string= cmd "help") (print-help))
    ((string= cmd "quit") (setf *repl-running* nil))
    ((string= cmd "exit") (setf *repl-running* nil))
    ((string= cmd "load") (handle-load args))
    ((string= cmd "compile") (handle-compile args))
    ((string= cmd "deliver") (handle-deliver args))
    ((string= cmd "profile") (handle-profile args))
    ((string= cmd "unprofile") (handle-unprofile args))
    ((string= cmd "trace") (handle-trace args))
    ((string= cmd "untrace") (handle-untrace args))
    ((string= cmd "time")
     (handle-time (when args
                    (subseq line (+ 6 (search "time" line :test #'char-equal))))))
    ((string= cmd "disasm")
     (handle-disasm (when args
                      (subseq line (+ 7 (search "disasm" line :test #'char-equal))))))
    (t (format t "Unknown command: :~A~%Type :help for available commands.~%" cmd))))

;;; Paren counting for multi-line
(defun count-parens (str)
  "Count unbalanced parens. Positive = more opens, negative = more closes."
  (let ((count 0)
        (in-string nil)
        (escape nil))
    (loop for c across str do
      (cond
        (escape (setf escape nil))
        ((char= c #\\) (setf escape t))
        ((char= c #\") (setf in-string (not in-string)))
        ((not in-string)
         (case c
           (#\( (incf count))
           (#\) (decf count))))))
    count))

;;; History management
(defun load-history ()
  "Load command history from file."
  (when (probe-file *history-file*)
    (handler-case
        (cl-readline:read-history (namestring *history-file*))
      (error () nil))))

(defun save-history ()
  "Save command history to file."
  (handler-case
      (cl-readline:write-history (namestring *history-file*))
    (error () nil)))

;;; Readline-based input
(defun read-with-readline (prompt)
  "Read a line using readline with editing and history."
  (let ((line (cl-readline:readline :prompt prompt :add-history t)))
    (when (null line)
      (setf *repl-running* nil))
    line))

(defun read-multiline-readline ()
  "Read potentially multi-line input with readline."
  (let ((first-line (read-with-readline *prompt*)))
    (when (null first-line)
      (return-from read-multiline-readline nil))
    (let ((paren-count (count-parens first-line))
          (lines (list first-line)))
      ;; If parens are balanced or no parens at all, return single line
      (when (<= paren-count 0)
        (return-from read-multiline-readline first-line))
      ;; Read continuation lines until parens balance
      (loop while (> paren-count 0) do
        (let ((cont-line (read-with-readline *continuation-prompt*)))
          (when (null cont-line)
            (return-from read-multiline-readline nil))
          (push cont-line lines)
          (incf paren-count (count-parens cont-line))))
      ;; Join lines with newlines
      (format nil "~{~A~%~}" (nreverse lines)))))

;;; Main REPL
(defun repl ()
  "Main REPL loop with readline support."
  (print-banner)
  (load-history)
  (setf *repl-running* t)
  (unwind-protect
      (loop while *repl-running* do
        (let ((line (read-multiline-readline)))
          (when line
            (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) line)))
              (unless (string= trimmed "")
                (let ((parsed (parse-command trimmed)))
                  (if parsed
                      (process-command (car parsed) (cdr parsed) trimmed)
                      ;; Evaluate as Lisp expression
                      (handler-case
                          (let* ((expr (read-from-string trimmed))
                                 (result (eval expr)))
                            (format t "~S~%" result))
                        (end-of-file ()
                          (format t "Incomplete expression~%"))
                        (error (e)
                          (format t "Error: ~A~%" e))))))))))
    ;; Cleanup
    (save-history)
    (format t "~%Goodbye!~%")))

(defun main ()
  "Entry point for habu REPL."
  ;; Process command line arguments
  (let ((args sb-ext:*posix-argv*))
    ;; Skip program name and sbcl args - find files after "--"
    (let ((file-args (member "--" args :test #'string=)))
      (when (and file-args (cdr file-args))
        ;; Load files specified on command line
        (dolist (file (cdr file-args))
          (when (probe-file file)
            (format t "Loading ~A...~%" file)
            (load file))))))
  ;; Start REPL
  (repl))
