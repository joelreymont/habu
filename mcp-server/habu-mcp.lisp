;;;; habu-mcp.lisp - MCP Server for Habu Lisp Compiler
;;;;
;;;; A persistent SBCL process with Habu loaded, communicating via JSON-RPC/MCP.
;;;; Provides full SBCL REPL capabilities to Claude Code.
;;;;
;;;; Tools:
;;;; - lisp_eval: Evaluate any Lisp expression with full SBCL
;;;; - lisp_compile: Compile Habu source to ARM64
;;;; - lisp_disasm: Disassemble ARM64 bytes
;;;; - lisp_jit: Compile and execute via mmap
;;;; - lisp_trace: Trace/untrace functions
;;;; - lisp_inspect: Describe objects
;;;;
;;;; Usage: sbcl --load habu-mcp.lisp

(require :sb-posix)
(require :asdf)

;;; Suppress all warnings during load - critical for MCP stability
(declaim (sb-ext:muffle-conditions cl:warning cl:style-warning))

;;; Prevent SBCL from outputting anything during load
(setf *standard-output* (make-broadcast-stream))
(setf *error-output* (make-broadcast-stream))
(setf *trace-output* (make-broadcast-stream))

;;; Load Habu compiler
(handler-case
    (let* ((mcp-dir (make-pathname :directory (pathname-directory *load-truename*)))
           (bootstrap-dir (merge-pathnames (make-pathname :directory '(:relative :up "bootstrap"))
                                           mcp-dir)))
      (push bootstrap-dir asdf:*central-registry*)
      (asdf:load-system :habu))
  (error (e)
    ;; Log error to stderr for debugging
    (format *error-output* "Failed to load Habu: ~A~%" e)
    (force-output *error-output*)))

;;; Restore output streams for MCP communication
(setf *standard-output* (sb-sys:make-fd-stream 1 :output t :buffering :line))
(setf *error-output* (sb-sys:make-fd-stream 2 :output t :buffering :line))
(setf *trace-output* *error-output*)

;;; Capture stream for eval output
(defvar *capture-stream* nil)

(defpackage :habu-mcp
  (:use :cl))

(in-package :habu-mcp)

;;; Forward declarations to suppress style warnings
(declaim (ftype (function (string character) list) split-string))
(declaim (ftype (function (t) t) read-macho-symbols))

;;; ============================================================
;;; Minimal JSON Parser/Serializer
;;; ============================================================

(defun parse-json (str)
  "Parse JSON string to Lisp data (alists for objects, lists for arrays)"
  (let ((pos 0)
        (len (length str)))
    (labels ((skip-ws ()
               (loop while (and (< pos len)
                                (member (char str pos) '(#\Space #\Tab #\Newline #\Return)))
                     do (incf pos)))
             (peek () (and (< pos len) (char str pos)))
             (consume () (prog1 (char str pos) (incf pos)))
             (parse-value ()
               (skip-ws)
               (case (peek)
                 (#\" (parse-string))
                 (#\{ (parse-object))
                 (#\[ (parse-array))
                 (#\t (consume) (consume) (consume) (consume) t)
                 (#\f (consume) (consume) (consume) (consume) (consume) nil)
                 (#\n (consume) (consume) (consume) (consume) :null)
                 (t (parse-number))))
             (parse-string ()
               (consume) ; opening quote
               (let ((chars nil))
                 (loop
                   (let ((ch (consume)))
                     (cond
                       ((char= ch #\") (return (coerce (nreverse chars) 'string)))
                       ((char= ch #\\)
                        (let ((escaped (consume)))
                          (push (case escaped
                                  (#\n #\Newline)
                                  (#\t #\Tab)
                                  (#\r #\Return)
                                  (#\\ #\\)
                                  (#\" #\")
                                  (t escaped))
                                chars)))
                       (t (push ch chars)))))))
             (parse-number ()
               (let ((start pos)
                     (has-dot nil))
                 (when (eql (peek) #\-) (consume))
                 (loop while (and (peek) (digit-char-p (peek))) do (consume))
                 (when (eql (peek) #\.)
                   (setf has-dot t)
                   (consume)
                   (loop while (and (peek) (digit-char-p (peek))) do (consume)))
                 (when (member (peek) '(#\e #\E))
                   (consume)
                   (when (member (peek) '(#\+ #\-)) (consume))
                   (loop while (and (peek) (digit-char-p (peek))) do (consume)))
                 (let ((num-str (subseq str start pos)))
                   (if has-dot
                       (read-from-string num-str)
                       (parse-integer num-str)))))
             (parse-object ()
               (consume) ; {
               (skip-ws)
               (if (eql (peek) #\})
                   (progn (consume) nil)
                   (let ((pairs nil))
                     (loop
                       (skip-ws)
                       (let ((key (parse-string)))
                         (skip-ws) (consume) ; :
                         (skip-ws)
                         (push (cons key (parse-value)) pairs))
                       (skip-ws)
                       (case (peek)
                         (#\, (consume))
                         (#\} (consume) (return (nreverse pairs)))
                         (t (return (nreverse pairs))))))))
             (parse-array ()
               (consume) ; [
               (skip-ws)
               (if (eql (peek) #\])
                   (progn (consume) nil)
                   (let ((items nil))
                     (loop
                       (skip-ws)
                       (push (parse-value) items)
                       (skip-ws)
                       (case (peek)
                         (#\, (consume))
                         (#\] (consume) (return (nreverse items)))
                         (t (return (nreverse items)))))))))
      (parse-value))))

(defun json-escape-string (str)
  "Escape a string for JSON output"
  (with-output-to-string (out)
    (loop for ch across str do
      (case ch
        (#\Newline (write-string "\\n" out))
        (#\Tab (write-string "\\t" out))
        (#\Return (write-string "\\r" out))
        (#\\ (write-string "\\\\" out))
        (#\" (write-string "\\\"" out))
        (t (if (< (char-code ch) 32)
               (format out "\\u~4,'0X" (char-code ch))
               (write-char ch out)))))))

(defun serialize-json (obj)
  "Serialize Lisp data to JSON string"
  (with-output-to-string (s)
    (labels ((emit (x)
               (cond
                 ((eq x :null) (write-string "null" s))
                 ((eq x :empty-object) (write-string "{}" s))
                 ((eq x nil) (write-string "null" s))
                 ((eq x t) (write-string "true" s))
                 ((stringp x)
                  (write-char #\" s)
                  (write-string (json-escape-string x) s)
                  (write-char #\" s))
                 ((numberp x) (princ x s))
                 ((and (consp x) (consp (car x)) (stringp (caar x)))
                  ;; alist with string keys -> object
                  (emit-object x))
                 ((listp x) (emit-array x))
                 ((vectorp x) (emit-array (coerce x 'list)))
                 (t (write-char #\" s)
                    (write-string (json-escape-string (princ-to-string x)) s)
                    (write-char #\" s))))
             (emit-object (alist)
               (write-char #\{ s)
               (let ((first t))
                 (dolist (pair alist)
                   (unless first (write-char #\, s))
                   (setf first nil)
                   (write-char #\" s)
                   (write-string (json-escape-string (car pair)) s)
                   (write-char #\" s)
                   (write-char #\: s)
                   (emit (cdr pair))))
               (write-char #\} s))
             (emit-array (items)
               (write-char #\[ s)
               (let ((first t))
                 (dolist (item items)
                   (unless first (write-char #\, s))
                   (setf first nil)
                   (emit item)))
               (write-char #\] s)))
      (emit obj))))

;;; ============================================================
;;; JSON-RPC / MCP Helpers
;;; ============================================================

(defun jget (obj key)
  "Get value from JSON object (alist) by string key"
  (cdr (assoc key obj :test #'string=)))

(defun make-response (id result)
  `(("jsonrpc" . "2.0")
    ("id" . ,id)
    ("result" . ,result)))

(defun make-error-response (id code message)
  `(("jsonrpc" . "2.0")
    ("id" . ,id)
    ("error" . (("code" . ,code)
                ("message" . ,message)))))

(defun make-text-content (text)
  `((("type" . "text")
     ("text" . ,text))))

;;; ============================================================
;;; Tool Definitions
;;; ============================================================

(defparameter *tools*
  '(("lisp_eval"
     "Evaluate a Lisp expression in SBCL with full Common Lisp capabilities. Returns the result and any printed output. Use this for any Lisp evaluation, REPL interaction, or to call Habu compiler functions."
     (("code" "string" "Lisp code to evaluate (can be multiple expressions)" t)
      ("timeout" "number" "Timeout in seconds (default: 60)" nil)))

    ("lisp_compile"
     "Compile Habu Lisp source to ARM64 machine code. Returns bytecode size and hex dump."
     (("source" "string" "Lisp source (defun or expression)" t)))

    ("lisp_disasm"
     "Disassemble ARM64 machine code bytes to human-readable assembly."
     (("hex" "string" "Hex string of machine code (spaces allowed)" t)))

    ("lisp_jit"
     "Compile Lisp expression and execute it in-process via JIT (mmap RWX). Returns the tagged result."
     (("expr" "string" "Lisp expression to compile and execute" t)))

    ("lisp_trace"
     "Enable or disable function tracing. When enabled, shows all calls with arguments and return values."
     (("function" "string" "Function name (e.g., \"habu:codegen\")" t)
      ("enable" "boolean" "true to start tracing, false to stop" t)))

    ("lisp_inspect"
     "Describe a Lisp object or symbol. Shows type, value, and documentation if available."
     (("object" "string" "Object or symbol to inspect (evaluated first)" t)))

    ("lisp_apropos"
     "Search for symbols matching a substring. Useful for discovering available functions."
     (("pattern" "string" "Substring to search for in symbol names" t)
      ("package" "string" "Package to search in (default: all)" nil)))

    ("lisp_paren_check"
     "Check parenthesis balance in a Lisp source file. Reports any mismatched parens with line/column context."
     (("file" "string" "Path to Lisp file to check" t)))

    ("lisp_hexdump"
     "Hex dump bytes from a file. Shows offset, hex bytes, and ASCII representation like xxd."
     (("file" "string" "Path to file to dump" t)
      ("offset" "number" "Starting byte offset (default 0)" nil)
      ("length" "number" "Number of bytes to dump (default 256, max 4096)" nil)
      ("width" "number" "Bytes per line (default 16)" nil)))

    ("lisp_tagged_value"
     "Decode a Habu tagged value. Shows the type and value for fixnums, cons, symbols, vectors, strings, closures, and nil."
     (("value" "number" "Tagged value (as integer)" t)))

    ("lisp_heap_info"
     "Show Habu heap layout information. Displays the memory layout at x27 including intern table, lambda counter, heap bounds, and allocation pointer."
     ())

    ("lisp_stack_frames"
     "Walk ARM64 stack frames from a core dump or live process. Shows return addresses and tries to map them to function symbols."
     (("binary" "string" "Path to binary for symbol lookup" t)
      ("fp" "string" "Frame pointer (x29) value in hex" t)
      ("sp" "string" "Stack pointer value in hex" t)
      ("depth" "number" "Maximum frames to show (default 20)" nil)))

    ("lisp_codesign"
     "Code sign a Mach-O binary for macOS execution. Uses ad-hoc signing (-s -)."
     (("binary" "string" "Path to binary to sign" t)))

    ("lisp_run"
     "Run a binary and capture output, exit code, and crash info if any."
     (("binary" "string" "Path to binary to run" t)
      ("args" "string" "Command line arguments (optional)" nil)
      ("stdin" "string" "Input to send to stdin (optional)" nil)
      ("timeout" "number" "Timeout in seconds (default 30)" nil)))

    ("lisp_debug"
     "Run binary under lldb and capture crash info including registers and backtrace."
     (("binary" "string" "Path to binary to debug" t)
      ("args" "string" "Command line arguments (optional)" nil)))

    ("lisp_gc_analyze"
     "Analyze GC behavior for a crash. Shows heap state, from/to spaces, and checks for forwarding pointer issues. Run this after a SIGSEGV to understand GC-related crashes."
     (("x27" "string" "x27 register value (heap base) in hex" t)
      ("x28" "string" "x28 register value (alloc ptr) in hex" t)
      ("crash_addr" "string" "Crash address in hex (optional)" nil)))

    ("lisp_check_ptr"
     "Check if a tagged pointer is valid. Detects forwarding pointers (tag 7), nil, and validates heap range."
     (("ptr" "string" "Tagged pointer value in hex" t)
      ("x27" "string" "x27 (heap base) in hex for range check" nil)))

    ("lisp_env_slots"
     "Show environment slot layout. The environment frame at x20 contains local variables that may hold heap pointers."
     (("x20" "string" "x20 register value (env base) in hex" t)
      ("count" "number" "Number of slots to show (default 16)" nil)))

    ("lisp_gc_roots_info"
     "Show what the GC considers as roots. Explains why some stack values aren't updated during GC."
     ())

    ("lisp_lldb_script"
     "Generate an lldb script for debugging GC issues. Includes breakpoints, memory inspection commands, and watchpoints."
     (("binary" "string" "Path to binary" t)
      ("break_on_gc" "boolean" "Set breakpoint on GC-COLLECT (default true)" nil)
      ("watch_env" "boolean" "Watch environment slot changes (default false)" nil)))

    ("lisp_traced_eval"
     "Evaluate Lisp code with function tracing enabled. Traces specified functions during evaluation and returns both the result and trace output."
     (("code" "string" "Lisp code to evaluate" t)
      ("functions" "string" "Space-separated list of functions to trace (e.g., \"habu:codegen habu:lift-lambdas\")" t)
      ("timeout" "number" "Timeout in seconds (default: 60)" nil)))

    ;; Beads (bd) issue tracking tools
    ("bd_ready"
     "Show work items with no blockers. Use this to find available tasks."
     ())

    ("bd_list"
     "List issues with optional status filter."
     (("status" "string" "Filter by status: pending, in_progress, blocked, done (optional)" nil)))

    ("bd_show"
     "Show details of a specific issue."
     (("id" "string" "Issue ID (e.g., habu-abc)" t)))

    ("bd_create"
     "Create a new issue/task."
     (("title" "string" "Issue title" t)
      ("type" "string" "Type: bug, task, feature (default: task)" nil)
      ("priority" "number" "Priority 1-3, 1=highest (default: 2)" nil)
      ("description" "string" "Detailed description (optional)" nil)))

    ("bd_update"
     "Update an issue's status or add notes."
     (("id" "string" "Issue ID" t)
      ("status" "string" "New status: pending, in_progress, blocked, done" nil)
      ("note" "string" "Add a note to the issue" nil)))

    ("bd_close"
     "Close a completed issue. IMPORTANT: Commit changes BEFORE closing."
     (("id" "string" "Issue ID to close" t)
      ("note" "string" "Closing note (optional)" nil)))))

(defun format-tool-schema (name description params)
  (let ((props (mapcar (lambda (p)
                         (cons (first p)
                               `(("type" . ,(second p))
                                 ("description" . ,(third p)))))
                       params))
        (required (remove nil (mapcar (lambda (p)
                                        (when (fourth p) (first p)))
                                      params))))
    `(("name" . ,name)
      ("description" . ,description)
      ("inputSchema" .
       (("type" . "object")
        ("properties" . ,(if props props :empty-object))
        ,@(when required `(("required" . ,required))))))))

;;; ============================================================
;;; Tool Implementations
;;; ============================================================

(defparameter *eval-timeout* 60 "Timeout in seconds for eval operations (default 60s)")

(defvar *eval-worker-pid* nil "PID of current eval worker for subprocess cleanup")

(defun kill-child-processes ()
  "Kill any child processes spawned by the eval worker.
   Uses pkill to kill processes whose parent is this SBCL process."
  (let ((our-pid (sb-posix:getpid)))
    (ignore-errors
      (sb-ext:run-program "/usr/bin/pkill"
                          (list "-P" (format nil "~D" our-pid))
                          :wait t :output nil :error nil))))

(defun safe-eval (code-string &optional timeout-override)
  "Safely evaluate Lisp code with robust timeout using separate thread.
   The worker thread is interrupted if it exceeds the timeout.
   Also kills any child processes on timeout."
  (let* ((output (make-string-output-stream))
         (timeout (or timeout-override *eval-timeout*))
         (result-lock (sb-thread:make-mutex :name "eval-result"))
         (result-ready nil)
         (result-value nil)
         (result-error nil)
         (worker nil))
    ;; Create worker thread
    (setf worker
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (let ((*standard-output* output)
                       (*error-output* output)
                       (*trace-output* output))
                   (let ((val (with-input-from-string (in code-string)
                                (let ((last-val nil))
                                  (loop
                                    (let ((form (read in nil :eof)))
                                      (when (eq form :eof) (return last-val))
                                      (setf last-val (eval form))))))))
                     (sb-thread:with-mutex (result-lock)
                       (setf result-value val
                             result-ready t))))
               (error (e)
                 (sb-thread:with-mutex (result-lock)
                   (setf result-error (format nil "~A" e)
                         result-ready t)))))
           :name "mcp-eval-worker"))
    ;; Wait with timeout, polling every 100ms
    (let ((deadline (+ (get-internal-real-time)
                       (* timeout internal-time-units-per-second))))
      (loop
        (sb-thread:with-mutex (result-lock)
          (when result-ready (return)))
        (when (>= (get-internal-real-time) deadline)
          ;; Timeout - first kill any child processes
          (kill-child-processes)
          ;; Then interrupt the worker thread
          (ignore-errors
            (sb-thread:interrupt-thread worker
              (lambda () (error "Evaluation timed out"))))
          ;; Give it 500ms to handle the interrupt
          (sleep 0.5)
          ;; If still alive, terminate forcibly
          (when (sb-thread:thread-alive-p worker)
            (ignore-errors (sb-thread:terminate-thread worker)))
          (setf result-error (format nil "Evaluation timed out after ~D seconds" timeout))
          (return))
        (sleep 0.1)))
    ;; Build result
    (let ((out-str (get-output-stream-string output)))
      (if result-error
          (format nil "~@[Output:~%~A~%~]Error: ~A"
                  (if (string= out-str "") nil out-str)
                  result-error)
          (format nil "~@[~A~]~@[=> ~S~]"
                  (if (string= out-str "") nil (format nil "~A~%" out-str))
                  result-value)))))

(defun tool-lisp-eval (args)
  (let ((code (jget args "code"))
        (timeout (jget args "timeout")))
    (safe-eval code (when (numberp timeout) (floor timeout)))))

(defun tool-lisp-compile (args)
  (let ((source (jget args "source")))
    (safe-eval
     (format nil
             "(let* ((form (read-from-string ~S))
                     (habu::*function-table* (make-hash-table))
                     (compiled (habu:compile-forms (list form)))
                     (fns (car compiled))
                     (main-ir (cadr compiled)))
                (if fns
                    (let* ((fn-code (habu:codegen-main fns main-ir))
                           (len (length fn-code)))
                      (format nil \"Size: ~~D bytes~~%%Hex: ~~{~~2,'0X~~}\" len fn-code))
                    (let* ((code (habu:codegen main-ir nil nil))
                           (len (length code)))
                      (format nil \"Size: ~~D bytes~~%%Hex: ~~{~~2,'0X~~}\" len code))))"
             source))))

(defun tool-lisp-disasm (args)
  (let ((hex (jget args "hex")))
    (safe-eval
     (format nil
             "(let* ((clean (remove-if (lambda (c) (member c '(#\\Space #\\Newline #\\Tab))) ~S))
                     (bytes (loop for i from 0 below (length clean) by 2
                                  collect (parse-integer clean :start i :end (+ i 2) :radix 16))))
                (with-output-to-string (*standard-output*)
                  (habu::disassemble-bytes bytes)))"
             hex))))

(defun tool-lisp-jit (args)
  "JIT compile and execute an expression via subprocess.
   Returns the result for small fixnums (0-255) as exit code."
  (let ((expr (jget args "expr")))
    (safe-eval
     (format nil
             "(let* ((form (read-from-string ~S))
                     (result (habu:jit-eval form)))
                (format nil \"Result: ~~A\" result))"
             expr))))

(defun tool-lisp-trace (args)
  (let ((fn-name (jget args "function"))
        (enable (jget args "enable")))
    (if enable
        (safe-eval (format nil "(trace ~A)" fn-name))
        (safe-eval (format nil "(untrace ~A)" fn-name)))))

(defun tool-lisp-inspect (args)
  (let ((obj (jget args "object")))
    (safe-eval
     (format nil
             "(let ((val (eval (read-from-string ~S))))
                (with-output-to-string (*standard-output*)
                  (describe val)))"
             obj))))

(defun tool-lisp-apropos (args)
  (let ((pattern (jget args "pattern"))
        (pkg (jget args "package")))
    (if (and pkg (not (string= pkg "")))
        (safe-eval (format nil "(apropos ~S (find-package ~S))" pattern pkg))
        (safe-eval (format nil "(apropos ~S)" pattern)))))

(defun tool-lisp-paren-check (args)
  "Check parenthesis balance in a Lisp file."
  (let ((file (jget args "file")))
    (handler-case
        (with-open-file (stream file :direction :input)
          (let ((balance 0)
                (line-num 1)
                (col-num 0)
                (in-string nil)
                (in-line-comment nil)
                (in-block-comment 0)
                (prev-char nil)
                (paren-stack nil)
                (lines (make-array 100 :adjustable t :fill-pointer 0))
                (min-balance 0)
                (min-balance-line 0)
                (min-balance-col 0))
            ;; Read all lines for context
            (file-position stream 0)
            (loop for line = (read-line stream nil nil)
                  while line
                  do (vector-push-extend line lines))
            ;; Reset and parse
            (file-position stream 0)
            (loop for char = (read-char stream nil nil)
                  while char
                  do
                     (if (char= char #\Newline)
                         (progn
                           (setf in-line-comment nil)
                           (incf line-num)
                           (setf col-num 0))
                         (incf col-num))
                     (cond
                       ((> in-block-comment 0)
                        (cond
                          ((and (char= char #\|) (eql prev-char #\#))
                           (incf in-block-comment))
                          ((and (char= char #\#) (eql prev-char #\|))
                           (decf in-block-comment))))
                       (in-line-comment nil)
                       (in-string
                        (when (and (char= char #\") (not (eql prev-char #\\)))
                          (setf in-string nil)))
                       ((and (char= char #\|) (eql prev-char #\#))
                        (setf in-block-comment 1))
                       ((char= char #\;)
                        (setf in-line-comment t))
                       ((char= char #\")
                        (setf in-string t))
                       ((and (char= char #\\) (eql prev-char #\#))
                        (read-char stream nil nil)
                        (incf col-num))
                       ((char= char #\()
                        (incf balance)
                        (push (list line-num col-num balance) paren-stack))
                       ((char= char #\))
                        (decf balance)
                        (when paren-stack (pop paren-stack))
                        (when (< balance min-balance)
                          (setf min-balance balance
                                min-balance-line line-num
                                min-balance-col col-num))))
                     (setf prev-char char))
            ;; Report results
            (cond
              ((= balance 0)
               (format nil "File ~A is balanced." file))
              ((> balance 0)
               (with-output-to-string (out)
                 (format out "ERROR: ~D unclosed open paren(s) in ~A~%~%" balance file)
                 (format out "Unclosed parens (most recent first):~%")
                 (loop for (ln col bal) in paren-stack
                       for i from 1 to (min 10 (length paren-stack))
                       do (format out "  ~D. Line ~D, col ~D (balance was ~D)~%" i ln col bal)
                          (when (and (> ln 0) (<= ln (length lines)))
                            (format out "      ~A~%" (aref lines (1- ln)))
                            (format out "      ~A^~%" (make-string (1- col) :initial-element #\Space))))))
              (t
               (with-output-to-string (out)
                 (format out "ERROR: ~D extra close paren(s) in ~A~%~%" (- balance) file)
                 (format out "First extra close paren at line ~D, col ~D~%" min-balance-line min-balance-col)
                 (when (and (> min-balance-line 0) (<= min-balance-line (length lines)))
                   (format out "~%Context:~%")
                   (loop for i from (max 1 (- min-balance-line 3))
                               to (min (length lines) (+ min-balance-line 3))
                         do (format out "~4D: ~A~A~%"
                                   i
                                   (aref lines (1- i))
                                   (if (= i min-balance-line) " <-- HERE" "")))))))))
      (error (e)
        (format nil "Error checking file: ~A" e)))))

(defun tool-lisp-hexdump (args)
  "Hex dump bytes from a file."
  (let ((file (jget args "file"))
        (offset (or (jget args "offset") 0))
        (length (min (or (jget args "length") 256) 4096))
        (width (or (jget args "width") 16)))
    (handler-case
        (with-open-file (stream file :direction :input
                                     :element-type '(unsigned-byte 8))
          (file-position stream offset)
          (let ((buffer (make-array length :element-type '(unsigned-byte 8))))
            (let ((bytes-read (read-sequence buffer stream)))
              (with-output-to-string (out)
                (loop for i from 0 below bytes-read by width
                      for line-offset = (+ offset i)
                      do
                         ;; Offset
                         (format out "~8,'0X: " line-offset)
                         ;; Hex bytes
                         (loop for j from 0 below width
                               for idx = (+ i j)
                               do (if (< idx bytes-read)
                                      (format out "~2,'0X " (aref buffer idx))
                                      (format out "   ")))
                         ;; ASCII
                         (format out " ")
                         (loop for j from 0 below width
                               for idx = (+ i j)
                               do (when (< idx bytes-read)
                                    (let ((byte (aref buffer idx)))
                                      (if (and (>= byte 32) (<= byte 126))
                                          (write-char (code-char byte) out)
                                          (write-char #\. out)))))
                         (format out "~%"))))))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun tool-lisp-tagged-value (args)
  "Decode a Habu tagged value."
  (let ((value (jget args "value")))
    (if (not (integerp value))
        "Error: value must be an integer"
        (let ((tag (logand value #xF)))
          (with-output-to-string (out)
            (format out "Raw value: ~16,'0X~%" value)
            (format out "Tag bits:  ~D (~4,'0B)~%" tag tag)
            (cond
              ;; Nil: 0x06
              ((= value #x06)
               (format out "Type: NIL~%")
               (format out "Value: NIL~%"))
              ;; Fixnum: tag 0, value in upper bits
              ((= tag 0)
               (let ((fixnum (ash value -4)))
                 (format out "Type: FIXNUM~%")
                 (format out "Value: ~D (0x~X)~%" fixnum fixnum)))
              ;; Cons: tag 1
              ((= tag 1)
               (let ((ptr (logand value (lognot #xF))))
                 (format out "Type: CONS~%")
                 (format out "Pointer: 0x~X~%" ptr)
                 (format out "  CAR at: 0x~X~%" ptr)
                 (format out "  CDR at: 0x~X~%" (+ ptr 8))))
              ;; Symbol: tag 2
              ((= tag 2)
               (let ((ptr (logand value (lognot #xF))))
                 (format out "Type: SYMBOL~%")
                 (format out "Pointer: 0x~X~%" ptr)
                 (format out "  Name string at: 0x~X~%" ptr)))
              ;; Vector: tag 3
              ((= tag 3)
               (let ((ptr (logand value (lognot #xF))))
                 (format out "Type: VECTOR~%")
                 (format out "Pointer: 0x~X~%" ptr)
                 (format out "  Length at: 0x~X~%" ptr)
                 (format out "  Data at: 0x~X~%" (+ ptr 8))))
              ;; String: tag 4
              ((= tag 4)
               (let ((ptr (logand value (lognot #xF))))
                 (format out "Type: STRING~%")
                 (format out "Pointer: 0x~X~%" ptr)
                 (format out "  Length at: 0x~X~%" ptr)
                 (format out "  Chars at: 0x~X~%" (+ ptr 8))))
              ;; Closure: tag 5
              ((= tag 5)
               (let ((ptr (logand value (lognot #xF))))
                 (format out "Type: CLOSURE~%")
                 (format out "Pointer: 0x~X~%" ptr)
                 (format out "  Code addr at: 0x~X~%" ptr)
                 (format out "  Env at: 0x~X~%" (+ ptr 8))))
              ;; Tag 6 (but not exactly 0x06)
              ((= tag 6)
               (format out "Type: UNKNOWN (tag 6, not nil)~%")
               (format out "This may be an invalid value~%"))
              ;; Unknown
              (t
               (format out "Type: UNKNOWN~%")
               (format out "Tag ~D is not a valid Habu tag~%" tag))))))))

(defun tool-lisp-heap-info (args)
  "Show Habu heap layout information."
  (declare (ignore args))
  (with-output-to-string (out)
    (format out "Habu Heap Layout (at x27 base register)~%")
    (format out "========================================~%~%")
    (format out "Simple GC Mode:~%")
    (format out "  [x27+0]:   from_start (from-space start)~%")
    (format out "  [x27+8]:   to_start (to-space start)~%")
    (format out "  [x27+16]:  from_end (from-space end)~%")
    (format out "  [x27+24]:  half_heap_size (usually 32MB = 0x2000000)~%")
    (format out "  [x27+32]:  space_flag (0 or 1)~%")
    (format out "  [x27+40]:  gc_state~%")
    (format out "  [x27+48]:  symbol_counter~%")
    (format out "  [x27+56]:  symbol_table~%")
    (format out "  [x27+64]:  heap data starts~%~%")
    (format out "Generational GC Mode (extends above):~%")
    (format out "  [x27+80]:  nursery-start~%")
    (format out "  [x27+88]:  nursery-end (also old-space-start)~%")
    (format out "  [x27+96]:  card-table-start~%")
    (format out "  [x27+104]: old-space-half-size~%")
    (format out "  [x27+112]: old-space-flag~%")
    (format out "  [x27+120]: old-space-alloc~%")
    (format out "  [x27+128]: heap data starts~%~%")
    (format out "Register Usage:~%")
    (format out "  x27: GC globals base pointer~%")
    (format out "  x28: Heap bump pointer (allocation)~%")
    (format out "  x26: Code base register~%")
    (format out "  x20: Environment frame base~%")
    (format out "  x24: Closure environment pointer~%~%")
    (format out "To inspect live heap in lldb:~%")
    (format out "  (lldb) register read x27 x28~%")
    (format out "  (lldb) memory read -c 64 $x27~%")))

(defun tool-lisp-stack-frames (args)
  "Walk ARM64 stack frames and map to symbols."
  (let ((binary (jget args "binary"))
        (fp-str (jget args "fp"))
        (sp-str (jget args "sp"))
        (depth (or (jget args "depth") 20)))
    (declare (ignore depth))
    (handler-case
        (let ((fp (parse-integer (string-left-trim "0x" fp-str) :radix 16))
              (sp (parse-integer (string-left-trim "0x" sp-str) :radix 16)))
          ;; Read symbol table from binary
          (let ((symbols (read-macho-symbols binary)))
            (with-output-to-string (out)
              (format out "Stack Frames (fp=0x~X, sp=0x~X)~%~%" fp sp)
              (format out "Note: This shows the expected frame layout.~%")
              (format out "Use lldb to read actual memory:~%")
              (format out "  (lldb) memory read -c 16 0x~X~%~%" fp)
              (format out "Frame Layout (ARM64 standard):~%")
              (format out "  [fp+0]:  saved fp (x29)~%")
              (format out "  [fp+8]:  return address (x30)~%")
              (format out "  [fp+16]: saved x19~%")
              (format out "  [fp+24]: saved x20~%")
              (format out "  ... more saved registers ...~%~%")
              (when symbols
                (format out "Symbols in binary (~D total):~%" (length symbols))
                (loop for sym in (subseq symbols 0 (min 20 (length symbols)))
                      do (format out "  ~A @ 0x~X~%" (car sym) (cdr sym)))
                (when (> (length symbols) 20)
                  (format out "  ... and ~D more~%" (- (length symbols) 20)))))))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun read-macho-symbols (path)
  "Read symbol table from Mach-O binary. Returns alist of (name . address)."
  (handler-case
      (with-open-file (stream path :direction :input
                                   :element-type '(unsigned-byte 8))
        (let ((header (make-array 32 :element-type '(unsigned-byte 8))))
          (read-sequence header stream)
          ;; Check magic
          (let ((magic (logior (aref header 0)
                              (ash (aref header 1) 8)
                              (ash (aref header 2) 16)
                              (ash (aref header 3) 24))))
            (unless (= magic #xFEEDFACF)  ; MH_MAGIC_64
              (return-from read-macho-symbols nil))
            ;; Parse load commands to find LC_SYMTAB
            (let ((ncmds (logior (aref header 16)
                                (ash (aref header 17) 8)
                                (ash (aref header 18) 16)
                                (ash (aref header 19) 24))))
              (file-position stream 32)  ; After header
              (dotimes (i ncmds)
                (let ((cmd-header (make-array 8 :element-type '(unsigned-byte 8))))
                  (read-sequence cmd-header stream)
                  (let ((cmd (logior (aref cmd-header 0)
                                    (ash (aref cmd-header 1) 8)))
                        (cmdsize (logior (aref cmd-header 4)
                                        (ash (aref cmd-header 5) 8)
                                        (ash (aref cmd-header 6) 16)
                                        (ash (aref cmd-header 7) 24))))
                    (when (= cmd 2)  ; LC_SYMTAB
                      (let ((symtab-data (make-array 16 :element-type '(unsigned-byte 8))))
                        (read-sequence symtab-data stream)
                        (let ((symoff (logior (aref symtab-data 0)
                                             (ash (aref symtab-data 1) 8)
                                             (ash (aref symtab-data 2) 16)
                                             (ash (aref symtab-data 3) 24)))
                              (nsyms (logior (aref symtab-data 4)
                                            (ash (aref symtab-data 5) 8)
                                            (ash (aref symtab-data 6) 16)
                                            (ash (aref symtab-data 7) 24)))
                              (stroff (logior (aref symtab-data 8)
                                             (ash (aref symtab-data 9) 8)
                                             (ash (aref symtab-data 10) 16)
                                             (ash (aref symtab-data 11) 24))))
                          ;; Read string table
                          (file-position stream stroff)
                          (let ((strtab (make-array 65536 :element-type '(unsigned-byte 8))))
                            (read-sequence strtab stream)
                            ;; Read symbols
                            (file-position stream symoff)
                            (let ((symbols nil))
                              (dotimes (j (min nsyms 500))
                                (let ((nlist (make-array 16 :element-type '(unsigned-byte 8))))
                                  (read-sequence nlist stream)
                                  (let ((strx (logior (aref nlist 0)
                                                     (ash (aref nlist 1) 8)
                                                     (ash (aref nlist 2) 16)
                                                     (ash (aref nlist 3) 24)))
                                        (value (logior (aref nlist 8)
                                                      (ash (aref nlist 9) 8)
                                                      (ash (aref nlist 10) 16)
                                                      (ash (aref nlist 11) 24)
                                                      (ash (aref nlist 12) 32)
                                                      (ash (aref nlist 13) 40)
                                                      (ash (aref nlist 14) 48)
                                                      (ash (aref nlist 15) 56))))
                                    (when (and (< strx 65536) (> value 0))
                                      (let ((name (with-output-to-string (s)
                                                   (loop for k from strx
                                                         while (and (< k 65536)
                                                                   (not (zerop (aref strtab k))))
                                                         do (write-char (code-char (aref strtab k)) s)))))
                                        (when (> (length name) 0)
                                          (push (cons name value) symbols)))))))
                              (return-from read-macho-symbols (nreverse symbols)))))))
                    (file-position stream (+ (- (file-position stream) 8) cmdsize)))))))))
    (error () nil)))

(defun tool-lisp-codesign (args)
  "Code sign a binary using ad-hoc signing."
  (let ((binary (jget args "binary")))
    (handler-case
        (let ((output (with-output-to-string (s)
                        (sb-ext:run-program "/usr/bin/codesign"
                                           (list "-f" "-s" "-" binary)
                                           :output s :error s))))
          (format nil "Signed: ~A~%~A" binary output))
      (error (e)
        (format nil "Error signing ~A: ~A" binary e)))))

(defun tool-lisp-run (args)
  "Run a binary and capture output/exit code."
  (let ((binary (jget args "binary"))
        (cmd-args (or (jget args "args") ""))
        (stdin-input (jget args "stdin"))
        (timeout (or (jget args "timeout") 30)))
    (handler-case
        (let* ((args-list (if (string= cmd-args "")
                              nil
                              (split-string cmd-args #\Space)))
               ;; Use stdin stream if input provided, else /dev/null
               (proc (sb-ext:run-program binary args-list
                                        :input (if stdin-input :stream nil)
                                        :output :stream
                                        :error :stream
                                        :wait nil)))
          ;; Write stdin input if provided
          (when stdin-input
            (let ((input-stream (sb-ext:process-input proc)))
              (write-string stdin-input input-stream)
              (terpri input-stream)
              (close input-stream)))
          ;; Wait with timeout (sleep to avoid busy loop)
          (let ((start-time (get-internal-real-time))
                (killed nil))
            (loop
              (when (not (sb-ext:process-alive-p proc))
                (return))
              (when (> (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)
                       timeout)
                (sb-ext:process-kill proc 9)
                (setf killed t)
                (sleep 0.1)  ; Give process time to die
                (return))
              (sleep 0.1))  ; Sleep to avoid busy-wait
            (sb-ext:process-wait proc)  ; Ensure process is reaped
            (let ((stdout (with-output-to-string (s)
                           (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                                 while line do (format s "~A~%" line))))
                  (stderr (with-output-to-string (s)
                           (loop for line = (read-line (sb-ext:process-error proc) nil nil)
                                 while line do (format s "~A~%" line))))
                  (exit-code (sb-ext:process-exit-code proc)))
              (with-output-to-string (out)
                (format out "Exit code: ~A~%" exit-code)
                (when killed
                  (format out "(Killed by timeout after ~A seconds)~%" timeout))
                (when (and exit-code (> exit-code 128))
                  (format out "Signal: ~A (~A)~%"
                          (- exit-code 128)
                          (case (- exit-code 128)
                            (4 "SIGILL - illegal instruction")
                            (6 "SIGABRT - abort")
                            (9 "SIGKILL - killed")
                            (10 "SIGBUS - bus error")
                            (11 "SIGSEGV - segmentation fault")
                            (t "unknown"))))
                (when (> (length stdout) 0)
                  (format out "~%Stdout:~%~A" stdout))
                (when (> (length stderr) 0)
                  (format out "~%Stderr:~%~A" stderr))))))
      (error (e)
        (format nil "Error running ~A: ~A" binary e)))))

(defun split-string (string char)
  "Split string by char."
  (loop for start = 0 then (1+ end)
        for end = (position char string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun tool-lisp-debug (args)
  "Run binary under lldb and capture crash info."
  (let ((binary (jget args "binary"))
        (cmd-args (or (jget args "args") ""))
        (timeout 30))  ; 30 second timeout for debug sessions
    (declare (ignore cmd-args))
    (handler-case
        (let* ((proc (sb-ext:run-program "/usr/bin/lldb"
                                        (list binary
                                              "-o" "run"
                                              "-o" "register read x0 x1 x9 x19 x20 x24 x26 x27 x28 pc sp"
                                              "-o" "bt"
                                              "-o" "disassemble -p -c 10"
                                              "-o" "quit")
                                        :input nil  ; Prevent hanging on stdin
                                        :output :stream
                                        :error :stream
                                        :wait nil)))  ; Don't block
          ;; Wait with timeout
          (let ((start-time (get-internal-real-time))
                (killed nil))
            (loop
              (when (not (sb-ext:process-alive-p proc))
                (return))
              (when (> (/ (- (get-internal-real-time) start-time)
                         internal-time-units-per-second)
                       timeout)
                (sb-ext:process-kill proc 9)
                (setf killed t)
                (sleep 0.1)
                (return))
              (sleep 0.1))
            (sb-ext:process-wait proc)
            (let ((output (with-output-to-string (s)
                           (when killed
                             (format s "(Debug session killed after ~A second timeout)~%~%" timeout))
                           (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                                 while line do (format s "~A~%" line)))))
              output)))
      (error (e)
        (format nil "Error debugging ~A: ~A" binary e)))))

(defun tool-lisp-gc-analyze (args)
  "Analyze GC state from register values."
  (let ((x27-str (jget args "x27"))
        (x28-str (jget args "x28"))
        (crash-str (jget args "crash_addr")))
    (handler-case
        (let* ((x27 (parse-integer (string-left-trim "0x" x27-str) :radix 16))
               (x28 (parse-integer (string-left-trim "0x" x28-str) :radix 16))
               (crash-addr (when crash-str
                             (parse-integer (string-left-trim "0x" crash-str) :radix 16)))
               (heap-data-offset 96)
               (half-heap #x4000000)  ; 64MB
               (from-start (+ x27 heap-data-offset))
               (from-end (+ from-start half-heap))
               (to-start (+ from-end))
               (to-end (+ to-start half-heap)))
          (with-output-to-string (out)
            (format out "=== GC State Analysis ===~%~%")
            (format out "Heap Base (x27):     0x~X~%" x27)
            (format out "Alloc Ptr (x28):     0x~X~%" x28)
            (format out "~%")
            (format out "Heap Layout (assuming half_heap=64MB):~%")
            (format out "  Globals:     0x~X - 0x~X (~D bytes)~%"
                    x27 (+ x27 heap-data-offset) heap-data-offset)
            (format out "  From-space:  0x~X - 0x~X~%" from-start from-end)
            (format out "  To-space:    0x~X - 0x~X~%" to-start to-end)
            (format out "~%")
            ;; Analyze x28 position
            (let ((alloc-offset (- x28 x27)))
              (format out "Allocation Analysis:~%")
              (format out "  x28 - x27 = 0x~X (~:D bytes)~%" alloc-offset alloc-offset)
              (cond
                ((< x28 from-start)
                 (format out "  STATUS: x28 is BEFORE heap data - INVALID~%"))
                ((< x28 from-end)
                 (let ((used (- x28 from-start)))
                   (format out "  STATUS: x28 is in from-space~%")
                   (format out "  Used: ~:D bytes (~,1F%)~%"
                           used (* 100.0 (/ used half-heap)))))
                ((= x28 from-end)
                 (format out "  STATUS: x28 AT from-end - heap is FULL~%")
                 (format out "  GC ran but heap is still full (all objects live)~%"))
                (t
                 (format out "  STATUS: x28 BEYOND from-end - heap OVERFLOW~%")
                 (format out "  Overflow by ~:D bytes~%" (- x28 from-end))
                 (format out "  GC may not be triggering correctly~%"))))
            ;; Analyze crash address if provided
            (when crash-addr
              (format out "~%Crash Address Analysis:~%")
              (format out "  Address: 0x~X~%" crash-addr)
              (cond
                ((= crash-addr 0)
                 (format out "  NULL pointer dereference~%")
                 (format out "  Likely cause: accessing car/cdr of nil or stale pointer~%"))
                ((< crash-addr #x1000)
                 (format out "  Low address - probably nil (0x06) with tag cleared~%"))
                ((and (>= crash-addr from-start) (< crash-addr from-end))
                 (format out "  Address is in from-space~%")
                 (format out "  This is VALID before GC but STALE after GC~%")
                 (format out "  After GC, from-space contains forwarding pointers~%"))
                ((and (>= crash-addr to-start) (< crash-addr to-end))
                 (format out "  Address is in to-space~%")
                 (format out "  This should be valid if pointing to live data~%"))
                (t
                 (format out "  Address outside heap - may be code or unmapped~%"))))
            ;; Recommendations
            (format out "~%Debugging Steps:~%")
            (format out "1. In lldb, examine heap globals:~%")
            (format out "   memory read -s8 -c12 -fx $x27~%")
            (format out "2. Check from_end value at [x27+16]:~%")
            (format out "   memory read -s8 -c1 -fx ($x27+16)~%")
            (format out "3. If crash is nil access, check what was loaded:~%")
            (format out "   Look at the ldr instruction before crash~%")
            (format out "4. If crash is stale pointer, the env frame wasn't updated:~%")
            (format out "   GC only updates registers, not stack variables~%")))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun tool-lisp-check-ptr (args)
  "Check if a pointer is valid, forwarding, or stale."
  (let ((ptr-str (jget args "ptr"))
        (x27-str (jget args "x27")))
    (handler-case
        (let* ((ptr (parse-integer (string-left-trim "0x" ptr-str) :radix 16))
               (x27 (when x27-str
                      (parse-integer (string-left-trim "0x" x27-str) :radix 16)))
               (tag (logand ptr #xF))
               (base (logand ptr (lognot #xF))))
          (with-output-to-string (out)
            (format out "Pointer Analysis: 0x~X~%~%" ptr)
            (format out "Tag:  ~D (0x~X)~%" tag tag)
            (format out "Base: 0x~X~%~%" base)
            ;; Decode tag
            (format out "Tag interpretation:~%")
            (cond
              ((= ptr #x06)
               (format out "  This is NIL~%")
               (format out "  Accessing car/cdr of nil will crash (base = 0)~%"))
              ((= tag 0)
               (let ((val (ash ptr -4)))
                 (format out "  FIXNUM: ~D (0x~X)~%" val val)
                 (format out "  This is immediate data, not a pointer~%")))
              ((= tag 1)
               (format out "  CONS cell at 0x~X~%" base)
               (format out "  CAR at: 0x~X, CDR at: 0x~X~%" base (+ base 8)))
              ((= tag 2)
               (format out "  SYMBOL at 0x~X~%" base))
              ((= tag 3)
               (format out "  VECTOR at 0x~X~%" base))
              ((= tag 4)
               (format out "  STRING at 0x~X~%" base))
              ((= tag 5)
               (format out "  CLOSURE at 0x~X~%" base))
              ((= tag 6)
               (format out "  Tag 6 but not nil (0x06)~%")
               (format out "  This is likely INVALID~%"))
              ((= tag 7)
               (format out "  FORWARDING POINTER!~%")
               (format out "  This indicates a GC-moved object~%")
               (format out "  New location: 0x~X~%" base)
               (format out "  If you see this, the pointer wasn't updated after GC~%"))
              (t
               (format out "  UNKNOWN tag~%")))
            ;; Range check if x27 provided
            (when (and x27 (> base 0))
              (let* ((heap-start (+ x27 96))
                     (heap-size #x8000000)  ; 128MB total
                     (heap-end (+ heap-start heap-size)))
                (format out "~%Heap range check:~%")
                (cond
                  ((< base heap-start)
                   (format out "  BEFORE heap - not a heap pointer~%"))
                  ((< base (+ heap-start #x4000000))
                   (format out "  In from-space (first 64MB)~%")
                   (format out "  Valid BEFORE GC, stale AFTER GC~%"))
                  ((< base heap-end)
                   (format out "  In to-space (second 64MB)~%")
                   (format out "  Valid AFTER GC~%"))
                  (t
                   (format out "  BEYOND heap - invalid~%")))))))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun tool-lisp-env-slots (args)
  "Show environment slot layout."
  (let ((x20-str (jget args "x20"))
        (count (or (jget args "count") 16)))
    (handler-case
        (let ((x20 (parse-integer (string-left-trim "0x" x20-str) :radix 16)))
          (with-output-to-string (out)
            (format out "Environment Frame Layout~%")
            (format out "========================~%~%")
            (format out "x20 (env base): 0x~X~%~%" x20)
            (format out "Slots (relative to x20):~%")
            (dotimes (i count)
              (let ((offset (* i 8)))
                (format out "  [x20-0x~2,'0X]: slot ~D (arg/local ~D)~%"
                        offset i i)))
            (format out "~%")
            (format out "To inspect in lldb:~%")
            (format out "  memory read -s8 -c~D -fx ($x20 - ~D)~%"
                    count (* count 8))
            (format out "~%")
            (format out "IMPORTANT: These slots may contain heap pointers that~%")
            (format out "are NOT updated by GC. After GC runs, any pointer here~%")
            (format out "that was in from-space will be stale (points to forwarding~%")
            (format out "pointer or garbage).~%")))
      (error (e)
        (format nil "Error: ~A" e)))))

(defun tool-lisp-gc-roots-info (args)
  "Explain what GC considers as roots."
  (declare (ignore args))
  (with-output-to-string (out)
    (format out "=== GC Root Information ===~%~%")
    (format out "The Habu GC updates the following roots during collection:~%~%")
    (format out "UPDATED by GC:~%")
    (format out "  1. Saved registers (on GC's own stack):~%")
    (format out "     - x0-x7  (arguments/return values)~%")
    (format out "     - x24    (closure environment)~%")
    (format out "     - x25    (saved)~%")
    (format out "  2. Intern table at [x27+0]~%")
    (format out "~%")
    (format out "NOT UPDATED by GC:~%")
    (format out "  1. Environment slots at [x20-N]~%")
    (format out "     - These are local variables on caller's stack~%")
    (format out "     - GC doesn't know about them~%")
    (format out "  2. Values in caller's stack frame~%")
    (format out "  3. Heap pointers stored in other heap objects~%")
    (format out "     - These ARE updated via Cheney scanning~%")
    (format out "~%")
    (format out "THE PROBLEM:~%")
    (format out "When GC moves an object:~%")
    (format out "  1. Object copied from from-space to to-space~%")
    (format out "  2. Forwarding pointer left at old location~%")
    (format out "  3. Saved registers are updated to new location~%")
    (format out "  4. BUT [x20-N] slots still have old pointers~%")
    (format out "~%")
    (format out "After GC returns:~%")
    (format out "  - Code loads from [x20-N], gets old pointer~%")
    (format out "  - Tries to access object at old address~%")
    (format out "  - Finds forwarding pointer (tag 7) or garbage~%")
    (format out "  - CRASH!~%")
    (format out "~%")
    (format out "SOLUTIONS:~%")
    (format out "  1. Stack maps: Compiler emits info about stack layout~%")
    (format out "  2. Conservative scanning: GC scans entire stack~%")
    (format out "  3. Reload after GC: Codegen reloads from safe locations~%")
    (format out "  4. Different allocation: Put all temp values in registers~%")))

(defun tool-lisp-traced-eval (args)
  "Evaluate code with tracing enabled for specified functions."
  (let ((code (jget args "code"))
        (functions (jget args "functions"))
        (timeout (jget args "timeout")))
    (let ((fn-list (split-string functions #\Space)))
      ;; Build code that traces, evals, untraces
      (safe-eval
       (format nil
               "(let ((*trace-output* *standard-output*))
                  (unwind-protect
                      (progn
                        ~{(trace ~A)~%~}
                        (eval (read-from-string ~S)))
                    ~{(ignore-errors (untrace ~A))~%~}))"
               fn-list code fn-list)
       (when (numberp timeout) (floor timeout))))))

;;; ============================================================
;;; Beads (bd) Tool Implementations
;;; ============================================================

(defun run-bd-command (args-list)
  "Run bd command and return output."
  (handler-case
      (let ((proc (sb-ext:run-program "bd" args-list
                                      :search t
                                      :input nil
                                      :output :stream
                                      :error :stream
                                      :wait t)))
        (let ((stdout (with-output-to-string (s)
                        (loop for line = (read-line (sb-ext:process-output proc) nil nil)
                              while line do (format s "~A~%" line))))
              (stderr (with-output-to-string (s)
                        (loop for line = (read-line (sb-ext:process-error proc) nil nil)
                              while line do (format s "~A~%" line))))
              (exit-code (sb-ext:process-exit-code proc)))
          (if (zerop exit-code)
              stdout
              (format nil "~A~@[Error: ~A~]" stdout (if (string= stderr "") nil stderr)))))
    (error (e)
      (format nil "Error running bd: ~A" e))))

(defun tool-bd-ready (args)
  (declare (ignore args))
  (run-bd-command (list "ready")))

(defun tool-bd-list (args)
  (let ((status (jget args "status")))
    (if (and status (not (string= status "")))
        (run-bd-command (list "list" "--status" status))
        (run-bd-command (list "list")))))

(defun tool-bd-show (args)
  (let ((id (jget args "id")))
    (run-bd-command (list "show" id))))

(defun tool-bd-create (args)
  (let ((title (jget args "title"))
        (type (or (jget args "type") "task"))
        (priority (or (jget args "priority") 2))
        (description (jget args "description")))
    (let ((cmd-args (list "create" title "-t" type "-p" (format nil "~D" priority))))
      (when (and description (not (string= description "")))
        (setf cmd-args (append cmd-args (list "-d" description))))
      (run-bd-command cmd-args))))

(defun tool-bd-update (args)
  (let ((id (jget args "id"))
        (status (jget args "status"))
        (note (jget args "note")))
    (let ((cmd-args (list "update" id)))
      (when (and status (not (string= status "")))
        (setf cmd-args (append cmd-args (list "--status" status))))
      (when (and note (not (string= note "")))
        (setf cmd-args (append cmd-args (list "-n" note))))
      (run-bd-command cmd-args))))

(defun tool-bd-close (args)
  (let ((id (jget args "id"))
        (note (jget args "note")))
    (let ((cmd-args (list "close" id)))
      (when (and note (not (string= note "")))
        (setf cmd-args (append cmd-args (list "-n" note))))
      (run-bd-command cmd-args))))

(defun tool-lisp-lldb-script (args)
  "Generate lldb script for GC debugging."
  (let ((binary (jget args "binary"))
        (break-gc (if (jget args "break_on_gc") t t))  ; default true
        (watch-env (jget args "watch_env")))
    (with-output-to-string (out)
      (format out "# LLDB script for debugging GC issues~%")
      (format out "# Usage: lldb -s this_script.lldb ~A~%~%" binary)
      (format out "target create \"~A\"~%~%" binary)
      ;; Breakpoints
      (when break-gc
        (format out "# Break on GC~%")
        (format out "breakpoint set -n GC-COLLECT~%")
        (format out "breakpoint command add 1~%")
        (format out "printf \"\\n=== GC-COLLECT called ===\\n\"~%")
        (format out "register read x27 x28~%")
        (format out "printf \"Heap at [x27]: \"~%")
        (format out "memory read -s8 -c4 -fx $x27~%")
        (format out "printf \"\\n\"~%")
        (format out "continue~%")
        (format out "DONE~%~%"))
      ;; GC return breakpoint
      (format out "# Break when GC returns to see updated state~%")
      (format out "# Find return address in caller and set breakpoint~%~%")
      ;; Memory inspection aliases
      (format out "# Useful commands:~%")
      (format out "# Show heap globals:~%")
      (format out "#   memory read -s8 -c12 -fx $x27~%")
      (format out "#~%")
      (format out "# Show environment slots:~%")
      (format out "#   memory read -s8 -c16 -fx ($x20 - 128)~%")
      (format out "#~%")
      (format out "# Check a tagged pointer:~%")
      (format out "#   p/x $x0 & 0xf  # show tag~%")
      (format out "#   p/x $x0 & ~~0xf # show base~%")
      (format out "#~%")
      (format out "# Check for forwarding pointer (tag 7):~%")
      (format out "#   p ($x0 & 0xf) == 7~%")
      (format out "~%")
      ;; Watchpoint for env slot
      (when watch-env
        (format out "# Watch environment slot (set $x20 first):~%")
        (format out "# watchpoint set expression -w write -- ($x20 - 0x48)~%~%"))
      ;; Run command
      (format out "# Run the program:~%")
      (format out "run~%"))))

(defun dispatch-tool (name args)
  (cond
    ((string= name "lisp_eval") (tool-lisp-eval args))
    ((string= name "lisp_compile") (tool-lisp-compile args))
    ((string= name "lisp_disasm") (tool-lisp-disasm args))
    ((string= name "lisp_jit") (tool-lisp-jit args))
    ((string= name "lisp_trace") (tool-lisp-trace args))
    ((string= name "lisp_inspect") (tool-lisp-inspect args))
    ((string= name "lisp_apropos") (tool-lisp-apropos args))
    ((string= name "lisp_paren_check") (tool-lisp-paren-check args))
    ((string= name "lisp_hexdump") (tool-lisp-hexdump args))
    ((string= name "lisp_tagged_value") (tool-lisp-tagged-value args))
    ((string= name "lisp_heap_info") (tool-lisp-heap-info args))
    ((string= name "lisp_stack_frames") (tool-lisp-stack-frames args))
    ((string= name "lisp_codesign") (tool-lisp-codesign args))
    ((string= name "lisp_run") (tool-lisp-run args))
    ((string= name "lisp_debug") (tool-lisp-debug args))
    ((string= name "lisp_gc_analyze") (tool-lisp-gc-analyze args))
    ((string= name "lisp_check_ptr") (tool-lisp-check-ptr args))
    ((string= name "lisp_env_slots") (tool-lisp-env-slots args))
    ((string= name "lisp_gc_roots_info") (tool-lisp-gc-roots-info args))
    ((string= name "lisp_lldb_script") (tool-lisp-lldb-script args))
    ((string= name "lisp_traced_eval") (tool-lisp-traced-eval args))
    ;; Beads (bd) tools
    ((string= name "bd_ready") (tool-bd-ready args))
    ((string= name "bd_list") (tool-bd-list args))
    ((string= name "bd_show") (tool-bd-show args))
    ((string= name "bd_create") (tool-bd-create args))
    ((string= name "bd_update") (tool-bd-update args))
    ((string= name "bd_close") (tool-bd-close args))
    (t (format nil "Unknown tool: ~A" name))))

;;; ============================================================
;;; MCP Protocol Handlers
;;; ============================================================

(defun handle-initialize (id params)
  (declare (ignore params))
  (make-response id
    `(("protocolVersion" . "2024-11-05")
      ("capabilities" . (("tools" . :empty-object)))
      ("serverInfo" . (("name" . "habu-mcp")
                       ("version" . "1.0.0"))))))

(defun handle-tools-list (id)
  (make-response id
    `(("tools" . ,(mapcar (lambda (tool)
                            (apply #'format-tool-schema tool))
                          *tools*)))))

(defun handle-tools-call (id params)
  (let* ((name (jget params "name"))
         (args (jget params "arguments"))
         (result (handler-case
                     (dispatch-tool name args)
                   (error (e)
                     (format nil "Error: ~A" e)))))
    (make-response id
      `(("content" . ,(make-text-content result))))))

(defun handle-request (request)
  "Handle a JSON-RPC request, return response or nil"
  (let ((method (jget request "method"))
        (id (jget request "id"))
        (params (jget request "params")))
    (cond
      ((string= method "initialize")
       (handle-initialize id params))
      ((string= method "notifications/initialized")
       nil)
      ((string= method "tools/list")
       (handle-tools-list id))
      ((string= method "tools/call")
       (handle-tools-call id params))
      ((string= method "ping")
       (make-response id (jget params "data")))
      (t
       (when id
         (make-error-response id -32601
                              (format nil "Method not found: ~A" method)))))))

;;; ============================================================
;;; Main Server Loop
;;; ============================================================

(defun read-line-safe ()
  "Read a line from stdin, return nil on EOF"
  (handler-case
      (read-line *standard-input* nil nil)
    (error () nil)))

(defun run-server ()
  "Main MCP server loop - read JSON-RPC from stdin, write to stdout"
  (let ((stdin (sb-sys:make-fd-stream 0 :input t :buffering :line))
        (stdout (sb-sys:make-fd-stream 1 :output t :buffering :line)))
    (let ((*standard-input* stdin)
          (*standard-output* stdout))
      (loop
        (let ((line (read-line-safe)))
          (unless line (return))
          (when (> (length line) 0)
            (let* ((request (handler-case (parse-json line)
                              (error () nil)))
                   (response (when (and request (listp request))
                               (handler-case (handle-request request)
                                 (error (e)
                                   (make-error-response
                                    (ignore-errors (jget request "id"))
                                    -32603
                                    (format nil "Internal error: ~A" e)))))))
              (when response
                (write-line (serialize-json response))
                (force-output)))))))))

;;; Entry point
(run-server)
(sb-ext:exit :code 0)
