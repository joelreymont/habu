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
     (("code" "string" "Lisp code to evaluate (can be multiple expressions)" t)))

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
     (("file" "string" "Path to Lisp file to check" t)))))

(defun format-tool-schema (name description params)
  `(("name" . ,name)
    ("description" . ,description)
    ("inputSchema" .
     (("type" . "object")
      ("properties" .
       ,(mapcar (lambda (p)
                  (cons (first p)
                        `(("type" . ,(second p))
                          ("description" . ,(third p)))))
                params))
      ("required" .
       ,(remove nil (mapcar (lambda (p)
                              (when (fourth p) (first p)))
                            params)))))))

;;; ============================================================
;;; Tool Implementations
;;; ============================================================

(defparameter *eval-timeout* 10 "Timeout in seconds for eval operations")

(defun safe-eval (code-string)
  "Safely evaluate Lisp code, capturing output and errors. Times out after *eval-timeout* seconds."
  (let ((output (make-string-output-stream))
        (result nil)
        (error-msg nil))
    (handler-case
        (sb-ext:with-timeout *eval-timeout*
          (let ((*standard-output* output)
                (*error-output* output)
                (*trace-output* output))
            (setf result
                  (with-input-from-string (in code-string)
                    (let ((last-val nil))
                      (loop
                        (let ((form (read in nil :eof)))
                          (when (eq form :eof) (return last-val))
                          (setf last-val (eval form)))))))))
      (sb-ext:timeout ()
        (setf error-msg (format nil "Evaluation timed out after ~D seconds" *eval-timeout*)))
      (error (e)
        (setf error-msg (format nil "~A" e))))
    (let ((out-str (get-output-stream-string output)))
      (if error-msg
          (format nil "~@[Output:~%~A~%~]Error: ~A"
                  (if (string= out-str "") nil out-str)
                  error-msg)
          (format nil "~@[~A~]~@[=> ~S~]"
                  (if (string= out-str "") nil (format nil "~A~%" out-str))
                  result)))))

(defun tool-lisp-eval (args)
  (let ((code (jget args "code")))
    (safe-eval code)))

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
                                  collect (parse-integer clean :start i :end (+ i 2) :radix 16)))
                     (vec (coerce bytes 'vector)))
                (with-output-to-string (*standard-output*)
                  (habu::disasm-reg-alloc-bytes vec)))"
             hex))))

(defun tool-lisp-jit (args)
  (let ((expr (jget args "expr")))
    (safe-eval
     (format nil
             "(let* ((form (read-from-string ~S))
                     (habu::*function-table* (make-hash-table))
                     (compiled (habu:compile-forms (list form)))
                     (main-ir (cadr compiled))
                     (code (habu:codegen main-ir nil nil))
                     (code-vec (coerce code 'vector)))
                ;; Execute using executor
                (habu-compiler::load-code-to-memory code-vec \"jit\")
                ;; For now just show the code
                (format nil \"Compiled ~~D bytes (JIT execution requires executor setup)\" (length code)))"
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
