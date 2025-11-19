;;;; c-backend.lisp - Generate C code from Habu expressions
;;;;
;;;; This is a temporary backend for testing standalone mode.
;;;; It generates C code that calls the Habu runtime, which can then
;;;; be compiled and linked with the runtime.o files.

(in-package :habu-compiler)

(defvar *lambda-counter* 0
  "Counter for generating unique lambda function names")

(defvar *generated-lambdas* nil
  "List of generated lambda functions: ((name params body) ...)")

(defun reset-lambda-state ()
  "Reset lambda generation state"
  (setf *lambda-counter* 0)
  (setf *generated-lambdas* nil))

(defparameter *builtin-operators*
  '(+ - * / = < > <= >= cons car cdr list if cond progn begin setq while
    quote make-vector vector-ref vector-set print read-file write-file funcall
    print-value println string-length
    make-string-from-cstr string-ref fgets-line readline
    fixnum? cons? string? nil? symbol? vector?
    string=? make-symbol symbol-name symbol=?)
  "Built-in operators that should not be treated as free variables")

(defun free-variables (expr bound-vars)
  "Find free variables in an expression (variables not in bound-vars)"
  (cond
    ;; Variable reference
    ((symbolp expr)
     (if (or (member expr bound-vars)
             (member expr *builtin-operators*))
         nil
         (list expr)))

    ;; Lambda - parameters become bound
    ((and (consp expr) (eq (car expr) 'lambda))
     (let ((params (second expr))
           (body (third expr)))
       (free-variables body (append params bound-vars))))

    ;; Let - bindings become bound after evaluation
    ((and (consp expr) (eq (car expr) 'let))
     (let* ((bindings (second expr))
            (body (third expr))
            (binding-vars (mapcar #'first bindings))
            (binding-exprs (mapcar #'second bindings)))
       ;; Free vars in binding expressions (before vars are bound)
       (let ((binding-free (apply #'append
                                  (mapcar (lambda (e) (free-variables e bound-vars))
                                          binding-exprs))))
         ;; Free vars in body (with new bindings)
         (let ((body-free (free-variables body (append binding-vars bound-vars))))
           (remove-duplicates (append binding-free body-free))))))

    ;; Quoted expression - no free variables
    ((and (consp expr) (eq (car expr) 'quote))
     nil)

    ;; Compound expression - recurse
    ((consp expr)
     (remove-duplicates
      (apply #'append
             (mapcar (lambda (sub) (free-variables sub bound-vars)) expr))))

    ;; Base case: atom (number, string, etc.)
    (t nil)))

(defun discover-lambdas (expr)
  "Walk expression tree and record all lambda expressions for later generation.
   This must be called before habu-expr-to-c to ensure lambda numbering is consistent."
  (cond
    ;; Lambda - record it and process body
    ((and (consp expr) (eq (car expr) 'lambda))
     (let* ((params (second expr))
            (body (third expr))
            (lambda-name (format nil "lambda_~D" (incf *lambda-counter*)))
            ;; Find captured variables (free vars in body not including params)
            (captured (free-variables body params)))
       ;; Record lambda with captured variables
       (push (list lambda-name params body captured) *generated-lambdas*)
       ;; Recursively discover nested lambdas in body
       (discover-lambdas body)))

    ;; Recurse into compound expressions
    ((consp expr)
     (dolist (sub expr)
       (discover-lambdas sub)))

    ;; Base case: atoms, do nothing
    (t nil)))

(defun sanitize-c-name (symbol)
  "Convert a Lisp symbol name to a valid C identifier"
  (let ((name (if (symbolp symbol)
                  (symbol-name symbol)
                  (string symbol))))
    ;; Replace special characters: - becomes _, ? becomes _P, = becomes _EQ
    (with-output-to-string (out)
      (loop for ch across name
            do (cond ((char= ch #\-) (write-char #\_ out))
                     ((char= ch #\?) (write-string "_P" out))
                     ((char= ch #\=) (write-string "_EQ" out))
                     ((char= ch #\+) (write-string "_PLUS" out))
                     ((char= ch #\*) (write-string "_STAR" out))
                     ((char= ch #\/) (write-string "_SLASH" out))
                     ((char= ch #\<) (write-string "_LT" out))
                     ((char= ch #\>) (write-string "_GT" out))
                     ((char= ch #\!) (write-string "_BANG" out))
                     (t (write-char ch out)))))))

(defun habu-expr-to-c (expr &optional (indent 0))
  "Convert a Habu expression to C code"
  (let ((ind (make-string indent :initial-element #\Space)))
    (cond
      ;; Quoted fixnum
      ((and (consp expr) (eq (car expr) 'quote) (integerp (cadr expr)))
       (format nil "fixnum_to_value(~D)" (cadr expr)))

      ;; Quoted string
      ((and (consp expr) (eq (car expr) 'quote) (stringp (cadr expr)))
       (format nil "habu_make_string(~S, ~D)" (cadr expr) (length (cadr expr))))

      ;; Cons
      ((and (consp expr) (eq (car expr) 'cons))
       (format nil "habu_cons(~A, ~A)"
               (habu-expr-to-c (second expr) indent)
               (habu-expr-to-c (third expr) indent)))

      ;; Car
      ((and (consp expr) (eq (car expr) 'car))
       (format nil "habu_car(~A)"
               (habu-expr-to-c (second expr) indent)))

      ;; Cdr
      ((and (consp expr) (eq (car expr) 'cdr))
       (format nil "habu_cdr(~A)"
               (habu-expr-to-c (second expr) indent)))

      ;; List
      ((and (consp expr) (eq (car expr) 'list))
       (if (null (cdr expr))
           "NIL"
           ;; Build list as nested cons calls: (list 1 2 3) => (cons 1 (cons 2 (cons 3 nil)))
           (let ((elements (cdr expr)))
             (labels ((build-list (elems)
                        (if (null elems)
                            "NIL"
                            (format nil "habu_cons(~A, ~A)"
                                    (habu-expr-to-c (car elems) indent)
                                    (build-list (cdr elems))))))
               (build-list elements)))))

      ;; If
      ((and (consp expr) (eq (car expr) 'if))
       (let ((cond-expr (second expr))
             (then-expr (third expr))
             (else-expr (fourth expr)))
         (format nil "(is_nil(~A) ? ~A : ~A)"
                 (habu-expr-to-c cond-expr indent)
                 (if else-expr
                     (habu-expr-to-c else-expr indent)
                     "NIL")
                 (habu-expr-to-c then-expr indent))))

      ;; Cond
      ((and (consp expr) (eq (car expr) 'cond))
       (let ((clauses (cdr expr)))
         (labels ((emit-cond (clauses)
                    (if (null clauses)
                        "NIL"
                        (let* ((clause (car clauses))
                               (test (car clause))
                               (body (cadr clause)))
                          (if (eq test 't)
                              ;; Final else clause
                              (habu-expr-to-c body indent)
                              ;; Conditional clause
                              (format nil "(is_nil(~A) ? ~A : ~A)"
                                      (habu-expr-to-c test indent)
                                      (emit-cond (cdr clauses))
                                      (habu-expr-to-c body indent)))))))
           (emit-cond clauses))))

      ;; Comparison operators
      ((and (consp expr) (member (car expr) '(= < > <= >=)))
       (let ((op (ecase (car expr)
                   (= "==")
                   (< "<")
                   (> ">")
                   (<= "<=")
                   (>= ">="))))
         (format nil "(value_to_fixnum(~A) ~A value_to_fixnum(~A) ? fixnum_to_value(1) : NIL)"
                 (habu-expr-to-c (second expr) indent)
                 op
                 (habu-expr-to-c (third expr) indent))))

      ;; Nil/null
      ((and (consp expr) (eq (car expr) 'quote) (null (cadr expr)))
       "NIL")

      ;; Arithmetic
      ((and (consp expr) (member (car expr) '(+ - * /)))
       (let ((op (ecase (car expr)
                   (+ "+")
                   (- "-")
                   (* "*")
                   (/ "/"))))
         (format nil "fixnum_to_value(value_to_fixnum(~A) ~A value_to_fixnum(~A))"
                 (habu-expr-to-c (second expr) indent)
                 op
                 (habu-expr-to-c (third expr) indent))))

      ;; Print (without newline)
      ((and (consp expr) (eq (car expr) 'print))
       (format nil "habu_print_value(~A)"
               (habu-expr-to-c (second expr) indent)))

      ;; Vector operations
      ((and (consp expr) (eq (car expr) 'make-vector))
       (format nil "habu_make_vector(~A)"
               (habu-expr-to-c (second expr) indent)))

      ((and (consp expr) (eq (car expr) 'vector-ref))
       (format nil "habu_vector_ref(~A, value_to_fixnum(~A))"
               (habu-expr-to-c (second expr) indent)
               (habu-expr-to-c (third expr) indent)))

      ((and (consp expr) (eq (car expr) 'vector-set))
       (format nil "({habu_vector_set(~A, value_to_fixnum(~A), ~A); NIL;})"
               (habu-expr-to-c (second expr) indent)
               (habu-expr-to-c (third expr) indent)
               (habu-expr-to-c (fourth expr) indent)))

      ;; Read-file
      ((and (consp expr) (eq (car expr) 'read-file))
       (format nil "habu_read_file(~A)"
               (habu-expr-to-c (second expr) indent)))

      ;; Write-file
      ((and (consp expr) (eq (car expr) 'write-file))
       (format nil "habu_write_file(~A, ~A)"
               (habu-expr-to-c (second expr) indent)
               (habu-expr-to-c (third expr) indent)))

      ;; Readline - takes a string, returns char* (as habu_value_t for NULL check)
      ((and (consp expr) (eq (car expr) 'readline))
       (let ((prompt-arg (second expr)))
         (cond
           ;; Direct string literal
           ((stringp prompt-arg)
            (format nil "(habu_value_t)lineedit_readline(~S)" prompt-arg))
           ;; Quoted string literal
           ((and (consp prompt-arg) (eq (car prompt-arg) 'quote) (stringp (cadr prompt-arg)))
            (format nil "(habu_value_t)lineedit_readline(~S)" (cadr prompt-arg)))
           ;; General expression - extract C string from habu string
           (t
            (format nil "({habu_value_t str = ~A; (habu_value_t)lineedit_readline(habu_string_to_cstr(str));})"
                    (habu-expr-to-c prompt-arg indent))))))

      ;; Print-value
      ((and (consp expr) (eq (car expr) 'print-value))
       (format nil "habu_print_value(~A)"
               (habu-expr-to-c (second expr) indent)))

      ;; Println (just print newline)
      ((and (consp expr) (eq (car expr) 'println))
       (format nil "({printf(\"\\n\"); NIL;})"))

      ;; String-length (C string from char*)
      ((and (consp expr) (eq (car expr) 'string-length))
       (format nil "fixnum_to_value((char*)~A ? strlen((char*)~A) : 0)"
               (habu-expr-to-c (second expr) indent)
               (habu-expr-to-c (second expr) indent)))

      ;; Make-string-from-cstr (char* to habu string)
      ((and (consp expr) (eq (car expr) 'make-string-from-cstr))
       (format nil "({char* s = (char*)~A; s ? habu_make_string(s, strlen(s)) : NIL;})"
               (habu-expr-to-c (second expr) indent)))

      ;; String-ref (get character at index, returns fixnum)
      ((and (consp expr) (eq (car expr) 'string-ref))
       (format nil "habu_string_ref(~A, value_to_fixnum(~A))"
               (habu-expr-to-c (second expr) indent)
               (habu-expr-to-c (third expr) indent)))

      ;; String-length-raw (get length of Habu string object)
      ((and (consp expr) (eq (car expr) 'string-length-raw))
       (format nil "fixnum_to_value(habu_string_length_raw(~A))"
               (habu-expr-to-c (second expr) indent)))

      ;; Fgets-line (read line from stdin, returns char* as habu_value_t)
      ((and (consp expr) (eq (car expr) 'fgets-line))
       (format nil "(habu_value_t)habu_fgets_line()"))

      ;; Readline (line editing with prompt, returns char* as habu_value_t)
      ((and (consp expr) (eq (car expr) 'readline))
       (let ((prompt-arg (second expr)))
         (cond
           ;; Direct string literal
           ((stringp prompt-arg)
            (format nil "(habu_value_t)lineedit_readline(~S)" prompt-arg))
           ;; Quoted string literal
           ((and (consp prompt-arg) (eq (car prompt-arg) 'quote) (stringp (cadr prompt-arg)))
            (format nil "(habu_value_t)lineedit_readline(~S)" (cadr prompt-arg)))
           ;; General expression - extract C string from habu string
           (t
            (format nil "({habu_value_t str = ~A; (habu_value_t)lineedit_readline(habu_string_to_cstr(str));})"
                    (habu-expr-to-c prompt-arg indent))))))

      ;; Get-tag (fundamental primitive for type checking)
      ((and (consp expr) (eq (car expr) 'get-tag))
       (format nil "habu_get_tag(~A)"
               (habu-expr-to-c (second expr) indent)))

      ;; Symbol operations
      ((and (consp expr) (eq (car expr) 'make-symbol))
       (format nil "habu_make_symbol_from_string(~A)"
               (habu-expr-to-c (second expr) indent)))

      ((and (consp expr) (eq (car expr) 'symbol-name))
       (format nil "habu_symbol_name(~A)"
               (habu-expr-to-c (second expr) indent)))

      ;; Let bindings
      ((and (consp expr) (eq (car expr) 'let))
       (let* ((bindings (second expr))
              (body (third expr))
              (ind (make-string indent :initial-element #\Space)))
         (with-output-to-string (s)
           (format s "({~%")  ; GCC statement expression
           (dolist (binding bindings)
             (format s "~Ahabu_value_t ~A = ~A;~%"
                     ind
                     (sanitize-c-name (first binding))
                     (habu-expr-to-c (second binding) (+ indent 2))))
           (format s "~A~A;~%"
                   ind
                   (habu-expr-to-c body (+ indent 2)))
           (format s "~A})" ind))))

      ;; Progn/begin - sequential evaluation
      ((and (consp expr) (member (car expr) '(progn begin)))
       (let ((exprs (cdr expr)))
         (if (null exprs)
             "NIL"
             (with-output-to-string (s)
               (format s "({~%")
               (loop for e in (butlast exprs)
                     do (format s "  ~A;~%" (habu-expr-to-c e (+ indent 2))))
               (format s "  ~A;~%})"
                       (habu-expr-to-c (car (last exprs)) (+ indent 2)))))))

      ;; Setq - variable mutation
      ((and (consp expr) (eq (car expr) 'setq))
       (let ((var (second expr))
             (value (third expr)))
         (format nil "(~A = ~A)"
                 (sanitize-c-name var)
                 (habu-expr-to-c value indent))))

      ;; While loop
      ((and (consp expr) (eq (car expr) 'while))
       (let ((condition (second expr))
             (body (third expr)))
         (with-output-to-string (s)
           (format s "({~%")
           (format s "  while (!is_nil(~A)) {~%"
                   (habu-expr-to-c condition (+ indent 4)))
           (format s "    ~A;~%"
                   (habu-expr-to-c body (+ indent 6)))
           (format s "  }~%")
           (format s "  NIL;~%")
           (format s "})"))))

      ;; Variable reference (bare symbol)
      ((symbolp expr)
       (sanitize-c-name expr))

      ;; String literals (unquoted strings)
      ((stringp expr)
       ;; Need to generate habu string from C string literal
       (format nil "habu_make_string(~S, ~D)" expr (length expr)))

      ;; Lambda - generate closure
      ((and (consp expr) (eq (car expr) 'lambda))
       ;; Lambda should already be discovered by discover-lambdas pass
       ;; Just generate the closure creation with the right lambda number
       (let* ((lambda-num (incf *lambda-counter*))
              (lambda-name (format nil "lambda_~D" lambda-num))
              ;; Find this lambda's spec to get captured variables
              (lambda-spec (find-if (lambda (spec) (string= (first spec) lambda-name))
                                   *generated-lambdas*))
              (captured (when lambda-spec (fourth lambda-spec))))
         (if (null captured)
             ;; No captures - use NIL environment
             (format nil "habu_make_closure((void*)~A, NIL)" lambda-name)
             ;; Has captures - create environment vector
             (with-output-to-string (s)
               (format s "({~%")
               (format s "  habu_value_t env = habu_make_vector(fixnum_to_value(~D));~%" (length captured))
               (loop for var in captured
                     for i from 0
                     do (format s "  habu_vector_set(env, ~D, ~A);~%" i (sanitize-c-name var)))
               (format s "  habu_make_closure((void*)~A, env);~%})" lambda-name)))))

      ;; Funcall - call a closure
      ((and (consp expr) (eq (car expr) 'funcall))
       (let* ((fn-expr (second expr))
              (args (cddr expr))
              (arg-count (length args))
              ;; Generate typedef signature
              (typedef-params (with-output-to-string (sig)
                                (dotimes (i arg-count)
                                  (when (> i 0) (format sig ", "))
                                  (format sig "habu_value_t"))
                                (when (> arg-count 0) (format sig ", "))
                                (format sig "habu_value_t"))))  ; env parameter
         (with-output-to-string (s)
           (format s "({~%")
           (format s "  typedef habu_value_t (*closure_fn_t)(~A);~%" typedef-params)
           (format s "  habu_value_t fn = ~A;~%" (habu-expr-to-c fn-expr indent))
           (format s "  closure_fn_t code = (closure_fn_t)habu_closure_code(fn);~%")
           (format s "  habu_value_t env = habu_closure_env(fn);~%")
           (format s "  code(~{~A~^, ~});~%"
                   (append (mapcar (lambda (arg) (habu-expr-to-c arg indent)) args)
                           (list "env")))
           (format s "})"))))

      ;; Function call (unknown function) - fallback for user-defined functions
      ((and (consp expr) (symbolp (car expr)))
       ;; Generic function call
       (let ((fn-name (sanitize-c-name (car expr)))
             (args (cdr expr)))
         (format nil "~A(~{~A~^, ~})"
                 fn-name
                 (mapcar (lambda (arg) (habu-expr-to-c arg indent)) args))))

      (t
       (error "Unsupported expression for C backend: ~S" expr)))))

(defun extract-defuns (expr)
  "Extract all defun forms from an expression, return (functions . body)"
  (let ((functions nil)
        (body nil))
    (labels ((process (e)
               (cond
                 ((and (consp e) (eq (car e) 'defun))
                  (push e functions))
                 ((and (consp e) (member (car e) '(progn begin)))
                  (dolist (sub (cdr e))
                    (process sub)))
                 (t
                  (setf body e)))))
      (process expr))
    (cons (nreverse functions) body)))

(defun generate-c-function (defun-expr)
  "Generate a C function from a defun expression"
  (let* ((name (second defun-expr))
         (params (third defun-expr))
         (body (fourth defun-expr)))
    (with-output-to-string (s)
      (format s "habu_value_t ~A(" (sanitize-c-name name))
      (loop for param in params
            for i from 0
            do (when (> i 0) (format s ", "))
            do (format s "habu_value_t ~A" (sanitize-c-name param)))
      (format s ") {~%")
      (format s "    return ~A;~%" (habu-expr-to-c body 4))
      (format s "}~%"))))

(defun generate-c-lambda (lambda-spec)
  "Generate a C function from a lambda specification (name params body captured)
   Lambda functions take an additional env parameter at the end"
  (let* ((name (first lambda-spec))
         (params (second lambda-spec))
         (body (third lambda-spec))
         (captured (fourth lambda-spec)))
    (with-output-to-string (s)
      (format s "habu_value_t ~A(" name)
      (loop for param in params
            for i from 0
            do (when (> i 0) (format s ", "))
            do (format s "habu_value_t ~A" (sanitize-c-name param)))
      ;; Add environment parameter
      (when params (format s ", "))
      (format s "habu_value_t env) {~%")

      ;; Extract captured variables from environment
      (when captured
        (loop for var in captured
              for i from 0
              do (format s "    habu_value_t ~A = habu_vector_ref(env, ~D);~%"
                         (sanitize-c-name var) i)))

      (format s "    return ~A;~%" (habu-expr-to-c body 4))
      (format s "}~%"))))

(defun generate-c-standalone (expr &key (output-file "habu_generated.c"))
  "Generate a standalone C program from a Habu expression"
  ;; Reset lambda state
  (reset-lambda-state)

  (let* ((defuns-and-body (extract-defuns expr))
         (functions (car defuns-and-body))
         (body (cdr defuns-and-body)))

    ;; First pass: discover all lambdas (including nested ones)
    (when body
      (discover-lambdas body))

    ;; Get collected lambdas (in reverse order, so reverse them back)
    (let ((lambdas (nreverse *generated-lambdas*)))

      ;; Reset counter for code generation pass (must match discovery order)
      (setf *lambda-counter* 0)

      ;; Second pass: generate C expression
      (let ((c-expr (if body (habu-expr-to-c body) "NIL")))

    (with-open-file (out output-file
                         :direction :output
                         :if-exists :supersede)
      (format out "#include \"habu.h\"~%")
      (format out "#include \"object.h\"~%")
      (format out "#include <stdio.h>~%")
      (format out "#include <string.h>~%")
      (format out "#include <stdlib.h>~%")
      (format out "~%")

      ;; Forward declarations for defuns
      (dolist (fn functions)
        (let ((name (second fn))
              (params (third fn)))
          (format out "habu_value_t ~A(" (sanitize-c-name name))
          (loop for param in params
                for i from 0
                do (when (> i 0) (format out ", "))
                do (format out "habu_value_t"))
          (format out ");~%")))
      (when functions (format out "~%"))

      ;; Forward declarations for lambdas
      (dolist (lambda-spec lambdas)
        (let ((name (first lambda-spec))
              (params (second lambda-spec)))
          (format out "habu_value_t ~A(" name)
          (loop for param in params
                for i from 0
                do (when (> i 0) (format out ", "))
                do (format out "habu_value_t"))
          (when params (format out ", "))
          (format out "habu_value_t);~%")))
      (when lambdas (format out "~%"))

      ;; Function definitions for defuns
      (dolist (fn functions)
        (format out "~A~%" (generate-c-function fn)))

      ;; Function definitions for lambdas
      (dolist (lambda-spec lambdas)
        (format out "~A~%" (generate-c-lambda lambda-spec)))

      ;; Main function
      (format out "int main(void) {~%")
      (format out "    habu_init(4 * 1024 * 1024);~%")
      (format out "    ~%")
      (format out "    habu_value_t result = ~A;~%"  c-expr)
      (format out "    ~%")
      (format out "    if (is_fixnum(result)) {~%")
      (format out "        printf(\"Result: %lld\\n\", (long long)value_to_fixnum(result));~%")
      (format out "    } else {~%")
      (format out "        printf(\"Result: 0x%llx (tagged pointer)\\n\", (unsigned long long)result);~%")
      (format out "    }~%")
      (format out "    ~%")
      (format out "    habu_shutdown();~%")
      (format out "    return 0;~%")
      (format out "}~%"))

        (format t "~%Generated C code: ~A~%" output-file)
        output-file))))

(defun compile-and-run-c (c-file)
  "Compile C file with runtime and run it"
  (let ((exe-file (format nil "~A.out" (pathname-name c-file))))
    (format t "~%Compiling ~A...~%" c-file)

    ;; Compile with runtime
    (let ((compile-result
           #+sbcl
           (sb-ext:run-program
            "/usr/bin/clang"
            (list "-O2" "-I" "runtime"
                  c-file
                  "runtime/gc.o"
                  "runtime/runtime.o"
                  "runtime/region.o"
                  "runtime/io.o"
                  "runtime/reader.o"
                  "-o" exe-file)
            :output t
            :error t)))

      (if (zerop (sb-ext:process-exit-code compile-result))
          (progn
            (format t "~%Running ~A...~%" exe-file)
            #+sbcl
            (sb-ext:run-program (format nil "./~A" exe-file) '()
                               :output t
                               :error t)
            exe-file)
          (error "Compilation failed")))))
