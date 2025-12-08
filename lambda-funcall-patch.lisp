;;; Lambda and Funcall Implementation for h0-compile
;;; This file contains the implementation to be integrated into habu0.lisp

;;; ==================================================================
;;; PART 1: IR Tags (add after ir-tag-keywordp)
;;; ==================================================================

(defun ir-tag-lambda () #x22)     ; lambda (closure creation)
(defun ir-tag-funcall () #x23)    ; funcall (closure invocation)

;;; ==================================================================
;;; PART 2: Free Variable Analysis (add before h0-compile)
;;; ==================================================================

;; Check if a symbol is in the environment (string-based lookup)
(defun h0-in-env (sym env)
  (if (null env)
      nil
      (if (string= (symbol-name sym) (car (car env)))
          t
          (h0-in-env sym (cdr env)))))

;; Check if a symbol is in a list (using string= on symbol names)
(defun h0-member-sym (sym lst)
  (if (null lst)
      nil
      (if (string= (symbol-name sym) (symbol-name (car lst)))
          t
          (h0-member-sym sym (cdr lst)))))

;; Add symbol to list if not already present (using string comparison)
(defun h0-add-free (sym acc)
  (if (h0-member-sym sym acc)
      acc
      (cons sym acc)))

;; Collect free variables from expression
(defun h0-collect-free (expr bound env acc)
  (cond
    ((null expr) acc)
    ;; Symbol - check if it's a free variable
    ((symbolp expr)
     (if (and (h0-in-env expr env)
              (not (h0-member-sym expr bound)))
         (h0-add-free expr acc)
         acc))
    ;; Not a list - done
    ((not (consp expr)) acc)
    ;; Quote - don't descend
    ((sym= (car expr) "QUOTE") acc)
    ;; Lambda - params bind, recurse on body
    ((sym= (car expr) "LAMBDA")
     (let ((params (cadr expr))
           (body (caddr expr)))
       (h0-collect-free body (h0-append-lists params bound) env acc)))
    ;; Let - values use current bound, body uses extended bound
    ((sym= (car expr) "LET")
     (let* ((bindings (cadr expr))
            (body (caddr expr))
            (names (h0-binding-names bindings))
            (vals (h0-binding-vals bindings))
            (acc2 (h0-collect-free-list vals bound env acc))
            (new-bound (h0-append-lists names bound)))
       (h0-collect-free body new-bound env acc2)))
    ;; Let* - sequential binding
    ((sym= (car expr) "LET*")
     (let* ((bindings (cadr expr))
            (body (caddr expr)))
       (h0-collect-free-let* bindings body bound env acc)))
    ;; Default - recurse on all subforms
    (t (h0-collect-free-list expr bound env acc))))

;; Collect from list of expressions
(defun h0-collect-free-list (exprs bound env acc)
  (if (null exprs)
      acc
      (let ((acc2 (h0-collect-free (car exprs) bound env acc)))
        (h0-collect-free-list (cdr exprs) bound env acc2))))

;; Handle let* sequential bindings
(defun h0-collect-free-let* (bindings body bound env acc)
  (if (null bindings)
      (h0-collect-free body bound env acc)
      (let* ((b (car bindings))
             (name (car b))
             (val (cadr b))
             (acc2 (h0-collect-free val bound env acc))
             (new-bound (cons name bound)))
        (h0-collect-free-let* (cdr bindings) body new-bound env acc2))))

;; Extract names from let bindings
(defun h0-binding-names (bindings)
  (if (null bindings)
      nil
      (cons (car (car bindings))
            (h0-binding-names (cdr bindings)))))

;; Extract values from let bindings
(defun h0-binding-vals (bindings)
  (if (null bindings)
      nil
      (cons (cadr (car bindings))
            (h0-binding-vals (cdr bindings)))))

;; Append two lists
(defun h0-append-lists (a b)
  (if (null a)
      b
      (cons (car a) (h0-append-lists (cdr a) b))))

;; Find free variables in expression
(defun h0-find-free-vars (expr bound env)
  (h0-collect-free expr bound env nil))

;; Get environment offset for a variable (by symbol)
(defun h0-get-var-offset (sym env)
  (if (null env)
      nil
      (if (string= (symbol-name sym) (car (car env)))
          #x0
          (let ((rest-off (h0-get-var-offset sym (cdr env))))
            (if rest-off
                (+ rest-off #x1)
                nil)))))

;; Get offsets for list of free variables
(defun h0-get-free-offsets (free-vars env)
  (if (null free-vars)
      nil
      (cons (h0-get-var-offset (car free-vars) env)
            (h0-get-free-offsets (cdr free-vars) env))))

;; Build environment for lambda body
;; Params are at offsets 0..n-1, free vars at n..n+m-1
(defun h0-make-param-env (params free-vars)
  (h0-make-env-with-offset params #x0
    (h0-make-env-with-offset free-vars (h0-list-length params) nil)))

;; Create environment entries with offsets starting at base
(defun h0-make-env-with-offset (syms base rest)
  (if (null syms)
      rest
      (cons (cons (symbol-name (car syms)) nil)
            (h0-make-env-with-offset (cdr syms) (+ base #x1) rest))))

;; Get length of a list
(defun h0-list-length (lst)
  (if (null lst)
      #x0
      (+ #x1 (h0-list-length (cdr lst)))))

;; Compile list of arguments to IR
(defun h0-compile-args (args env fenv)
  (if (null args)
      nil
      (cons (h0-compile (car args) env fenv)
            (h0-compile-args (cdr args) env fenv))))

;;; ==================================================================
;;; PART 3: Compilation Cases (add in h0-compile before default case)
;;; ==================================================================

;; Add these cases before the "Default - unknown operator" case:

         ;; LAMBDA - create closure
         ((sym= op "LAMBDA")
          (let* ((params (cadr expr))
                 (body (caddr expr))
                 ;; Find free variables
                 (free-vars (h0-find-free-vars body params env))
                 ;; Get offsets for free variables in current env
                 (free-offsets (h0-get-free-offsets free-vars env))
                 ;; Build environment for body: params come first, then free vars
                 (param-env (h0-make-param-env params free-vars))
                 ;; Compile body with new environment
                 (body-ir (h0-compile body param-env fenv)))
            ;; IR format: (tag params body-ir free-vars free-offsets)
            (list (ir-tag-lambda) params body-ir free-vars free-offsets)))
         ;; FUNCALL - call function value
         ((sym= op "FUNCALL")
          (let* ((fn-ir (h0-compile (cadr expr) env fenv))
                 (args (cddr expr))
                 (args-ir (h0-compile-args args env fenv)))
            ;; IR format: (tag fn-ir args-ir-list)
            (list (ir-tag-funcall) fn-ir args-ir)))

;;; ==================================================================
;;; PART 4: Code Generation (add in h0-codegen before default case)
;;; ==================================================================

;; Add these cases before the "Default - CRASH: unknown IR tag" case:

    ;; LAMBDA: Create closure on heap
    ;; Closure format: [num-free:8][code-ptr:8][captured-vals...]
    ;; Returns tagged pointer with tag 5 (function tag)
    ((h0-has-tag-n ir (ir-tag-lambda))
     (let* ((params (cadr ir))
            (body-ir (caddr ir))
            (free-vars (cadddr ir))
            (free-offsets (nth #x4 ir))
            (num-free (h0-list-length free-vars)))
       ;; For now: minimal stub implementation
       ;; Full implementation requires:
       ;; 1. Lambda lifting (extract lambda to top-level function)
       ;; 2. Generate code for the lambda body
       ;; 3. Allocate closure on heap with captured values
       ;; 4. Store function pointer and captured values
       ;; 5. Return tagged pointer to closure
       (fatal-error "h0-codegen: LAMBDA not yet fully implemented in codegen")))

    ;; FUNCALL: Call a closure
    ;; Closure format: [num-free:8][code-ptr:8][captured-vals...]
    ((h0-has-tag-n ir (ir-tag-funcall))
     (let* ((fn-ir (cadr ir))
            (args-ir (caddr ir)))
       ;; For now: minimal stub implementation
       ;; Full implementation requires:
       ;; 1. Evaluate closure expression to get tagged pointer
       ;; 2. Untag pointer to get closure struct
       ;; 3. Extract code pointer from [offset 8]
       ;; 4. Extract captured values from [offset 16+]
       ;; 5. Evaluate arguments
       ;; 6. Set up stack frame with captured values + arguments
       ;; 7. Call the code pointer
       ;; 8. Restore stack frame
       (fatal-error "h0-codegen: FUNCALL not yet fully implemented in codegen")))

;;; ==================================================================
;;; IMPLEMENTATION NOTES
;;; ==================================================================

;;; This implementation provides:
;;; 1. IR representation for lambda and funcall
;;; 2. Free variable detection
;;; 3. Closure IR construction with captured variable tracking
;;; 4. Compilation infrastructure
;;;
;;; What remains for full implementation:
;;; 1. Lambda lifting - extract lambda bodies to top-level functions
;;; 2. Code generation for lambda bodies
;;; 3. Heap allocation for closures
;;; 4. Runtime calling convention for closures
;;; 5. Stack frame management for captured values
;;;
;;; The stub implementations will error at codegen time, which is
;;; appropriate since full closure support requires significant
;;; additional infrastructure (lambda lifting, calling conventions, etc.)
