;;;; SBCL-only loader stubs for Habu codegen (keeps main file standalone)
;;;; Do NOT use in production; only for bring-up/testing in SBCL host

(defpackage :habu-sbcl-codegen
  (:use :cl :habu-shim)
  (:export codegen-expr compile-expr compile-to-arm64-with-runtime compile-to-arm64
           make-runtime-addrs runtime-lookup *runtime-addrs*
           compile-program-with-functions-with-runtime compile-program-with-functions
           env-lookup env-extend compile-forms))

(in-package :habu-sbcl-codegen)

(defparameter *runtime-addrs* nil)
(defparameter *collected-lambdas* nil)
(defparameter *macro-env* nil
  "Alist of (macro-name . expander-function) for compile-time macros")
(defparameter *block-counter* 0
  "Counter for generating unique block IDs")
(defparameter *block-env* nil
  "Alist of (block-name . block-id) for tracking active blocks during compilation")
(defparameter *catch-counter* 0
  "Counter for generating unique catch IDs")
(defparameter *catch-env* nil
  "Alist of (tag-expr . (id result-var exited-var)) for tracking active catch blocks")
(defparameter *struct-accessors* nil
  "Alist of (accessor-name . slot-index) for struct slot accessors")
(defparameter *handler-counter* 0
  "Counter for generating unique handler IDs")
(defparameter *handler-env* nil
  "List of handler-case entries for signal to find")
(defparameter *restart-counter* 0
  "Counter for generating unique restart IDs")
(defparameter *restart-env* nil
  "List of restart-case entries for invoke-restart to find")
(defparameter *class-env* nil
  "Alist of (class-name parent-class slot-names slot-initforms) for CLOS classes")
(defparameter *method-env* nil
  "Alist of ((generic-name . specializer) . lambda-form) for methods")
(defparameter *stack-frame-size* #xFF0)
(defparameter *env-base-offset* #x180)
(defparameter *temp-slot-base* #x40)
(defparameter *temp-slot-guard* #x180)
(defparameter *arg-spill-base* #x200)
(defparameter *arg-spill-stride* #x8)
(defparameter *max-arg-spill-count*
  (/ (- *stack-frame-size* *arg-spill-base*) *arg-spill-stride*))

(defun string->char-codes (s)
  "Return list of integer char codes from string S."
  (loop for ch across s collect (char-code ch)))

(defun destructuring-bind-expand (pattern value-form)
  "Expand destructuring-bind pattern into a list of (var expr) bindings.
   PATTERN can be a symbol, or a list of patterns.
   VALUE-FORM is the expression holding the value being matched."
  (cond
    ;; Simple variable binding
    ((symbolp pattern)
     (if (eq pattern '&rest)
         nil  ; Skip &rest keyword itself
         (list (list pattern value-form))))
    ;; Nested pattern (a . b) or (a b c ...)
    ((consp pattern)
     (let ((bindings '())
           (rest-pattern nil)
           (pos 0))
       ;; Handle &rest in pattern
       (let ((rest-pos (position '&rest pattern)))
         (when rest-pos
           (setf rest-pattern (nth (1+ rest-pos) pattern))
           (setf pattern (subseq pattern 0 rest-pos))))
       ;; Generate bindings for each element
       (dolist (subpat pattern)
         (let ((accessor (case pos
                           (0 `(car ,value-form))
                           (1 `(cadr ,value-form))
                           (2 `(caddr ,value-form))
                           (3 `(cadddr ,value-form))
                           (t `(nth ,pos ,value-form)))))
           (setf bindings (append bindings (destructuring-bind-expand subpat accessor))))
         (incf pos))
       ;; Handle &rest binding
       (when rest-pattern
         (let ((rest-accessor (case pos
                                (0 value-form)
                                (1 `(cdr ,value-form))
                                (2 `(cddr ,value-form))
                                (3 `(cdddr ,value-form))
                                (t `(nthcdr ,pos ,value-form)))))
           (setf bindings (append bindings (destructuring-bind-expand rest-pattern rest-accessor)))))
       bindings))
    ;; NIL or other - no bindings
    (t nil)))

(defun quote->ir (obj)
  "Lower a quoted object to IR using cons/runtime construction.
Supports fixnums, nil, lists, symbols, strings, characters, and vectors of those."
  (cond
    ((fixnum? obj) (list 'lit obj))
    ((null obj) (list 'lit #x0))
    ((characterp obj) (list 'lit (char-code obj)))  ; Characters -> fixnum codes
    ((stringp obj) (cons 'string-lit (string->char-codes obj)))
    ((symbolp obj) (list 'symbol-lit (symbol-name obj)))
    ((vectorp obj) (cons 'vector-lit (map 'list #'quote->ir obj)))
    ((consp obj) (list 'cons-call (quote->ir (car obj)) (quote->ir (cdr obj))))
    (t (list 'lit #x0))))

(defun codegen-string-from-chars (chars temp-depth)
  "Build string literal from CHAR integer list; returns code yielding string in x0."
  (let* ((len (length chars))
         (vec-slot (temp-slot-offset temp-depth))
         (alloc (append (arm64-movz 0 len)
                        (arm64-ldr 11 19 56)  ; make-vector
                        (arm64-blr 11)
                        (arm64-str 0 31 vec-slot)))
         (body alloc))
    (loop for ch in chars
          for idx from 0 do
            (let* ((tagged (ash ch 4))
                   (store (append
                            (arm64-ldr 0 31 vec-slot) ; x0 = vector
                            (arm64-movz 1 idx)        ; x1 = index
                            (if (< tagged #x10000)
                                (arm64-movz 2 tagged)
                                (arm64-load-addr 2 tagged))
                            (arm64-ldr 11 19 64)      ; vector-set
                            (arm64-blr 11))))
              (setf body (append body store))))
    (append body
            (arm64-ldr 0 31 vec-slot)   ; x0 = vector
            (arm64-ldr 9 19 80)         ; make-string-from-vector
            (arm64-blr 9))))

(defun codegen-vector-literal (elements runtime-addrs fn-offsets current-offset temp-depth)
  "Emit code for vector literal ELEMENTS (already IR), return vector in x0."
  (let* ((len (length elements))
         (vec-slot (temp-slot-offset temp-depth))
         (alloc (append (arm64-movz 0 len)
                        (arm64-ldr 11 19 56)
                        (arm64-blr 11)
                        (arm64-str 0 31 vec-slot)))
         (cursor (if current-offset (+ current-offset (count-instrs alloc)) nil))
         (body alloc))
    (loop for el in elements
          for idx from 0 do
            (let* ((el-code (codegen-expr el runtime-addrs fn-offsets cursor (+ temp-depth 1)))
                   (store (append
                            (arm64-mov 2 0)            ; value -> x2
                            (arm64-ldr 0 31 vec-slot)  ; x0 = vector
                            (arm64-movz 1 idx)         ; x1 = index
                            (arm64-ldr 11 19 64)       ; vector-set
                            (arm64-blr 11)))
                   (step (+ (count-instrs el-code) (count-instrs store))))
              (setf body (append body el-code store))
              (when cursor (setf cursor (+ cursor step)))))
    (append body (arm64-ldr 0 31 vec-slot))))

(defun collect-var-offsets (ir)
  "Collect all variable offsets referenced in IR. Uses hash set for O(N) dedup."
  (let ((seen (make-hash-table :test #'eql)))
    (labels ((collect (ir)
               (cond
                 ((null ir) nil)
                 ((has-tag? ir 'var)
                  (setf (gethash (cadr ir) seen) t))
                 ((has-tag? ir 'capture) nil)
                 ((consp ir)
                  (collect (car ir))
                  (collect (cdr ir)))
                 (t nil))))
      (collect ir)
      ;; Convert hash keys to list
      (let ((result nil))
        (maphash (lambda (k v) (declare (ignore v)) (push k result)) seen)
        result))))

(defun rewrite-captures (ir capture-map)
  "Rewrite IR var nodes whose offset is in capture-map to capture nodes."
  (cond
    ((null ir) nil)
    ((has-tag? ir 'var)
     (let* ((off (cadr ir))
            (entry (assoc off capture-map)))
       (if entry
           (list 'capture (cdr entry))
           ir)))
    ((consp ir) (cons (rewrite-captures (car ir) capture-map)
                      (rewrite-captures (cdr ir) capture-map)))
    (t ir)))

(defun find-mutated-vars (expr)
  "Find all variable names that are mutated (via setq) in EXPR.
   Returns a list of variable names (symbols)."
  (let ((result nil))
    (labels ((walk (e)
               (cond
                 ((atom e) nil)
                 ;; setq mutates its first argument
                 ((and (eq (car e) 'setq) (cdr e))
                  (push (cadr e) result)
                  (mapc #'walk (cddr e)))
                 ;; setf on a variable mutates it
                 ((and (eq (car e) 'setf) (cdr e) (symbolp (cadr e)))
                  (push (cadr e) result)
                  (mapc #'walk (cddr e)))
                 ;; incf/decf mutate their argument
                 ((and (member (car e) '(incf decf)) (cdr e) (symbolp (cadr e)))
                  (push (cadr e) result)
                  (mapc #'walk (cddr e)))
                 ;; Don't descend into nested let/labels that shadow the var
                 ;; For simplicity, we descend anyway - shadowed vars will be different
                 (t (mapc #'walk e)))))
      (walk expr)
      (remove-duplicates result :test #'eq))))

(defun find-captured-vars (expr)
  "Find all variable names that might be captured by closures in EXPR.
   Returns a list of variable names (symbols)."
  (let ((result nil))
    (labels ((walk (e)
               (cond
                 ((atom e) nil)
                 ;; Lambda captures free variables
                 ((eq (car e) 'lambda)
                  (let* ((params (cadr e))
                         (body (cddr e)))
                    ;; Find free vars in body that aren't in params
                    (let ((free (find-free-vars-in-body body params)))
                      (dolist (v free) (push v result)))
                    (mapc #'walk body)))
                 ;; Labels functions capture free variables
                 ((eq (car e) 'labels)
                  (let* ((defs (cadr e))
                         (body (cddr e)))
                    ;; Each labels function can capture vars
                    (dolist (def defs)
                      (let* ((fname (car def))
                             (fparams (cadr def))
                             (fbody (cddr def))
                             (all-fn-names (mapcar #'car defs))
                             (free (find-free-vars-in-body fbody
                                                           (append fparams all-fn-names))))
                        (dolist (v free) (push v result))))
                    (mapc #'walk body)))
                 ;; Flet is similar
                 ((eq (car e) 'flet)
                  (let* ((defs (cadr e))
                         (body (cddr e)))
                    (dolist (def defs)
                      (let* ((fparams (cadr def))
                             (fbody (cddr def))
                             (free (find-free-vars-in-body fbody fparams)))
                        (dolist (v free) (push v result))))
                    (mapc #'walk body)))
                 ;; Tagbody transforms to labels, so forms inside capture vars
                 ;; Skip tags (symbols/integers), collect free vars from forms
                 ((eq (car e) 'tagbody)
                  (let ((body (cdr e)))
                    ;; Find all free vars in non-tag forms
                    (dolist (item body)
                      (unless (or (symbolp item) (integerp item))
                        ;; This form will end up in a closure body
                        (let ((free (find-free-vars-in-body (list item) nil)))
                          (dolist (v free) (push v result)))
                        ;; Also recurse into the form
                        (walk item)))))
                 (t (mapc #'walk e)))))
      (walk expr)
      (remove-duplicates result :test #'eq))))

(defun find-free-vars-in-body (body bound-vars)
  "Find variables referenced in BODY that are not in BOUND-VARS.
   Returns a list of variable names (symbols)."
  (let ((result nil))
    (labels ((walk (e)
               (cond
                 ((null e) nil)
                 ((symbolp e)
                  (unless (or (member e bound-vars :test #'eq)
                              (keywordp e)
                              (eq e t)
                              (eq e nil))
                    (push e result)))
                 ((atom e) nil)
                 ;; Let/let* introduce new bindings
                 ((member (car e) '(let let*))
                  (let* ((bindings (cadr e))
                         (new-bound (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
                         (body-forms (cddr e)))
                    ;; Walk binding initializers
                    (dolist (b bindings)
                      (when (and (consp b) (cdr b))
                        (walk (cadr b))))
                    ;; Walk body with extended bound vars
                    (let ((bound-vars (append new-bound bound-vars)))
                      (dolist (form body-forms)
                        (walk form)))))
                 ;; Lambda introduces params as bound
                 ((eq (car e) 'lambda)
                  (let* ((params (cadr e))
                         (body-forms (cddr e))
                         (bound-vars (append params bound-vars)))
                    (dolist (form body-forms)
                      (walk form))))
                 ;; Labels introduces function names and params
                 ((eq (car e) 'labels)
                  (let* ((defs (cadr e))
                         (fn-names (mapcar #'car defs))
                         (body-forms (cddr e))
                         (bound-vars (append fn-names bound-vars)))
                    (dolist (form body-forms)
                      (walk form))))
                 ;; Quote doesn't reference vars
                 ((eq (car e) 'quote) nil)
                 (t (mapc #'walk e)))))
      (walk (if (consp body) (cons 'progn body) body))
      (remove-duplicates result :test #'eq))))

(defun box-mutable-captured-vars (expr)
  "Transform EXPR to box variables that are both captured and mutated.
   This ensures mutations inside closures affect the outer variable."
  (let* ((mutated (find-mutated-vars expr))
         (captured (find-captured-vars expr)))
    ;; Only box vars that are both mutated AND captured
    (let ((boxed-vars (intersection mutated captured :test #'eq)))
      (if boxed-vars
          (transform-with-boxing expr boxed-vars)
          expr))))

(defun transform-with-boxing (expr boxed-vars)
  "Transform EXPR to use boxing for variables in BOXED-VARS.
   - At let binding: wrap init in (cons init nil) if var is boxed
   - At var ref: use (car var) if boxed
   - At setq: use (setcar var val) if boxed"
  (labels ((transform (e bound-boxed)
             ;; bound-boxed is list of boxed vars currently in scope
             (cond
               ((null e) nil)
               ;; Symbol reference: unbox if this var is boxed and in scope
               ((symbolp e)
                (if (member e bound-boxed :test #'eq)
                    (list 'car e)
                    e))
               ((atom e) e)
               ;; Quote: don't transform
               ((eq (car e) 'quote) e)
               ;; setq: use (setf (car var) ...) if variable is boxed
               ((eq (car e) 'setq)
                (let ((var (cadr e))
                      (val (caddr e)))
                  (if (member var bound-boxed :test #'eq)
                      ;; (setf (car var) transformed-val)
                      (list 'setf (list 'car var) (transform val bound-boxed))
                      ;; Regular setq with transformed value
                      (list 'setq var (transform val bound-boxed)))))
               ;; setf on variable: use (setf (car var) ...) if boxed
               ((and (eq (car e) 'setf) (symbolp (cadr e)))
                (let ((var (cadr e))
                      (val (caddr e)))
                  (if (member var bound-boxed :test #'eq)
                      (list 'setf (list 'car var) (transform val bound-boxed))
                      (list 'setf var (transform val bound-boxed)))))
               ;; incf: transform to (setf (car var) (+ (car var) delta)) if boxed
               ((and (eq (car e) 'incf) (symbolp (cadr e)))
                (let ((var (cadr e))
                      (delta (or (caddr e) 1)))
                  (if (member var bound-boxed :test #'eq)
                      (list 'setf (list 'car var) (list '+ (list 'car var) delta))
                      (list 'incf var delta))))
               ;; decf: transform to (setf (car var) (- (car var) delta)) if boxed
               ((and (eq (car e) 'decf) (symbolp (cadr e)))
                (let ((var (cadr e))
                      (delta (or (caddr e) 1)))
                  (if (member var bound-boxed :test #'eq)
                      (list 'setf (list 'car var) (list '- (list 'car var) delta))
                      (list 'decf var delta))))
               ;; let/let*: box initializers for boxed vars, extend bound-boxed
               ((member (car e) '(let let*))
                (let* ((bindings (cadr e))
                       (body (cddr e))
                       (new-boxed nil))
                  ;; Transform bindings
                  (let ((new-bindings
                          (mapcar (lambda (b)
                                    (let* ((var (if (consp b) (car b) b))
                                           (init (if (consp b) (cadr b) nil))
                                           (is-boxed (member var boxed-vars :test #'eq)))
                                      (when is-boxed (push var new-boxed))
                                      (if (consp b)
                                          (if is-boxed
                                              ;; Box: (var (cons init nil))
                                              (list var (list 'cons (transform init bound-boxed) nil))
                                              ;; Not boxed
                                              (list var (transform init bound-boxed)))
                                          b)))
                                  bindings))
                        (extended-boxed (append new-boxed bound-boxed)))
                    (list* (car e) new-bindings
                           (mapcar (lambda (form) (transform form extended-boxed)) body)))))
               ;; lambda: params shadow outer boxed vars
               ((eq (car e) 'lambda)
                (let* ((params (cadr e))
                       (body (cddr e))
                       ;; Remove params from bound-boxed (they shadow)
                       (new-boxed (remove-if (lambda (v) (member v params :test #'eq)) bound-boxed)))
                  (list* 'lambda params
                         (mapcar (lambda (form) (transform form new-boxed)) body))))
               ;; labels: transform each function's body
               ((eq (car e) 'labels)
                (let* ((defs (cadr e))
                       (body (cddr e))
                       (fn-names (mapcar #'car defs)))
                  (list* 'labels
                         (mapcar (lambda (def)
                                   (let* ((fname (car def))
                                          (fparams (cadr def))
                                          (fbody (cddr def))
                                          ;; Params shadow boxed vars
                                          (new-boxed (remove-if (lambda (v)
                                                                  (or (member v fparams :test #'eq)
                                                                      (member v fn-names :test #'eq)))
                                                                bound-boxed)))
                                     (list* fname fparams
                                            (mapcar (lambda (form) (transform form new-boxed)) fbody))))
                                 defs)
                         (mapcar (lambda (form) (transform form bound-boxed)) body))))
               ;; flet: similar to labels
               ((eq (car e) 'flet)
                (let* ((defs (cadr e))
                       (body (cddr e)))
                  (list* 'flet
                         (mapcar (lambda (def)
                                   (let* ((fname (car def))
                                          (fparams (cadr def))
                                          (fbody (cddr def))
                                          (new-boxed (remove-if (lambda (v) (member v fparams :test #'eq))
                                                                bound-boxed)))
                                     (list* fname fparams
                                            (mapcar (lambda (form) (transform form new-boxed)) fbody))))
                                 defs)
                         (mapcar (lambda (form) (transform form bound-boxed)) body))))
               ;; General case: transform all subforms
               (t (mapcar (lambda (sub) (transform sub bound-boxed)) e)))))
    (transform expr nil)))

(defun encode-word-le (word)
  "Encode 32-bit word into little-endian byte list for smoke output."
  (list (logand word #xFF)
        (logand (ash word -8) #xFF)
        (logand (ash word -16) #xFF)
        (logand (ash word -24) #xFF)))

(defun pick-runtime-imm (runtime-addrs fallback)
  "Choose a low 16-bit immediate from runtime-addrs (alist), else fallback."
  (let ((entry (car runtime-addrs)))
    (if entry
        (logand (cdr entry) #xFFFF)
        (logand fallback #xFFFF))))

(defun has-tag? (ir tag)
  (and (consp ir) (eq (car ir) tag)))

(defun env-lookup (sym env)
  "Look up a symbol in the environment, returns stack offset or nil"
  (cond
    ((null env) nil)
    ((eq (caar env) sym) (cdar env))
    (t (env-lookup sym (cdr env)))))

(defun env-extend (bindings env)
  "Extend environment with new bindings, allocating stack offsets"
  ;; Find the maximum offset in current environment
  (let ((max-offset (if env
                        (apply #'max (mapcar #'cdr env))
                        -1)))
    (append
      ;; New bindings get offsets starting after the max
      (let ((offset (+ max-offset 1)))
        (mapcar (lambda (binding)
                  (cons (car binding)      ; Variable name
                        (prog1 offset      ; Current offset
                          (incf offset)))) ; Next offset
                bindings))
      env)))

(defun runtime-lookup (name runtime-addrs)
  "SBCL shim: lookup name in alist runtime-addrs (symbol . addr)."
  (if (nil? runtime-addrs)
      #x0
      (let* ((entry (car runtime-addrs))
             (entry-name (car entry))
             (entry-addr (cdr entry)))
        (if (eq name entry-name)
            entry-addr
            (runtime-lookup name (cdr runtime-addrs))))))

(defun make-runtime-addrs (cons-addr car-addr cdr-addr
                              &key make-closure closure-code closure-env
                                code-base make-vector vector-set vector-ref
                                make-string-from-vector make-symbol-from-string
                                string-length-raw symbol-name string-ref)
  "Create runtime address table for codegen (alist of symbol . addr).
Cons/car/cdr are required; others should be provided in production."
  (flet ((fail (what) (error "Missing runtime address: ~A" what)))
    (unless cons-addr (fail 'habu_cons))
    (unless car-addr (fail 'habu_car))
    (unless cdr-addr (fail 'habu_cdr))
    (remove-if (lambda (entry) (null (cdr entry)))
               (list (cons 'habu_cons cons-addr)
                     (cons 'habu_car car-addr)
                     (cons 'habu_cdr cdr-addr)
                     (cons 'habu_make_closure make-closure)
                     (cons 'habu_closure_code closure-code)
                     (cons 'habu_closure_env closure-env)
                     (cons 'habu_code_base code-base)
                     (cons 'habu_make_vector make-vector)
                     (cons 'habu_vector_set vector-set)
                     (cons 'habu_vector_ref vector-ref)
                     (cons 'habu_make_string_from_vector make-string-from-vector)
                     (cons 'habu_make_symbol_from_string make-symbol-from-string)
                     (cons 'habu_string_length_raw string-length-raw)
                     (cons 'habu_symbol_name symbol-name)
                     (cons 'habu_string_ref string-ref)))))

(defun op= (sym name)
  "Package-agnostic symbol name comparison."
  (and (symbolp sym) (string= (symbol-name sym) name)))

;;; ============================================
;;; Macro System
;;; ============================================

(defun macro-function-lookup (name)
  "Look up macro expander function for NAME in *macro-env*."
  (let ((entry (assoc name *macro-env* :test #'eq)))
    (if entry (cdr entry) nil)))

(defun macroexpand-1-habu (form)
  "Expand FORM once if it's a macro call. Returns (values expansion expanded-p)."
  (if (and (consp form) (symbolp (car form)))
      (let ((expander (macro-function-lookup (car form))))
        (if expander
            (values (apply expander (cdr form)) t)
            (values form nil)))
      (values form nil)))

(defun macroexpand-habu (form)
  "Fully expand FORM until it's no longer a macro call."
  (multiple-value-bind (expansion expanded-p) (macroexpand-1-habu form)
    (if expanded-p
        (macroexpand-habu expansion)
        form)))

(defun register-macro (name expander)
  "Register a macro with NAME and EXPANDER function."
  (let ((existing (assoc name *macro-env* :test #'eq)))
    (if existing
        (setf (cdr existing) expander)
        (push (cons name expander) *macro-env*))))

(defun contains-return-from-p (form block-name)
  "Check if FORM contains a return-from to BLOCK-NAME."
  (cond
    ((atom form) nil)
    ((and (eq (car form) 'return-from)
          (eq (cadr form) block-name))
     t)
    (t (some (lambda (sub) (contains-return-from-p sub block-name)) form))))

(defun transform-return-from (form block-name result-var exited-var)
  "Transform return-from calls in FORM to set result/exited vars."
  (cond
    ((atom form) form)
    ((and (eq (car form) 'return-from)
          (eq (cadr form) block-name))
     ;; Transform to: (progn (setq result value) (setq exited 1) result)
     ;; Return result so the value propagates correctly
     `(progn (setq ,result-var ,(or (caddr form) nil))
             (setq ,exited-var 1)
             ,result-var))
    ;; Don't descend into nested blocks with same name
    ((and (eq (car form) 'block)
          (eq (cadr form) block-name))
     form)
    (t (mapcar (lambda (sub) (transform-return-from sub block-name result-var exited-var))
               form))))

(defun transform-go (form tag-map)
  "Transform (go tag) calls in FORM to calls to the corresponding tag function.
   TAG-MAP is an alist of (tag-name . fn-name)."
  (cond
    ((atom form) form)
    ;; (go tag) -> (fn-name)
    ((eq (car form) 'go)
     (let* ((target-tag (cadr form))
            (entry (assoc target-tag tag-map :test #'equal)))
       (if entry
           (list (cdr entry))  ; Call the tag function
           form)))  ; Unknown tag, leave as is
    ;; Don't descend into nested tagbody
    ((eq (car form) 'tagbody)
     form)
    (t (mapcar (lambda (sub) (transform-go sub tag-map))
               form))))

(defun is-go-call-p (form tag-fns)
  "Return true if FORM is a call to one of the tag functions (i.e., a transformed go)."
  (and (consp form)
       (null (cdr form))
       (member (car form) tag-fns)))

(defun truncate-at-go (forms tag-fns)
  "Return FORMS truncated at the first go call. Also return whether a go was found."
  (let ((result nil))
    (dolist (f forms)
      (push f result)
      (when (is-go-call-p f tag-fns)
        (return-from truncate-at-go (values (nreverse result) t))))
    (values (nreverse result) nil)))

(defun sbcl-comma-p (obj)
  #+sbcl
  (let ((sym (find-symbol "COMMA" "SB-IMPL")))
    (and sym (eq (type-of obj) sym)))
  #-sbcl nil)

(defun sbcl-comma-kind (obj)
  #+sbcl (funcall (symbol-function (find-symbol "COMMA-KIND" "SB-IMPL")) obj)
  #-sbcl 0)

(defun sbcl-comma-expr (obj)
  #+sbcl (funcall (symbol-function (find-symbol "COMMA-EXPR" "SB-IMPL")) obj)
  #-sbcl nil)

(defun expand-quasiquote-ir (obj env fenv)
  "Expand quasiquoted OBJ into IR with unquotes evaluated via compile-expr."
  (cond
    ((sbcl-comma-p obj)
     (let ((kind (sbcl-comma-kind obj))
           (expr (sbcl-comma-expr obj)))
       (if (= kind 0)
           (compile-expr expr env fenv)
           (error "unquote-splicing not supported in quasiquote IR expansion: ~S" obj))))
    ;; ,expr -> compile expr
    ((and (consp obj) (op= (car obj) "UNQUOTE"))
     (compile-expr (cadr obj) env fenv))
    ;; ,@expr unsupported for now
    ((and (consp obj) (op= (car obj) "UNQUOTE-SPLICING"))
     (error "unquote-splicing not supported in quasiquote IR expansion: ~S" obj))
    ;; Vector literal
    ((vectorp obj)
     (list 'vector-lit (map 'list (lambda (el) (expand-quasiquote-ir el env fenv)) obj)))
    ;; Cons -> cons-call of car/cdr
    ((consp obj)
     (list 'cons-call
           (expand-quasiquote-ir (car obj) env fenv)
           (expand-quasiquote-ir (cdr obj) env fenv)))
    ;; Atom -> literal lowering
    (t (quote->ir obj))))

;; ARM64 Instruction Encoders (Functional)
(defun arm64-movz (rd imm)
  "MOVZ Xd, #imm16 - Move zero-extended 16-bit immediate
   Encoding: bits [20:5] = imm16, bits [4:0] = rd"
  (let* ((imm16 (logand imm #xFFFF))
         (base #xD2800000)
         (imm-bits (ash imm16 5))
         (encoded (logior base imm-bits rd)))
    (encode-word-le encoded)))

(defun arm64-add (rd rn rm)
  "ADD Xd, Xn, Xm - Add registers"
  (let* ((base #x8B000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-sub (rd rn rm)
  "SUB Xd, Xn, Xm - Subtract registers"
  (let* ((base #xCB000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-mul (rd rn rm)
  "MUL Xd, Xn, Xm - Multiply registers"
  (let* ((base #x9B007C00)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-sdiv (rd rn rm)
  "SDIV Xd, Xn, Xm - Signed divide"
  (let* ((base #x9AC00C00)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-lsl (rd rn shift)
  "LSL Xd, Xn, #shift - Logical shift left immediate (alias for UBFM)"
  (let* ((base #xD3400000)
         (shift-bits (logand shift #x3F))
         (immr (logand (- 64 shift-bits) #x3F))  ; -shift mod 64
         (imms (logand (- 63 shift-bits) #x3F))  ; 63 - shift
         (encoded (logior base
                          (ash immr 16)
                          (ash imms 10)
                          (ash rn 5)
                          rd)))
    (encode-word-le encoded)))

(defun arm64-lsr (rd rn shift)
  "LSR Xd, Xn, #shift - Logical shift right immediate"
  (let* ((base #xD3400000)
         (shift-bits (logand shift #x3F))
         (encoded (logior base
                          (ash shift-bits 16)  ; immr
                          (ash 63 10)          ; imms = 63
                          (ash rn 5)
                          rd)))
    (encode-word-le encoded)))

(defun arm64-mov (rd rm)
  "MOV Xd, Xm - Move register (via ORR)"
  (let* ((base #xAA0003E0)
         (encoded (logior base (ash rm 16) rd)))
    (encode-word-le encoded)))

(defun arm64-and (rd rn rm)
  "AND Xd, Xn, Xm - Bitwise AND registers"
  (let* ((base #x8A000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-orr (rd rn rm)
  "ORR Xd, Xn, Xm - Bitwise OR registers"
  (let* ((base #xAA000000)
         (encoded (logior base (ash rm 16) (ash rn 5) rd)))
    (encode-word-le encoded)))

(defun arm64-ldr (rt rn offset)
  "LDR Xt, [Xn, #offset] - Load register from memory
   offset is in bytes, must be 8-byte aligned, encoded as offset/8"
  (let* ((base #xF9400000)
         (imm12 (logand (/ offset 8) #xFFF))
         (encoded (logior base (ash imm12 10) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-str (rt rn offset)
  "STR Xt, [Xn, #offset] - Store register to memory
   offset is in bytes, must be 8-byte aligned, encoded as offset/8"
  (let* ((base #xF9000000)
         (imm12 (logand (/ offset 8) #xFFF))
         (encoded (logior base (ash imm12 10) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-add-imm (rd rn imm)
  "ADD Xd, Xn, #imm12 - Add immediate (use sp register properly)"
  (let* ((base #x91000000)
         (imm12 (logand imm #xFFF))
         ;; ARM64 uses reg 31 to mean SP in some contexts
         (rn-bits (if (= rn 31) 31 rn))
         (rd-bits (if (= rd 31) 31 rd))
         (encoded (logior base (ash imm12 10) (ash rn-bits 5) rd-bits)))
    (encode-word-le encoded)))

(defun arm64-sub-imm (rd rn imm)
  "SUB Xd, Xn, #imm12 - Subtract immediate (use sp register properly)"
  (let* ((base #xD1000000)
         (imm12 (logand imm #xFFF))
         ;; ARM64 uses reg 31 to mean SP in some contexts
         (rn-bits (if (= rn 31) 31 rn))
         (rd-bits (if (= rd 31) 31 rd))
         (encoded (logior base (ash imm12 10) (ash rn-bits 5) rd-bits)))
    (encode-word-le encoded)))

(defun arm64-stp (rt1 rt2 rn imm)
  "STP Xt1, Xt2, [Xn, #imm] - Store pair of registers
   imm is in bytes, must be multiple of 8, encoded as imm/8"
  (let* ((base #xA9000000)
         (pre-index (if (< imm 0) #x00800000 0))  ; Pre-index if negative
         (post-index 0)  ; Not used for now
         (imm7 (logand (/ (abs imm) 8) #x7F))
         (encoded (logior base pre-index post-index (ash imm7 15) (ash rt2 10) (ash rn 5) rt1)))
    (encode-word-le encoded)))

(defun arm64-ldp (rt1 rt2 rn imm)
  "LDP Xt1, Xt2, [Xn, #imm] - Load pair of registers
   Uses offset mode for imm >= 0, post-index for negative (though we don't use negative)
   imm is in bytes, must be multiple of 8, encoded as imm/8"
  (let* ((base #xA9400000)  ; Offset mode
         (imm7 (logand (/ imm 8) #x7F))
         (encoded (logior base (ash imm7 15) (ash rt2 10) (ash rn 5) rt1)))
    (encode-word-le encoded)))

(defun arm64-cmp (rn rm)
  "CMP Xn, Xm - Compare registers (sets flags)"
  (let* ((base #xEB00001F)  ; SUBS XZR, Xn, Xm
         (encoded (logior base (ash rm 16) (ash rn 5))))
    (encode-word-le encoded)))

(defun arm64-cset (rd cond)
  "CSET Xd, cond - Conditional set (1 if condition, else 0)"
  (let* ((base #x9A9F07E0)  ; CSINC Xd, XZR, XZR, invert(cond)
         (inv-cond (logxor cond 1))  ; Invert condition
         (encoded (logior base (ash inv-cond 12) rd)))
    (encode-word-le encoded)))

(defun arm64-b (offset)
  "B offset - Unconditional branch (offset in instructions, signed 26-bit)"
  (let* ((base #x14000000)
         (offset-bits (if (< offset 0)
                          (logand (+ offset #x4000000) #x3FFFFFF)
                          (logand offset #x3FFFFFF)))
         (encoded (logior base offset-bits)))
    (encode-word-le encoded)))

(defun arm64-bl (offset)
  "BL offset - Branch with link (offset in instructions, signed 26-bit)"
  (let* ((base #x94000000)
         ;; Handle negative offsets properly with 26-bit two's complement
         (offset-bits (if (< offset 0)
                         (logand (+ offset #x4000000) #x3FFFFFF)  ; Add 2^26 for two's complement
                         (logand offset #x3FFFFFF)))
         (encoded (logior base offset-bits)))
    (encode-word-le encoded)))

(defun arm64-b-cond (cond offset)
  "B.cond offset - Conditional branch (offset in instructions, signed 19-bit)"
  (let* ((base #x54000000)
         (offset-19bit (logand offset #x7FFFF))  ; Mask to 19 bits
         (offset-bits (ash offset-19bit 5))       ; Shift to bits [23:5]
         (encoded (logior base offset-bits cond)))
    (encode-word-le encoded)))

(defun arm64-ret ()
  "RET - Return from subroutine"
  (encode-word-le #xD65F03C0))

(defun arm64-movk (rd imm shift)
  "MOVK Xd, #imm16, LSL #shift - Move with keep (loads 16 bits without clearing others)
   shift must be 0, 16, 32, or 48"
  (let* ((imm16 (logand imm #xFFFF))
         (hw (/ shift 16))  ; Which 16-bit chunk (0, 1, 2, or 3)
         (base #xF2800000)
         (imm-bits (ash imm16 5))
         (hw-bits (ash hw 21))
         (encoded (logior base hw-bits imm-bits rd)))
    (encode-word-le encoded)))

(defun arm64-blr (rn)
  "BLR Xn - Branch with link to register"
  (let* ((base #xD63F0000)
         (encoded (logior base (ash rn 5))))
    (encode-word-le encoded)))

(defun arm64-str-pre (rt rn offset)
  "STR Xt, [Xn, #offset]! - Store register with pre-decrement
   Used for push: STR x0, [sp, #-16]!"
  ;; Encoding: 1111 1000 0 imm9 11 Rn Rt
  ;; Base for STR pre-index: F8001C00
  (let* ((base #xF8001C00)  ; Pre-index variant with writeback
         (imm9 (logand offset #x1FF))  ; 9-bit immediate (already in two's complement)
         (encoded (logior base (ash imm9 12) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-ldr-post (rt rn offset)
  "LDR Xt, [Xn], #offset - Load register with post-increment
   Used for pop: LDR x0, [sp], #16"
  ;; Encoding: 1111 1000 0100 0000 01 imm9 Rn Rt
  ;; Base for LDR post-index: F8400400
  (let* ((base #xF8400400)  ; Post-index variant
         (imm9 (logand offset #x1FF))  ; 9-bit immediate
         (encoded (logior base (ash imm9 12) (ash rn 5) rt)))
    (encode-word-le encoded)))

(defun arm64-push (rt)
  "Push register onto stack using SUB + STR"
  ;; Decrement stack pointer then store
  (append (arm64-sub-imm 31 31 16)    ; sp = sp - 16
          (arm64-str rt 31 0)))       ; [sp] = rt

(defun arm64-pop (rt)
  "Pop register from stack using LDR + ADD"
  ;; Load then increment stack pointer
  (append (arm64-ldr rt 31 0)         ; rt = [sp]
          (arm64-add-imm 31 31 16)))  ; sp = sp + 16

(defun arm64-load-addr (rd addr)
  "Load 64-bit address into register using MOVZ + MOVK sequence"
  (let ((bits-0-15 (logand addr #xFFFF))
        (bits-16-31 (logand (ash addr -16) #xFFFF))
        (bits-32-47 (logand (ash addr -32) #xFFFF))
        (bits-48-63 (logand (ash addr -48) #xFFFF)))
    (append (arm64-movz rd bits-0-15)
            (arm64-movk rd bits-16-31 16)
            (arm64-movk rd bits-32-47 32)
            (arm64-movk rd bits-48-63 48))))

(defun temp-slot-offset (temp-depth)
  "Stack offset (bytes) for temporary storage at a given nesting depth."
  (let ((offset (+ *temp-slot-base* (* temp-depth #x8))))
    ;; Keep temps within the stack frame and below env base
    (when (>= offset *temp-slot-guard*)
      (error "temp-depth ~A exceeds frame temp area (offset #x~X)" temp-depth offset))
    offset))

(defun arg-spill-offset (index)
  "Stack offset for staged arguments before calls (8-byte stride)."
  (let ((offset (+ *arg-spill-base* (* index *arg-spill-stride*))))
    (when (>= offset *stack-frame-size*)
      (error "argument index ~A exceeds spill area (offset #x~X)" index offset))
    offset))

(defun codegen-expr (ir runtime-addrs &optional fn-offsets current-offset (temp-depth 0))
  "Enhanced codegen: literals, arithmetic, runtime calls with depth-tracked temps"
  (cond
    ;; Literal: load tagged fixnum (value << 4)
    ((has-tag? ir 'lit)
     (let* ((value (cadr ir))
            (tagged (ash value 4)))  ; Tag fixnum: value << 4
       (if (and (>= tagged 0) (< tagged #x10000))
           (arm64-movz 0 tagged)
           (arm64-load-addr 0 tagged))))

    ;; Variable: load from stack (negative offset from x20 = environment base)
    ((has-tag? ir 'var)
     (let ((offset (cadr ir)))
       ;; Variables are stored at negative offsets from x20 (stack grows down)
       ;; Use x1 as temp to compute address
       (append
         (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
         (arm64-ldr 0 1 0))))                ; Load from [x1 + 0]

    ;; Set variable: store value to stack, return the value
    ((has-tag? ir 'set-var)
     (let* ((offset (cadr ir))
            (val-ir (caddr ir))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset temp-depth)))
       ;; Compute value, store to variable slot, value stays in x0
       (append val-code
               (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
               (arm64-str 0 1 0))))               ; Store x0 to [x1 + 0]

    ;; String literal: build vector of chars then make-string-from-vector
    ((has-tag? ir 'string-lit)
     (codegen-string-from-chars (cdr ir) temp-depth))

    ;; Symbol literal: build string then symbol-from-string
    ((has-tag? ir 'symbol-lit)
     (let* ((str-code (codegen-string-from-chars (string->char-codes (cadr ir)) temp-depth))
            (cursor (if current-offset (+ current-offset (count-instrs str-code)) nil)))
       (append str-code
               (arm64-ldr 9 19 88) ; make-symbol-from-string
               (arm64-blr 9))))

    ;; Vector literal
    ((has-tag? ir 'vector-lit)
     (codegen-vector-literal (cdr ir) runtime-addrs fn-offsets current-offset temp-depth))

    ;; Tag inspection: (get-tag x) => fixnum tag bits
    ((has-tag? ir 'get-tag)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code
               (arm64-movz 1 #xF)   ; mask
               (arm64-and 0 0 1)    ; tag in x0
               (arm64-lsl 0 0 4)))) ; tag as fixnum

    ;; Captured variable: load from closure env vector in x24
    ((has-tag? ir 'capture)
     (let ((idx (cadr ir)))
       (append
         (arm64-mov 0 24)                   ; x0 = closure env vector
         (arm64-movz 1 idx)                 ; x1 = index (raw)
         (arm64-ldr 9 19 72)                ; x9 = habu_vector_ref (slot 9)
         (arm64-blr 9))))                   ; x0 = env[idx]

    ;; Addition: (add left right)
    ;; Use depth-indexed temp slot to park left operand while evaluating right
    ((has-tag? ir 'add)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before left
               left-code                     ; Compute left → x0
               (arm64-str 0 31 left-slot)    ; Save left to temp
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right
               right-code                    ; Compute right → x0
               (arm64-mov 1 0)               ; Move right to x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp to x0
               (arm64-add 0 0 1))))        ; x0 = x0 + x1

    ;; Subtraction: (sub left right)
    ;; Use depth-indexed temp slot to park left operand while evaluating right
    ((has-tag? ir 'sub)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before left
               left-code
               (arm64-str 0 31 left-slot)    ; Save left to temp
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right
               right-code
               (arm64-mov 1 0)               ; Move right to x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp to x0
               (arm64-sub 0 0 1))))        ; x0 = x0 - x1

    ;; Multiplication: (mul left right) - must untag/retag
    ;; Temp slot holds untagged left operand while computing right
    ((has-tag? ir 'mul)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 3)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before left
               left-code
               (arm64-lsr 0 0 4)             ; Untag left
               (arm64-str 0 31 left-slot)    ; Save untagged left to temp
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right
               right-code
               (arm64-lsr 1 0 4)             ; Untag right into x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp into x0
               (arm64-mul 0 0 1)             ; Multiply x0 = x0 * x1
               (arm64-lsl 0 0 4))))        ; Retag result

    ;; Comparison: (cmp-eq left right) - returns tagged 1 or 0
    ((has-tag? ir 'cmp-eq)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 0)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Less than: (cmp-lt left right)
    ((has-tag? ir 'cmp-lt)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 11)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Greater than: (cmp-gt left right)
    ((has-tag? ir 'cmp-gt)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 12)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Less than or equal: (cmp-le left right)
    ((has-tag? ir 'cmp-le)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 13)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Greater than or equal: (cmp-ge left right)
    ((has-tag? ir 'cmp-ge)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 10)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Division: (div left right)
    ((has-tag? ir 'div)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-lsr 1 0 4)             ; untag right
               (arm64-ldr 0 31 left-slot)
               (arm64-lsr 0 0 4)             ; untag left
               (arm64-sdiv 0 0 1)            ; x0 = left/right
               (arm64-lsl 0 0 4))))          ; retag

    ;; Modulo: (mod left right)
    ((has-tag? ir 'mod)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-lsr 1 0 4)             ; untag right
               (arm64-ldr 0 31 left-slot)
               (arm64-lsr 0 0 4)             ; untag left
               (arm64-sdiv 2 0 1)            ; x2 = quotient
               (arm64-mul 2 2 1)             ; x2 = quotient * right
               (arm64-sub 0 0 2)             ; x0 = left - product
               (arm64-lsl 0 0 4))))          ; retag

    ;; Remainder: (rem left right)
    ((has-tag? ir 'rem)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-lsr 1 0 4)             ; untag right
               (arm64-ldr 0 31 left-slot)
               (arm64-lsr 0 0 4)             ; untag left
               (arm64-sdiv 2 0 1)            ; x2 = quotient
               (arm64-mul 2 2 1)             ; x2 = quotient * right
               (arm64-sub 0 0 2)             ; x0 = remainder
               (arm64-lsl 0 0 4))))          ; retag

    ;; Not equal: (cmp-ne left right)
    ((has-tag? ir 'cmp-ne)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                             (+ current-offset 1 (count-instrs left-code) 2)
                             nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (arm64-str 24 31 x24-slot)
               left-code
               (arm64-str 0 31 left-slot)
               (arm64-ldr 24 31 x24-slot)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 left-slot)
               (arm64-cmp 0 1)
               (arm64-cset 0 1)
               (arm64-lsl 0 0 4))))        ; Tag result

    ;; Conditional: (if-expr test then else)
    ((has-tag? ir 'if-expr)
     (let* ((test-ir (cadr ir))
            (then-ir (caddr ir))
            (else-ir (cadddr ir))
            (test-code (codegen-expr test-ir runtime-addrs fn-offsets current-offset temp-depth))
            (test-len (count-instrs test-code))
            (then-code (codegen-expr then-ir runtime-addrs fn-offsets
                                     (if current-offset
                                         (+ current-offset test-len 2)
                                         nil)
                                     temp-depth))
            (then-len (/ (length then-code) 4))
            (else-code (codegen-expr else-ir runtime-addrs fn-offsets
                                     (if current-offset
                                         (+ current-offset test-len 2 then-len 1)
                                         nil)
                                     temp-depth))
            (else-len (/ (length else-code) 4)))
       ;; Layout: CMP, B.EQ → else-code, then-code, B skip-else, else-code
       ;; True branch (non-zero) falls through to then-code; false jumps to else-code
       ;; Offsets from the B instructions:
       ;;   B.EQ: offset = then-len + 2 (skip then-code + following B)
       ;;   B (skip else): offset = else-len + 1
       (append test-code
               (arm64-cmp 0 31)            ; Compare result with 0 (XZR)
               (arm64-b-cond 0 (+ 2 then-len)) ; Jump to else if zero
               then-code
               (arm64-b (+ 1 else-len))    ; Skip else after then
               else-code)))

    ;; Block expression: (block-expr block-id body-ir)
    ;; The body may contain return-from-expr which branches to the end
    ;; For now, we track blocks and their exit offsets in a special variable
    ((has-tag? ir 'block-expr)
     (let* ((block-id (cadr ir))
            (body-ir (caddr ir))
            ;; Compile body, tracking that we're in this block
            ;; return-from-expr will generate code that sets x0 and branches forward
            (body-code (codegen-expr body-ir runtime-addrs fn-offsets current-offset temp-depth))
            (body-len (count-instrs body-code)))
       ;; Body code followed by nothing - return-from branches jump past body-len
       ;; We need to patch return-from branches, but for now just generate body
       ;; The return-from-expr generates a forward branch that needs patching
       body-code))

    ;; Return-from expression: (return-from-expr block-id value-ir)
    ;; For now, this just evaluates value - true non-local exit requires more infrastructure
    ;; TODO: Implement proper non-local exit with branch patching
    ((has-tag? ir 'return-from-expr)
     (let* ((block-id (cadr ir))
            (value-ir (caddr ir))
            (value-code (codegen-expr value-ir runtime-addrs fn-offsets current-offset temp-depth)))
       ;; Just evaluate value for now - proper exit would branch to block end
       value-code))

    ;; Cons: (cons-call left right) - call runtime cons via table
    ;;   Runtime table pointer is in x19 (saved by prologue)
    ;;   Save/restore x24 around arg evaluation in case args contain funcalls
    ((has-tag? ir 'cons-call)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            ;; cursor: save x24 + left-code + store left + restore x24
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (left-cursor (if current-offset
                            (+ current-offset 1 (count-instrs left-code) 2)
                            nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets left-cursor nested-depth)))
       ;; Call cons(left, right) using runtime table[0]
       (append (arm64-str 24 31 x24-slot)   ; Save x24 before any arg evaluation
               left-code                     ; Compute left → x0
               (arm64-str 0 31 left-slot)    ; Save left to temp slot
               (arm64-ldr 24 31 x24-slot)    ; Restore x24 before right arg
               right-code                    ; Compute right → x0
               (arm64-mov 1 0)               ; Move right to x1
               (arm64-ldr 0 31 left-slot)    ; Load left from temp slot
               (arm64-ldr 9 19 0)            ; Load cons from table: LDR x9, [x19, #0]
               (arm64-blr 9))))             ; Call cons(x0, x1) → result in x0

    ;; Vector ref: (vector-ref vec idx)
    ((has-tag? ir 'vector-ref)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (vec-slot (temp-slot-offset temp-depth))
            (vec-code (codegen-expr vec-ir runtime-addrs fn-offsets current-offset temp-depth))
            (cursor (if current-offset (+ current-offset (count-instrs vec-code)) nil))
            (idx-code (codegen-expr idx-ir runtime-addrs fn-offsets cursor (+ temp-depth 1))))
       (append vec-code
               (arm64-str 0 31 vec-slot)
               idx-code
               (arm64-lsr 1 0 4)           ; untag index
               (arm64-ldr 0 31 vec-slot)
               (arm64-ldr 9 19 72) ; vector-ref at offset 72 (index 9)
               (arm64-blr 9))))

    ;; Make vector: (make-vector-call size-ir)
    ;; habu_make_vector(size) -> vector
    ((has-tag? ir 'make-vector-call)
     (let* ((size-ir (cadr ir))
            (size-code (codegen-expr size-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append size-code
               (arm64-lsr 0 0 4)           ; untag size
               (arm64-ldr 9 19 56)         ; make-vector at offset 56 (index 7)
               (arm64-blr 9))))

    ;; Vector set: (vector-set-call vec-ir idx-ir val-ir)
    ;; habu_vector_set(vec, idx, val)
    ((has-tag? ir 'vector-set-call)
     (let* ((vec-ir (cadr ir))
            (idx-ir (caddr ir))
            (val-ir (cadddr ir))
            (slot1 (temp-slot-offset temp-depth))
            (slot2 (temp-slot-offset (+ temp-depth 1)))
            (vec-code (codegen-expr vec-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (idx-code (codegen-expr idx-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2))))
       (append vec-code
               (arm64-str 0 31 slot1)      ; save vec
               idx-code
               (arm64-str 0 31 slot2)      ; save idx (tagged)
               val-code
               (arm64-mov 2 0)             ; x2 = value
               (arm64-ldr 1 31 slot2)      ; x1 = idx (tagged)
               (arm64-lsr 1 1 4)           ; untag idx
               (arm64-ldr 0 31 slot1)      ; x0 = vec
               (arm64-ldr 9 19 64)         ; vector-set at offset 64 (index 8)
               (arm64-blr 9)
               (arm64-mov 0 2))))          ; return the value that was set

    ;; Vector length: (vector-length-call vec-ir)
    ;; Returns length as tagged fixnum
    ((has-tag? ir 'vector-length-call)
     (let* ((vec-ir (cadr ir))
            (vec-code (codegen-expr vec-ir runtime-addrs fn-offsets current-offset temp-depth)))
       ;; Vector layout: [header:16][length:8][data...]
       ;; Tagged vector pointer points to length field (tag 0x3)
       ;; Clear tag by: x1 = x0 & 0xF; x0 = x0 - x1
       (append vec-code
               (arm64-movz 1 #xF)          ; x1 = 0xF (tag mask)
               (arm64-and 1 0 1)           ; x1 = x0 & 0xF (just the tag)
               (arm64-sub 0 0 1)           ; x0 = x0 - tag (clear tag)
               (arm64-ldr 0 0 0)           ; Load length from vector struct
               (arm64-lsl 0 0 4))))        ; Tag as fixnum

    ;; Car: (car-call arg) - call runtime car via table
    ((has-tag? ir 'car-call)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code                     ; Compute arg → x0
               (arm64-ldr 9 19 8)           ; Load car from table: LDR x9, [x19, #8]
               (arm64-blr 9))))             ; Call car(x0) → result in x0

    ;; Cdr: (cdr-call arg) - call runtime cdr via table
    ((has-tag? ir 'cdr-call)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code                     ; Compute arg → x0
               (arm64-ldr 9 19 16)          ; Load cdr from table: LDR x9, [x19, #16]
               (arm64-blr 9))))             ; Call cdr(x0) → result in x0

    ;; Set-car: (setcar-call cons-ir val-ir) - call runtime set_car, return value
    ((has-tag? ir 'setcar-call)
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (cons-slot (temp-slot-offset temp-depth))
            (val-slot (temp-slot-offset (1+ temp-depth)))
            (cons-code (codegen-expr cons-ir runtime-addrs fn-offsets current-offset temp-depth))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append cons-code
               (arm64-str 0 31 cons-slot)   ; Save cons to temp slot
               val-code
               (arm64-str 0 31 val-slot)    ; Save value to temp slot
               (arm64-ldr 0 31 cons-slot)   ; x0 = cons (arg 1)
               (arm64-ldr 1 31 val-slot)    ; x1 = value (arg 2)
               (arm64-ldr 9 19 112)         ; Load set_car from table (slot 14)
               (arm64-blr 9)                ; Call set_car(cons, value) -> void
               (arm64-ldr 0 31 val-slot)))) ; Return the value

    ;; Set-cdr: (setcdr-call cons-ir val-ir) - call runtime set_cdr, return value
    ((has-tag? ir 'setcdr-call)
     (let* ((cons-ir (cadr ir))
            (val-ir (caddr ir))
            (cons-slot (temp-slot-offset temp-depth))
            (val-slot (temp-slot-offset (1+ temp-depth)))
            (cons-code (codegen-expr cons-ir runtime-addrs fn-offsets current-offset temp-depth))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append cons-code
               (arm64-str 0 31 cons-slot)   ; Save cons to temp slot
               val-code
               (arm64-str 0 31 val-slot)    ; Save value to temp slot
               (arm64-ldr 0 31 cons-slot)   ; x0 = cons (arg 1)
               (arm64-ldr 1 31 val-slot)    ; x1 = value (arg 2)
               (arm64-ldr 9 19 120)         ; Load set_cdr from table (slot 15)
               (arm64-blr 9)                ; Call set_cdr(cons, value) -> void
               (arm64-ldr 0 31 val-slot)))) ; Return the value

    ;; Symbol-name
    ((has-tag? ir 'symbol-name)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code
               (arm64-ldr 9 19 104) ; symbol-name
               (arm64-blr 9))))

    ;; String length (returns fixnum)
    ((has-tag? ir 'string-len)
     (let* ((arg-ir (cadr ir))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code
               (arm64-ldr 9 19 96) ; string-length-raw
               (arm64-blr 9)
               (arm64-lsl 0 0 4)))) ; tag length as fixnum

    ;; String-ref (returns tagged char code)
    ;; Runtime table: [16] = offset 128 = string-ref
    ((has-tag? ir 'string-ref-call)
     (let* ((str-ir (cadr ir))
            (idx-ir (caddr ir))
            (slot1 (temp-slot-offset temp-depth))
            (str-code (codegen-expr str-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1)))
            (idx-code (codegen-expr idx-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append str-code
               (arm64-str 0 31 slot1)         ; save string
               idx-code
               (arm64-lsr 1 0 4)              ; x1 = untagged index
               (arm64-ldr 0 31 slot1)         ; x0 = string
               (arm64-ldr 9 19 128)           ; string-ref at offset 128 (index 16)
               (arm64-blr 9))))               ; returns tagged char code

    ;; String-concat: (string-concat-call str1 str2)
    ;; Runtime table: [24] = offset 192 = habu_string_concat
    ((has-tag? ir 'string-concat-call)
     (let* ((str1-ir (cadr ir))
            (str2-ir (caddr ir))
            (slot1 (temp-slot-offset temp-depth))
            (str1-code (codegen-expr str1-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1)))
            (str2-code (codegen-expr str2-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append
        str1-code
        (arm64-str 0 31 slot1)            ; save str1
        str2-code
        (arm64-mov 1 0)                   ; x1 = str2
        (arm64-ldr 0 31 slot1)            ; x0 = str1
        (arm64-ldr 9 19 192)              ; habu_string_concat at offset 192 (index 24)
        (arm64-blr 9))))                  ; returns concatenated string

    ;; String-substring: (string-substring-call str start end)
    ;; Runtime table: [25] = offset 200 = habu_string_substring
    ((has-tag? ir 'string-substring-call)
     (let* ((str-ir (cadr ir))
            (start-ir (caddr ir))
            (end-ir (cadddr ir))
            (slot0 (temp-slot-offset temp-depth))
            (slot1 (temp-slot-offset (+ temp-depth 1)))
            (str-code (codegen-expr str-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (start-code (codegen-expr start-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (end-code (codegen-expr end-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2))))
       (append
        str-code
        (arm64-str 0 31 slot0)            ; save str
        start-code
        (arm64-str 0 31 slot1)            ; save start
        end-code
        (arm64-mov 2 0)                   ; x2 = end
        (arm64-ldr 1 31 slot1)            ; x1 = start
        (arm64-ldr 0 31 slot0)            ; x0 = str
        (arm64-ldr 9 19 200)              ; habu_string_substring at offset 200 (index 25)
        (arm64-blr 9))))                  ; returns substring

    ;; Fixnum-to-string: (fixnum-to-string-call num)
    ;; Runtime table: [26] = offset 208 = habu_fixnum_to_string
    ((has-tag? ir 'fixnum-to-string-call)
     (let* ((num-ir (cadr ir))
            (num-code (codegen-expr num-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append
        num-code                          ; x0 = tagged fixnum
        (arm64-ldr 9 19 208)              ; habu_fixnum_to_string at offset 208 (index 26)
        (arm64-blr 9))))                  ; returns string

    ;; Make-string-from-vector: (make-string-from-vector-call vec)
    ;; Runtime table: [10] = offset 80 = habu_make_string_from_vector
    ((has-tag? ir 'make-string-from-vector-call)
     (let* ((vec-ir (cadr ir))
            (vec-code (codegen-expr vec-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append
        vec-code                          ; x0 = tagged vector
        (arm64-ldr 9 19 80)               ; habu_make_string_from_vector at offset 80 (index 10)
        (arm64-blr 9))))                  ; returns string

    ;; Multiple values: (values-call count v0 v1 v2 v3)
    ;; Runtime table: [17] = offset 136 = habu_values_set
    ;; habu_values_set(count, v0, v1, v2, v3) -> returns v0
    ((has-tag? ir 'values-call)
     (let* ((count (cadr ir))
            (v0-ir (caddr ir))
            (v1-ir (cadddr ir))
            (v2-ir (nth 4 ir))
            (v3-ir (nth 5 ir))
            (slot0 (temp-slot-offset temp-depth))
            (slot1 (temp-slot-offset (+ temp-depth 1)))
            (slot2 (temp-slot-offset (+ temp-depth 2)))
            (slot3 (temp-slot-offset (+ temp-depth 3)))
            ;; Generate code for each value
            (v0-code (codegen-expr v0-ir runtime-addrs fn-offsets current-offset (+ temp-depth 4)))
            (v1-code (codegen-expr v1-ir runtime-addrs fn-offsets current-offset (+ temp-depth 4)))
            (v2-code (codegen-expr v2-ir runtime-addrs fn-offsets current-offset (+ temp-depth 4)))
            (v3-code (codegen-expr v3-ir runtime-addrs fn-offsets current-offset (+ temp-depth 4))))
       (append
        ;; Evaluate v0-v3 and save to temp slots
        v0-code (arm64-str 0 31 slot0)
        v1-code (arm64-str 0 31 slot1)
        v2-code (arm64-str 0 31 slot2)
        v3-code (arm64-str 0 31 slot3)
        ;; Load args: x0=count (untagged), x1=v0, x2=v1, x3=v2, x4=v3
        (arm64-movz 0 count)              ; x0 = count (untagged for C)
        (arm64-ldr 1 31 slot0)            ; x1 = v0
        (arm64-ldr 2 31 slot1)            ; x2 = v1
        (arm64-ldr 3 31 slot2)            ; x3 = v2
        (arm64-ldr 4 31 slot3)            ; x4 = v3
        (arm64-ldr 9 19 136)              ; habu_values_set at offset 136 (index 17)
        (arm64-blr 9))))                  ; returns primary value

    ;; Values-get: (values-get-call index primary)
    ;; Runtime table: [18] = offset 144 = habu_values_get
    ;; habu_values_get(index, primary) -> returns Nth value
    ((has-tag? ir 'values-get-call)
     (let* ((idx-ir (cadr ir))
            (primary-ir (caddr ir))
            (slot1 (temp-slot-offset temp-depth))
            (idx-code (codegen-expr idx-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1)))
            (primary-code (codegen-expr primary-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append
        idx-code
        (arm64-str 0 31 slot1)            ; save index (tagged)
        primary-code
        (arm64-mov 1 0)                   ; x1 = primary value
        (arm64-ldr 0 31 slot1)            ; x0 = index (tagged)
        (arm64-lsr 0 0 4)                 ; untag index
        (arm64-ldr 9 19 144)              ; habu_values_get at offset 144 (index 18)
        (arm64-blr 9))))                  ; returns Nth value

    ;; Hash table operations
    ;; Runtime table indices: 19=make-hash-table, 20=gethash, 21=puthash, 22=remhash, 23=hash-table-count
    ;; Offsets: 19*8=152, 20*8=160, 21*8=168, 22*8=176, 23*8=184

    ;; make-hash-table: (make-hash-table-call capacity-ir)
    ;; habu_make_hash_table(capacity) -> hash table
    ((has-tag? ir 'make-hash-table-call)
     (let* ((capacity-ir (cadr ir))
            (capacity-code (codegen-expr capacity-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append
        capacity-code
        (arm64-ldr 9 19 152)              ; habu_make_hash_table at offset 152 (index 19)
        (arm64-blr 9))))                  ; returns hash table

    ;; gethash: (gethash-call key-ir ht-ir default-ir)
    ;; habu_gethash(key, ht, default) -> value
    ((has-tag? ir 'gethash-call)
     (let* ((key-ir (cadr ir))
            (ht-ir (caddr ir))
            (default-ir (cadddr ir))
            (slot1 (temp-slot-offset temp-depth))
            (slot2 (temp-slot-offset (+ temp-depth 1)))
            (key-code (codegen-expr key-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (ht-code (codegen-expr ht-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (default-code (codegen-expr default-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2))))
       (append
        key-code
        (arm64-str 0 31 slot1)            ; save key
        ht-code
        (arm64-str 0 31 slot2)            ; save ht
        default-code
        (arm64-mov 2 0)                   ; x2 = default
        (arm64-ldr 1 31 slot2)            ; x1 = ht
        (arm64-ldr 0 31 slot1)            ; x0 = key
        (arm64-ldr 9 19 160)              ; habu_gethash at offset 160 (index 20)
        (arm64-blr 9))))                  ; returns value or default

    ;; puthash: (puthash-call key-ir value-ir ht-ir)
    ;; habu_puthash(key, value, ht) -> value
    ((has-tag? ir 'puthash-call)
     (let* ((key-ir (cadr ir))
            (value-ir (caddr ir))
            (ht-ir (cadddr ir))
            (slot1 (temp-slot-offset temp-depth))
            (slot2 (temp-slot-offset (+ temp-depth 1)))
            (key-code (codegen-expr key-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (value-code (codegen-expr value-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2)))
            (ht-code (codegen-expr ht-ir runtime-addrs fn-offsets current-offset (+ temp-depth 2))))
       (append
        key-code
        (arm64-str 0 31 slot1)            ; save key
        value-code
        (arm64-str 0 31 slot2)            ; save value
        ht-code
        (arm64-mov 2 0)                   ; x2 = ht
        (arm64-ldr 1 31 slot2)            ; x1 = value
        (arm64-ldr 0 31 slot1)            ; x0 = key
        (arm64-ldr 9 19 168)              ; habu_puthash at offset 168 (index 21)
        (arm64-blr 9))))                  ; returns value

    ;; remhash: (remhash-call key-ir ht-ir)
    ;; habu_remhash(key, ht) -> boolean
    ((has-tag? ir 'remhash-call)
     (let* ((key-ir (cadr ir))
            (ht-ir (caddr ir))
            (slot1 (temp-slot-offset temp-depth))
            (key-code (codegen-expr key-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1)))
            (ht-code (codegen-expr ht-ir runtime-addrs fn-offsets current-offset (+ temp-depth 1))))
       (append
        key-code
        (arm64-str 0 31 slot1)            ; save key
        ht-code
        (arm64-mov 1 0)                   ; x1 = ht
        (arm64-ldr 0 31 slot1)            ; x0 = key
        (arm64-ldr 9 19 176)              ; habu_remhash at offset 176 (index 22)
        (arm64-blr 9))))                  ; returns boolean

    ;; hash-table-count: (hash-table-count-call ht-ir)
    ;; habu_hash_table_count(ht) -> fixnum
    ((has-tag? ir 'hash-table-count-call)
     (let* ((ht-ir (cadr ir))
            (ht-code (codegen-expr ht-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append
        ht-code
        (arm64-ldr 9 19 184)              ; habu_hash_table_count at offset 184 (index 23)
        (arm64-blr 9))))                  ; returns count as tagged fixnum

    ;; Let expression: (let-expr bind-values body-ir num-bindings env-offsets)
    ;; Must track current-offset properly for function calls in bindings/body
    ((has-tag? ir 'let-expr)
     (let* ((bind-values (cadr ir))
            (body-ir (caddr ir))
            (num-bindings (cadddr ir))
            (env-offsets (nth 4 ir))  ; Get environment offsets for this let's bindings
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1))
            ;; Track offset as we generate code: start after save x24 (1 instruction)
            (cursor (if current-offset (+ current-offset 1) nil))
            (accum (arm64-str 24 31 x24-slot)))  ; save x24 at start
       ;; Generate binding values with proper offset tracking
       (dolist (pair (mapcar #'list bind-values env-offsets))
         (let* ((val-ir (car pair))
                (offset (cadr pair))
                ;; Each binding: LDR x24 (1) + binding-code + SUB (1) + STR (1)
                (restore (arm64-ldr 24 31 x24-slot))
                (bind-cursor (if cursor (+ cursor 1) nil))  ; +1 for LDR x24
                (bind-code (codegen-expr val-ir runtime-addrs fn-offsets bind-cursor nested-depth))
                (store-code (append
                              (arm64-sub-imm 1 20 (* offset 8))  ; x1 = x20 - (offset * 8)
                              (arm64-str 0 1 0)))                ; Store at [x1 + 0]
                (block-instrs (+ 1 (count-instrs bind-code) 2)))  ; LDR + bind + SUB + STR
           (setf accum (append accum restore bind-code store-code))
           (when cursor
             (setf cursor (+ cursor block-instrs)))))
       ;; Restore x24 before body and generate body
       (let* ((restore (arm64-ldr 24 31 x24-slot))
              (body-cursor (if cursor (+ cursor 1) nil))  ; +1 for LDR x24
              (body-code (codegen-expr body-ir runtime-addrs fn-offsets body-cursor nested-depth)))
         (append accum restore body-code))))

    ;; Progn: evaluate each subexpression in order, return last result
    ;; Must preserve x24 across forms since funcalls may clobber it
    ((has-tag? ir 'progn)
     (let* ((exprs (cdr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1))
            (cursor (if current-offset (+ current-offset 1) nil))  ; +1 for save
            (accum (arm64-str 24 31 x24-slot)))  ; save x24 at start
       (loop for sub in exprs
             for idx from 0 do
               (let* ((restore (if (> idx 0)
                                   (arm64-ldr 24 31 x24-slot)  ; restore before each subsequent form
                                   nil))
                      ;; Account for restore instruction before chunk when idx > 0
                      (chunk-cursor (if (and cursor (> idx 0)) (+ cursor 1) cursor))
                      (chunk (codegen-expr sub runtime-addrs fn-offsets chunk-cursor nested-depth))
                      (instrs (+ (count-instrs restore) (count-instrs chunk))))
                 (setf accum (append accum restore chunk))
                 (when cursor
                   (setf cursor (+ cursor instrs)))))
       accum))

    ;; Lambda reference: build closure for compiled lambda
    ((has-tag? ir 'lambda-ref)
     (let* ((lambda-name (cadr ir))
            (fn-entry (assoc lambda-name fn-offsets))
            (fn-offset (if fn-entry (cadr fn-entry) 0))
            (captures (if fn-entry (caddr fn-entry) nil))
            (capture-count (length captures))
            (offset-bytes (* fn-offset 4))
            (code-slot (temp-slot-offset temp-depth))
            (env-slot (+ code-slot 8)))
       ;; Runtime table layout:
       ;;   [0] cons, [8] car, [16] cdr, [24] make-closure, [32] closure-code, [40] closure-env, [48] code base
       ;;   [56] make-vector, [64] vector-set, [72] vector-ref
       ;;   [80] make-string-from-vector, [88] make-symbol-from-string, [96] string-length-raw, [104] symbol-name
       ;; Package forms are folded at compile time; the runtime table stops at symbol-name.
       (append
         (arm64-ldr 9 19 48)              ; x9 = code base
         (arm64-load-addr 10 offset-bytes); x10 = offset bytes
         (arm64-add 0 9 10)               ; x0 = code base + offset
         (arm64-str 0 31 code-slot)       ; save code pointer
         ;; Allocate env vector if needed
         (if (= capture-count 0)
             (append
               (arm64-movz 1 0)           ; x1 = NIL
               (arm64-ldr 11 19 24)       ; make-closure
               (arm64-blr 11))
             (append
               (arm64-movz 0 capture-count) ; x0 = length
               (arm64-ldr 11 19 56)         ; x11 = make-vector
               (arm64-blr 11)               ; x0 = vector
               (arm64-str 0 31 env-slot)    ; save vector
               ;; Store captures
               (apply #'append
                      (mapcar (lambda (off idx)
                                (append
                                  (arm64-ldr 0 31 env-slot) ; x0 = vector
                                  (arm64-movz 1 idx)        ; x1 = index
                                  (arm64-sub-imm 2 20 (* off 8)) ; x2 = x20 - off*8
                                  (arm64-ldr 2 2 0)         ; x2 = captured value
                                  (arm64-ldr 11 19 64)      ; x11 = vector-set
                                  (arm64-blr 11)))
                              captures
                              (loop for i from 0 below capture-count collect i)))
               ;; Make closure
               (arm64-ldr 0 31 code-slot)  ; x0 = code pointer
               (arm64-ldr 1 31 env-slot)   ; x1 = env vector
               (arm64-ldr 11 19 24)        ; make-closure
               (arm64-blr 11))))))         ; x0 = closure

    ;; Call closure: evaluate fn-expr to closure, load code pointer, call with args
    ;; Args are stored to temp slots (not arg-spill area) to avoid clobbering by nested funcalls.
    ;; IMPORTANT: Args need CALLER's x24 for var-refs, not callee's env.
    ((has-tag? ir 'call-closure)
     (let* ((fn-ir (cadr ir))
            (arg-irs (caddr ir))
            (num-args (length arg-irs))
            ;; Stack slot layout (all relative to temp-depth):
            ;;   0: caller-x24  1: closure  2: code  3: callee-env
            ;;   4..(4+num-args-1): args
            ;;   (4+num-args)...: available for nested expressions
            (caller-x24-slot (temp-slot-offset temp-depth))
            (closure-slot (temp-slot-offset (+ temp-depth 1)))
            (code-slot (temp-slot-offset (+ temp-depth 2)))
            (callee-env-slot (temp-slot-offset (+ temp-depth 3)))
            (arg-base (+ temp-depth 4))  ; args start at slot 4
            (nested-depth (+ arg-base num-args))  ; nested exprs use slots after args
            (extra-count (max 0 (- num-args 5)))
            ;; fn-code uses nested-depth since slots 0-3 are reserved for our use
            (fn-code (codegen-expr fn-ir runtime-addrs fn-offsets
                                   (if current-offset (+ current-offset 1) nil)
                                   nested-depth))
            (setup (append
                     (arm64-str 24 31 caller-x24-slot)  ; save caller's x24 FIRST
                     fn-code                           ; closure in x0
                     (arm64-str 0 31 closure-slot)     ; save closure value
                     ;; Get code pointer via runtime helper
                     (arm64-ldr 9 19 32)               ; x9 = closure_code
                     (arm64-blr 9)                     ; x0 = code pointer
                     (arm64-str 0 31 code-slot)        ; save code pointer
                     ;; Get closure env and save to stack (DON'T set x24 yet!)
                     (arm64-ldr 0 31 closure-slot)     ; x0 = closure value
                     (arm64-ldr 9 19 40)               ; x9 = closure_env
                     (arm64-blr 9)                     ; x0 = env pointer
                     (arm64-str 0 31 callee-env-slot))) ; save callee's env for later
            (stage-code setup)
            (cursor (if current-offset
                        (+ current-offset (count-instrs stage-code))
                        nil)))
       ;; Stage args to temp slots (not arg-spill area!)
       ;; Each arg is stored to temp-slot-offset(arg-base + idx)
       ;; Before each arg, restore caller's x24 so var-refs work correctly
       (loop for arg-ir in arg-irs
             for idx from 0 do
               (let* ((arg-slot (temp-slot-offset (+ arg-base idx)))
                      (restore-x24 (arm64-ldr 24 31 caller-x24-slot))  ; restore caller's x24 (1 instr)
                      ;; Account for restore-x24 instruction before arg-code
                      (arg-cursor (if cursor (+ cursor 1) nil))
                      (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets arg-cursor nested-depth))
                      (store (arm64-str 0 31 arg-slot))  ; store to temp slot, not arg-spill
                      (block (append restore-x24 arg-code store))
                      (block-len (count-instrs block)))
                 (setf stage-code (append stage-code block))
                 (when cursor (incf cursor block-len))))
       (let* (;; Load args from temp slots into registers x0-x4
              (load-code
                (cond
                  ((= num-args 0) nil)
                  ((= num-args 1)
                   (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0))))
                  ((= num-args 2)
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))))
                  ((= num-args 3)
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))
                     (arm64-ldr 2 31 (temp-slot-offset (+ arg-base 2)))))
                  ((= num-args 4)
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))
                     (arm64-ldr 2 31 (temp-slot-offset (+ arg-base 2)))
                     (arm64-ldr 3 31 (temp-slot-offset (+ arg-base 3)))))
                  (t
                   (append
                     (arm64-ldr 0 31 (temp-slot-offset (+ arg-base 0)))
                     (arm64-ldr 1 31 (temp-slot-offset (+ arg-base 1)))
                     (arm64-ldr 2 31 (temp-slot-offset (+ arg-base 2)))
                     (arm64-ldr 3 31 (temp-slot-offset (+ arg-base 3)))
                     (arm64-ldr 4 31 (temp-slot-offset (+ arg-base 4)))))))
              ;; For >5 args, point x25 to the extra args in temp slots
              (set-extra-ptr (if (> extra-count 0)
                                 (arm64-sub-imm 25 31 (- (temp-slot-offset (+ arg-base 5))))
                                 (arm64-movz 25 0)))
              (arg-count-code (arm64-movz 23 num-args))
              ;; Set x24 to callee's env for the actual call
              (restore-env (arm64-ldr 24 31 callee-env-slot))
              (pre-call (append stage-code load-code set-extra-ptr arg-count-code restore-env (arm64-ldr 9 31 code-slot))))
         (append
           pre-call
           (arm64-blr 9)                      ; call
           ;; Restore caller's x24 after call returns - callee had its own env in x24
           (arm64-ldr 24 31 caller-x24-slot)))))

    ;; Function call: (call-fn name arg-irs)
    ((has-tag? ir 'call-fn)
     (let* ((fn-name (cadr ir))
            (arg-irs (caddr ir))
            (num-args (length arg-irs))
            (extra-count (max 0 (- num-args 5)))
            (max-capacity (1- *max-arg-spill-count*))
            (fn-entry (assoc fn-name fn-offsets))
            (fn-offset (if fn-entry (cadr fn-entry) 0))
            ;; Save x24 to temp slot so we can restore it before each arg evaluation
            ;; This handles the case where an arg contains a funcall that clobbers x24
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1)))
       (when (> num-args max-capacity)
         (error "call-fn ~A has ~A args; exceeds spill capacity ~A" fn-name num-args max-capacity))
       (let* ((cursor (if current-offset (+ current-offset 3) nil))  ; +3 for save x24 + x27 setup + first restore
              (stage-code (append
                            (arm64-str 24 31 x24-slot)   ; save caller's x24
                            (arm64-add-imm 27 31 0))))   ; x27 = sp for stable base
         ;; Stage all arguments in order into the spill area using x27 as a stable base
         ;; Before each arg, restore x24 in case previous arg clobbered it
         (loop for arg-ir in arg-irs
               for idx from 0 do
                 (let* ((restore-x24 (arm64-ldr 24 31 x24-slot))  ; restore x24 before each arg
                        (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets cursor nested-depth))
                        (store (arm64-str 0 27 (arg-spill-offset idx)))
                        (block (append restore-x24 arg-code store))
                        (block-len (count-instrs block)))
                   (setf stage-code (append stage-code block))
                   (when cursor (incf cursor block-len))))
         (let* ((load-code
                  (cond
                    ((= num-args 0) nil)
                    ((= num-args 1)
                     (arm64-ldr 0 27 (arg-spill-offset 0)))
                    ((= num-args 2)
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))))
                    ((= num-args 3)
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))
                       (arm64-ldr 2 27 (arg-spill-offset 2))))
                    ((= num-args 4)
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))
                       (arm64-ldr 2 27 (arg-spill-offset 2))
                       (arm64-ldr 3 27 (arg-spill-offset 3))))
                    (t
                     (append
                       (arm64-ldr 0 27 (arg-spill-offset 0))
                       (arm64-ldr 1 27 (arg-spill-offset 1))
                       (arm64-ldr 2 27 (arg-spill-offset 2))
                       (arm64-ldr 3 27 (arg-spill-offset 3))
                       (arm64-ldr 4 27 (arg-spill-offset 4))))))
                (set-extra-ptr (if (> extra-count 0)
                                   (arm64-add-imm 25 27 (arg-spill-offset 5))
                                   (arm64-movz 25 0)))
                (arg-count-code (arm64-movz 23 num-args))
                (pre-call (append stage-code load-code set-extra-ptr arg-count-code))
                 (current-pc (if current-offset
                                 (+ current-offset (count-instrs pre-call))
                                 0))
                 (branch-offset (- fn-offset current-pc)))
            (append
              pre-call
              (arm64-bl branch-offset)
              ;; Restore x24 after call returns - the called function may have clobbered it
              (arm64-ldr 24 31 x24-slot)))))) 

    ;; Division: (div left right) - fixnum helper
    ((has-tag? ir 'div)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (temp-offset (temp-slot-offset temp-depth))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset temp-depth))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                      (if current-offset
                                          (+ current-offset (count-instrs left-code) 1)
                                          nil)
                                      (+ temp-depth 1))))
       (append left-code
               (arm64-str 0 31 temp-offset)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 temp-offset)
               (arm64-ldr 9 19 8)  ; habu_div at slot 1
               (arm64-blr 9))))

    ;; Modulo: (mod left right)
    ((has-tag? ir 'mod)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (temp-offset (temp-slot-offset temp-depth))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset temp-depth))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                      (if current-offset
                                          (+ current-offset (count-instrs left-code) 1)
                                          nil)
                                      (+ temp-depth 1))))
       (append left-code
               (arm64-str 0 31 temp-offset)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 temp-offset)
               (arm64-ldr 9 19 16) ; habu_mod at slot 2
               (arm64-blr 9))))

    ;; Remainder: (rem left right)
    ((has-tag? ir 'rem)
     (let* ((left-ir (cadr ir))
            (right-ir (caddr ir))
            (temp-offset (temp-slot-offset temp-depth))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets current-offset temp-depth))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets
                                      (if current-offset
                                          (+ current-offset (count-instrs left-code) 1)
                                          nil)
                                      (+ temp-depth 1))))
       (append left-code
               (arm64-str 0 31 temp-offset)
               right-code
               (arm64-mov 1 0)
               (arm64-ldr 0 31 temp-offset)
               (arm64-ldr 9 19 24) ; habu_rem at slot 3
               (arm64-blr 9))))

    ;; Default: zero
    (t (arm64-movz 0 0))))

(defun compile-expr (expr env fenv)
  "Enhanced IR generation: literals, arithmetic operations"
  ;; First, expand macros if expr is a macro call
  (let ((expanded (if (and (consp expr) (symbolp (car expr)))
                      (macroexpand-habu expr)
                      expr)))
    (compile-expr-internal expanded env fenv)))

(defun compile-expr-internal (expr env fenv)
  "Internal compile-expr after macro expansion."
  (cond
    ;; Fixnum literal
    ((fixnum? expr)
     (list 'lit expr))

    ;; Character literal - convert to its code
    ((characterp expr)
     (list 'lit (char-code expr)))

    ;; String literal - self-evaluating
    ((stringp expr)
     (cons 'string-lit (string->char-codes expr)))

    ;; Symbol (variable or keyword)
    ((symbol? expr)
     ;; Keywords are self-evaluating - compile as symbol literals
     (if (keywordp expr)
         (list 'symbol-lit (symbol-name expr))
         ;; Regular symbols are variable references
         (let ((off (env-lookup expr env)))
           (if off (list 'var off) (list 'lit 0)))))

    ;; List (function call or special form)
    ((consp expr)
     (let ((op (car expr)))
       (cond
         ;; User-defined functions take precedence over built-ins
         ;; This allows shadowing built-in names like length, member, etc.
         ((and (symbolp op) fenv (assoc op fenv))
          (let ((args (cdr expr)))
            (list 'call-fn op
                  (mapcar (lambda (arg) (compile-expr arg env fenv)) args))))

         ;; Addition (variadic: fold (+ a b c) -> (+ (+ a b) c))
         ((eq op '+)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))           ; (+) => 0
              ((null (cdr args))                     ; (+ a) => a
               (compile-expr (car args) env fenv))
              ((null (cddr args))                    ; (+ a b)
               (list 'add
                     (compile-expr (car args) env fenv)
                     (compile-expr (cadr args) env fenv)))
              (t                                     ; (+ a b c ...) => (+ (+ a b) c ...)
               (compile-expr (cons '+ (cons (list '+ (car args) (cadr args))
                                           (cddr args)))
                            env fenv)))))

         ;; Subtraction (variadic: fold (- a b c) -> (- (- a b) c))
         ((eq op '-)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))           ; (-) => 0
              ((null (cdr args))                     ; (- a) => negate
               (list 'sub (list 'lit 0) (compile-expr (car args) env fenv)))
              ((null (cddr args))                    ; (- a b)
               (list 'sub
                     (compile-expr (car args) env fenv)
                     (compile-expr (cadr args) env fenv)))
              (t                                     ; (- a b c ...) => (- (- a b) c ...)
               (compile-expr (cons '- (cons (list '- (car args) (cadr args))
                                           (cddr args)))
                            env fenv)))))

         ;; Multiplication (variadic: fold (* a b c) -> (* (* a b) c))
         ((eq op '*)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 1))           ; (*) => 1
              ((null (cdr args))                     ; (* a) => a
               (compile-expr (car args) env fenv))
              ((null (cddr args))                    ; (* a b)
               (list 'mul
                     (compile-expr (car args) env fenv)
                     (compile-expr (cadr args) env fenv)))
              (t                                     ; (* a b c ...) => (* (* a b) c ...)
               (compile-expr (cons '* (cons (list '* (car args) (cadr args))
                                           (cddr args)))
                            env fenv)))))

         ;; Division
         ((eq op '/)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'div
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Modulo
         ((eq op 'mod)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'mod
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Remainder
         ((eq op 'rem)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'rem
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Exponentiation (integer only, non-negative exponent)
         ;; Transform: (expt base exp) -> (labels ((pow (b n) (if (= n 0) 1 (* b (pow b (- n 1)))))) (pow base exp))
         ((eq op 'expt)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((base-expr (cadr expr))
                    (exp-expr (caddr expr)))
                (compile-expr
                 `(labels ((expt-iter (b n acc)
                             (if (= n 0)
                                 acc
                                 (expt-iter b (- n 1) (* acc b)))))
                    (expt-iter ,base-expr ,exp-expr 1))
                 env fenv))
              (list 'lit 0)))

         ;; Equality comparison
         ((eq op '=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Less than
         ((eq op '<)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-lt
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Greater than
         ((eq op '>)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-gt
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Less than or equal
         ((eq op '<=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-le
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Greater than or equal
         ((eq op '>=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-ge
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Not equal (standard Lisp /=)
         ((eq op '/=)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-ne
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Let binding
         ((eq op 'let)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((bindings (cadr expr))
                     (body-forms (cddr expr))
                     ;; Wrap multiple body forms in progn
                     (body (if (cdr body-forms)
                               (cons 'progn body-forms)
                               (car body-forms)))
                     ;; Extract binding pairs
                     (bind-pairs (mapcar (lambda (b)
                                          (if (consp b)
                                              (list (car b)
                                                    (compile-expr (cadr b) env fenv))
                                              (list b (list 'lit 0))))
                                        bindings))
                     ;; Create new environment with binding names
                     (bind-names (mapcar #'car bind-pairs))
                     (bind-values (mapcar #'cadr bind-pairs))
                     (new-env (env-extend (mapcar #'list bind-names) env))
                     ;; Get the offsets for each binding
                     (env-offsets (mapcar (lambda (name)
                                           (env-lookup name new-env))
                                         bind-names))
                     ;; Compile body in new environment
                     (body-ir (compile-expr body new-env fenv)))
                (list 'let-expr bind-values body-ir (length bindings) env-offsets))
              (list 'lit 0)))

         ;; Let* binding (sequential)
         ((eq op 'let*)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((bindings (cadr expr))
                     (body-forms (cddr expr))
                     ;; Wrap multiple body forms in progn
                     (body (if (cdr body-forms)
                               (cons 'progn body-forms)
                               (car body-forms))))
                (if (null bindings)
                    (compile-expr body env fenv)
                    ;; Transform to nested lets
                    (compile-expr
                     `(let (,(car bindings))
                        (let* ,(cdr bindings) ,body))
                     env fenv)))
              (list 'lit 0)))

         ;; Setq (variable mutation)
         ((eq op 'setq)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((var (cadr expr))
                     (val-expr (caddr expr))
                     (offset (env-lookup var env)))
                (if offset
                    (list 'set-var offset (compile-expr val-expr env fenv))
                    (list 'lit 0))) ; unknown var
              (list 'lit 0)))

         ;; Setf (generalized assignment)
         ((eq op 'setf)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((place (cadr expr))
                    (val-expr (caddr expr)))
                (cond
                  ;; Simple variable: same as setq
                  ((symbolp place)
                   (let ((offset (env-lookup place env)))
                     (if offset
                         (list 'set-var offset (compile-expr val-expr env fenv))
                         (list 'lit 0))))
                  ;; (setf (car x) val) -> (set-car x val)
                  ((and (consp place) (eq (car place) 'car) (consp (cdr place)))
                   (list 'setcar-call
                         (compile-expr (cadr place) env fenv)
                         (compile-expr val-expr env fenv)))
                  ;; (setf (cdr x) val) -> (set-cdr x val)
                  ((and (consp place) (eq (car place) 'cdr) (consp (cdr place)))
                   (list 'setcdr-call
                         (compile-expr (cadr place) env fenv)
                         (compile-expr val-expr env fenv)))
                  ;; (setf (gethash key ht) val) -> (puthash key val ht)
                  ((and (consp place) (op= (car place) "GETHASH") (consp (cdr place)) (consp (cddr place)))
                   (let ((key (cadr place))
                         (ht (caddr place)))
                     (list 'puthash-call
                           (compile-expr key env fenv)
                           (compile-expr val-expr env fenv)
                           (compile-expr ht env fenv))))
                  ;; (setf (vector-ref vec idx) val) -> (vector-set vec idx val)
                  ((and (consp place) (op= (car place) "VECTOR-REF") (consp (cdr place)) (consp (cddr place)))
                   (list 'vector-set-call
                         (compile-expr (cadr place) env fenv)
                         (compile-expr (caddr place) env fenv)
                         (compile-expr val-expr env fenv)))
                  ;; (setf (struct-accessor obj) val) -> (vector-set obj slot-idx val)
                  ((and (consp place) (consp (cdr place))
                        (let ((accessor-entry (assoc (car place) *struct-accessors*)))
                          (when accessor-entry
                            (let ((slot-idx (cdr accessor-entry))
                                  (obj-expr (cadr place)))
                              (return-from compile-expr-internal
                                (list 'vector-set-call
                                      (compile-expr obj-expr env fenv)
                                      (list 'lit slot-idx)
                                      (compile-expr val-expr env fenv)))))))
                   ;; Handled by the return-from above
                   (list 'lit 0))
                  ;; Other places not yet supported
                  (t (list 'lit 0))))
              (list 'lit 0)))

         ;; Macrolet (local macros)
         ;; (macrolet ((name (args) body) ...) expr)
         ;; Temporarily extends *macro-env* for the body
         ((eq op 'macrolet)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((macro-defs (cadr expr))
                     (body-forms (cddr expr))
                     (body (if (cdr body-forms)
                               (cons 'progn body-forms)
                               (car body-forms)))
                     ;; Save current macro env
                     (saved-macro-env *macro-env*))
                ;; Register local macros
                (dolist (def macro-defs)
                  (let* ((name (car def))
                         (params (cadr def))
                         (macro-body (caddr def))
                         (expander (eval `(lambda ,params ,macro-body))))
                    (register-macro name expander)))
                ;; Compile body with extended macro env
                (let ((result (compile-expr body env fenv)))
                  ;; Restore macro env
                  (setf *macro-env* saved-macro-env)
                  result))
              (list 'lit 0)))

         ;; Labels (local recursive functions)
         ;; Transform using cons cells as mutable boxes for proper closure semantics:
         ;; (labels ((f (x) body)) expr) ->
         ;;   (let* ((f-box (cons nil nil)))
         ;;     (progn (setf (car f-box) (lambda (x) body'))
         ;;            expr'))
         ;; where body' and expr' transform (f args...) to (funcall (car f-box) args...)
         ((eq op 'labels)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((fn-defs (cadr expr))
                     (body-forms (cddr expr))
                     ;; Wrap multiple body forms in progn
                     (body (if (cdr body-forms)
                               (cons 'progn body-forms)
                               (car body-forms)))
                     (fn-names (mapcar #'car fn-defs))
                     ;; Create box names for each function
                     (box-names (mapcar (lambda (name)
                                          (intern (concatenate 'string (symbol-name name) "-BOX")))
                                        fn-names))
                     (name-to-box (mapcar #'cons fn-names box-names))
                     ;; Transform calls to local functions to use (car box)
                     (transform-calls (lambda (e)
                                        (labels ((xform (x)
                                                   (cond
                                                     ((not (consp x)) x)
                                                     ;; (f args...) -> (funcall (car f-box) args...)
                                                     ((and (symbolp (car x))
                                                           (assoc (car x) name-to-box))
                                                      (let ((box (cdr (assoc (car x) name-to-box))))
                                                        `(funcall (car ,box) ,@(mapcar #'xform (cdr x)))))
                                                     (t (mapcar #'xform x)))))
                                          (xform e))))
                     ;; Build let* bindings: (f-box (cons nil nil))
                     (let-bindings (mapcar (lambda (box) (list box '(cons #x0 #x0))) box-names))
                     ;; Build setf assignments: (setf (car f-box) (lambda ...))
                     (setf-forms (mapcar (lambda (def)
                                           (let* ((name (car def))
                                                  (params (cadr def))
                                                  (fn-body-forms (cddr def))
                                                  ;; Wrap multiple body forms in progn
                                                  (fn-body (if (cdr fn-body-forms)
                                                               (cons 'progn fn-body-forms)
                                                               (car fn-body-forms)))
                                                  (box (cdr (assoc name name-to-box)))
                                                  (xformed-body (funcall transform-calls fn-body)))
                                             `(setf (car ,box) (lambda ,params ,xformed-body))))
                                         fn-defs))
                     ;; Transform body calls
                     (xformed-body (funcall transform-calls body))
                     ;; Build: (let* ((f-box (cons nil nil)) ...) (progn (setf (car f-box) ...) ... body))
                     (transformed `(let* ,let-bindings
                                     (progn ,@setf-forms ,xformed-body))))
                (compile-expr transformed env fenv))
              (list 'lit 0)))

         ;; Flet (local non-recursive functions)
         ;; Transform: (flet ((f (x) body)) expr) ->
         ;;            (let ((f (lambda (x) body))) expr')
         ;; where expr' has calls (f ...) transformed to (funcall f ...)
         ((eq op 'flet)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((fn-defs (cadr expr))
                     (body-forms (cddr expr))
                     ;; Wrap multiple body forms in progn
                     (body (if (cdr body-forms)
                               (cons 'progn body-forms)
                               (car body-forms)))
                     (fn-names (mapcar #'car fn-defs))
                     ;; Transform calls to local functions into funcall
                     (transform-calls (lambda (e)
                                        (labels ((xform (x)
                                                   (cond
                                                     ((not (consp x)) x)
                                                     ((and (symbolp (car x))
                                                           (member (car x) fn-names))
                                                      (cons 'funcall (cons (car x) (mapcar #'xform (cdr x)))))
                                                     (t (mapcar #'xform x)))))
                                          (xform e))))
                     ;; Build let bindings with lambdas (no need for nil + setq since non-recursive)
                     (let-bindings (mapcar (lambda (def)
                                             (let* ((name (car def))
                                                    (params (cadr def))
                                                    (fn-body-forms (cddr def))
                                                    ;; Wrap multiple body forms in progn
                                                    (fn-body (if (cdr fn-body-forms)
                                                                 (cons 'progn fn-body-forms)
                                                                 (car fn-body-forms))))
                                               `(,name (lambda ,params ,fn-body))))
                                           fn-defs))
                     ;; Transform body calls
                     (xformed-body (funcall transform-calls body))
                     ;; Build: (let ((f (lambda ...)) ...) body')
                     (transformed `(let ,let-bindings ,xformed-body)))
                (compile-expr transformed env fenv))
              (list 'lit 0)))

         ;; Incf (increment in place)
         ((eq op 'incf)
          (if (consp (cdr expr))
              (let* ((place (cadr expr))
                     (delta (if (consp (cddr expr)) (caddr expr) 1)))
                (if (symbolp place)
                    (compile-expr `(setq ,place (+ ,place ,delta)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Decf (decrement in place)
         ((eq op 'decf)
          (if (consp (cdr expr))
              (let* ((place (cadr expr))
                     (delta (if (consp (cddr expr)) (caddr expr) 1)))
                (if (symbolp place)
                    (compile-expr `(setq ,place (- ,place ,delta)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Push (add to front of list variable)
         ((eq op 'push)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (place (caddr expr)))
                (if (symbolp place)
                    (compile-expr `(setq ,place (cons ,item ,place)) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Pop (remove and return first element)
         ((eq op 'pop)
          (if (consp (cdr expr))
              (let ((place (cadr expr)))
                (if (symbolp place)
                    ;; Returns car, then sets to cdr
                    ;; Need progn: (prog1 (car place) (setq place (cdr place)))
                    ;; For now, just return car (mutation happens but value is cdr)
                    (compile-expr `(car ,place) env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Quote
         ((eq op 'quote)
          (if (consp (cdr expr))
              (quote->ir (cadr expr))
              (list 'lit #x0)))

         ;; Function - (function name) or (function (lambda ...))
         ((eq op 'function)
          (if (consp (cdr expr))
              (let ((fn-arg (cadr expr)))
                (cond
                  ;; (function (lambda ...)) - compile the lambda
                  ((and (consp fn-arg) (eq (car fn-arg) 'lambda))
                   (compile-expr fn-arg env fenv))
                  ;; (function name) - look up in fenv and create lambda-ref
                  ((symbolp fn-arg)
                   (if (and fenv (assoc fn-arg fenv))
                       ;; It's a user-defined function - create lambda-ref
                       (list 'lambda-ref fn-arg)
                       ;; Not found - return 0
                       (list 'lit 0)))
                  (t (list 'lit 0))))
              (list 'lit 0)))

         ;; Progn
         ((eq op 'progn)
          (let ((body (cdr expr)))
            (if body
                (cons 'progn (mapcar (lambda (form) (compile-expr form env fenv)) body))
                (list 'lit #x0))))

         ;; Prog1: evaluate forms, return first result
         ((eq op 'prog1)
          (if (consp (cdr expr))
              (let ((first-form (cadr expr))
                    (rest-forms (cddr expr)))
                (compile-expr
                 `(let ((result ,first-form))
                    ,@rest-forms
                    result)
                 env fenv))
              (list 'lit 0)))

         ;; Prog2: evaluate forms, return second result
         ((eq op 'prog2)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((first-form (cadr expr))
                    (second-form (caddr expr))
                    (rest-forms (cdddr expr)))
                (compile-expr
                 `(progn
                    ,first-form
                    (let ((result ,second-form))
                      ,@rest-forms
                      result))
                 env fenv))
              (list 'lit 0)))

         ;; Conditional
         ((eq op 'if)
          (if (and (consp (cdr expr))
                   (consp (cddr expr))
                   (consp (cdddr expr)))
              (list 'if-expr
                    (compile-expr (cadr expr) env fenv)   ; test
                    (compile-expr (caddr expr) env fenv)  ; then
                    (compile-expr (cadddr expr) env fenv)) ; else
              ;; Two-arg if: (if test then) -> (if test then nil)
              (if (and (consp (cdr expr)) (consp (cddr expr)))
                  (list 'if-expr
                        (compile-expr (cadr expr) env fenv)
                        (compile-expr (caddr expr) env fenv)
                        (list 'lit #x0))
                  (list 'lit 0))))

         ;; Not (logical negation)
         ((eq op 'not)
          (if (consp (cdr expr))
              (list 'if-expr
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit 0)   ; if true -> nil
                    (list 'lit 1))  ; if false -> t (1, will be tagged to #x10)
              (list 'lit 1))) ; (not) with no args -> t

         ;; Block - establishes a named exit point
         ;; (block name body...) - body can use (return-from name value) to exit early
         ;; Transforms to: (let (($result nil) ($exited nil))
         ;;                  (progn form1-guarded form2-guarded ...)
         ;;                  $result)
         ;; where form-guarded = (if (not $exited) (setq $result form))
         ;; return-from transforms to: (progn (setq $result value) (setq $exited 1))
         ((eq op 'block)
          (if (and (consp (cdr expr)) (symbolp (cadr expr)))
              (let* ((block-name (cadr expr))
                     (body (cddr expr))
                     (block-id (incf *block-counter*))
                     (result-var (intern (format nil "$BLOCK-~A-RESULT" block-id)))
                     (exited-var (intern (format nil "$BLOCK-~A-EXITED" block-id)))
                     (old-block-env *block-env*)
                     ;; Add block info to environment
                     (new-block-env (cons (list block-name block-id result-var exited-var) old-block-env)))
                ;; Set block env for duration of compilation
                (setq *block-env* new-block-env)
                (unwind-protect
                    ;; Wrap each body form with exit check and transform return-from
                    ;; Check original forms for return-from before transforming
                    (let* ((guarded-forms
                            (mapcar (lambda (orig-form)
                                      (let ((xformed (transform-return-from orig-form block-name result-var exited-var)))
                                        (if (contains-return-from-p orig-form block-name)
                                            ;; Has return-from - don't wrap in setq, transform handles result
                                            `(if (not ,exited-var) ,xformed)
                                            ;; Normal form - set result
                                            `(if (not ,exited-var) (setq ,result-var ,xformed)))))
                                    body))
                           ;; Build transformed expression
                           ;; Note: let only takes ONE body form, so wrap in progn
                           (transformed `(let ((,result-var nil)
                                               (,exited-var nil))
                                           (progn ,@guarded-forms ,result-var))))
                      (compile-expr transformed env fenv))
                  ;; Restore old block env
                  (setq *block-env* old-block-env)))
              (list 'lit 0)))

         ;; Return-from - exit from a named block with a value
         ;; Transforms to: (progn (setq $result value) (setq $exited 1))
         ((eq op 'return-from)
          (if (and (consp (cdr expr)) (symbolp (cadr expr)))
              (let* ((block-name (cadr expr))
                     (value-expr (if (consp (cddr expr)) (caddr expr) nil))
                     (entry (assoc block-name *block-env*)))
                (if entry
                    (let* ((result-var (caddr entry))
                           (exited-var (cadddr entry))
                           (transformed `(progn
                                           (setq ,result-var ,(or value-expr nil))
                                           (setq ,exited-var 1))))
                      (compile-expr transformed env fenv))
                    ;; Block not found - just evaluate value
                    (if value-expr
                        (compile-expr value-expr env fenv)
                        (list 'lit 0))))
              (list 'lit 0)))

         ;; Catch - establishes a catch point with tag
         ;; (catch tag body...) - if body throws to tag, return the thrown value
         ;; Implementation: similar to block but with dynamic tag
         ((eq op 'catch)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((tag-expr (cadr expr))
                     (body (cddr expr))
                     (catch-id (incf *catch-counter*))
                     (result-var (intern (format nil "$CATCH-~A-RESULT" catch-id)))
                     (exited-var (intern (format nil "$CATCH-~A-EXITED" catch-id)))
                     (tag-var (intern (format nil "$CATCH-~A-TAG" catch-id)))
                     (old-catch-env *catch-env*)
                     ;; Store tag-var and vars for throw to find
                     (new-catch-env (cons (list tag-var catch-id result-var exited-var)
                                          old-catch-env)))
                ;; Set catch env for duration of compilation
                (setq *catch-env* new-catch-env)
                (unwind-protect
                    ;; Transform to: (let ((tag <tag-expr>) (result nil) (exited nil))
                    ;;                 (progn body-forms) result)
                    ;; throw will set result and exited when tag matches
                    (let* ((guarded-forms
                            (mapcar (lambda (form)
                                      `(if (not ,exited-var) (setq ,result-var ,form)))
                                    body))
                           (transformed `(let ((,tag-var ,tag-expr)
                                               (,result-var nil)
                                               (,exited-var nil))
                                           (progn ,@guarded-forms ,result-var))))
                      (compile-expr transformed env fenv))
                  (setq *catch-env* old-catch-env)))
              (list 'lit 0)))

         ;; Throw - throws value to matching catch point
         ;; (throw tag value) - find enclosing catch with matching tag, return value from it
         ;; When throwing to an outer catch, also set exited flags for all inner catches
         ((eq op 'throw)
          (if (consp (cdr expr))
              (let* ((tag-expr (cadr expr))
                     (value-expr (if (consp (cddr expr)) (caddr expr) nil)))
                ;; Generate code to check each catch tag at runtime
                ;; When match found, set ALL exited flags from innermost to matching catch
                (if *catch-env*
                    (let ((throw-val-var (intern (format nil "$THROW-VAL-~A" (incf *catch-counter*)))))
                      ;; Build nested if checking each catch
                      ;; catches-so-far accumulates catches we need to exit when match found
                      (labels ((build-checks (catches catches-so-far)
                                 (if (null catches)
                                     ;; No matching catch - return value (error in full impl)
                                     throw-val-var
                                     (let* ((catch-entry (car catches))
                                            (tag-var (car catch-entry))
                                            (result-var (caddr catch-entry))
                                            (exited-var (cadddr catch-entry))
                                            (all-exits (cons catch-entry catches-so-far))
                                            ;; Generate setq for all exited flags
                                            (exit-setqs (mapcar (lambda (c) `(setq ,(cadddr c) 1))
                                                                all-exits)))
                                       `(if (eq ,tag-var ,tag-expr)
                                            (progn (setq ,result-var ,throw-val-var)
                                                   ,@exit-setqs
                                                   ,result-var)
                                            ,(build-checks (cdr catches) all-exits))))))
                        (compile-expr
                         `(let ((,throw-val-var ,(or value-expr nil)))
                            ,(build-checks *catch-env* nil))
                         env fenv)))
                    ;; No catch in scope - just evaluate value
                    (if value-expr
                        (compile-expr value-expr env fenv)
                        (list 'lit 0))))
              (list 'lit 0)))

         ;; Tagbody - establishes tags for go jumps
         ;; (tagbody {tag | form}*) - tags are symbols/integers, forms are executed
         ;; go jumps to a tag, execution continues from there
         ;; Returns nil
         ;; Implementation: transform to labels where each tag is a function
         ((eq op 'tagbody)
          (let* ((body (cdr expr))
                 ;; Parse body into segments: list of (tag . forms)
                 ;; First segment may have no explicit tag (use $START)
                 (segments nil)
                 (current-tag (gensym "TAGBODY-START"))
                 (current-forms nil))
            ;; Scan body, collecting forms and starting new segments at tags
            (dolist (item body)
              (cond
                ;; Tag (symbol or integer)
                ((or (symbolp item) (integerp item))
                 ;; Save current segment
                 (push (cons current-tag (nreverse current-forms)) segments)
                 (setq current-tag item)
                 (setq current-forms nil))
                ;; Form
                (t
                 (push item current-forms))))
            ;; Save final segment
            (push (cons current-tag (nreverse current-forms)) segments)
            (setq segments (nreverse segments))
            ;; Build labels with each tag as a function
            ;; Each function executes its forms then falls through to next tag
            ;; Last tag returns nil
            (let* ((tag-names (mapcar #'car segments))
                   (tag-fns (mapcar (lambda (s) (gensym (format nil "TAG-~A-" (car s)))) segments))
                   (tag-map (mapcar #'cons tag-names tag-fns))
                   ;; Build function definitions
                   (fn-defs
                    (loop for seg in segments
                          for fn-name in tag-fns
                          for next-fn in (append (cdr tag-fns) (list nil))
                          collect
                          (let* ((tag (car seg))
                                 (forms (cdr seg))
                                 ;; Transform go calls in forms to calls to target tag fn
                                 (xformed-forms
                                  (mapcar (lambda (f)
                                            (transform-go f tag-map))
                                          forms)))
                            ;; Truncate at first go call (dead code elimination)
                            ;; Only add fallthrough if no go is present
                            (multiple-value-bind (truncated has-go)
                                (truncate-at-go xformed-forms tag-fns)
                              (let* ((body-forms (cond
                                                   ;; Has go: don't add fallthrough
                                                   (has-go truncated)
                                                   ;; No go, has next: add fallthrough
                                                   (next-fn (append truncated (list (list next-fn))))
                                                   ;; No go, last segment: return nil
                                                   (t (append truncated (list nil)))))
                                     (body (if (cdr body-forms)
                                               `(progn ,@body-forms)
                                               (or (car body-forms) nil))))
                                `(,fn-name () ,body))))))
                   ;; Start by calling first tag function
                   (start-call (list (car tag-fns)))
                   ;; Build complete labels form
                   (transformed `(labels ,fn-defs ,start-call)))
              (compile-expr transformed env fenv))))

         ;; Go - jump to a tag within enclosing tagbody
         ;; Handled by transform-go during tagbody compilation
         ;; If we see a bare (go tag) here, it's an error (no enclosing tagbody)
         ((eq op 'go)
          (list 'lit 0))  ; Error: go outside tagbody

         ;; Cond (multi-way conditional) - transforms to nested if
         ((eq op 'cond)
          (labels ((expand-cond (clauses)
                     (if (null clauses)
                         (list 'lit #x0)  ; no clause matched -> nil
                         (let* ((clause (car clauses))
                                (test (car clause))
                                (body (cdr clause)))
                           (cond
                             ;; (t body...) or (otherwise body...) - default clause
                             ((or (eq test t) (eq test 'otherwise))
                              (if body
                                  (if (cdr body)
                                      (compile-expr (cons 'progn body) env fenv)
                                      (compile-expr (car body) env fenv))
                                  (list 'lit 1))) ; bare t -> t (1, tags to #x10)
                             ;; Empty body: return test value if true
                             ((null body)
                              (let ((test-ir (compile-expr test env fenv)))
                                (list 'if-expr test-ir test-ir (expand-cond (cdr clauses)))))
                             ;; Normal clause with body
                             (t
                              (list 'if-expr
                                    (compile-expr test env fenv)
                                    (if (cdr body)
                                        (compile-expr (cons 'progn body) env fenv)
                                        (compile-expr (car body) env fenv))
                                    (expand-cond (cdr clauses)))))))))
            (expand-cond (cdr expr))))

         ;; Case (multi-way conditional by key comparison)
         ;; (case keyform (key1 body1) ((key2 key3) body2) (otherwise default))
         ((eq op 'case)
          (if (consp (cdr expr))
              (let ((keyform (cadr expr))
                    (clauses (cddr expr)))
                (compile-expr
                 `(let ((key ,keyform))
                    (cond ,@(mapcar (lambda (clause)
                                      (let ((keys (car clause))
                                            (body (cdr clause)))
                                        (cond
                                          ;; otherwise/t clause
                                          ((or (eq keys 'otherwise) (eq keys t))
                                           `(t ,@body))
                                          ;; Multiple keys
                                          ((consp keys)
                                           `((or ,@(mapcar (lambda (k) `(eql key ',k)) keys))
                                             ,@body))
                                          ;; Single key
                                          (t `((eql key ',keys) ,@body)))))
                                    clauses)))
                 env fenv))
              (list 'lit 0)))

         ;; Ecase (case with error on no match) - for now same as case
         ((eq op 'ecase)
          (if (consp (cdr expr))
              (let ((keyform (cadr expr))
                    (clauses (cddr expr)))
                ;; Transform ecase to case with error default
                (compile-expr
                 `(case ,keyform
                    ,@clauses
                    (otherwise (error "ECASE: no matching clause")))
                 env fenv))
              (list 'lit 0)))

         ;; Typecase (dispatch based on type)
         ;; (typecase expr (type1 body1) (type2 body2) ...)
         ((eq op 'typecase)
          (if (consp (cdr expr))
              (let ((keyform (cadr expr))
                    (clauses (cddr expr)))
                (compile-expr
                 `(let ((obj ,keyform))
                    (cond ,@(mapcar (lambda (clause)
                                      (let ((type-spec (car clause))
                                            (body (cdr clause)))
                                        (cond
                                          ((or (eq type-spec 'otherwise) (eq type-spec t))
                                           `(t ,@body))
                                          ((eq type-spec 'cons) `((consp obj) ,@body))
                                          ((eq type-spec 'list) `((listp obj) ,@body))
                                          ((eq type-spec 'null) `((null obj) ,@body))
                                          ((eq type-spec 'symbol) `((symbolp obj) ,@body))
                                          ((eq type-spec 'fixnum) `((numberp obj) ,@body))
                                          ((eq type-spec 'integer) `((numberp obj) ,@body))
                                          ((eq type-spec 'number) `((numberp obj) ,@body))
                                          ((eq type-spec 'string) `((stringp obj) ,@body))
                                          ((eq type-spec 'vector) `((vectorp obj) ,@body))
                                          ((eq type-spec 'function) `((functionp obj) ,@body))
                                          ((eq type-spec 'atom) `((atom obj) ,@body))
                                          (t `(nil ,@body)))))  ; Unknown type - never match
                                    clauses)))
                 env fenv))
              (list 'lit 0)))

         ;; When (guard form)
         ((eq op 'when)
          (if (consp (cdr expr))
              (let ((test (cadr expr))
                    (body (cddr expr)))
                (list 'if-expr
                      (compile-expr test env fenv)
                      (if body
                          (if (cdr body)
                              (compile-expr (cons 'progn body) env fenv)
                              (compile-expr (car body) env fenv))
                          (list 'lit #x0))
                      (list 'lit #x0)))
              (list 'lit #x0)))

         ;; Unless (negated guard form)
         ((eq op 'unless)
          (if (consp (cdr expr))
              (let ((test (cadr expr))
                    (body (cddr expr)))
                (list 'if-expr
                      (compile-expr test env fenv)
                      (list 'lit #x0)  ; if true -> nil
                      (if body
                          (if (cdr body)
                              (compile-expr (cons 'progn body) env fenv)
                              (compile-expr (car body) env fenv))
                          (list 'lit #x0))))
              (list 'lit #x0)))

         ;; And (short-circuit conjunction)
         ((eq op 'and)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 1))  ; (and) -> t (1, tags to #x10)
              ((null (cdr args)) (compile-expr (car args) env fenv)) ; (and x) -> x
              (t ; (and a b ...) -> (if a (and b ...) nil)
               (list 'if-expr
                     (compile-expr (car args) env fenv)
                     (compile-expr (cons 'and (cdr args)) env fenv)
                     (list 'lit 0))))))

         ;; Or (short-circuit disjunction)
         ((eq op 'or)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit #x0))  ; (or) -> nil
              ((null (cdr args)) (compile-expr (car args) env fenv)) ; (or x) -> x
              (t ; (or a b ...) -> need temp to preserve value
               ;; Simple version: (if a a (or b ...)) - evaluates a twice
               ;; For now use this; optimize later with let if needed
               (let ((first-ir (compile-expr (car args) env fenv)))
                 (list 'if-expr
                       first-ir
                       first-ir  ; returns first if true (evaluates twice)
                       (compile-expr (cons 'or (cdr args)) env fenv)))))))

         ;; Null predicate (also nil?)
         ((or (eq op 'null) (op= op "NIL?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Consp predicate (tag #x1) (also cons?)
         ((or (eq op 'consp) (op= op "CONS?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 1)) ; tag 1 (will be tagged to #x10, matching get-tag result)
              (list 'lit 0)))

         ;; Atom predicate (not consp)
         ((eq op 'atom)
          (if (consp (cdr expr))
              (list 'if-expr
                    (list 'cmp-eq
                          (list 'get-tag (compile-expr (cadr expr) env fenv))
                          (list 'lit 1)) ; cons tag
                    (list 'lit 0)   ; cons -> nil (not atom)
                    (list 'lit 1))  ; not cons -> t (is atom)
              (list 'lit 1)))

         ;; Numberp predicate (tag #x0 = fixnum) (also fixnum?)
         ((or (eq op 'numberp) (op= op "FIXNUM?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit #x0)) ; tag 0
              (list 'lit #x0)))

         ;; Symbolp predicate (tag #x2) (also symbol?)
         ((or (eq op 'symbolp) (op= op "SYMBOL?"))
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 2)) ; tag 2
              (list 'lit 0)))

         ;; Stringp predicate (tag #x4)
         ((eq op 'stringp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 4)) ; tag 4
              (list 'lit 0)))

         ;; Vectorp predicate (tag #x3)
         ((eq op 'vectorp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 3)) ; tag 3
              (list 'lit 0)))

         ;; Functionp predicate (tag #x5 = closure)
         ((eq op 'functionp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 5)) ; tag 5
              (list 'lit 0)))

         ;; Hash-table-p predicate (tag #x6)
         ((op= op "HASH-TABLE-P")
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'get-tag (compile-expr (cadr expr) env fenv))
                    (list 'lit 6)) ; tag 6
              (list 'lit 0)))

         ;; Listp predicate (null or cons)
         ((eq op 'listp)
          (if (consp (cdr expr))
              (let ((arg-ir (compile-expr (cadr expr) env fenv)))
                ;; listp = (or (null x) (consp x))
                (list 'if-expr
                      (list 'cmp-eq arg-ir (list 'lit 0)) ; null check
                      (list 'lit 1) ; null -> t
                      (list 'cmp-eq
                            (list 'get-tag arg-ir)
                            (list 'lit 1)))) ; cons check (tag 1)
              (list 'lit 0)))

         ;; Zerop predicate
         ((eq op 'zerop)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Plusp predicate (> 0)
         ((eq op 'plusp)
          (if (consp (cdr expr))
              (list 'cmp-gt
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Minusp predicate (< 0)
         ((eq op 'minusp)
          (if (consp (cdr expr))
              (list 'cmp-lt
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Endp - end of list test (same as null for proper lists)
         ((eq op 'endp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit #x0))
              (list 'lit #x0)))

         ;; Keywordp - check if symbol is a keyword
         ;; Keywords have names starting with ":"
         ((op= op "KEYWORDP")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((sym ,(cadr expr)))
                  (if (symbolp sym)
                      (let ((name (symbol-name sym)))
                        (if (> (string-length name) #x0)
                            (if (= (string-ref name #x0) #x3A)  ; #\: = 58 = #x3A
                                #x1
                                #x0)
                            #x0))
                      #x0))
               env fenv)
              (list 'lit 0)))

         ;; Constantp - check if form is a constant (stub: only literals)
         ((op= op "CONSTANTP")
          (if (consp (cdr expr))
              (let ((form (cadr expr)))
                ;; At compile time, check if it's a literal
                (if (or (numberp form) (stringp form) (characterp form)
                        (and (consp form) (eq (car form) 'quote)))
                    (list 'lit 1)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Eq (pointer equality)
         ((eq op 'eq)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Eql (same as eq for fixnums, symbols, chars)
         ((eq op 'eql)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'cmp-eq
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; 1+ (increment by 1)
         ((eq op '1+)
          (if (consp (cdr expr))
              (list 'add
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit 1))
              (list 'lit 0)))

         ;; 1- (decrement by 1)
         ((eq op '1-)
          (if (consp (cdr expr))
              (list 'sub
                    (compile-expr (cadr expr) env fenv)
                    (list 'lit 1))
              (list 'lit 0)))

         ;; Abs (absolute value) - (if (< x 0) (- 0 x) x)
         ((eq op 'abs)
          (if (consp (cdr expr))
              (let ((arg-ir (compile-expr (cadr expr) env fenv)))
                (list 'if-expr
                      (list 'cmp-lt arg-ir (list 'lit 0))
                      (list 'sub (list 'lit 0) arg-ir)
                      arg-ir))
              (list 'lit 0)))

         ;; Max (maximum of two values)
         ((eq op 'max)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((a-ir (compile-expr (cadr expr) env fenv))
                    (b-ir (compile-expr (caddr expr) env fenv)))
                (list 'if-expr
                      (list 'cmp-gt a-ir b-ir)
                      a-ir
                      b-ir))
              (if (consp (cdr expr))
                  (compile-expr (cadr expr) env fenv)
                  (list 'lit 0))))

         ;; Min (minimum of two values)
         ((eq op 'min)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((a-ir (compile-expr (cadr expr) env fenv))
                    (b-ir (compile-expr (caddr expr) env fenv)))
                (list 'if-expr
                      (list 'cmp-lt a-ir b-ir)
                      a-ir
                      b-ir))
              (if (consp (cdr expr))
                  (compile-expr (cadr expr) env fenv)
                  (list 'lit 0))))

         ;; Evenp (test if even)
         ((eq op 'evenp)
          (if (consp (cdr expr))
              (list 'cmp-eq
                    (list 'rem (compile-expr (cadr expr) env fenv) (list 'lit 2))
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Oddp (test if odd)
         ((eq op 'oddp)
          (if (consp (cdr expr))
              (list 'cmp-ne
                    (list 'rem (compile-expr (cadr expr) env fenv) (list 'lit 2))
                    (list 'lit 0))
              (list 'lit 0)))

         ;; Signum (sign of number: -1, 0, or 1)
         ((eq op 'signum)
          (if (consp (cdr expr))
              (compile-expr
               `(let ((n ,(cadr expr)))
                  (cond ((< n #x0) #x-1)
                        ((> n #x0) #x1)
                        (t #x0)))
               env fenv)
              (list 'lit 0)))

         ;; Gcd (greatest common divisor) - Euclidean algorithm
         ((eq op 'gcd)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))
              ((null (cdr args)) (compile-expr `(abs ,(car args)) env fenv))
              ((null (cddr args))
               (compile-expr
                `(labels ((gcd-iter (a b)
                            (if (= b #x0)
                                a
                                (gcd-iter b (rem a b)))))
                   (gcd-iter (abs ,(car args)) (abs ,(cadr args))))
                env fenv))
              (t (compile-expr `(gcd (gcd ,(car args) ,(cadr args)) ,@(cddr args)) env fenv)))))

         ;; Lcm (least common multiple)
         ((eq op 'lcm)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 1))
              ((null (cdr args)) (compile-expr `(abs ,(car args)) env fenv))
              ((null (cddr args))
               (compile-expr
                `(let ((a (abs ,(car args)))
                       (b (abs ,(cadr args))))
                   (if (= a #x0)
                       #x0
                       (* (/ a (gcd a b)) b)))
                env fenv))
              (t (compile-expr `(lcm (lcm ,(car args) ,(cadr args)) ,@(cddr args)) env fenv)))))

         ;; Cadr (car of cdr)
         ((eq op 'cadr)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Caddr (car of cdr of cdr)
         ((eq op 'caddr)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (cadr expr) env fenv))))
              (list 'lit 0)))

         ;; Cadddr (car of cdr of cdr of cdr)
         ((eq op 'cadddr)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (cadr expr) env fenv)))))
              (list 'lit 0)))

         ;; Cddr (cdr of cdr)
         ((eq op 'cddr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Cdddr (cdr of cdr of cdr)
         ((eq op 'cdddr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (cadr expr) env fenv))))
              (list 'lit 0)))

         ;; Cddddr (cdr of cdr of cdr of cdr)
         ((eq op 'cddddr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (cadr expr) env fenv)))))
              (list 'lit 0)))

         ;; Caar (car of car)
         ((eq op 'caar)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'car-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Cdar (cdr of car)
         ((eq op 'cdar)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (list 'car-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; First (same as car)
         ((eq op 'first)
          (if (consp (cdr expr))
              (list 'car-call (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Second (same as cadr)
         ((eq op 'second)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call (compile-expr (cadr expr) env fenv)))
              (list 'lit 0)))

         ;; Third (same as caddr)
         ((eq op 'third)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call (compile-expr (cadr expr) env fenv))))
              (list 'lit 0)))

         ;; Fourth
         ((eq op 'fourth)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call (compile-expr (cadr expr) env fenv)))))
              (list 'lit 0)))

         ;; Fifth
         ((eq op 'fifth)
          (if (consp (cdr expr))
              (list 'car-call
                    (list 'cdr-call
                          (list 'cdr-call
                                (list 'cdr-call
                                      (list 'cdr-call (compile-expr (cadr expr) env fenv))))))
              (list 'lit 0)))

         ;; Rest (same as cdr)
         ((eq op 'rest)
          (if (consp (cdr expr))
              (list 'cdr-call (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; List (create list from args)
         ((eq op 'list)
          (let ((args (cdr expr)))
            (if (null args)
                (list 'lit 0)  ; (list) -> nil
                (labels ((build-list (items)
                           (if (null items)
                               (list 'lit 0)
                               (list 'cons-call
                                     (compile-expr (car items) env fenv)
                                     (build-list (cdr items))))))
                  (build-list args)))))

         ;; List* (last arg is tail, rest consed)
         ((eq op 'list*)
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))
              ((null (cdr args)) (compile-expr (car args) env fenv))
              (t (labels ((build-list* (items)
                            (if (null (cdr items))
                                (compile-expr (car items) env fenv)
                                (list 'cons-call
                                      (compile-expr (car items) env fenv)
                                      (build-list* (cdr items))))))
                   (build-list* args))))))

         ;; Acons (add to front of alist: (cons (cons key val) alist))
         ((eq op 'acons)
          (if (and (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr)))
              (list 'cons-call
                    (list 'cons-call
                          (compile-expr (cadr expr) env fenv)   ; key
                          (compile-expr (caddr expr) env fenv)) ; value
                    (compile-expr (cadddr expr) env fenv))      ; alist
              (list 'lit 0)))

         ;; Nth (get nth element) - compile-time unrolled for small n, or loop for variables
         ((eq op 'nth)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((n-expr (cadr expr))
                    (list-expr (caddr expr)))
                (if (and (integerp n-expr) (<= n-expr 10))
                    ;; Small constant index: unroll to car/cdr chain
                    (let ((list-ir (compile-expr list-expr env fenv))
                          (result nil))
                      (setf result list-ir)
                      (dotimes (i n-expr)
                        (setf result (list 'cdr-call result)))
                      (list 'car-call result))
                    ;; Variable index: transform to labels loop
                    (let* ((loop-fn (gensym "NTH-LOOP"))
                           (idx-var (gensym "IDX"))
                           (lst-var (gensym "LST"))
                           (transformed
                            `(labels ((,loop-fn (,idx-var ,lst-var)
                                        (if (= ,idx-var 0)
                                            (car ,lst-var)
                                            (,loop-fn (- ,idx-var 1) (cdr ,lst-var)))))
                               (,loop-fn ,n-expr ,list-expr))))
                      (compile-expr transformed env fenv))))
              (list 'lit 0)))

         ;; Nthcdr (get nth tail) - compile-time unrolled for small n, or loop for variables
         ((eq op 'nthcdr)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((n-expr (cadr expr))
                    (list-expr (caddr expr)))
                (if (and (integerp n-expr) (<= n-expr 10))
                    ;; Small constant index: unroll to cdr chain
                    (let ((list-ir (compile-expr list-expr env fenv))
                          (result nil))
                      (setf result list-ir)
                      (dotimes (i n-expr)
                        (setf result (list 'cdr-call result)))
                      result)
                    ;; Variable index: transform to labels loop
                    (let* ((loop-fn (gensym "NTHCDR-LOOP"))
                           (idx-var (gensym "IDX"))
                           (lst-var (gensym "LST"))
                           (transformed
                            `(labels ((,loop-fn (,idx-var ,lst-var)
                                        (if (= ,idx-var 0)
                                            ,lst-var
                                            (,loop-fn (- ,idx-var 1) (cdr ,lst-var)))))
                               (,loop-fn ,n-expr ,list-expr))))
                      (compile-expr transformed env fenv))))
              (list 'lit 0)))

         ;; Elt (generic element access - same as nth for lists)
         ((eq op 'elt)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((seq-expr (cadr expr))
                    (idx-expr (caddr expr)))
                (if (and (integerp idx-expr) (<= idx-expr 10))
                    (let ((seq-ir (compile-expr seq-expr env fenv))
                          (result nil))
                      (setf result seq-ir)
                      (dotimes (i idx-expr)
                        (setf result (list 'cdr-call result)))
                      (list 'car-call result))
                    ;; Variable index: transform to labels loop (same as nth)
                    (let* ((loop-fn (gensym "ELT-LOOP"))
                           (idx-var (gensym "IDX"))
                           (lst-var (gensym "LST"))
                           (transformed
                            `(labels ((,loop-fn (,idx-var ,lst-var)
                                        (if (= ,idx-var 0)
                                            (car ,lst-var)
                                            (,loop-fn (- ,idx-var 1) (cdr ,lst-var)))))
                               (,loop-fn ,idx-expr ,seq-expr))))
                      (compile-expr transformed env fenv))))
              (list 'lit 0)))

         ;; Identity function
         ((eq op 'identity)
          (if (consp (cdr expr))
              (compile-expr (cadr expr) env fenv)
              (list 'lit 0)))

         ;; Constantly - returns first arg (simplified for constant arg)
         ((eq op 'constantly)
          (if (consp (cdr expr))
              (compile-expr (cadr expr) env fenv)
              (list 'lit 0)))

         ;; Length - recursive list length
         ((eq op 'length)
          (if (consp (cdr expr))
              (let ((lst-arg (cadr expr)))
                ;; Transform to: (labels ((len (lst) (if (null lst) 0 (+ 1 (len (cdr lst)))))) (len arg))
                (compile-expr
                 `(labels ((len-iter (lst acc)
                             (if (null lst)
                                 acc
                                 (len-iter (cdr lst) (+ acc #x1)))))
                    (len-iter ,lst-arg #x0))
                 env fenv))
              (list 'lit 0)))

         ;; Append - recursive list append (two-argument version)
         ((eq op 'append)
          (cond
            ;; (append) -> nil
            ((null (cdr expr)) (list 'lit 0))
            ;; (append x) -> x
            ((null (cddr expr)) (compile-expr (cadr expr) env fenv))
            ;; (append x y) -> recursive implementation
            ((null (cdddr expr))
             (let ((lst1 (cadr expr))
                   (lst2 (caddr expr)))
               (compile-expr
                `(labels ((app (xs ys)
                            (if (null xs)
                                ys
                                (cons (car xs) (app (cdr xs) ys)))))
                   (app ,lst1 ,lst2))
                env fenv)))
            ;; (append x y z ...) -> (append x (append y z ...))
            (t (compile-expr
                `(append ,(cadr expr) (append ,@(cddr expr)))
                env fenv))))

         ;; Assoc - find key in alist
         ((eq op 'assoc)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((key (cadr expr))
                    (alist (caddr expr)))
                (compile-expr
                 `(labels ((assoc-iter (k al)
                             (if (null al)
                                 #x0
                                 (if (eq k (car (car al)))
                                     (car al)
                                     (assoc-iter k (cdr al))))))
                    (assoc-iter ,key ,alist))
                 env fenv))
              (list 'lit 0)))

         ;; Member - find item in list
         ((eq op 'member)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((member-iter (x xs)
                             (if (null xs)
                                 #x0
                                 (if (eq x (car xs))
                                     xs
                                     (member-iter x (cdr xs))))))
                    (member-iter ,item ,lst))
                 env fenv))
              (list 'lit 0)))

         ;; Reverse - reverse a list
         ((eq op 'reverse)
          (if (consp (cdr expr))
              (let ((lst (cadr expr)))
                (compile-expr
                 `(labels ((rev (xs acc)
                             (if (null xs)
                                 acc
                                 (rev (cdr xs) (cons (car xs) acc)))))
                    (rev ,lst #x0))
                 env fenv))
              (list 'lit 0)))

         ;; Dotimes - (dotimes (i n [result]) body...)
         ;; Iterates i from 0 to n-1, returns result (default nil)
         ((eq op 'dotimes)
          (if (and (consp (cdr expr)) (consp (cadr expr)))
              (let* ((var-form (cadr expr))
                     (var (car var-form))
                     (count-form (cadr var-form))
                     (result-form (if (consp (cddr var-form)) (caddr var-form) #x0))
                     (body (cddr expr))
                     (body-expr (if (null (cdr body)) (car body) `(progn ,@body))))
                (compile-expr
                 `(labels ((dotimes-iter (,var limit)
                             (if (>= ,var limit)
                                 ,result-form
                                 (progn
                                   ,body-expr
                                   (dotimes-iter (+ ,var #x1) limit)))))
                    (dotimes-iter #x0 ,count-form))
                 env fenv))
              (list 'lit 0)))

         ;; Dolist - (dolist (x list [result]) body...)
         ;; Iterates over each element of list, returns result (default nil)
         ((eq op 'dolist)
          (if (and (consp (cdr expr)) (consp (cadr expr)))
              (let* ((var-form (cadr expr))
                     (var (car var-form))
                     (list-form (cadr var-form))
                     (result-form (if (consp (cddr var-form)) (caddr var-form) #x0))
                     (body (cddr expr))
                     (body-expr (if (null (cdr body)) (car body) `(progn ,@body))))
                (compile-expr
                 `(labels ((dolist-iter (remaining)
                             (if (null remaining)
                                 ,result-form
                                 (let ((,var (car remaining)))
                                   (progn
                                     ,body-expr
                                     (dolist-iter (cdr remaining)))))))
                    (dolist-iter ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Do - (do ((var init step)...) (end-test result...) body...)
         ;; General iteration with parallel update
         ((eq op 'do)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((var-specs (cadr expr))
                     (end-clause (caddr expr))
                     (body (cdddr expr))
                     (end-test (car end-clause))
                     (result-forms (cdr end-clause))
                     (result-expr (if result-forms
                                      (if (null (cdr result-forms))
                                          (car result-forms)
                                          `(progn ,@result-forms))
                                      #x0))
                     (vars (mapcar #'car var-specs))
                     (inits (mapcar #'cadr var-specs))
                     (steps (mapcar (lambda (spec)
                                      (if (cddr spec) (caddr spec) (car spec)))
                                    var-specs))
                     (iter-fn (gensym "DO-ITER")))
                ;; Transform to labels form
                (compile-expr
                 `(labels ((,iter-fn ,vars
                             (if ,end-test
                                 ,result-expr
                                 (progn
                                   ,@(if body body '(nil))
                                   (,iter-fn ,@steps)))))
                    (,iter-fn ,@inits))
                 env fenv))
              (list 'lit 0)))

         ;; Do* - (do* ((var init step)...) (end-test result...) body...)
         ;; General iteration with sequential update
         ((eq op 'do*)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((var-specs (cadr expr))
                     (end-clause (caddr expr))
                     (body (cdddr expr))
                     (end-test (car end-clause))
                     (result-forms (cdr end-clause))
                     (result-expr (if result-forms
                                      (if (null (cdr result-forms))
                                          (car result-forms)
                                          `(progn ,@result-forms))
                                      #x0))
                     (vars (mapcar #'car var-specs))
                     (inits (mapcar #'cadr var-specs))
                     (steps (mapcar (lambda (spec)
                                      (if (cddr spec) (caddr spec) (car spec)))
                                    var-specs))
                     (iter-fn (gensym "DO*-ITER")))
                ;; Transform to labels with let* for sequential binding
                (compile-expr
                 `(let* ,(mapcar #'list vars inits)
                    (labels ((,iter-fn ()
                               (if ,end-test
                                   ,result-expr
                                   (progn
                                     ,@(if body body '(nil))
                                     ,@(mapcar (lambda (v s) `(setq ,v ,s)) vars steps)
                                     (,iter-fn)))))
                      (,iter-fn)))
                 env fenv))
              (list 'lit 0)))

         ;; Pop - (pop place) - remove and return first element
         ((op= op "POP")
          (if (consp (cdr expr))
              (let ((place (cadr expr)))
                (compile-expr
                 `(let ((result (car ,place)))
                    (setq ,place (cdr ,place))
                    result)
                 env fenv))
              (list 'lit 0)))

         ;; Pushnew - (pushnew item place) - push if not already member
         ((op= op "PUSHNEW")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (place (caddr expr)))
                (compile-expr
                 `(unless (member ,item ,place)
                    (push ,item ,place))
                 env fenv))
              (list 'lit 0)))

         ;; Multiple-value-list - (multiple-value-list form) - collect values as list
         ((op= op "MULTIPLE-VALUE-LIST")
          (if (consp (cdr expr))
              (let ((form (cadr expr)))
                ;; Execute form, then collect all values into a list
                ;; For now, support up to 4 values
                (compile-expr
                 `(multiple-value-bind (v0 v1 v2 v3) ,form
                    (list v0 v1 v2 v3))
                 env fenv))
              (list 'lit 0)))

         ;; Nth-value - (nth-value n form) - get nth value
         ((op= op "NTH-VALUE")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((n (cadr expr))
                    (form (caddr expr)))
                (compile-expr
                 `(let ((primary ,form))
                    (values-get ,n primary))
                 env fenv))
              (list 'lit 0)))

         ;; Psetq - parallel setq: (psetq var1 val1 var2 val2 ...)
         ;; All values evaluated before any assignment
         ((op= op "PSETQ")
          (if (consp (cdr expr))
              (let* ((pairs (cdr expr))
                     (vars '())
                     (vals '())
                     (temps '()))
                ;; Parse var/val pairs
                (loop while pairs do
                  (push (car pairs) vars)
                  (push (cadr pairs) vals)
                  (push (gensym "PSETQ-TMP") temps)
                  (setf pairs (cddr pairs)))
                (setf vars (nreverse vars))
                (setf vals (nreverse vals))
                (setf temps (nreverse temps))
                ;; Generate: (let ((tmp1 val1) (tmp2 val2) ...) (setq var1 tmp1) (setq var2 tmp2) ...)
                (compile-expr
                 `(let ,(mapcar #'list temps vals)
                    ,@(mapcar (lambda (var tmp) `(setq ,var ,tmp)) vars temps)
                    nil)
                 env fenv))
              (list 'lit 0)))

         ;; Rotatef - rotate values: (rotatef a b c) => a<-b, b<-c, c<-a
         ((op= op "ROTATEF")
          (if (consp (cdr expr))
              (let* ((places (cdr expr))
                     (n (length places))
                     (temps (loop repeat n collect (gensym "ROT-TMP"))))
                ;; Save all values, then rotate assignments
                (compile-expr
                 `(let ,(mapcar #'list temps places)
                    ,@(loop for i from 0 below n
                            for place in places
                            for next-idx = (mod (1+ i) n)
                            collect `(setq ,place ,(nth next-idx temps)))
                    nil)
                 env fenv))
              (list 'lit 0)))

         ;; Shiftf - shift values: (shiftf a b c newval) => returns old-a, a<-b, b<-c, c<-newval
         ((op= op "SHIFTF")
          (if (>= (length (cdr expr)) 2)
              (let* ((args (cdr expr))
                     (places (butlast args))
                     (newval (car (last args)))
                     (n (length places))
                     (temps (loop repeat n collect (gensym "SHIFT-TMP"))))
                ;; Save all values, shift assignments, return first saved
                (compile-expr
                 `(let ,(mapcar #'list temps places)
                    ,@(loop for i from 0 below (1- n)
                            for place in places
                            collect `(setq ,place ,(nth (1+ i) temps)))
                    (setq ,(car (last places)) ,newval)
                    ,(car temps))  ; return original first value
                 env fenv))
              (list 'lit 0)))

         ;; The - type declaration (stub - just returns value)
         ((op= op "THE")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (compile-expr (caddr expr) env fenv)
              (list 'lit 0)))

         ;; Coerce - type coercion (limited support)
         ((op= op "COERCE")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((obj (cadr expr))
                    (type (caddr expr)))
                ;; For now, just return the object (no real coercion)
                (compile-expr obj env fenv))
              (list 'lit 0)))

         ;; Make-instance - create a CLOS class instance
         ;; (make-instance 'class :slot1 val1 :slot2 val2 ...)
         ((op= op "MAKE-INSTANCE")
          (if (consp (cdr expr))
              (let* ((class-form (cadr expr))
                     (initargs (cddr expr))
                     ;; Get class name (handle quoted symbol)
                     (class-name (if (and (consp class-form) (eq (car class-form) 'quote))
                                     (cadr class-form)
                                     class-form))
                     ;; Look up class info
                     (class-info (assoc class-name *class-env*))
                     (slots (if class-info (caddr class-info) nil))
                     (initforms (if class-info (cadddr class-info) nil))
                     (num-slots (length slots)))
                (if class-info
                    ;; Generate vector construction with slot initialization
                    (let* ((v-var (intern (format nil "$MAKE-INSTANCE-V-~A" (incf *handler-counter*))))
                           ;; Parse initargs: (:slot val ...) -> alist
                           (initarg-alist
                            (labels ((parse-initargs (args)
                                       (if (null args)
                                           nil
                                           (if (and (keywordp (car args)) (cdr args))
                                               (let ((slot-name (intern (symbol-name (car args)))))
                                                 (cons (cons slot-name (cadr args))
                                                       (parse-initargs (cddr args))))
                                               (parse-initargs (cdr args))))))
                              (parse-initargs initargs)))
                           ;; Generate slot value expressions
                           (slot-values
                            (loop for slot in slots
                                  for initform in initforms
                                  collect (let ((initarg (assoc slot initarg-alist)))
                                            (if initarg
                                                (cdr initarg)
                                                (or initform nil)))))
                           ;; Build let form
                           (init-form
                            `(let ((,v-var (make-vector ,(+ 1 num-slots))))
                               (vector-set ,v-var 0 ',class-name)
                               ,@(loop for val in slot-values
                                       for i from 1
                                       collect `(vector-set ,v-var ,i ,val))
                               ,v-var)))
                      (compile-expr init-form env fenv))
                    ;; Class not found, return nil
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Slot-value - access slot by name at runtime
         ;; (slot-value obj 'slot-name)
         ((op= op "SLOT-VALUE")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((obj-expr (cadr expr))
                     (slot-form (caddr expr))
                     ;; Get slot name (handle quoted symbol)
                     (slot-name (if (and (consp slot-form) (eq (car slot-form) 'quote))
                                    (cadr slot-form)
                                    nil)))
                (if slot-name
                    ;; Find slot index by looking up in all classes
                    (let ((slot-index nil))
                      (dolist (class-entry *class-env*)
                        (let* ((slots (caddr class-entry))
                               (pos (position slot-name slots)))
                          (when pos
                            (setq slot-index (+ pos 1))  ; +1 for class tag
                            (return))))
                      (if slot-index
                          (compile-expr `(vector-ref ,obj-expr ,slot-index) env fenv)
                          (list 'lit 0)))  ; Slot not found
                    ;; Dynamic slot lookup not supported yet
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Class-of - get class name of an object
         ;; (class-of obj) returns the class symbol from first vector slot
         ((op= op "CLASS-OF")
          (if (consp (cdr expr))
              (compile-expr `(vector-ref ,(cadr expr) 0) env fenv)
              (list 'lit 0)))

         ;; Typep - check if object is of a type (basic CLOS support)
         ;; (typep obj 'class-name)
         ((op= op "TYPEP")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((obj-expr (cadr expr))
                     (type-form (caddr expr))
                     (type-name (if (and (consp type-form) (eq (car type-form) 'quote))
                                    (cadr type-form)
                                    nil)))
                (if type-name
                    ;; Check if vectorp and first element matches type
                    (compile-expr
                     `(and (vectorp ,obj-expr)
                           (> (vector-length ,obj-expr) 0)
                           (eq (vector-ref ,obj-expr 0) ',type-name))
                     env fenv)
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Destructuring-bind - (destructuring-bind pattern expr &body body)
         ;; Pattern-matches a list structure and binds variables
         ((op= op "DESTRUCTURING-BIND")
          (if (and (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr)))
              (let* ((pattern (cadr expr))
                     (value-expr (caddr expr))
                     (body (cdddr expr))
                     (bindings (destructuring-bind-expand pattern 'dbind-val)))
                (compile-expr
                 `(let ((dbind-val ,value-expr))
                    (let* ,bindings
                      ,@body))
                 env fenv))
              (list 'lit 0)))

         ;; Mapcar - (mapcar fn list) - apply fn to each element, return list of results
         ((eq op 'mapcar)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((mapcar-iter (fn lst)
                             (if (null lst)
                                 #x0
                                 (cons (funcall fn (car lst))
                                       (mapcar-iter fn (cdr lst))))))
                    (mapcar-iter ,fn-form ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Mapc - (mapc fn list) - like mapcar but returns the original list
         ((eq op 'mapc)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((mapc-iter (fn lst orig)
                             (if (null lst)
                                 orig
                                 (progn
                                   (funcall fn (car lst))
                                   (mapc-iter fn (cdr lst) orig)))))
                    (let ((the-list ,list-form))
                      (mapc-iter ,fn-form the-list the-list)))
                 env fenv))
              (list 'lit 0)))

         ;; Reduce - (reduce fn list &optional init) - fold function over list
         ((eq op 'reduce)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((fn-form (cadr expr))
                     (list-form (caddr expr))
                     (init-form (if (consp (cdddr expr)) (cadddr expr) nil)))
                (if init-form
                    ;; With initial value
                    (compile-expr
                     `(labels ((reduce-iter (fn lst acc)
                                 (if (null lst)
                                     acc
                                     (reduce-iter fn (cdr lst) (funcall fn acc (car lst))))))
                        (reduce-iter ,fn-form ,list-form ,init-form))
                     env fenv)
                    ;; Without initial value - use first element
                    (compile-expr
                     `(labels ((reduce-iter (fn lst acc)
                                 (if (null lst)
                                     acc
                                     (reduce-iter fn (cdr lst) (funcall fn acc (car lst))))))
                        (let ((the-list ,list-form))
                          (if (null the-list)
                              #x0
                              (reduce-iter ,fn-form (cdr the-list) (car the-list)))))
                     env fenv)))
              (list 'lit 0)))

         ;; Mapcan - (mapcan fn list) - like mapcar but appends results with nconc
         ((eq op 'mapcan)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((mapcan-iter (fn lst acc)
                             (if (null lst)
                                 acc
                                 (mapcan-iter fn (cdr lst)
                                              (nconc acc (funcall fn (car lst)))))))
                    (mapcan-iter ,fn-form ,list-form nil))
                 env fenv))
              (list 'lit 0)))

         ;; Maplist - (maplist fn list) - apply fn to successive cdrs
         ((eq op 'maplist)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((maplist-iter (fn lst)
                             (if (null lst)
                                 nil
                                 (cons (funcall fn lst)
                                       (maplist-iter fn (cdr lst))))))
                    (maplist-iter ,fn-form ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Mapcon - (mapcon fn list) - like maplist but appends results with nconc
         ((eq op 'mapcon)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((mapcon-iter (fn lst acc)
                             (if (null lst)
                                 acc
                                 (mapcon-iter fn (cdr lst)
                                              (nconc acc (funcall fn lst))))))
                    (mapcon-iter ,fn-form ,list-form nil))
                 env fenv))
              (list 'lit 0)))

         ;; Mapl - (mapl fn list) - like maplist but returns original list
         ((eq op 'mapl)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((mapl-iter (fn lst orig)
                             (if (null lst)
                                 orig
                                 (progn
                                   (funcall fn lst)
                                   (mapl-iter fn (cdr lst) orig)))))
                    (let ((the-list ,list-form))
                      (mapl-iter ,fn-form the-list the-list)))
                 env fenv))
              (list 'lit 0)))

         ;; Every - (every pred list) - returns T (1) if pred is true for all elements
         ((eq op 'every)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((every-iter (pred lst)
                             (if (null lst)
                                 #x1
                                 (if (funcall pred (car lst))
                                     (every-iter pred (cdr lst))
                                     #x0))))
                    (every-iter ,pred-form ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Some - (some pred list) - returns first non-nil result, or nil
         ((eq op 'some)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((some-iter (pred lst)
                             (if (null lst)
                                 #x0
                                 (let ((result (funcall pred (car lst))))
                                   (if result
                                       result
                                       (some-iter pred (cdr lst)))))))
                    (some-iter ,pred-form ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Notevery - (notevery pred list) - returns T if some element fails pred
         ((eq op 'notevery)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((notevery-iter (pred lst)
                             (if (null lst)
                                 #x0
                                 (if (funcall pred (car lst))
                                     (notevery-iter pred (cdr lst))
                                     #x1))))
                    (notevery-iter ,pred-form ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Notany - (notany pred list) - returns T if no element satisfies pred
         ((eq op 'notany)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((notany-iter (pred lst)
                             (if (null lst)
                                 #x1
                                 (if (funcall pred (car lst))
                                     #x0
                                     (notany-iter pred (cdr lst))))))
                    (notany-iter ,pred-form ,list-form))
                 env fenv))
              (list 'lit 0)))

         ;; Apply - (apply fn args-list) - call fn with args spread from list
         ;; For the patterns used in the compiler, we optimize known functions
         ((eq op 'apply)
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((fn-form (cadr expr))
                    (args-form (caddr expr)))
                ;; Check for (apply #'append ...) or (apply #'max ...)
                (cond
                  ;; (apply #'append list-of-lists) -> fold append over list
                  ((and (consp fn-form)
                        (or (eq (car fn-form) 'function)
                            (eq (car fn-form) 'quote))
                        (eq (cadr fn-form) 'append))
                   (compile-expr
                    `(labels ((apply-append (lists acc)
                                (if (null lists)
                                    acc
                                    (apply-append (cdr lists) (append acc (car lists))))))
                       (apply-append ,args-form nil))
                    env fenv))
                  ;; (apply #'max list-of-numbers) -> reduce max over list
                  ((and (consp fn-form)
                        (or (eq (car fn-form) 'function)
                            (eq (car fn-form) 'quote))
                        (eq (cadr fn-form) 'max))
                   (compile-expr
                    `(labels ((apply-max (lst best)
                                (if (null lst)
                                    best
                                    (let ((el (car lst)))
                                      (apply-max (cdr lst) (if (> el best) el best))))))
                       (let ((the-list ,args-form))
                         (if (null the-list)
                             #x0
                             (apply-max (cdr the-list) (car the-list)))))
                    env fenv))
                  ;; General apply - dispatch based on arg count (up to 5 args)
                  (t
                   (compile-expr
                    `(let ((fn ,fn-form)
                           (args ,args-form))
                       (let ((len (length args)))
                         (cond
                           ((= len #x0) (funcall fn))
                           ((= len #x1) (funcall fn (car args)))
                           ((= len #x2) (funcall fn (car args) (cadr args)))
                           ((= len #x3) (funcall fn (car args) (cadr args) (caddr args)))
                           ((= len #x4) (funcall fn (car args) (cadr args) (caddr args) (cadddr args)))
                           (t (funcall fn (car args) (cadr args) (caddr args) (cadddr args)
                                       (car (cddddr args)))))))
                    env fenv))))
              (list 'lit 0)))

         ;; Loop - compile-time transformation to labels + recursion
         ;; Simple implementation for common patterns used in compiler
         ((eq op 'loop)
          (let* ((clauses (cdr expr))
                 (result-form nil))
            ;;(format t "[LOOP DEBUG] clauses=~S len=~A~%" clauses (length clauses))
            ;;(format t "[LOOP DEBUG] (car clauses)=~S (eq? ~A)~%" (car clauses) (eq (car clauses) 'for))
            ;;(format t "[LOOP DEBUG] (caddr clauses)=~S~%" (caddr clauses))
            ;; Parse clauses
            (cond
              ;; (loop for var in list collect expr)
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "COLLECT"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (collect-expr (cadr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var acc)
                                   (if (null ,list-var)
                                       (reverse acc)
                                       (let ((,var (car ,list-var)))
                                         (iter (cdr ,list-var) (cons ,collect-expr acc))))))
                          (iter ,list-expr nil)))))
              ;; (loop for var from start below end collect expr)
              ((and (>= (length clauses) 7)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "FROM")
                    (op= (car (cddddr clauses)) "BELOW")
                    (op= (caddr (cddddr clauses)) "COLLECT"))
               (let ((var (cadr clauses))
                     (start (cadddr clauses))
                     (end (cadr (cddddr clauses)))
                     (collect-expr (cadddr (cddddr clauses))))
                 (setq result-form
                       `(labels ((iter (,var acc)
                                   (if (>= ,var ,end)
                                       (reverse acc)
                                       (iter (+ ,var #x1) (cons ,collect-expr acc)))))
                          (iter ,start nil)))))
              ;; (loop for var across vec collect expr)
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "ACROSS")
                    (op= (car (cddddr clauses)) "COLLECT"))
               (let ((var (cadr clauses))
                     (vec-expr (cadddr clauses))
                     (collect-expr (cadr (cddddr clauses))))
                 (setq result-form
                       `(let ((vec ,vec-expr))
                          (labels ((iter (idx acc)
                                     (if (>= idx (length vec))
                                         (reverse acc)
                                         (let ((,var (elt vec idx)))
                                           (iter (+ idx #x1) (cons ,collect-expr acc))))))
                            (iter #x0 nil))))))
              ;; (loop for var in list for idx from 0 do body)
              ((and (>= (length clauses) 8)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "FOR")
                    (op= (caddr (cddddr clauses)) "FROM")
                    (op= (car (cddddr (cddddr clauses))) "DO"))
               (let* ((var1 (cadr clauses))
                      (list-expr (cadddr clauses))
                      (var2 (cadr (cddddr clauses)))
                      (start (cadddr (cddddr clauses)))
                      (do-expr (cadr (cddddr (cddddr clauses))))
                      (list-var (intern (concatenate 'string (symbol-name var1) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var ,var2)
                                   (if (null ,list-var)
                                       nil
                                       (let ((,var1 (car ,list-var)))
                                         (progn
                                           ,do-expr
                                           (iter (cdr ,list-var) (+ ,var2 #x1)))))))
                          (iter ,list-expr ,start)))))
              ;; (loop until condition do body)
              ((and (>= (length clauses) 3)
                    (op= (car clauses) "UNTIL")
                    (op= (caddr clauses) "DO"))
               (let ((condition (cadr clauses))
                     (do-expr (cadddr clauses)))
                 (setq result-form
                       `(labels ((iter ()
                                   (if ,condition
                                       nil
                                       (progn
                                         ,do-expr
                                         (iter)))))
                          (iter)))))

              ;; (loop while condition do body)
              ((and (>= (length clauses) 3)
                    (op= (car clauses) "WHILE")
                    (op= (caddr clauses) "DO"))
               (let ((condition (cadr clauses))
                     (do-expr (cadddr clauses)))
                 (setq result-form
                       `(labels ((iter ()
                                   (if ,condition
                                       (progn
                                         ,do-expr
                                         (iter))
                                       nil)))
                          (iter)))))

              ;; (loop for var in list do body) - without collect
              ((and (>= (length clauses) 4)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "DO"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (do-expr (cadr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var)
                                   (if (null ,list-var)
                                       nil
                                       (let ((,var (car ,list-var)))
                                         (progn
                                           ,do-expr
                                           (iter (cdr ,list-var)))))))
                          (iter ,list-expr)))))

              ;; (loop for var from start below end do body) - without collect
              ((and (>= (length clauses) 6)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "FROM")
                    (op= (car (cddddr clauses)) "BELOW")
                    (op= (caddr (cddddr clauses)) "DO"))
               (let ((var (cadr clauses))
                     (start (cadddr clauses))
                     (end (cadr (cddddr clauses)))
                     (do-expr (cadddr (cddddr clauses))))
                 (setq result-form
                       `(labels ((iter (,var)
                                   (if (>= ,var ,end)
                                       nil
                                       (progn
                                         ,do-expr
                                         (iter (+ ,var #x1))))))
                          (iter ,start)))))

              ;; (loop for var in list when pred collect expr) - conditional collect
              ((and (>= (length clauses) 7)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "WHEN")
                    (op= (caddr (cddddr clauses)) "COLLECT"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (when-pred (cadr (cddddr clauses)))
                      (collect-expr (cadddr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var acc)
                                   (if (null ,list-var)
                                       (reverse acc)
                                       (let ((,var (car ,list-var)))
                                         (if ,when-pred
                                             (iter (cdr ,list-var) (cons ,collect-expr acc))
                                             (iter (cdr ,list-var) acc))))))
                          (iter ,list-expr nil)))))

              ;; (loop for var in list unless pred collect expr) - conditional collect (negative)
              ((and (>= (length clauses) 7)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "UNLESS")
                    (op= (caddr (cddddr clauses)) "COLLECT"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (unless-pred (cadr (cddddr clauses)))
                      (collect-expr (cadddr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var acc)
                                   (if (null ,list-var)
                                       (reverse acc)
                                       (let ((,var (car ,list-var)))
                                         (if ,unless-pred
                                             (iter (cdr ,list-var) acc)
                                             (iter (cdr ,list-var) (cons ,collect-expr acc)))))))
                          (iter ,list-expr nil)))))

              ;; (loop for var in list sum expr) - sum accumulator
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "SUM"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (sum-expr (cadr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var acc)
                                   (if (null ,list-var)
                                       acc
                                       (let ((,var (car ,list-var)))
                                         (iter (cdr ,list-var) (+ acc ,sum-expr))))))
                          (iter ,list-expr #x0)))))

              ;; (loop for var in list count pred) - count matching elements
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "COUNT"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (count-pred (cadr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(labels ((iter (,list-var cnt)
                                   (if (null ,list-var)
                                       cnt
                                       (let ((,var (car ,list-var)))
                                         (if ,count-pred
                                             (iter (cdr ,list-var) (+ cnt #x1))
                                             (iter (cdr ,list-var) cnt))))))
                          (iter ,list-expr #x0)))))

              ;; (loop for var in list maximize expr) - find maximum
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "MAXIMIZE"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (max-expr (cadr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(let ((the-list ,list-expr))
                          (if (null the-list)
                              #x0
                              (labels ((iter (,list-var best)
                                         (if (null ,list-var)
                                             best
                                             (let* ((,var (car ,list-var))
                                                    (val ,max-expr))
                                               (iter (cdr ,list-var)
                                                     (if (> val best) val best))))))
                                (let ((,var (car the-list)))
                                  (iter (cdr the-list) ,max-expr))))))))

              ;; (loop for var in list minimize expr) - find minimum
              ((and (>= (length clauses) 5)
                    (op= (car clauses) "FOR")
                    (op= (caddr clauses) "IN")
                    (op= (car (cddddr clauses)) "MINIMIZE"))
               (let* ((var (cadr clauses))
                      (list-expr (cadddr clauses))
                      (min-expr (cadr (cddddr clauses)))
                      (list-var (intern (concatenate 'string (symbol-name var) "$LIST"))))
                 (setq result-form
                       `(let ((the-list ,list-expr))
                          (if (null the-list)
                              #x0
                              (labels ((iter (,list-var best)
                                         (if (null ,list-var)
                                             best
                                             (let* ((,var (car ,list-var))
                                                    (val ,min-expr))
                                               (iter (cdr ,list-var)
                                                     (if (< val best) val best))))))
                                (let ((,var (car the-list)))
                                  (iter (cdr the-list) ,min-expr))))))))

              ;; (loop repeat n do body) - repeat n times
              ((and (>= (length clauses) 3)
                    (op= (car clauses) "REPEAT")
                    (op= (caddr clauses) "DO"))
               (let ((n-expr (cadr clauses))
                     (do-expr (cadddr clauses)))
                 (setq result-form
                       `(labels ((iter (n)
                                   (if (<= n #x0)
                                       nil
                                       (progn
                                         ,do-expr
                                         (iter (- n #x1))))))
                          (iter ,n-expr)))))

              ;; (loop repeat n collect expr) - repeat and collect
              ((and (>= (length clauses) 3)
                    (op= (car clauses) "REPEAT")
                    (op= (caddr clauses) "COLLECT"))
               (let ((n-expr (cadr clauses))
                     (collect-expr (cadddr clauses)))
                 (setq result-form
                       `(labels ((iter (n acc)
                                   (if (<= n #x0)
                                       (reverse acc)
                                       (iter (- n #x1) (cons ,collect-expr acc)))))
                          (iter ,n-expr nil)))))

              (t
               (setq result-form '(lit 0))))
            (compile-expr result-form env fenv)))

         ;; Error - signal an error condition
         ;; (error msg &rest args) - evaluates first arg and returns it as error code
         ;; A full implementation would print to stderr and exit with error code
         ;; For now, evaluate the first arg (which might be an error code or string)
         ((op= op "ERROR")
          (if (consp (cdr expr))
              ;; Evaluate first argument (error code or message) and return it
              (compile-expr (cadr expr) env fenv)
              (list 'lit 0)))

         ;; Unwind-protect - ensure cleanup code runs
         ;; (unwind-protect protected-form cleanup-forms...)
         ;; Evaluates protected-form, then always evaluates cleanup-forms,
         ;; returning the result of protected-form
         ((op= op "UNWIND-PROTECT")
          (if (consp (cdr expr))
              (let* ((protected-form (cadr expr))
                     (cleanup-forms (cddr expr))
                     (result-var (intern (format nil "$UNWIND-RESULT-~A" (incf *catch-counter*)))))
                ;; Note: let only takes ONE body form, so wrap in progn and return result-var last
                (compile-expr
                 `(let ((,result-var ,protected-form))
                    (progn ,@cleanup-forms ,result-var))
                 env fenv))
              (list 'lit 0)))

         ;; Handler-case - handle conditions with type dispatch
         ;; (handler-case protected-form (type (var) body...)*)
         ;; If signal is called during protected-form with matching type,
         ;; control transfers to that handler with the condition value bound to var
         ;; Implementation uses catch/throw for non-local control transfer
         ((op= op "HANDLER-CASE")
          (if (consp (cdr expr))
              (let* ((protected-form (cadr expr))
                     (clauses (cddr expr))
                     (handler-id (incf *handler-counter*))
                     (catch-tag (intern (format nil "$HANDLER-TAG-~A" handler-id)))
                     (result-var (intern (format nil "$HANDLER-RESULT-~A" handler-id)))
                     (thrown-var (intern (format nil "$HANDLER-THROWN-~A" handler-id)))
                     (type-var (intern (format nil "$HANDLER-TYPE-~A" handler-id)))
                     (value-var (intern (format nil "$HANDLER-VALUE-~A" handler-id)))
                     (old-handler-env *handler-env*)
                     ;; Parse clauses into (type var body-forms) entries
                     (handlers
                      (mapcar (lambda (clause)
                                (let* ((cond-type (car clause))
                                       (lambda-list (cadr clause))
                                       (handler-body (cddr clause))
                                       (var (if (consp lambda-list) (car lambda-list) nil)))
                                  (list cond-type var handler-body)))
                              clauses))
                     ;; Handler types for signal to check
                     (handler-types (mapcar #'car handlers))
                     ;; Create handler-env entry with catch tag and handler types
                     (new-handler-env (cons (list handler-id catch-tag handler-types)
                                            old-handler-env)))
                ;; Set handler env during compilation
                (setq *handler-env* new-handler-env)
                (unwind-protect
                    (let* (;; Build handler dispatch: check each type in order
                           (dispatch-form
                            (labels ((build-dispatch (remaining)
                                       (if (null remaining)
                                           value-var  ; No match, return signaled value
                                           (let* ((handler (car remaining))
                                                  (cond-type (car handler))
                                                  (var (cadr handler))
                                                  (body-forms (caddr handler))
                                                  (body-expr (if var
                                                                 `(let ((,var ,value-var))
                                                                    (progn ,@body-forms))
                                                                 `(progn ,@body-forms))))
                                             (if (or (eq cond-type 't) (eq cond-type 'condition))
                                                 body-expr  ; catch-all handler
                                                 `(if (eq ,type-var ',cond-type)
                                                      ,body-expr
                                                      ,(build-dispatch (cdr remaining))))))))
                              (build-dispatch handlers)))
                           ;; Full transformed form using catch/throw
                           ;; catch returns protected-form normally, or thrown value if signal
                           ;; thrown value is (cons type value)
                           (transformed
                            `(let ((,result-var (catch ',catch-tag ,protected-form)))
                               ;; Check if result is a condition (cons with type tag)
                               (if (consp ,result-var)
                                   (let ((,type-var (car ,result-var))
                                         (,value-var (cdr ,result-var)))
                                     ;; Dispatch to matching handler
                                     ,dispatch-form)
                                   ,result-var))))
                      (compile-expr transformed env fenv))
                  (setq *handler-env* old-handler-env)))
              (list 'lit 0)))

         ;; Signal - signal a condition
         ;; (signal type &optional value)
         ;; If a handler-case is active and handles this type, throw to it
         ;; Otherwise return normally (value or nil)
         ((op= op "SIGNAL")
          (if (consp (cdr expr))
              (let* ((type-expr (cadr expr))
                     (value-expr (if (consp (cddr expr)) (caddr expr) nil)))
                (if *handler-env*
                    ;; Handler in scope - check if it handles this type and throw
                    (let* ((handler-entry (car *handler-env*))
                           (handler-id (car handler-entry))
                           (catch-tag (cadr handler-entry))
                           (handler-types (caddr handler-entry))
                           (sig-val-var (intern (format nil "$SIG-VAL-~A" (incf *handler-counter*))))
                           ;; Check if any handler matches this type
                           (has-catch-all (or (member 't handler-types) (member 'condition handler-types)))
                           ;; Build type check
                           (type-checks
                            (if has-catch-all
                                #x1  ; catch-all always matches (use 1 for true)
                                (labels ((build-type-check (types)
                                           (if (null types)
                                               nil
                                               (if (cdr types)
                                                   `(or (eq ,type-expr ',(car types))
                                                        ,(build-type-check (cdr types)))
                                                   `(eq ,type-expr ',(car types))))))
                                  (build-type-check handler-types)))))
                      (compile-expr
                       `(let ((,sig-val-var ,(or value-expr nil)))
                          (if ,type-checks
                              ;; Throw (cons type value) to handler-case
                              (throw ',catch-tag (cons ,type-expr ,sig-val-var))
                              ,sig-val-var))  ; No handler matches, return normally
                       env fenv))
                    ;; No handler in scope - just evaluate and return value
                    (if value-expr
                        (compile-expr value-expr env fenv)
                        (list 'lit 0))))
              (list 'lit 0)))

         ;; Restart-case - establish restarts that can be invoked from handlers
         ;; (restart-case form (restart-name (args) body...)*)
         ;; When invoke-restart is called with matching name, execute that restart body
         ;; Implementation uses catch/throw for non-local control transfer
         ((op= op "RESTART-CASE")
          (if (consp (cdr expr))
              (let* ((protected-form (cadr expr))
                     (restart-clauses (cddr expr))
                     (restart-id (incf *restart-counter*))
                     (catch-tag (intern (format nil "$RESTART-TAG-~A" restart-id)))
                     (result-var (intern (format nil "$RESTART-RESULT-~A" restart-id)))
                     (name-var (intern (format nil "$RESTART-NAME-~A" restart-id)))
                     (args-var (intern (format nil "$RESTART-ARGS-~A" restart-id)))
                     (old-restart-env *restart-env*)
                     ;; Parse restart clauses: (name (args) body...)
                     (restarts
                      (mapcar (lambda (clause)
                                (let* ((restart-name (car clause))
                                       (lambda-list (cadr clause))
                                       (restart-body (cddr clause)))
                                  (list restart-name lambda-list restart-body)))
                              restart-clauses))
                     ;; Restart names for invoke-restart to check
                     (restart-names (mapcar #'car restarts))
                     ;; Create restart-env entry with catch tag and restart names
                     (new-restart-env (cons (list restart-id catch-tag restart-names)
                                            old-restart-env)))
                ;; Set restart env during compilation
                (setq *restart-env* new-restart-env)
                (unwind-protect
                    (let* (;; Build restart dispatch
                           (dispatch-form
                            (labels ((build-dispatch (remaining)
                                       (if (null remaining)
                                           args-var  ; No match, return args
                                           (let* ((restart (car remaining))
                                                  (restart-name (car restart))
                                                  (lambda-list (cadr restart))
                                                  (body-forms (caddr restart))
                                                  ;; Bind args to lambda-list vars if present
                                                  (body-expr
                                                   (if (and lambda-list (consp lambda-list))
                                                       ;; Bind first arg (args-var already has the value)
                                                       `(let ((,(car lambda-list) ,args-var))
                                                          (progn ,@body-forms))
                                                       `(progn ,@body-forms))))
                                             `(if (eq ,name-var ',restart-name)
                                                  ,body-expr
                                                  ,(build-dispatch (cdr remaining)))))))
                              (build-dispatch restarts)))
                           ;; Full transformed form using catch/throw
                           ;; thrown value is (cons name args)
                           (transformed
                            `(let ((,result-var (catch ',catch-tag ,protected-form)))
                               ;; Check if result is a restart call (cons with name)
                               (if (consp ,result-var)
                                   (let ((,name-var (car ,result-var))
                                         (,args-var (cdr ,result-var)))
                                     ;; Dispatch to matching restart
                                     ,dispatch-form)
                                   ,result-var))))
                      (compile-expr transformed env fenv))
                  (setq *restart-env* old-restart-env)))
              (list 'lit 0)))

         ;; Invoke-restart - invoke an established restart
         ;; (invoke-restart name &rest args)
         ;; Find restart by name and throw to it
         ((op= op "INVOKE-RESTART")
          (if (consp (cdr expr))
              (let* ((name-expr (cadr expr))
                     (args-exprs (cddr expr)))
                (if *restart-env*
                    ;; Find matching restart and throw to it
                    (let* ((restart-entry (car *restart-env*))
                           (restart-id (car restart-entry))
                           (catch-tag (cadr restart-entry))
                           (restart-names (caddr restart-entry))
                           ;; Check if name matches any restart
                           (type-checks
                            (labels ((build-check (names)
                                       (if (null names)
                                           nil
                                           (if (cdr names)
                                               `(or (eq ,name-expr ',(car names))
                                                    ,(build-check (cdr names)))
                                               `(eq ,name-expr ',(car names))))))
                              (build-check restart-names)))
                           ;; Build args value (single arg or nil)
                           (args-form
                            (if args-exprs
                                (car args-exprs)  ; Just first arg for simplicity
                                nil)))
                      (compile-expr
                       `(if ,type-checks
                            ;; Throw (cons name args) to restart-case
                            (throw ',catch-tag (cons ,name-expr ,args-form))
                            nil)  ; No matching restart
                       env fenv))
                    ;; No restart in scope
                    (list 'lit 0)))
              (list 'lit 0)))

         ;; Format - basic format string processing
         ;; (format dest control-string &rest args)
         ;; Supported directives: ~A, ~S, ~D (consume arg), ~% (newline)
         ;; For now: evaluates args in order based on directives, returns nil (0)
         ;; When dest is nil, should return formatted string (not yet implemented)
         ;; When dest is t, outputs to stdout (requires I/O primitives)
         ((op= op "FORMAT")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((dest (cadr expr))
                     (control-string (caddr expr))
                     (args (cdddr expr)))
                ;; Parse control string and count directives that consume args
                (if (stringp control-string)
                    (let ((arg-count 0)
                          (forms nil))
                      ;; Count ~A, ~S, ~D directives (they consume args)
                      (let ((i 0)
                            (len (length control-string)))
                        (loop while (< i len) do
                          (when (and (char= (char control-string i) #\~)
                                     (< (1+ i) len))
                            (let ((directive (char-upcase (char control-string (1+ i)))))
                              (when (member directive '(#\A #\S #\D))
                                (if (< arg-count (length args))
                                    (let ((arg (nth arg-count args)))
                                      (push arg forms)
                                      (incf arg-count))))))
                          (incf i)))
                      ;; Evaluate all consumed args and return last value (or 0)
                      (if forms
                          (compile-expr `(progn ,@(nreverse forms)) env fenv)
                          (list 'lit 0)))
                    ;; Not a string literal - evaluate args anyway
                    (if args
                        (compile-expr `(progn ,@args) env fenv)
                        (list 'lit 0))))
              (list 'lit 0)))

         ;; Remove-if - filter out elements that match predicate
         ((op= op "REMOVE-IF")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((filter-iter (remaining acc)
                             (if (null remaining)
                                 (reverse acc)
                                 (if (funcall ,pred (car remaining))
                                     (filter-iter (cdr remaining) acc)
                                     (filter-iter (cdr remaining) (cons (car remaining) acc))))))
                    (filter-iter ,lst nil))
                 env fenv))
              (list 'lit 0)))

         ;; Remove-if-not - filter out elements that don't match predicate
         ((op= op "REMOVE-IF-NOT")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((pred (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((filter-iter (remaining acc)
                             (if (null remaining)
                                 (reverse acc)
                                 (if (funcall ,pred (car remaining))
                                     (filter-iter (cdr remaining) (cons (car remaining) acc))
                                     (filter-iter (cdr remaining) acc)))))
                    (filter-iter ,lst nil))
                 env fenv))
              (list 'lit 0)))

         ;; Remove-duplicates - remove duplicate elements
         ((op= op "REMOVE-DUPLICATES")
          (if (consp (cdr expr))
              (let ((lst (cadr expr)))
                (compile-expr
                 `(labels ((dedup-iter (remaining seen)
                             (if (null remaining)
                                 (reverse seen)
                                 (let ((el (car remaining)))
                                   (if (member el seen)
                                       (dedup-iter (cdr remaining) seen)
                                       (dedup-iter (cdr remaining) (cons el seen)))))))
                    (dedup-iter ,lst nil))
                 env fenv))
              (list 'lit 0)))

        ;; Cons
         ((eq op 'cons)
         (if (and (consp (cdr expr)) (consp (cddr expr)))
             (list 'cons-call
                   (compile-expr (cadr expr) env fenv)
                   (compile-expr (caddr expr) env fenv))
             (list 'lit 0)))

         ;; Vector ref
         ((op= op "VECTOR-REF")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'vector-ref
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Make vector: (make-vector size) -> vector
         ((op= op "MAKE-VECTOR")
          (if (consp (cdr expr))
              (list 'make-vector-call
                    (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Vector set: (vector-set vector index value) -> value
         ((op= op "VECTOR-SET")
          (if (and (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr)))
              (list 'vector-set-call
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv)
                    (compile-expr (cadddr expr) env fenv))
              (list 'lit 0)))

         ;; Vector length: (vector-length vector) -> fixnum
         ((op= op "VECTOR-LENGTH")
          (if (consp (cdr expr))
              (list 'vector-length-call
                    (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Symbol-name
         ((op= op "SYMBOL-NAME")
          (if (consp (cdr expr))
              (list 'symbol-name (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; String length
         ((op= op "STRING-LENGTH")
          (if (consp (cdr expr))
              (list 'string-len (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; String-ref: (string-ref string index) -> character code
         ((op= op "STRING-REF")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'string-ref-call
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Char-code: (char-code char) -> integer
         ;; In Habu, characters are already represented as fixnum codes
         ((op= op "CHAR-CODE")
          (if (consp (cdr expr))
              (compile-expr (cadr expr) env fenv)
              (list 'lit 0)))

         ;; String=: (string= s1 s2) -> compares two strings
         ;; Transformed to a labels-based loop comparing characters
         ((op= op "STRING=")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((s1 (cadr expr))
                    (s2 (caddr expr)))
                (compile-expr
                 `(let ((str1 ,s1)
                        (str2 ,s2))
                    (let ((len1 (string-length str1))
                          (len2 (string-length str2)))
                      (if (= len1 len2)
                          (labels ((cmp (i)
                                     (if (= i len1)
                                         #x1  ; All chars matched
                                         (if (= (string-ref str1 i)
                                                (string-ref str2 i))
                                             (cmp (+ i #x1))
                                             #x0))))  ; Mismatch
                            (cmp #x0))
                          #x0)))  ; Different lengths
                 env fenv))
              (list 'lit 0)))

         ;; String-concat: (string-concat s1 s2) -> concatenates two strings
         ;; Uses runtime habu_string_concat
         ((op= op "STRING-CONCAT")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'string-concat-call
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Concatenate: (concatenate 'string s1 s2 ...) -> concatenate strings
         ;; For simplicity, just handles 2 strings or reduces multiple strings
         ((op= op "CONCATENATE")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((type-spec (cadr expr))
                     (strings (cddr expr)))
                ;; Only supports 'string type for now
                (if (= (length strings) 1)
                    (compile-expr (car strings) env fenv)
                    (if (= (length strings) 2)
                        (list 'string-concat-call
                              (compile-expr (car strings) env fenv)
                              (compile-expr (cadr strings) env fenv))
                        ;; More than 2: reduce via nested concat
                        (compile-expr
                         `(string-concat ,(car strings)
                                         (concatenate 'string ,@(cdr strings)))
                         env fenv))))
              (list 'lit 0)))

         ;; Subseq: (subseq string start &optional end) -> substring
         ((op= op "SUBSEQ")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((seq (cadr expr))
                    (start (caddr expr))
                    (end (if (consp (cdddr expr)) (cadddr expr) nil)))
                ;; If no end, use string-length
                (if end
                    (list 'string-substring-call
                          (compile-expr seq env fenv)
                          (compile-expr start env fenv)
                          (compile-expr end env fenv))
                    ;; No end specified - use string-length
                    (compile-expr
                     `(subseq ,seq ,start (string-length ,seq))
                     env fenv)))
              (list 'lit 0)))

         ;; Write-to-string: (write-to-string obj) -> converts obj to string
         ;; For now, only supports fixnums
         ((op= op "WRITE-TO-STRING")
          (if (consp (cdr expr))
              (list 'fixnum-to-string-call (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Princ-to-string: alias for write-to-string (simplified)
         ((op= op "PRINC-TO-STRING")
          (if (consp (cdr expr))
              (list 'fixnum-to-string-call (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Make-string: (make-string n &key initial-element) -> new string of length n
         ;; Transforms to vector construction + make-string-from-vector
         ((op= op "MAKE-STRING")
          (if (consp (cdr expr))
              (let* ((len-form (cadr expr))
                     (rest-args (cddr expr))
                     (init-char (if (and rest-args
                                         (op= (car rest-args) ":INITIAL-ELEMENT"))
                                    (cadr rest-args)
                                    #x20)))  ; default to space
                ;; Build vector of char codes, then make-string-from-vector
                (compile-expr
                 `(let ((n ,len-form)
                        (ch ,init-char))
                    (let ((vec (make-vector n)))
                      (dotimes (i n)
                        (vector-set vec i ch))
                      vec))  ; make-string-from-vector expects tagged vector
                 env fenv))
              (list 'lit 0)))

         ;; String-upcase: (string-upcase str) -> uppercase string
         ;; Transforms to building new string with uppercased chars
         ((op= op "STRING-UPCASE")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((str ,(cadr expr)))
                  (let ((len (string-length str))
                        (vec (make-vector (string-length str))))
                    (dotimes (i len)
                      (vector-set vec i (char-upcase (string-ref str i))))
                    (make-string-from-vector vec)))
               env fenv)
              (list 'lit 0)))

         ;; String-downcase: (string-downcase str) -> lowercase string
         ((op= op "STRING-DOWNCASE")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((str ,(cadr expr)))
                  (let ((len (string-length str))
                        (vec (make-vector (string-length str))))
                    (dotimes (i len)
                      (vector-set vec i (char-downcase (string-ref str i))))
                    (make-string-from-vector vec)))
               env fenv)
              (list 'lit 0)))

         ;; Make-string-from-vector: (make-string-from-vector vec) -> string
         ;; Direct call to runtime function
         ((op= op "MAKE-STRING-FROM-VECTOR")
          (if (consp (cdr expr))
              (list 'make-string-from-vector-call (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Char-upcase: convert lowercase char to uppercase
         ((op= op "CHAR-UPCASE")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((ch ,(cadr expr)))
                  (if (and (>= ch #x61) (<= ch #x7A))  ; a-z
                      (- ch #x20)
                      ch))
               env fenv)
              (list 'lit 0)))

         ;; Char-downcase: convert uppercase char to lowercase
         ((op= op "CHAR-DOWNCASE")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((ch ,(cadr expr)))
                  (if (and (>= ch #x41) (<= ch #x5A))  ; A-Z
                      (+ ch #x20)
                      ch))
               env fenv)
              (list 'lit 0)))

         ;; Upper-case-p: check if character is uppercase
         ((op= op "UPPER-CASE-P")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((ch ,(cadr expr)))
                  (and (>= ch #x41) (<= ch #x5A)))  ; A-Z
               env fenv)
              (list 'lit 0)))

         ;; Lower-case-p: check if character is lowercase
         ((op= op "LOWER-CASE-P")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((ch ,(cadr expr)))
                  (and (>= ch #x61) (<= ch #x7A)))  ; a-z
               env fenv)
              (list 'lit 0)))

         ;; Alpha-char-p: check if character is alphabetic
         ((op= op "ALPHA-CHAR-P")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((ch ,(cadr expr)))
                  (or (and (>= ch #x41) (<= ch #x5A))   ; A-Z
                      (and (>= ch #x61) (<= ch #x7A)))) ; a-z
               env fenv)
              (list 'lit 0)))

         ;; Digit-char-p: check if character is a digit
         ((op= op "DIGIT-CHAR-P")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((ch ,(cadr expr)))
                  (and (>= ch #x30) (<= ch #x39)))  ; 0-9
               env fenv)
              (list 'lit 0)))

         ;; Alphanumericp: check if character is alphanumeric
         ((op= op "ALPHANUMERICP")
          (if (consp (cdr expr))
              (compile-expr
               `(let ((ch ,(cadr expr)))
                  (or (and (>= ch #x41) (<= ch #x5A))   ; A-Z
                      (and (>= ch #x61) (<= ch #x7A))   ; a-z
                      (and (>= ch #x30) (<= ch #x39)))) ; 0-9
               env fenv)
              (list 'lit 0)))

         ;; Find: find item in list
         ((op= op "FIND")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((find-iter (lst)
                             (if (null lst)
                                 nil
                                 (if (eql (car lst) ,item)
                                     (car lst)
                                     (find-iter (cdr lst))))))
                    (find-iter ,lst))
                 env fenv))
              (list 'lit 0)))

         ;; Position: find position of item in list
         ((op= op "POSITION")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((pos-iter (lst idx)
                             (if (null lst)
                                 nil
                                 (if (eql (car lst) ,item)
                                     idx
                                     (pos-iter (cdr lst) (+ idx #x1))))))
                    (pos-iter ,lst #x0))
                 env fenv))
              (list 'lit 0)))

         ;; Count: count occurrences of item in list
         ((op= op "COUNT")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(labels ((count-iter (lst acc)
                             (if (null lst)
                                 acc
                                 (if (eql (car lst) ,item)
                                     (count-iter (cdr lst) (+ acc #x1))
                                     (count-iter (cdr lst) acc)))))
                    (count-iter ,lst #x0))
                 env fenv))
              (list 'lit 0)))

         ;; Last: return last cons of list
         ((op= op "LAST")
          (if (consp (cdr expr))
              (let ((lst (cadr expr)))
                (compile-expr
                 `(labels ((last-iter (lst)
                             (if (null (cdr lst))
                                 lst
                                 (last-iter (cdr lst)))))
                    (if (null ,lst) nil (last-iter ,lst)))
                 env fenv))
              (list 'lit 0)))

         ;; Butlast: return list without last element
         ((op= op "BUTLAST")
          (if (consp (cdr expr))
              (let ((lst (cadr expr)))
                (compile-expr
                 `(labels ((butlast-iter (lst)
                             (if (null (cdr lst))
                                 nil
                                 (cons (car lst) (butlast-iter (cdr lst))))))
                    (if (null ,lst) nil (butlast-iter ,lst)))
                 env fenv))
              (list 'lit 0)))

         ;; Nconc: destructively concatenate lists
         ((op= op "NCONC")
          (let ((args (cdr expr)))
            (cond
              ((null args) (list 'lit 0))
              ((null (cdr args)) (compile-expr (car args) env fenv))
              (t (compile-expr
                  `(let ((first ,(car args))
                         (second (nconc ,@(cdr args))))
                     (if (null first)
                         second
                         (labels ((find-end (lst)
                                    (if (null (cdr lst))
                                        lst
                                        (find-end (cdr lst)))))
                           (setf (cdr (find-end first)) second)
                           first)))
                  env fenv)))))

         ;; Copy-list: shallow copy of list
         ((op= op "COPY-LIST")
          (if (consp (cdr expr))
              (let ((lst (cadr expr)))
                (compile-expr
                 `(labels ((copy-iter (lst)
                             (if (null lst)
                                 nil
                                 (cons (car lst) (copy-iter (cdr lst))))))
                    (copy-iter ,lst))
                 env fenv))
              (list 'lit 0)))

         ;; Ldiff - returns leading portion of list up to sublist
         ((op= op "LDIFF")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((list-form (cadr expr))
                    (sublist-form (caddr expr)))
                (compile-expr
                 `(labels ((ldiff-iter (lst sub acc)
                             (if (or (null lst) (eq lst sub))
                                 (reverse acc)
                                 (ldiff-iter (cdr lst) sub (cons (car lst) acc)))))
                    (ldiff-iter ,list-form ,sublist-form nil))
                 env fenv))
              (list 'lit 0)))

         ;; Tailp - check if sublist is a tail of list
         ((op= op "TAILP")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((sublist-form (cadr expr))
                    (list-form (caddr expr)))
                (compile-expr
                 `(labels ((tailp-iter (lst sub)
                             (if (null lst)
                                 (null sub)
                                 (if (eq lst sub)
                                     #x1
                                     (tailp-iter (cdr lst) sub)))))
                    (tailp-iter ,list-form ,sublist-form))
                 env fenv))
              (list 'lit 0)))

         ;; Subst - substitute new for old in tree
         ((op= op "SUBST")
          (if (and (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr)))
              (let ((new-form (cadr expr))
                    (old-form (caddr expr))
                    (tree-form (cadddr expr)))
                (compile-expr
                 `(labels ((subst-iter (tree new old)
                             (if (eql tree old)
                                 new
                                 (if (consp tree)
                                     (cons (subst-iter (car tree) new old)
                                           (subst-iter (cdr tree) new old))
                                     tree))))
                    (subst-iter ,tree-form ,new-form ,old-form))
                 env fenv))
              (list 'lit 0)))

         ;; Copy-tree - deep copy of tree structure
         ((op= op "COPY-TREE")
          (if (consp (cdr expr))
              (let ((tree (cadr expr)))
                (compile-expr
                 `(labels ((copy-tree-iter (tree)
                             (if (consp tree)
                                 (cons (copy-tree-iter (car tree))
                                       (copy-tree-iter (cdr tree)))
                                 tree)))
                    (copy-tree-iter ,tree))
                 env fenv))
              (list 'lit 0)))

         ;; Getf - get property from property list
         ((op= op "GETF")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((plist (cadr expr))
                    (indicator (caddr expr))
                    (default (if (consp (cdddr expr)) (cadddr expr) #x0)))
                (compile-expr
                 `(labels ((getf-iter (lst ind)
                             (if (null lst)
                                 ,default
                                 (if (null (cdr lst))
                                     ,default
                                     (if (eql (car lst) ind)
                                         (cadr lst)
                                         (getf-iter (cddr lst) ind))))))
                    (getf-iter ,plist ,indicator))
                 env fenv))
              (list 'lit 0)))

         ;; Adjoin - add item to list if not already present
         ((op= op "ADJOIN")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((item (cadr expr))
                    (lst (caddr expr)))
                (compile-expr
                 `(if (member ,item ,lst)
                      ,lst
                      (cons ,item ,lst))
                 env fenv))
              (list 'lit 0)))

         ;; Union - set union of two lists
         ((op= op "UNION")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((list1 (cadr expr))
                    (list2 (caddr expr)))
                (compile-expr
                 `(labels ((union-iter (l1 l2 acc)
                             (if (null l1)
                                 (append (reverse acc) l2)
                                 (if (member (car l1) l2)
                                     (union-iter (cdr l1) l2 acc)
                                     (union-iter (cdr l1) l2 (cons (car l1) acc))))))
                    (union-iter ,list1 ,list2 nil))
                 env fenv))
              (list 'lit 0)))

         ;; Intersection - set intersection of two lists
         ((op= op "INTERSECTION")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((list1 (cadr expr))
                    (list2 (caddr expr)))
                (compile-expr
                 `(labels ((int-iter (l1 l2 acc)
                             (if (null l1)
                                 (reverse acc)
                                 (if (member (car l1) l2)
                                     (int-iter (cdr l1) l2 (cons (car l1) acc))
                                     (int-iter (cdr l1) l2 acc)))))
                    (int-iter ,list1 ,list2 nil))
                 env fenv))
              (list 'lit 0)))

         ;; Set-difference - elements in list1 not in list2
         ((op= op "SET-DIFFERENCE")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((list1 (cadr expr))
                    (list2 (caddr expr)))
                (compile-expr
                 `(labels ((diff-iter (l1 l2 acc)
                             (if (null l1)
                                 (reverse acc)
                                 (if (member (car l1) l2)
                                     (diff-iter (cdr l1) l2 acc)
                                     (diff-iter (cdr l1) l2 (cons (car l1) acc))))))
                    (diff-iter ,list1 ,list2 nil))
                 env fenv))
              (list 'lit 0)))

         ;; Subsetp - check if list1 is a subset of list2
         ((op= op "SUBSETP")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((list1 (cadr expr))
                    (list2 (caddr expr)))
                (compile-expr
                 `(every (lambda (x) (member x ,list2)) ,list1)
                 env fenv))
              (list 'lit 0)))

         ;; Get tag
         ((op= op "GET-TAG")
          (if (consp (cdr expr))
              (let ((arg-ir (compile-expr (cadr expr) env fenv)))
                (cond
                  ((has-tag? arg-ir 'string-lit) (list 'lit #x4))
                  ((has-tag? arg-ir 'symbol-lit) (list 'lit #x2))
                  ((has-tag? arg-ir 'vector-lit) (list 'lit #x3))
                  (t (list 'get-tag arg-ir))))
              (list 'lit 0)))

         ;; Multiple values: (values &rest vals) -> returns first value
         ;; Secondary values stored in global array via runtime call
         ((op= op "VALUES")
          (let ((vals (cdr expr)))
            (cond
              ((null vals)
               ;; (values) - no values, returns nil
               (list 'values-call 0 (list 'lit 0) (list 'lit 0) (list 'lit 0) (list 'lit 0)))
              ((null (cdr vals))
               ;; (values x) - single value, just return it
               (list 'values-call 1
                     (compile-expr (car vals) env fenv)
                     (list 'lit 0) (list 'lit 0) (list 'lit 0)))
              ((null (cddr vals))
               ;; (values x y) - two values
               (list 'values-call 2
                     (compile-expr (car vals) env fenv)
                     (compile-expr (cadr vals) env fenv)
                     (list 'lit 0) (list 'lit 0)))
              ((null (cdddr vals))
               ;; (values x y z) - three values
               (list 'values-call 3
                     (compile-expr (car vals) env fenv)
                     (compile-expr (cadr vals) env fenv)
                     (compile-expr (caddr vals) env fenv)
                     (list 'lit 0)))
              (t
               ;; (values x y z w) - four values (max supported)
               (list 'values-call 4
                     (compile-expr (car vals) env fenv)
                     (compile-expr (cadr vals) env fenv)
                     (compile-expr (caddr vals) env fenv)
                     (compile-expr (cadddr vals) env fenv))))))

         ;; Multiple value binding: (multiple-value-bind (vars...) expr body...)
         ;; Evaluates expr, binds vars to values, executes body
         ((op= op "MULTIPLE-VALUE-BIND")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let* ((vars (cadr expr))
                     (value-expr (caddr expr))
                     (body (cdddr expr))
                     (num-vars (length vars))
                     ;; Generate unique var for primary value
                     (primary-var (intern (format nil "$MV-PRIMARY-~A" (incf *catch-counter*)))))
                ;; Transform to:
                ;; (let ((primary (values-expr)))
                ;;   (let ((v0 primary)
                ;;         (v1 (values-get 1 primary))
                ;;         ...)
                ;;     body...))
                (compile-expr
                 `(let ((,primary-var ,value-expr))
                    (let* ,(loop for var in vars
                                 for i from 0
                                 collect (if (= i 0)
                                             `(,var ,primary-var)
                                             `(,var (values-get ,i ,primary-var))))
                      (progn ,@body)))
                 env fenv))
              (list 'lit 0)))

         ;; Values-get: (values-get index primary) -> retrieves Nth value
         ((op= op "VALUES-GET")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (list 'values-get-call
                    (compile-expr (cadr expr) env fenv)
                    (compile-expr (caddr expr) env fenv))
              (list 'lit 0)))

         ;; Hash tables
         ;; (make-hash-table &key size) -> hash table
         ((op= op "MAKE-HASH-TABLE")
          (let ((capacity (cadr (member :size (cdr expr)))))
            (list 'make-hash-table-call
                  (if capacity
                      (compile-expr capacity env fenv)
                      (list 'lit 0)))))  ; 0 means use default capacity

         ;; (gethash key hash-table &optional default) -> value
         ((op= op "GETHASH")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((key (cadr expr))
                    (ht (caddr expr))
                    (default (cadddr expr)))
                (list 'gethash-call
                      (compile-expr key env fenv)
                      (compile-expr ht env fenv)
                      (if default
                          (compile-expr default env fenv)
                          (list 'lit 0))))
              (list 'lit 0)))

         ;; (setf (gethash key ht) value) is handled by setf transformer
         ;; (puthash key value ht) -> value
         ((op= op "PUTHASH")
          (if (and (consp (cdr expr)) (consp (cddr expr)) (consp (cdddr expr)))
              (let ((key (cadr expr))
                    (value (caddr expr))
                    (ht (cadddr expr)))
                (list 'puthash-call
                      (compile-expr key env fenv)
                      (compile-expr value env fenv)
                      (compile-expr ht env fenv)))
              (list 'lit 0)))

         ;; (remhash key hash-table) -> boolean
         ((op= op "REMHASH")
          (if (and (consp (cdr expr)) (consp (cddr expr)))
              (let ((key (cadr expr))
                    (ht (caddr expr)))
                (list 'remhash-call
                      (compile-expr key env fenv)
                      (compile-expr ht env fenv)))
              (list 'lit 0)))

         ;; (hash-table-count hash-table) -> fixnum
         ((op= op "HASH-TABLE-COUNT")
          (if (consp (cdr expr))
              (list 'hash-table-count-call
                    (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Packages: no-op except find-symbol folded to symbol literal
         ((op= op "DEFPACKAGE") (list 'lit 0))
         ((op= op "IN-PACKAGE") (list 'lit 0))
         ((op= op "USE-PACKAGE") (list 'lit 0))
         ((op= op "EXPORT") (list 'lit 0))
         ((op= op "IMPORT") (list 'lit 0))
         ((op= op "FIND-SYMBOL")
          (let* ((name-expr (cadr expr))
                 (name (cond
                         ((and (consp name-expr) (op= (car name-expr) "QUOTE") (symbolp (cadr name-expr)))
                          (symbol-name (cadr name-expr)))
                         ((symbolp name-expr) (symbol-name name-expr))
                         ((stringp name-expr) name-expr)
                         (t nil))))
            (if name
                (list 'symbol-lit (string-upcase name))
                (list 'lit 0))))

         ;; Quasiquote
         ((op= op "QUASIQUOTE")
          (if (consp (cdr expr))
              (expand-quasiquote-ir (cadr expr) env fenv)
              (list 'lit 0)))

         ;; Lambda/closure
         ((op= op "LAMBDA")
         (let* ((raw-params (cadr expr))
                (raw-body (caddr expr))
                ;; Transform keyword params if present
                (transformed (transform-keyword-params raw-params raw-body))
                (params (if transformed (first transformed) raw-params))
                (body (if transformed (second transformed) raw-body))
                (lambda-name (gensym "lambda-"))
                (outer-max (if env (apply #'max (mapcar #'cdr env)) -1)))
           (multiple-value-bind (fixed optional rest key) (parse-params params)
             (declare (ignore key))  ; Already transformed away
             (let* ((optional-names (mapcar #'car optional))
                    (optional-supplied (mapcar (lambda (entry)
                                                 (or (caddr entry) (gensym "supplied-")))
                                               optional))
                    (bindings (append fixed optional-names optional-supplied (if rest (list rest) nil)))
                    (param-env-detect (env-extend (mapcar #'list bindings) env))
                    (opt-inits (mapcar (lambda (entry)
                                         (let ((init (cadr entry)))
                                           (if init
                                               (compile-expr init param-env-detect fenv)
                                               '(lit 0))))
                                       optional))
                    (body-ir-base (compile-expr body param-env-detect fenv))
                    (captured-offsets (remove-if-not (lambda (off) (<= off outer-max))
                                                     (collect-var-offsets body-ir-base)))
                    (capture-map (let ((idx 0))
                                   (mapcar (lambda (off)
                                             (prog1 (cons off idx)
                                               (incf idx)))
                                           captured-offsets)))
                    (body-ir (rewrite-captures body-ir-base capture-map))
                    (compiled (list lambda-name fixed optional-names opt-inits optional-supplied body-ir captured-offsets (1+ outer-max) rest)))
               (push compiled *collected-lambdas*)
               (list 'lambda-ref lambda-name)))))

         ;; Car
         ((eq op 'car)
          (if (consp (cdr expr))
              (list 'car-call
                    (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Cdr
         ((eq op 'cdr)
          (if (consp (cdr expr))
              (list 'cdr-call
                    (compile-expr (cadr expr) env fenv))
              (list 'lit 0)))

         ;; Funcall: call closure value
         ((eq op 'funcall)
          (let ((fn-expr (cadr expr))
                (args (cddr expr)))
            (list 'call-closure
                  (compile-expr fn-expr env fenv)
                  (mapcar (lambda (arg) (compile-expr arg env fenv)) args))))

         ;; Inline lambda application: ((lambda (...) ...) args...)
         ((consp op)
          (let ((fn (compile-expr op env fenv))
                (args (mapcar (lambda (arg) (compile-expr arg env fenv)) (cdr expr))))
            (list 'call-closure fn args)))

         ;; Function call - check if it's a user-defined function
         (t
          ;; Try to look up as a user function
          (if (and fenv (assoc op fenv))
              ;; It's a user-defined function
              (let ((args (cdr expr)))
                (list 'call-fn op
                      (mapcar (lambda (arg) (compile-expr arg env fenv)) args)))
              ;; Unknown operation
              (list 'lit 0))))))

    ;; Unknown
    (t (list 'lit 0))))

(defun codegen-main-with-runtime (ir runtime-addrs)
  "Generate main function with runtime table support
   Calling convention: x0 = runtime table pointer
   Prologue saves x19-x24 for runtime table, environment base, and callee-saved temps"
  (let ((body (codegen-expr ir runtime-addrs nil nil 0)))
    ;; Allocate 1024 bytes: ample space for locals/env below caller frame
    (append (arm64-sub-imm 31 31 1024)     ; SUB sp, sp, #1024 (stack frame)
            (arm64-stp 29 30 31 0)        ; STP x29, x30, [sp, #0]
            (arm64-stp 19 20 31 16)       ; STP x19, x20, [sp, #16]
            (arm64-stp 21 22 31 32)       ; STP x21, x22, [sp, #32]
            (arm64-stp 23 24 31 48)       ; STP x23, x24, [sp, #48]
            (arm64-mov 19 0)              ; MOV x19, x0 (save runtime table)
            ;; Set x20 to point to environment area well inside frame
            (arm64-add-imm 20 31 384)     ; ADD x20, sp, #x180
            body                           ; Function body
            (arm64-ldp 23 24 31 48)       ; LDP x23, x24, [sp, #48]
            (arm64-ldp 21 22 31 32)       ; LDP x21, x22, [sp, #32]
            (arm64-ldp 19 20 31 16)       ; LDP x19, x20, [sp, #16]
            (arm64-ldp 29 30 31 0)        ; LDP x29, x30, [sp, #0]
            (arm64-add-imm 31 31 1024)     ; ADD sp, sp, #1024 (restore stack)
            (arm64-ret))))

(defun compile-to-arm64-with-runtime (expr runtime-addrs)
  (codegen-main-with-runtime (compile-expr expr nil nil) runtime-addrs))

(defun compile-to-arm64 (expr)
  (compile-to-arm64-with-runtime expr nil))

;;; ============================================
;;; Multi-Function Compilation Stubs
;;; ============================================

(defun count-instrs (code)
  "Count number of 4-byte instructions in code list"
  (if (null code)
      0
      (+ 1 (count-instrs (nthcdr 4 code)))))

(defun parse-params (params)
  "Split params into fixed list, optional descriptors, rest symbol, and key descriptors.
Optional/key descriptors are (name init-form supplied-name)."
  (let ((fixed '())
        (optional '())
        (key '())
        (rest nil)
        (state :fixed))
    (dolist (p params)
      (cond
        ((eq p '&optional) (setf state :optional))
        ((eq p '&rest) (setf state :rest))
        ((eq p '&key) (setf state :key))
        ((eq state :fixed) (push (if (symbolp p) p (car p)) fixed))
        ((eq state :optional)
         (cond
           ((symbolp p) (push (list p nil nil) optional))
           ((consp p)
            (let ((name (car p))
                  (init (cadr p))
                  (supplied (caddr p)))
              (push (list name init supplied) optional)))))
        ((eq state :key)
         (cond
           ((symbolp p) (push (list p nil nil) key))
           ((consp p)
            (let ((name (car p))
                  (init (cadr p))
                  (supplied (caddr p)))
              (push (list name init supplied) key)))))
        ((eq state :rest) (setf rest p))))
    (values (nreverse fixed) (nreverse optional) rest (nreverse key))))

(defun transform-keyword-params (params body)
  "Transform (&key x y) params to (&rest args) with keyword extraction.
   Returns (new-params new-body) or nil if no transformation needed."
  (multiple-value-bind (fixed optional rest key) (parse-params params)
    (when key
      ;; Generate a rest param name if not present
      (let* ((rest-name (or rest (gensym "key-args-")))
             (search-fn (gensym "search-keys-"))
             ;; Build let bindings for each keyword
             (key-bindings
               (mapcar (lambda (k)
                         (let ((key-name (car k))
                               (default (or (cadr k) nil))
                               ;; Create keyword symbol from parameter name
                               (keyword (intern (symbol-name (car k)) :keyword)))
                           `(,key-name (,search-fn ,rest-name ',keyword ,default))))
                       key))
             ;; Wrap body in labels + let for keyword extraction
             (new-body
               `(labels ((,search-fn (lst key default)
                           (if (null lst)
                               default
                               (if (eq (car lst) key)
                                   (cadr lst)
                                   (,search-fn (cddr lst) key default)))))
                  (let ,key-bindings
                    ,body)))
             ;; Reconstruct params without &key section
             (new-params
               (append fixed
                       (when optional
                         (cons '&optional
                               (mapcar (lambda (o)
                                         (if (and (null (cadr o)) (null (caddr o)))
                                             (car o)
                                             o))
                                       optional)))
                       (list '&rest rest-name))))
        (list new-params new-body)))))

(defun compile-defun (name params body env fenv)
  "Compile defun into (name fixed optional body-ir captures param-base rest-param)"
  ;; Transform keyword params if present
  (let ((transformed (transform-keyword-params params body)))
    (when transformed
      (setf params (first transformed))
      (setf body (second transformed))))
  ;; Create environment with parameters as the initial bindings
  (multiple-value-bind (fixed optional rest key) (parse-params params)
    (declare (ignore key))  ; Already transformed away
    (let* ((optional-names (mapcar #'car optional))
           (optional-supplied (mapcar (lambda (entry)
                                        (or (caddr entry) (gensym "supplied-")))
                                      optional))
           (bindings (append fixed optional-names optional-supplied (if rest (list rest) nil)))
           (param-env (env-extend (mapcar #'list bindings) env))
           (param-base (if bindings
                           (env-lookup (car bindings) param-env)
                           0))
           (opt-inits (mapcar (lambda (entry)
                                (let ((init (cadr entry)))
                                  (if init
                                      (compile-expr init param-env fenv)
                                      '(lit 0))))
                              optional))
          ;; Add this function to fenv to allow recursive calls
          ;; Use a placeholder compiled-fn since we're still compiling it
          (recursive-fenv (cons (cons name nil) fenv))
          ;; Transform body to box captured mutable variables
          (boxed-body (box-mutable-captured-vars body))
          ;; Compile body in the parameter environment with recursive fenv
          (body-ir (compile-expr boxed-body param-env recursive-fenv)))
      (list name fixed optional-names opt-inits optional-supplied body-ir nil param-base rest))))

(defun compile-forms-helper (forms env fenv)
  "Compile list of forms, separating defuns from main expression
   Returns: (list-of-compiled-functions main-expression-ir)"
  (if (consp forms)
      (let ((form (car forms)))
        (cond
          ;; defmacro - register macro and continue
          ((and (consp form) (eq (car form) 'defmacro))
           (let* ((name (cadr form))
                  (params (caddr form))
                  (body (cadddr form))
                  ;; Create expander function using SBCL's eval
                  ;; The expander takes macro arguments and returns expanded code
                  (expander (eval `(lambda ,params ,body))))
             (register-macro name expander)
             ;; Continue with rest of forms (defmacro doesn't produce runtime code)
             (compile-forms-helper (cdr forms) env fenv)))

          ;; defclass - define a CLOS class
          ;; (defclass name (superclasses) ((slot :initform val) ...))
          ;; Class is stored as vector: [class-name slot1-val slot2-val ...]
          ((and (consp form) (eq (car form) 'defclass))
           (let* ((name (cadr form))
                  (superclasses (caddr form))  ; For now, single inheritance only
                  (slot-specs (cadddr form))
                  ;; Parse slot specs: ((slot :initform val) ...) or (slot ...)
                  (slot-info (mapcar (lambda (spec)
                                       (if (consp spec)
                                           (let* ((slot-name (car spec))
                                                  (initform-pos (position :initform spec))
                                                  (initform (if initform-pos
                                                                (nth (+ initform-pos 1) spec)
                                                                nil)))
                                             (cons slot-name initform))
                                           (cons spec nil)))
                                     slot-specs))
                  (slot-names (mapcar #'car slot-info))
                  (slot-initforms (mapcar #'cdr slot-info))
                  (parent (if (consp superclasses) (car superclasses) nil))
                  ;; Get inherited slots from parent class
                  (parent-info (if parent (assoc parent *class-env*) nil))
                  (parent-slots (if parent-info (caddr parent-info) nil))
                  (all-slots (append parent-slots slot-names))
                  (all-initforms (append (if parent-info (cadddr parent-info) nil) slot-initforms))
                  (num-slots (length all-slots))
                  ;; Register class
                  (class-entry (list name parent all-slots all-initforms)))
             ;; Store class info
             (push class-entry *class-env*)
             ;; Generate accessor functions
             (let* ((accessor-defuns
                     (loop for slot in all-slots
                           for i from 1
                           collect (let ((accessor-name (intern (concatenate 'string
                                                                  (symbol-name name) "-"
                                                                  (symbol-name slot)))))
                                     `(defun ,accessor-name (obj) (vector-ref obj ,i)))))
                    ;; Generate predicate
                    (predicate-name (intern (concatenate 'string (symbol-name name) "-P")))
                    (predicate-defun `(defun ,predicate-name (obj)
                                        (and (vectorp obj)
                                             (> (vector-length obj) 0)
                                             (eq (vector-ref obj 0) ',name))))
                    (all-defuns (cons predicate-defun accessor-defuns)))
               ;; Register accessors for setf support
               (loop for slot in all-slots
                     for i from 1
                     do (push (cons (intern (concatenate 'string (symbol-name name) "-" (symbol-name slot))) i)
                              *struct-accessors*))
               ;; Process generated defuns followed by rest of forms
               (compile-forms-helper (append all-defuns (cdr forms)) env fenv))))

          ;; defgeneric - declare a generic function
          ;; (defgeneric name (args) ...)
          ;; Track generic function name and arity for dispatcher generation
          ((and (consp form) (eq (car form) 'defgeneric))
           (let* ((name (cadr form))
                  (lambda-list (caddr form))
                  (arity (length lambda-list)))
             ;; Register generic if not already present
             (unless (assoc name *method-env*)
               (push (list name arity nil) *method-env*))
             (compile-forms-helper (cdr forms) env fenv)))

          ;; defmethod - define a method for a generic function
          ;; (defmethod name ((arg class) ...) body)
          ;; Single-dispatch on first arg. Generates specialized function.
          ((and (consp form) (eq (car form) 'defmethod))
           (let* ((name (cadr form))
                  (lambda-list (caddr form))
                  (body (cdddr form))
                  ;; Parse first arg to get specializer
                  (first-param (car lambda-list))
                  (param-name (if (consp first-param) (car first-param) first-param))
                  (specializer (if (consp first-param) (cadr first-param) t))
                  ;; All params for the function
                  (all-params (cons param-name (mapcar (lambda (p) (if (consp p) (car p) p)) (cdr lambda-list))))
                  ;; Create specialized function name: name/class
                  (method-fn-name (intern (concatenate 'string
                                                       (symbol-name name) "/"
                                                       (if (eq specializer t)
                                                           "T"
                                                           (symbol-name specializer)))))
                  ;; Create defun for specialized method
                  (method-defun `(defun ,method-fn-name ,all-params ,@body))
                  ;; Get or create generic entry
                  (generic-entry (assoc name *method-env*))
                  (arity (length all-params)))
             ;; Register generic if not present
             (if generic-entry
                 ;; Add method to existing generic
                 (let ((methods (caddr generic-entry)))
                   (setf (caddr generic-entry)
                         (cons (cons specializer method-fn-name) methods)))
                 ;; Create new generic entry
                 (push (list name arity (list (cons specializer method-fn-name))) *method-env*))
             ;; Process generated defun followed by rest of forms
             (compile-forms-helper (cons method-defun (cdr forms)) env fenv)))

          ;; defstruct - expand to constructor, predicate, and accessors
          ;; (defstruct name slot1 slot2 ...) expands to multiple defuns
          ;; Structure is stored as vector: [type-symbol slot1-val slot2-val ...]
          ((and (consp form) (eq (car form) 'defstruct))
           (let* ((name (cadr form))
                  (slots (cddr form))
                  (slot-names (mapcar (lambda (s) (if (consp s) (car s) s)) slots))
                  (num-slots (length slot-names))
                  (type-sym (intern (symbol-name name)))
                  ;; Generate function names
                  (constructor-name (intern (concatenate 'string "MAKE-" (symbol-name name))))
                  (predicate-name (intern (concatenate 'string (symbol-name name) "-P")))
                  (copier-name (intern (concatenate 'string "COPY-" (symbol-name name))))
                  ;; Generate accessor names
                  (accessor-names (mapcar (lambda (slot)
                                            (intern (concatenate 'string (symbol-name name) "-" (symbol-name slot))))
                                          slot-names))
                  ;; Generate constructor with keyword args
                  ;; (defun make-foo (&key slot1 slot2 ...)
                  ;;   (let ((v (make-vector (+ 1 num-slots))))
                  ;;     (vector-set v 0 'type-sym)
                  ;;     (vector-set v 1 slot1) ...
                  ;;     v))
                  (constructor-params (mapcan (lambda (s) (list (intern (concatenate 'string ":" (symbol-name s))) s)) slot-names))
                  (constructor-body
                   `(let ((v (make-vector ,(+ 1 num-slots))))
                      (vector-set v 0 ',type-sym)
                      ,@(loop for slot in slot-names
                              for i from 1
                              collect `(vector-set v ,i ,slot))
                      v))
                  (constructor-defun `(defun ,constructor-name (&key ,@slot-names) ,constructor-body))
                  ;; Generate predicate
                  ;; (defun foo-p (obj)
                  ;;   (and (vectorp obj)
                  ;;        (> (vector-length obj) 0)
                  ;;        (eq (vector-ref obj 0) 'type-sym)))
                  (predicate-defun `(defun ,predicate-name (obj)
                                      (and (vectorp obj)
                                           (> (vector-length obj) 0)
                                           (eq (vector-ref obj 0) ',type-sym))))
                  ;; Generate copier
                  ;; (defun copy-foo (obj)
                  ;;   (let ((new (make-vector (+ 1 num-slots))))
                  ;;     (vector-set new 0 (vector-ref obj 0))
                  ;;     (vector-set new 1 (vector-ref obj 1)) ...
                  ;;     new))
                  (copier-body
                   `(let ((new (make-vector ,(+ 1 num-slots))))
                      ,@(loop for i from 0 to num-slots
                              collect `(vector-set new ,i (vector-ref obj ,i)))
                      new))
                  (copier-defun `(defun ,copier-name (obj) ,copier-body))
                  ;; Generate accessors
                  ;; (defun foo-slot (obj) (vector-ref obj idx))
                  (accessor-defuns (loop for slot in slot-names
                                         for accessor in accessor-names
                                         for i from 1
                                         collect `(defun ,accessor (obj) (vector-ref obj ,i))))
                  ;; All generated defuns
                  (all-defuns (cons constructor-defun (cons predicate-defun (cons copier-defun accessor-defuns)))))
             ;; Register accessors for setf support
             (loop for accessor in accessor-names
                   for i from 1
                   do (push (cons accessor i) *struct-accessors*))
             ;; Process generated defuns followed by rest of forms
             (compile-forms-helper (append all-defuns (cdr forms)) env fenv)))

          ;; defun - compile function
          ((and (consp form) (eq (car form) 'defun))
           (let* ((name (cadr form))
                  (params (caddr form))
                  (body (cadddr form))
                  (compiled-fn (compile-defun name params body env fenv))
                  ;; Add to function environment
                  (new-fenv (cons (cons name compiled-fn) fenv))
                  ;; Compile rest of forms
                  (rest-result (compile-forms-helper (cdr forms) env new-fenv))
                  (rest-fns (car rest-result))
                  (main-ir (cadr rest-result)))
             ;; Return accumulated functions and main expression
             (list (cons compiled-fn rest-fns) main-ir)))

          ;; Not defun/defmacro - this is the main expression
          ;; Generate method dispatchers first, then compile main expression
          (t
           (let* ((dispatcher-defuns (generate-method-dispatchers))
                  ;; Get dispatcher names before clearing *method-env*
                  (dispatcher-names (mapcar #'car *method-env*)))
             ;; Clear *method-env* to prevent re-generation in recursive calls
             (setq *method-env* nil)
             (if dispatcher-defuns
                 ;; Compile dispatchers first, then main expression
                 (let* ((dispatcher-result (compile-forms-helper dispatcher-defuns env fenv))
                        (dispatcher-fns (car dispatcher-result))
                        (new-fenv (append (mapcar (lambda (n) (cons n t)) dispatcher-names) fenv))
                        (main-ir (compile-expr (box-mutable-captured-vars form) env new-fenv)))
                   (list dispatcher-fns main-ir))
                 ;; No dispatchers, just compile main expression
                 (list nil (compile-expr (box-mutable-captured-vars form) env fenv)))))))
      ;; No more forms
      (list nil '(lit 0))))

(defun generate-method-dispatcher-defun (generic-name arity methods)
  "Generate a defun for a generic function dispatcher."
  (let* ((params (loop for i from 0 below arity
                       collect (intern (format nil "ARG~D" i))))
         (dispatch-obj (car params))
         (default-method (cdr (assoc t methods)))
         (specialized (remove-if (lambda (m) (eq (car m) t)) methods))
         (cond-clauses
          (loop for (class . fn-name) in specialized
                collect `((typep ,dispatch-obj ',class)
                          (,fn-name ,@params))))
         (all-clauses
          (if default-method
              (append cond-clauses `((t (,default-method ,@params))))
              (append cond-clauses '((t 0))))))
    `(defun ,generic-name ,params
       (cond ,@all-clauses))))

(defun generate-method-dispatchers ()
  "Generate dispatcher defuns for all registered generic functions."
  (loop for (name arity methods) in *method-env*
        when methods
        collect (generate-method-dispatcher-defun name arity methods)))

(defun compile-forms (forms)
  "Compile list of top-level forms including defmacro and defun."
  (let ((*collected-lambdas* nil)
        (*macro-env* nil)           ; Fresh macro environment for each compilation
        (*struct-accessors* nil)    ; Fresh struct accessor registry
        (*class-env* nil)           ; Fresh CLOS class registry
        (*method-env* nil))         ; Fresh method registry
    (let* ((result (compile-forms-helper forms nil nil))
           (fns (car result))
           (main-ir (cadr result)))
      (list (append fns (nreverse *collected-lambdas*)) main-ir))))

(defun codegen-function-with-params (params optional-names optional-inits optional-supplied body-ir runtime-addrs &optional fn-offsets current-offset param-base rest-param)
  "Generate code for function with parameters
   Parameters are passed in x0-x7, stored to stack for access as variables"
  (let* ((required-count (length params))
         (optional-count (length optional-names))
         (supplied-count (length optional-supplied))
         (total-non-rest (+ required-count optional-count))
         (has-rest (not (null rest-param)))
         (rest-offset (if has-rest (+ param-base total-non-rest supplied-count) nil))
         (prologue-size 6)
         ;; Cache temp slots for incoming args to preserve register values while building &rest or filling optionals
         (arg0-slot (temp-slot-offset 0))
         (arg1-slot (temp-slot-offset 1))
         (arg2-slot (temp-slot-offset 2))
         (arg3-slot (temp-slot-offset 3))
         (arg4-slot (temp-slot-offset 4))
         (need-arg-save (or has-rest (> optional-count 0)))
         (arg-save-code (when need-arg-save
                          (append
                           (arm64-str 0 31 arg0-slot)
                           (arm64-str 1 31 arg1-slot)
                           (arm64-str 2 31 arg2-slot)
                           (arm64-str 3 31 arg3-slot)
                           (arm64-str 4 31 arg4-slot))))
         ;; Store fixed parameters. For &rest, load from saved slots to avoid clobbering incoming registers.
         (param-store-code
           (let ((code (if need-arg-save arg-save-code nil)))
             (dotimes (i required-count)
               (let* ((param-offset (* (+ param-base i) 8))
                      (load-arg
                        (if (< i 5)
                            (if need-arg-save
                                (arm64-ldr 22 31 (cond
                                                   ((= i 0) arg0-slot)
                                                   ((= i 1) arg1-slot)
                                                   ((= i 2) arg2-slot)
                                                   ((= i 3) arg3-slot)
                                                   (t arg4-slot)))
                                (arm64-mov 22 i))
                            (arm64-ldr 22 25 (* (- i 5) #x8))))
                      (store (append
                               load-arg
                               (arm64-sub-imm 21 20 param-offset)
                               (arm64-str 22 21 0))))
                 (setf code (append code store))))
             code))
         (param-store-size (count-instrs param-store-code))
         (optional-code
           (let ((code '())
                 (cursor (+ prologue-size param-store-size)))
             (dotimes (i optional-count)
               (let* ((opt-offset (* (+ param-base required-count i) 8))
                      (addr-reg 21)
                      (idx-reg 12)
                      (threshold (+ required-count i))
                      (default-expr-ir (nth i optional-inits))
                      (default-eval (codegen-expr default-expr-ir runtime-addrs fn-offsets
                                                  (if current-offset
                                                      (+ current-offset cursor)
                                                      nil)
                                                  0))
                      (store-default (append
                                      (arm64-sub-imm addr-reg 20 opt-offset)
                                      (arm64-str 0 addr-reg 0)))
                      (default-block (append default-eval store-default))
                      (supplied-value
                        (cond
                          ((= threshold 0) (arm64-ldr 22 31 arg0-slot))
                          ((= threshold 1) (arm64-ldr 22 31 arg1-slot))
                          ((= threshold 2) (arm64-ldr 22 31 arg2-slot))
                          ((= threshold 3) (arm64-ldr 22 31 arg3-slot))
                          ((= threshold 4) (arm64-ldr 22 31 arg4-slot))
                          (t (arm64-ldr 22 25 (* (- threshold 5) #x8)))))
                      (supplied-offset (* (+ param-base required-count optional-count i) 8))
                      (store-supplied-flag (append
                                             (arm64-movz 0 #x10)
                                             (arm64-sub-imm addr-reg 20 supplied-offset)
                                             (arm64-str 0 addr-reg 0)))
                      (store-supplied (append
                                       supplied-value
                                       (arm64-sub-imm addr-reg 20 opt-offset)
                                       (arm64-str 22 addr-reg 0)
                                       store-supplied-flag))
                      (store-default-flag (append
                                            (arm64-movz 0 0)
                                            (arm64-sub-imm addr-reg 20 supplied-offset)
                                            (arm64-str 0 addr-reg 0)))
                      (default-block (append default-eval store-default store-default-flag))
                      (skip-default (+ (count-instrs default-block) 1))
                      (skip-to-default (+ (count-instrs store-supplied) 2))
                      (block (append
                              (arm64-movz idx-reg threshold)
                              (arm64-cmp 23 idx-reg)
                              (arm64-b-cond #xD skip-to-default) ; if arg_count <= threshold -> default
                              store-supplied
                              (arm64-b skip-default)
                              default-block))
                      (block-len (count-instrs block)))
                 (setf code (append code block))
                 (incf cursor block-len)))
             code))
         (optional-size (count-instrs optional-code))
         ;; Store remaining extra list to rest param if needed
         (rest-code
          (when has-rest
            (let* ((rest-list-reg 13)
                   (idx-reg 12)
                   (arg-reg 14)
                   (five-reg 15)
                   (addr-reg 16)
                   (limit-reg 17)
                   (offset-reg 10)
                   (init-code (append
                                (arm64-movz rest-list-reg #x0)
                                (arm64-mov idx-reg 23)
                                (arm64-sub-imm idx-reg idx-reg #x1)
                                (arm64-movz five-reg #x5)
                                (arm64-movz limit-reg total-non-rest)))
                   (extras-load (append
                                  (arm64-sub-imm offset-reg idx-reg #x5)
                                  (arm64-lsl offset-reg offset-reg 3)
                                  (arm64-add offset-reg 25 offset-reg)
                                  (arm64-ldr arg-reg offset-reg 0)))
                   (reg-load (append
                               (arm64-add-imm addr-reg 31 arg0-slot)
                               (arm64-lsl offset-reg idx-reg 3)
                               (arm64-add addr-reg addr-reg offset-reg)
                               (arm64-ldr arg-reg addr-reg 0)))
                   (cons-body (append
                                (arm64-mov 0 arg-reg)
                                (arm64-mov 1 rest-list-reg)
                                (arm64-ldr 9 19 0)
                                (arm64-blr 9)
                                (arm64-mov rest-list-reg 0)
                                (arm64-sub-imm idx-reg idx-reg #x1)))
                   (extras-len (count-instrs extras-load))
                   (reg-len (count-instrs reg-load))
                   (cons-core-len (count-instrs cons-body))
                   (offset-to-done (+ extras-len reg-len cons-core-len 5))
                   (offset-to-reg (+ extras-len 2))
                   (offset-to-cons (+ reg-len 1))
                   (loop-back-offset (- (+ extras-len reg-len cons-core-len 5))))
              (append
               init-code
               (arm64-cmp idx-reg limit-reg)
               (arm64-b-cond #xB offset-to-done)
               (arm64-cmp idx-reg five-reg)
               (arm64-b-cond #xB offset-to-reg)
               extras-load
               (arm64-b offset-to-cons)
               reg-load
               cons-body
               (arm64-b loop-back-offset)
               (arm64-sub-imm 1 20 (* rest-offset 8))
               (arm64-str rest-list-reg 1 0)))))
         (rest-size (count-instrs rest-code))
         (body-offset (if current-offset
                          (+ current-offset prologue-size param-store-size optional-size rest-size)
                          nil))
         ;; Pass fn-offsets and body-offset to body generation
         (body (codegen-expr body-ir runtime-addrs fn-offsets body-offset 0)))
    (append
      ;; Function prologue
      (arm64-sub-imm 31 31 *stack-frame-size*)      ; Allocate stack frame
      (arm64-stp 29 30 31 0)         ; Save FP/LR
      (arm64-stp 19 20 31 16)        ; Save x19/x20
      (arm64-stp 21 22 31 32)        ; Save x21/x22
      (arm64-stp 23 24 31 48)        ; Save x23/x24
      ;; x25 may carry extra-arg pointer from caller; leave intact
      ;; x25 may carry extra-arg pointer from caller; leave intact
      ;; x19 already has runtime table from caller - don't overwrite!
      (arm64-add-imm 20 31 *env-base-offset*)      ; Set environment base

      ;; Store parameters to stack
      param-store-code

      ;; Handle optional parameters
      optional-code

      ;; Build &rest if present
      rest-code

      ;; Function body
      body

      ;; Function epilogue
      (arm64-ldp 23 24 31 48)        ; Restore x23/x24
      (arm64-ldp 21 22 31 32)        ; Restore x21/x22
      (arm64-ldp 19 20 31 16)        ; Restore x19/x20
      (arm64-ldp 29 30 31 0)         ; Restore FP/LR
      (arm64-add-imm 31 31 *stack-frame-size*)      ; Deallocate stack
      (arm64-ret))))

(defun calculate-function-offsets (compiled-fns start-offset runtime-addrs)
  "First pass: calculate function offsets by generating code without fn-offsets"
  (if (consp compiled-fns)
(destructuring-bind (name params optional-names optional-inits optional-supplied body-ir captures param-base rest-param)
          (car compiled-fns)
        (let* (;; Generate without fn-offsets to get size
               (fn-code (codegen-function-with-params params optional-names optional-inits optional-supplied body-ir runtime-addrs nil nil param-base rest-param))
               (fn-size (count-instrs fn-code))
               ;; Recursively calculate rest
               (rest-offsets (calculate-function-offsets (cdr compiled-fns)
                                                         (+ start-offset fn-size)
                                                         runtime-addrs)))
          (cons (list name start-offset captures param-base rest-param) rest-offsets)))
      nil))

(defun codegen-functions-with-offsets (compiled-fns fn-offsets current-offset runtime-addrs)
  "Second pass: generate functions with correct fn-offsets"
  (if (consp compiled-fns)
(destructuring-bind (name params optional-names optional-inits optional-supplied body-ir captures param-base rest-param)
          (car compiled-fns)
        ;; Generate with fn-offsets for proper function calls
        (let* ((fn-code (codegen-function-with-params params optional-names optional-inits optional-supplied body-ir runtime-addrs
                                                      fn-offsets current-offset param-base rest-param))
               (fn-size (count-instrs fn-code))
               ;; Generate rest
               (rest-code (codegen-functions-with-offsets (cdr compiled-fns) fn-offsets
                                                          (+ current-offset fn-size)
                                                          runtime-addrs)))
          (append fn-code rest-code)))
      nil))

(defun codegen-functions-helper (compiled-fns current-offset runtime-addrs)
  "Generate code for all compiled functions using iterative offset stabilization.
   Returns: (total-code function-offsets)"
  (let ((fn-offsets (calculate-function-offsets compiled-fns current-offset runtime-addrs))
        (stable nil)
        (codes nil))
    (loop until stable do
      (let ((current current-offset)
            (new-offsets '())
            (new-codes '()))
        (dolist (fn compiled-fns)
          (destructuring-bind (name params optional-names optional-inits optional-supplied body-ir captures param-base rest-param)
              fn
            (let* ((fn-code (codegen-function-with-params params optional-names optional-inits optional-supplied body-ir runtime-addrs
                                                          fn-offsets current param-base rest-param))
                   (fn-size (count-instrs fn-code)))
              (push fn-code new-codes)
              (push (list name current captures param-base rest-param) new-offsets)
              (incf current fn-size))))
        (setf new-offsets (nreverse new-offsets))
        (setf new-codes (nreverse new-codes))
        (if (equal (mapcar #'cadr new-offsets) (mapcar #'cadr fn-offsets))
            (setf stable t)
            (setf fn-offsets new-offsets))
        (when stable
          (setf codes new-codes))))
    (list (apply #'append codes) fn-offsets)))

(defun codegen-expr-with-fns (ir runtime-addrs fn-offsets current-offset)
  "Codegen with function offset tracking"
  (codegen-expr ir runtime-addrs fn-offsets current-offset 0))

(defun codegen-main-with-runtime-and-fns (ir runtime-addrs fn-offsets current-offset)
  "Generate main code with function offsets for calls"
  ;; Pass function offsets through to codegen
  ;; The body comes after the 7-instruction prologue
  (let ((body (codegen-expr-with-fns ir runtime-addrs fn-offsets (+ current-offset 7))))
    ;; Same prologue/epilogue as before
    (append (arm64-sub-imm 31 31 1024)     ; SUB sp, sp, #1024 (stack frame)
            (arm64-stp 29 30 31 0)        ; STP x29, x30, [sp, #0]
            (arm64-stp 19 20 31 16)       ; STP x19, x20, [sp, #16]
            (arm64-stp 21 22 31 32)       ; STP x21, x22, [sp, #32]
            (arm64-stp 23 24 31 48)       ; STP x23, x24, [sp, #48]
            (arm64-mov 19 0)              ; MOV x19, x0 (save runtime table)
            (arm64-add-imm 20 31 384)     ; ADD x20, sp, #x180
            body                           ; Function body
            (arm64-ldp 23 24 31 48)       ; LDP x23, x24, [sp, #48]
            (arm64-ldp 21 22 31 32)       ; LDP x21, x22, [sp, #32]
            (arm64-ldp 19 20 31 16)       ; LDP x19, x20, [sp, #16]
            (arm64-ldp 29 30 31 0)        ; LDP x29, x30, [sp, #0]
            (arm64-add-imm 31 31 1024)     ; ADD sp, sp, #1024 (restore stack)
            (arm64-ret))))

(defun compile-program-with-functions-with-runtime (forms runtime-addrs)
  "Compile entire program with function definitions
   Returns: complete machine code with main at offset 0 (entry point)"
  (let* ((compile-result (compile-forms forms))
         (compiled-fns (car compile-result))
         (main-ir (cadr compile-result))
         ;; Initial main to estimate size
         (main-code-temp (codegen-main-with-runtime-and-fns main-ir runtime-addrs nil 0))
         (main-size-temp (count-instrs main-code-temp))
         ;; First pass functions
         (fns-pass1 (codegen-functions-helper compiled-fns main-size-temp runtime-addrs))
         (fn-offsets-pass1 (cadr fns-pass1))
         ;; Main with first-pass offsets
         (main-code-pass1 (codegen-main-with-runtime-and-fns main-ir runtime-addrs fn-offsets-pass1 0))
         (main-size-final (count-instrs main-code-pass1))
         ;; Recompute function offsets with final main size if changed
         (fns-result (if (= main-size-final main-size-temp)
                         fns-pass1
                         (codegen-functions-helper compiled-fns main-size-final runtime-addrs)))
         (fn-offsets (cadr fns-result))
         (fns-code (car fns-result))
         ;; Final main with final offsets
         (main-code (codegen-main-with-runtime-and-fns main-ir runtime-addrs fn-offsets 0)))
    (append main-code fns-code)))

(defun compile-program-with-functions (forms)
  "Stub: compile program using default runtime addresses"
  (compile-program-with-functions-with-runtime forms nil))
