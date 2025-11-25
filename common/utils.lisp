;;;; Common Utilities for Habu Compiler
;;;; Pure Habu - no SBCL dependencies

;;; Check if IR has a specific tag
(defun has-tag? (ir tag)
  (and (cons? ir) (eq (car ir) tag)))

;;; Package-agnostic symbol name comparison
(defun op= (sym name)
  (and (symbol? sym) (string= (symbol-name sym) name)))

;;; Environment lookup - find symbol's stack offset
(defun env-lookup (sym env)
  (if (nil? env)
      nil
      (if (eq (car (car env)) sym)
          (cdr (car env))
          (env-lookup sym (cdr env)))))

;;; Extend environment with new bindings
(defun env-extend (bindings env)
  (labels ((max-offset (e acc)
             (if (nil? e)
                 acc
                 (max-offset (cdr e)
                             (let ((off (cdr (car e))))
                               (if (> off acc) off acc)))))
           (add-bindings (bs offset acc)
             (if (nil? bs)
                 acc
                 (add-bindings (cdr bs)
                               (+ offset 1)
                               (cons (cons (car (car bs)) offset) acc)))))
    (let ((max-off (if env (max-offset env -1) -1)))
      (append (reverse (add-bindings bindings (+ max-off 1) nil)) env))))

;;; Function environment lookup
(defun fenv-lookup (sym fenv)
  (if (nil? fenv)
      nil
      (if (eq (car (car fenv)) sym)
          (cdr (car fenv))
          (fenv-lookup sym (cdr fenv)))))

;;; Collect all variable offsets referenced in IR
(defun collect-var-offsets (ir)
  (if (nil? ir)
      nil
      (if (has-tag? ir 'var)
          (list (car (cdr ir)))
          (if (has-tag? ir 'capture)
              nil
              (if (cons? ir)
                  (remove-duplicates
                   (append (collect-var-offsets (car ir))
                           (collect-var-offsets (cdr ir))))
                  nil)))))

;;; Rewrite var nodes to capture nodes based on capture-map
(defun rewrite-captures (ir capture-map)
  (if (nil? ir)
      nil
      (if (has-tag? ir 'var)
          (let ((off (car (cdr ir))))
            (let ((entry (assoc off capture-map)))
              (if entry
                  (list 'capture (cdr entry))
                  ir)))
          (if (cons? ir)
              (cons (rewrite-captures (car ir) capture-map)
                    (rewrite-captures (cdr ir) capture-map))
              ir))))

;;; Remove duplicates from list
(defun remove-duplicates (lst)
  (labels ((iter (remaining seen)
             (if (nil? remaining)
                 (reverse seen)
                 (let ((el (car remaining)))
                   (if (member el seen)
                       (iter (cdr remaining) seen)
                       (iter (cdr remaining) (cons el seen)))))))
    (iter lst nil)))

;;; Count instructions in code (4 bytes each)
(defun count-instrs (code)
  (if (nil? code)
      0
      (ash (length code) -2)))

;;; Runtime address lookup
(defun runtime-lookup (name runtime-addrs)
  (let ((entry (assoc name runtime-addrs)))
    (if entry (cdr entry) 0)))

;;; Global counter for generating unique names
(setq *gensym-counter* 0)

;;; Generate unique symbol (counter-based)
;;; Returns a cons of (base-name . counter) as a pseudo-symbol
(defun make-unique-name (base)
  (let ((n *gensym-counter*))
    (setq *gensym-counter* (+ n 1))
    (cons base n)))

;;; Check if a name matches a unique-name
(defun unique-name-matches? (unique-name sym)
  (and (cons? unique-name)
       (eq (car unique-name) sym)))

;;; Transform an expression, replacing calls to local functions
;;; fn-names: list of local function names
;;; box-map: alist of (fn-name . box-var)
(defun transform-local-calls (expr fn-names box-map)
  (cond
    ((nil? expr) nil)
    ((symbol? expr) expr)
    ((not (cons? expr)) expr)
    ;; (fn args...) where fn is local -> (funcall (car fn-box) args...)
    ((and (symbol? (car expr)) (member (car expr) fn-names))
     (let ((box-var (cdr (assoc (car expr) box-map))))
       (cons 'funcall
             (cons (list 'car box-var)
                   (mapcar (lambda (arg) (transform-local-calls arg fn-names box-map))
                           (cdr expr))))))
    ;; Recurse into other forms
    (t (mapcar (lambda (sub) (transform-local-calls sub fn-names box-map)) expr))))
