#!/bin/bash
# Apply LAMBDA and FUNCALL support to habu0.lisp

# Backup the original
cp habu0.lisp habu0.lisp.backup

# Create a temporary Python script to do the insertions
cat > /tmp/patch_habu.py << 'PYTHON_SCRIPT'
import re

with open('habu0.lisp', 'r') as f:
    content = f.read()

# 1. Add IR tags after ir-tag-keywordp
ir_tags_addition = """(defun ir-tag-lambda () #x22)     ; lambda (closure creation)
(defun ir-tag-funcall () #x23)    ; funcall (closure invocation)
"""

content = content.replace(
    "(defun ir-tag-keywordp () #x21)   ; keywordp predicate\n\n;; Check if IR node has a specific tag",
    "(defun ir-tag-keywordp () #x21)   ; keywordp predicate\n" + ir_tags_addition + "\n;; Check if IR node has a specific tag"
)

# 2. Add free variable analysis functions before h0-compile
free_var_functions = """
;;; Free variable analysis for closures

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

;; Add symbol to list if not already present
(defun h0-add-free (sym acc)
  (if (h0-member-sym sym acc)
      acc
      (cons sym acc)))

;; Collect free variables from expression
(defun h0-collect-free (expr bound env acc)
  (cond
    ((null expr) acc)
    ((symbolp expr)
     (if (and (h0-in-env expr env)
              (not (h0-member-sym expr bound)))
         (h0-add-free expr acc)
         acc))
    ((not (consp expr)) acc)
    ((sym= (car expr) "QUOTE") acc)
    ((sym= (car expr) "LAMBDA")
     (let ((params (cadr expr))
           (body (caddr expr)))
       (h0-collect-free body (h0-append-lists params bound) env acc)))
    ((sym= (car expr) "LET")
     (let* ((bindings (cadr expr))
            (body (caddr expr))
            (names (h0-binding-names bindings))
            (vals (h0-binding-vals bindings))
            (acc2 (h0-collect-free-list vals bound env acc))
            (new-bound (h0-append-lists names bound)))
       (h0-collect-free body new-bound env acc2)))
    ((sym= (car expr) "LET*")
     (let* ((bindings (cadr expr))
            (body (caddr expr)))
       (h0-collect-free-let* bindings body bound env acc)))
    (t (h0-collect-free-list expr bound env acc))))

(defun h0-collect-free-list (exprs bound env acc)
  (if (null exprs)
      acc
      (let ((acc2 (h0-collect-free (car exprs) bound env acc)))
        (h0-collect-free-list (cdr exprs) bound env acc2))))

(defun h0-collect-free-let* (bindings body bound env acc)
  (if (null bindings)
      (h0-collect-free body bound env acc)
      (let* ((b (car bindings))
             (name (car b))
             (val (cadr b))
             (acc2 (h0-collect-free val bound env acc))
             (new-bound (cons name bound)))
        (h0-collect-free-let* (cdr bindings) body new-bound env acc2))))

(defun h0-binding-names (bindings)
  (if (null bindings)
      nil
      (cons (car (car bindings))
            (h0-binding-names (cdr bindings)))))

(defun h0-binding-vals (bindings)
  (if (null bindings)
      nil
      (cons (cadr (car bindings))
            (h0-binding-vals (cdr bindings)))))

(defun h0-append-lists (a b)
  (if (null a)
      b
      (cons (car a) (h0-append-lists (cdr a) b))))

(defun h0-find-free-vars (expr bound env)
  (h0-collect-free expr bound env nil))

(defun h0-get-var-offset (sym env)
  (if (null env)
      nil
      (if (string= (symbol-name sym) (car (car env)))
          #x0
          (let ((rest-off (h0-get-var-offset sym (cdr env))))
            (if rest-off
                (+ rest-off #x1)
                nil)))))

(defun h0-get-free-offsets (free-vars env)
  (if (null free-vars)
      nil
      (cons (h0-get-var-offset (car free-vars) env)
            (h0-get-free-offsets (cdr free-vars) env))))

(defun h0-make-param-env (params free-vars)
  (h0-make-env-with-offset params #x0
    (h0-make-env-with-offset free-vars (h0-list-length params) nil)))

(defun h0-make-env-with-offset (syms base rest)
  (if (null syms)
      rest
      (cons (cons (symbol-name (car syms)) nil)
            (h0-make-env-with-offset (cdr syms) (+ base #x1) rest))))

(defun h0-list-length (lst)
  (if (null lst)
      #x0
      (+ #x1 (h0-list-length (cdr lst)))))

(defun h0-compile-args (args env fenv)
  (if (null args)
      nil
      (cons (h0-compile (car args) env fenv)
            (h0-compile-args (cdr args) env fenv))))

"""

# Find location before h0-compile definition
h0_compile_match = re.search(r';; Compile expression to IR \(using numeric tags\)\n;; Uses sym= for string-based symbol comparison', content)
if h0_compile_match:
    pos = h0_compile_match.start()
    content = content[:pos] + free_var_functions + content[pos:]

with open('habu0.lisp', 'w') as f:
    f.write(content)

print("Step 1: Added IR tags and free variable analysis")
PYTHON_SCRIPT

python3 /tmp/patch_habu.py

echo "Patch applied successfully. Changes:"
echo "1. Added ir-tag-lambda and ir-tag-funcall IR tags"
echo "2. Added free variable analysis functions"
echo "3. Backup saved to habu0.lisp.backup"
