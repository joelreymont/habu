;;;; Source-to-Source Expansions
;;;; Shared macro-like transformations for SBCL bootstrap and native Habu.
;;;;
;;;; These are pure source transformations - no IR, no env, no fenv.
;;;; Each expand-* function takes source forms and returns source forms.
;;;;
;;;; Used by compiler.lisp's compile-expr-full to handle macro-like forms.

(in-package :habu)

;;; ============================================================
;;; Pattern Matching - (match expr (pattern body...)...)
;;; ============================================================
;;;
;;; Patterns:
;;;   _              wildcard (always matches)
;;;   nil            literal nil
;;;   symbol         variable binding
;;;   'sym           literal symbol
;;;   number         literal number
;;;   "string"       literal string
;;;   (cons p1 p2)   destructure cons
;;;   (list p1 ...)  exact-length list
;;;   (list* p1 ... rest)  list with rest

(defun expand-match (scrutinee clauses)
  "Expand (match scrutinee clauses...) to let + nested if."
  (let ((val-sym (gensym "M")))
    `(let ((,val-sym ,scrutinee))
       ,(expand-match-clauses val-sym clauses))))

(defun expand-match-clauses (val-sym clauses)
  "Expand match clauses to nested if/let code."
  (if (null clauses)
      nil
      (let* ((clause (car clauses))
             (pattern (car clause))
             (body `(progn ,@(cdr clause)))
             (rest-code (expand-match-clauses val-sym (cdr clauses))))
        (expand-pattern pattern val-sym body rest-code))))

(defun expand-pattern (pattern val-sym success fail)
  "Expand a single pattern to if/let code."
  (cond
    ;; Wildcard - always matches
    ((eq pattern '_) success)

    ;; nil literal
    ((null pattern)
     `(if (null ,val-sym) ,success ,fail))

    ;; Variable binding - always matches, binds value
    ((symbolp pattern)
     `(let ((,pattern ,val-sym)) ,success))

    ;; Quoted symbol - (quote sym) or 'sym
    ((and (consp pattern)
          (eq (car pattern) 'quote)
          (symbolp (cadr pattern)))
     `(if (eq ,val-sym ,pattern) ,success ,fail))

    ;; Number literal
    ((numberp pattern)
     `(if (= ,val-sym ,pattern) ,success ,fail))

    ;; String literal
    ((stringp pattern)
     `(if (string= ,val-sym ,pattern) ,success ,fail))

    ;; (cons p1 p2) - destructure cons cell
    ((and (consp pattern) (eq (car pattern) 'cons))
     (let ((p1 (cadr pattern))
           (p2 (caddr pattern))
           (car-sym (gensym "A"))
           (cdr-sym (gensym "D")))
       `(if (consp ,val-sym)
            (let ((,car-sym (car ,val-sym))
                  (,cdr-sym (cdr ,val-sym)))
              ,(expand-pattern p1 car-sym
                 (expand-pattern p2 cdr-sym success fail)
                 fail))
            ,fail)))

    ;; (list p1 p2 ...) - exact-length list
    ((and (consp pattern) (eq (car pattern) 'list))
     (expand-pattern (list-to-cons-pattern (cdr pattern))
                     val-sym success fail))

    ;; (list* p1 p2 ... rest) - list with rest
    ((and (consp pattern) (eq (car pattern) 'list*))
     (expand-pattern (list*-to-cons-pattern (cdr pattern))
                     val-sym success fail))

    ;; Unknown pattern - treat as always match
    (t success)))

(defun list-to-cons-pattern (elems)
  "Convert (a b c) to (cons a (cons b (cons c nil)))."
  (if (null elems)
      nil
      (list 'cons (car elems) (list-to-cons-pattern (cdr elems)))))

(defun list*-to-cons-pattern (elems)
  "Convert (a b c rest) to (cons a (cons b (cons c rest)))."
  (if (null (cdr elems))
      (car elems)
      (list 'cons (car elems) (list*-to-cons-pattern (cdr elems)))))

;;; ============================================================
;;; Control Flow Expansions
;;; ============================================================

(defun expand-cond (clauses)
  "Expand (cond (test1 body1) (test2 body2) ...) to nested if."
  (if (null clauses)
      nil
      (let* ((clause (car clauses))
             (test (car clause))
             (body (if (cdr clause)
                       `(progn ,@(cdr clause))
                       test)))  ; (cond (x)) returns x if true
        (if (eq test 't)
            body
            `(if ,test ,body ,(expand-cond (cdr clauses)))))))

(defun expand-and (args)
  "Expand (and a b c ...) to nested if. Short-circuits correctly."
  (cond
    ((null args) t)  ; (and) = t
    ((null (cdr args)) (car args))  ; (and x) = x
    (t `(if ,(car args)
            ,(expand-and (cdr args))
            nil))))

(defun expand-or (args)
  "Expand (or a b c ...) to let + nested if. Evaluates each arg once."
  (cond
    ((null args) nil)  ; (or) = nil
    ((null (cdr args)) (car args))  ; (or x) = x
    (t (let ((tmp (gensym "OR")))
         `(let ((,tmp ,(car args)))
            (if ,tmp ,tmp ,(expand-or (cdr args))))))))

;;; ============================================================
;;; Binding Form Expansions
;;; ============================================================

(defun expand-let* (bindings body)
  "Expand (let* ((v1 e1) (v2 e2)) body) to nested let."
  (if (null bindings)
      `(progn ,@body)
      `(let (,(car bindings))
         ,(expand-let* (cdr bindings) body))))

(defun expand-prog1 (first &rest rest)
  "Expand (prog1 first rest...) to let + progn returning first."
  (let ((tmp (gensym "P1")))
    `(let ((,tmp ,first))
       ,@rest
       ,tmp)))

;;; ============================================================
;;; Iteration Expansions
;;; ============================================================

(defun expand-dotimes (var-count-result body)
  "Expand (dotimes (var count [result]) body...) to labels loop."
  (let ((var (car var-count-result))
        (count (cadr var-count-result))
        (result (caddr var-count-result))
        (loop-fn (gensym "DOTIMES"))
        (limit (gensym "LIMIT")))
    `(let ((,limit ,count))
       (labels ((,loop-fn (,var)
                  (if (< ,var ,limit)
                      (progn ,@body (,loop-fn (+ ,var 1)))
                      ,result)))
         (,loop-fn 0)))))

(defun expand-dolist (var-list-result body)
  "Expand (dolist (var list [result]) body...) to labels loop."
  (let ((var (car var-list-result))
        (list-form (cadr var-list-result))
        (result (caddr var-list-result))
        (loop-fn (gensym "DOLIST"))
        (lst (gensym "LST")))
    `(let ((,lst ,list-form))
       (labels ((,loop-fn (,lst)
                  (if (null ,lst)
                      ,(or result nil)
                      (let ((,var (car ,lst)))
                        ,@body
                        (,loop-fn (cdr ,lst))))))
         (,loop-fn ,lst)))))

;;; ============================================================
;;; Utility Expansions
;;; ============================================================

(defun expand-nth (n list-form)
  "Expand (nth n list) to nested car/cdr for constant n, or loop otherwise."
  (if (and (integerp n) (<= n 10))
      ;; Small constant - inline car/cdr chain
      (expand-nth-inline n list-form)
      ;; Variable or large - use loop
      (let ((loop-fn (gensym "NTH")))
        `(labels ((,loop-fn (n lst)
                    (if (= n 0)
                        (car lst)
                        (,loop-fn (- n 1) (cdr lst)))))
           (,loop-fn ,n ,list-form)))))

(defun expand-nth-inline (n list-form)
  "Expand (nth n list) to car/cdr chain for small constant n."
  (if (= n 0)
      `(car ,list-form)
      `(car ,(expand-nthcdr-inline n list-form))))

(defun expand-nthcdr-inline (n list-form)
  "Expand (nthcdr n list) to cdr chain."
  (if (= n 0)
      list-form
      `(cdr ,(expand-nthcdr-inline (1- n) list-form))))

(defun expand-length (list-form)
  "Expand (length list) to iterative loop."
  (let ((loop-fn (gensym "LEN"))
        (lst (gensym "LST"))
        (acc (gensym "ACC")))
    `(labels ((,loop-fn (,lst ,acc)
                (if (null ,lst)
                    ,acc
                    (,loop-fn (cdr ,lst) (+ ,acc 1)))))
       (,loop-fn ,list-form 0))))

(defun expand-reverse (list-form)
  "Expand (reverse list) to iterative loop."
  (let ((loop-fn (gensym "REV"))
        (lst (gensym "LST"))
        (acc (gensym "ACC")))
    `(labels ((,loop-fn (,lst ,acc)
                (if (null ,lst)
                    ,acc
                    (,loop-fn (cdr ,lst) (cons (car ,lst) ,acc)))))
       (,loop-fn ,list-form nil))))
