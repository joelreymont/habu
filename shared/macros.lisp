;;; macros.lisp - Macro definitions for habu0 build
;;;
;;; These macros must be included BEFORE reg-alloc.lisp and codegen.lisp
;;; so that collect-defmacros can register them before compilation.
;;;
;;; The bootstrap compiler (compiler-sbcl.lisp) has special-form handling
;;; for DOLIST that produces dolist-ir, but ir-to-tac doesn't handle it.
;;; These macros expand BEFORE sys:compile sees them, avoiding the issue.

(defmacro dolist ((var list-form &optional result) &body body)
  "Iterate over list elements. Expands to labels loop."
  (let ((loop-fn (gensym "DOLIST"))
        (lst (gensym "LST")))
    `(let ((,lst ,list-form))
       (labels ((,loop-fn (,lst)
                  (if (null ,lst)
                      ,result
                      (let ((,var (car ,lst)))
                        ,@body
                        (,loop-fn (cdr ,lst))))))
         (,loop-fn ,lst)))))

(defmacro when (test &body body)
  "Execute body when test is true."
  `(if ,test (progn ,@body) nil))

(defmacro unless (test &body body)
  "Execute body when test is false."
  `(if ,test nil (progn ,@body)))

(defmacro dotimes ((var count &optional result) &body body)
  "Iterate var from 0 to count-1."
  (let ((loop-fn (gensym "DOTIMES"))
        (limit (gensym "LIMIT")))
    `(let ((,limit ,count))
       (labels ((,loop-fn (,var)
                  (if (< ,var ,limit)
                      (progn ,@body (,loop-fn (+ ,var 1)))
                      ,result)))
         (,loop-fn 0)))))
