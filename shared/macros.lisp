;;; macros.lisp - Macro definitions for habu0 build
;;;
;;; Defines non-CL macros used in habu0.lisp and bootstrap code.
;;; CL standard macros (dolist, dotimes, when, unless) use SBCL's native
;;; implementations during bootstrap.

;; Always use HABU package so macros are defined there for cross-compilation
(in-package :habu)

(defmacro while (test &body body)
  "While loop - expands to labels recursion to avoid package issues with LOOP"
  (let ((loop-fn (gensym "WHILE")))
    `(labels ((,loop-fn ()
                (if ,test
                    (progn ,@body (,loop-fn))
                    nil)))
       (,loop-fn))))

;;; Native habu0 needs its own incf/decf since CL:INCF can't be used
;;; These are only for native compilation, not SBCL bootstrap
#-sbcl
(defmacro incf (place &optional (delta 1))
  "Increment PLACE by DELTA (default 1)"
  `(setq ,place (+ ,place ,delta)))

#-sbcl
(defmacro decf (place &optional (delta 1))
  "Decrement PLACE by DELTA (default 1)"
  `(setq ,place (- ,place ,delta)))

;;; Native CL-like functions needed by reg-alloc.lisp
#-sbcl
(defun remove-if-not (pred lst)
  "Return list of elements where PRED returns true"
  (if (null lst)
      nil
      (if (funcall pred (car lst))
          (cons (car lst) (remove-if-not pred (cdr lst)))
          (remove-if-not pred (cdr lst)))))

#-sbcl
(defun remove-duplicates (lst)
  "Return list with duplicates removed (keeps first occurrence)"
  (labels ((helper (remaining seen acc)
             (if (null remaining)
                 (reverse acc)
                 (let ((x (car remaining)))
                   (if (member x seen)
                       (helper (cdr remaining) seen acc)
                       (helper (cdr remaining) (cons x seen) (cons x acc)))))))
    (helper lst nil nil)))

#-sbcl
(defun nreverse (lst)
  "Destructively reverse LST by modifying cdr pointers in place.
   Returns the new head of the list (which was the last cons cell)."
  (let ((prev nil)
        (curr lst))
    (while curr
      (let ((next (cdr curr)))
        (setcdr curr prev)
        (setq prev curr)
        (setq curr next)))
    prev))
