;;; macros.lisp - Macro definitions for habu0 build
;;;
;;; Defines non-CL macros used in habu0.lisp and bootstrap code.
;;; CL standard macros (dolist, dotimes, when, unless) use SBCL's native
;;; implementations during bootstrap.

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
