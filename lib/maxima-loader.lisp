;; Maxima source loader for Habu.
;;
;; Loads a broad ordered subset of Maxima source files with per-file
;; error reporting so we can iteratively raise compatibility.

(defparameter *maxima-source-dir* "/tmp/maxima/src/")
(defparameter *maxima-package-init* (concatenate 'string *maxima-source-dir* "maxima-package.lisp"))

;; Prefer upstream package definitions when available so package semantics
;; match Maxima source without local rewrites.
(when (probe-file *maxima-package-init*)
  (load *maxima-package-init*))

(load "lib/maxima-stubs.lisp")

(defparameter *maxima-files*
  '(
    ;; bootstrap
    "lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
    "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "opers"
    "utils" "merror" "mutils"

    ;; core language/runtime
    "sumcon" "sublis" "mformt" "outmis" "ar"
    "comm" "comm2" "mlisp" "mmacro" "buildq"
    "simp" "float" "csimp" "csimp2" "zero"
    "logarc" "rpart" "numeric" "server" "macsys" "mload"
    "suprv1" "mactex" "dskfn"

    ;; algebraic database
    "inmis" "db"

    ;; factoring / rational
    "compar" "askp" "lesfac" "factor" "algfac" "nalgfa" "ufact"
    "ifactor" "rat3a" "rat3b" "rat3c" "rat3d" "rat3e" "nrat4"
    "ratout" "result"

    ;; translator and evaluator support
    "transl" "transs" "trans1" "trans2" "trans3" "trans4" "trans5"
    "transf" "troper" "trutil" "trmode" "trdata" "trpred" "transq"
    "acall" "fcall" "evalw" "trprop" "mdefun"

    ;; pattern / reader / display
    "schatc" "matcom" "matrun" "nisimp" "nparse"
    "displa" "nforma" "grind"

    ;; algebra and trig
    "spgcd" "ezgcd" "trigi" "trigo" "trgred"
    "bessel" "ellipt" "airy" "intpol"

    ;; calculus and special functions
    ;; defint/residu currently trigger uncatchable VM stack overflow
    "sinint" "sin" "risch" "specfn"

    ;; matrix / determinant / limits / solve
    "mat" "linnew" "matrix" "sprdet" "newinv" "newdet"
    "tlimit" "limit"
    "solve" "psolve" "algsys" "sqrtdenest" "polyrz" "cpoly"

    ;; misc high-traffic modules
    "scs" "asum" "optim" "marray" "mdot" "irinte" "series"
    "numth" "laplac" "pade" "homog" "combin" "nset"
    "rand-mt19937" "maxmin" "nummod" "conjugate"
    "expintegral" "gamma" "mstuff"

    ;; final init
    "autol" "max_ext" "init-cl"))

(defvar *maxima-ok-count* 0)
(defvar *maxima-failed* nil)

(defun maxima-source-path (name)
  (concatenate 'string *maxima-source-dir* name ".lisp"))

(defun maxima-try-load (name)
  (let ((path (maxima-source-path name)))
    (handler-case
        (progn
          (load path)
          (setq *maxima-ok-count* (+ *maxima-ok-count* 1))
          (format t "[OK] ~A~%" name)
          t)
      (condition (e)
        (setq *maxima-failed* (cons (cons name e) *maxima-failed*))
        (format t "[ERR] ~A :: ~A~%" name e)
        nil))))

(defun maxima-load-all ()
  (setq *maxima-ok-count* 0)
  (setq *maxima-failed* nil)
  (dolist (name *maxima-files*)
    (maxima-try-load name))
  ;; DB initializes MAXIMA::CONTEXT to GLOBAL while globals/compar use
  ;; MAXIMA::$CONTEXT/$CONTEXTS with $GLOBAL. Keep them aligned for
  ;; with-new-context users (e.g. $integrate path via mfuncall/$supcontext).
  (when (and (boundp 'maxima::context)
             (boundp 'maxima::$context)
             (boundp 'maxima::$contexts)
             (symbolp maxima::$context)
             (consp maxima::$contexts)
             (null (member maxima::context maxima::$contexts :test #'eq))
             (member maxima::$context maxima::$contexts :test #'eq))
    (setf maxima::context maxima::$context))
  (let ((total (length *maxima-files*))
        (fail (length *maxima-failed*)))
    (format t "~%=== Maxima Loader Summary ===~%")
    (format t "source: ~A~%" *maxima-source-dir*)
    (format t "loaded: ~D/~D~%" *maxima-ok-count* total)
    (format t "failed: ~D~%" fail)
    (when (> fail 0)
      (format t "~%failed files:~%")
      (dolist (it (reverse *maxima-failed*))
        (format t "  ~A~%" (car it))))
    (values *maxima-ok-count* total fail)))

;; Manual entrypoint:
;;   (maxima-load-all)
