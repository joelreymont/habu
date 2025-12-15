;;;; IR to TAC - Convert tree IR to linear Three Address Code
;;;;
;;;; Input: ir-node (tree structure)
;;;; Output: list of tac-instr (linear sequence)
;;;;
;;;; Uses match macro for exhaustiveness checking.

(defpackage :habu.ir-to-tac
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.ir :ir-node)
  (:import-from :habu.tac
                :tac-lit :tac-nil :tac-t :tac-move :tac-var :tac-setvar
                :tac-global :tac-set-global
                :tac-add :tac-sub :tac-mul :tac-div :tac-mod :tac-neg
                :tac-eq :tac-eql :tac-lt :tac-gt :tac-le :tac-ge :tac-zerop
                :tac-not :tac-band :tac-bor :tac-bxor :tac-bsh :tac-bnot
                :tac-label :tac-goto :tac-if :tac-ifnot :tac-return
                :tac-param :tac-arg :tac-call :tac-funcall
                :tac-cons :tac-car :tac-cdr :tac-list
                :tac-null :tac-consp :tac-symbolp :tac-stringp :tac-numberp
                :tac-keywordp :tac-functionp
                :tac-string-length :tac-string-ref :tac-string-concat :tac-string-lit
                :tac-make-vector :tac-vector-ref :tac-vector-set :tac-vector-length
                :tac-make-symbol :tac-symbol-name :tac-intern :tac-symbol-lit
                :tac-keyword-name :tac-keyword-lit
                :tac-exit :tac-error)
  (:export :ir-to-tac))

(in-package :habu.ir-to-tac)

;;; Virtual register counter
(defvar *vreg-counter* 0)
(defvar *label-counter* 0)
(defvar *tac-instrs* nil)

(defun reset-tac-state ()
  (setf *vreg-counter* 0)
  (setf *label-counter* 0)
  (setf *tac-instrs* nil))

(defun next-vreg ()
  (prog1 *vreg-counter*
    (incf *vreg-counter*)))

(defun next-label (prefix)
  (prog1 (intern (format nil "~A~D" prefix *label-counter*))
    (incf *label-counter*)))

(defun emit (instr)
  (push instr *tac-instrs*))

;;; Main conversion function

(defun ir-to-tac (ir)
  "Convert IR tree to linear TAC.
   Returns: (list tac-instr)"
  (reset-tac-state)
  (let ((result (convert-ir ir)))
    (emit (tac-return result))
    (nreverse *tac-instrs*)))

(defun convert-ir (ir)
  "Convert IR node, emitting TAC instructions.
   Returns: vreg containing the result.
   Uses short names in match patterns (prefix 'ir' is automatic)."
  (match ir-node ir
    ;; === Literals ===
    (lit (value)
      (let ((dest (next-vreg)))
        (emit (tac-lit dest value))
        dest))

    (nil ()
      (let ((dest (next-vreg)))
        (emit (tac-nil dest))
        dest))

    (t ()
      (let ((dest (next-vreg)))
        (emit (tac-t dest))
        dest))

    (str (string)
      (let ((dest (next-vreg)))
        (emit (tac-string-lit dest string))
        dest))

    (sym (name)
      (let ((dest (next-vreg)))
        (emit (tac-symbol-lit dest name))
        dest))

    (kw (name)
      (let ((dest (next-vreg)))
        (emit (tac-keyword-lit dest name))
        dest))

    ;; === Variables ===
    (var (offset)
      (let ((dest (next-vreg)))
        (emit (tac-var dest offset))
        dest))

    (setq (offset value)
      (let ((src (convert-ir value)))
        (emit (tac-setvar offset src))
        src))

    (global (name)
      (let ((dest (next-vreg)))
        (emit (tac-global dest name))
        dest))

    (set-global (name value)
      (let ((src (convert-ir value)))
        (emit (tac-set-global name src))
        src))

    ;; === Arithmetic ===
    (add (left right)
      (convert-binop #'tac-add left right))

    (sub (left right)
      (convert-binop #'tac-sub left right))

    (mul (left right)
      (convert-binop #'tac-mul left right))

    (div (left right)
      (convert-binop #'tac-div left right))

    (mod (left right)
      (convert-binop #'tac-mod left right))

    (neg (value)
      (convert-unop #'tac-neg value))

    ;; === Comparison ===
    (eq (left right)
      (convert-binop #'tac-eq left right))

    (eql (left right)
      (convert-binop #'tac-eql left right))

    (lt (left right)
      (convert-binop #'tac-lt left right))

    (gt (left right)
      (convert-binop #'tac-gt left right))

    (le (left right)
      (convert-binop #'tac-le left right))

    (ge (left right)
      (convert-binop #'tac-ge left right))

    (zerop (value)
      (convert-unop #'tac-zerop value))

    ;; === Logical ===
    (not (value)
      (convert-unop #'tac-not value))

    (and (left right)
      (let ((dest (next-vreg))
            (false-label (next-label "AND_FALSE"))
            (end-label (next-label "AND_END")))
        (let ((l (convert-ir left)))
          (emit (tac-ifnot l false-label))
          (let ((r (convert-ir right)))
            (emit (tac-move dest r))
            (emit (tac-goto end-label))
            (emit (tac-label false-label))
            (emit (tac-nil dest))
            (emit (tac-label end-label))
            dest))))

    (or (left right)
      (let ((dest (next-vreg))
            (true-label (next-label "OR_TRUE"))
            (end-label (next-label "OR_END")))
        (let ((l (convert-ir left)))
          (emit (tac-move dest l))
          (emit (tac-if l true-label))
          (let ((r (convert-ir right)))
            (emit (tac-move dest r))
            (emit (tac-label true-label))
            dest))))

    ;; === Bitwise ===
    (band (left right)
      (convert-binop #'tac-band left right))

    (bor (left right)
      (convert-binop #'tac-bor left right))

    (bxor (left right)
      (convert-binop #'tac-bxor left right))

    (bsh (value shift)
      (convert-binop #'tac-bsh value shift))

    (bnot (value)
      (convert-unop #'tac-bnot value))

    ;; === Control Flow ===
    (if (test then else)
      (let ((dest (next-vreg))
            (else-label (next-label "IF_ELSE"))
            (end-label (next-label "IF_END")))
        (let ((cond-vreg (convert-ir test)))
          (emit (tac-ifnot cond-vreg else-label))
          (let ((then-vreg (convert-ir then)))
            (emit (tac-move dest then-vreg))
            (emit (tac-goto end-label))
            (emit (tac-label else-label))
            (let ((else-vreg (convert-ir else)))
              (emit (tac-move dest else-vreg))
              (emit (tac-label end-label))
              dest)))))

    (progn (forms)
      (if (null forms)
          (let ((dest (next-vreg)))
            (emit (tac-nil dest))
            dest)
          (let ((result nil))
            (dolist (form forms result)
              (setf result (convert-ir form))))))

    (while (test body)
      (let ((loop-label (next-label "WHILE_LOOP"))
            (end-label (next-label "WHILE_END"))
            (dest (next-vreg)))
        (emit (tac-nil dest))  ; default result is nil
        (emit (tac-label loop-label))
        (let ((cond-vreg (convert-ir test)))
          (emit (tac-ifnot cond-vreg end-label))
          (convert-ir body)  ; body result discarded
          (emit (tac-goto loop-label))
          (emit (tac-label end-label))
          dest)))

    (let (bindings body)
      ;; bindings is ((offset . init-ir) ...)
      (dolist (binding bindings)
        (let ((offset (car binding))
              (init-ir (cdr binding)))
          (let ((init-vreg (convert-ir init-ir)))
            (emit (tac-setvar offset init-vreg)))))
      (convert-ir body))

    ;; === Functions ===
    (call (name args)
      (let ((arg-vregs nil))
        ;; Evaluate arguments
        (dolist (arg args)
          (push (convert-ir arg) arg-vregs))
        (setf arg-vregs (nreverse arg-vregs))
        ;; Emit arg instructions
        (loop for vreg in arg-vregs
              for i from 0
              do (emit (tac-arg i vreg)))
        ;; Emit call
        (let ((dest (next-vreg)))
          (emit (tac-call dest name (length args)))
          dest)))

    (lambda (params body captures)
      ;; For now, emit as a symbol representing the lambda
      ;; Full closure support would need more work
      (let ((dest (next-vreg)))
        ;; TODO: proper lambda/closure compilation
        (emit (tac-nil dest))
        dest))

    (funcall (fn args)
      (let ((fn-vreg (convert-ir fn))
            (arg-vregs nil))
        ;; Evaluate arguments
        (dolist (arg args)
          (push (convert-ir arg) arg-vregs))
        (setf arg-vregs (nreverse arg-vregs))
        ;; Emit arg instructions
        (loop for vreg in arg-vregs
              for i from 0
              do (emit (tac-arg i vreg)))
        ;; Emit funcall
        (let ((dest (next-vreg)))
          (emit (tac-funcall dest fn-vreg (length args)))
          dest)))

    ;; === List Operations ===
    (cons (car cdr)
      (let ((car-vreg (convert-ir car))
            (cdr-vreg (convert-ir cdr))
            (dest (next-vreg)))
        (emit (tac-cons dest car-vreg cdr-vreg))
        dest))

    (car (cell)
      (convert-unop #'tac-car cell))

    (cdr (cell)
      (convert-unop #'tac-cdr cell))

    (list (elems)
      (let ((elem-vregs (mapcar #'convert-ir elems))
            (dest (next-vreg)))
        (emit (tac-list dest elem-vregs))
        dest))

    (length (lst)
      ;; TODO: implement proper length TAC instruction
      ;; For now, emit a call to length function
      (let ((list-vreg (convert-ir lst))
            (dest (next-vreg)))
        (emit (tac-arg 0 list-vreg))
        (emit (tac-call dest 'length 1))
        dest))

    ;; === Type Predicates ===
    (null (value)
      (convert-unop #'tac-null value))

    (consp (value)
      (convert-unop #'tac-consp value))

    (symbolp (value)
      (convert-unop #'tac-symbolp value))

    (stringp (value)
      (convert-unop #'tac-stringp value))

    (numberp (value)
      (convert-unop #'tac-numberp value))

    (keywordp (value)
      (convert-unop #'tac-keywordp value))

    (functionp (value)
      (convert-unop #'tac-functionp value))

    ;; === String Operations ===
    (string-length (str)
      (convert-unop #'tac-string-length str))

    (string-ref (str index)
      (convert-binop #'tac-string-ref str index))

    (string-concat (left right)
      (convert-binop #'tac-string-concat left right))

    ;; === Vector Operations ===
    (make-vector (size init)
      (let ((size-vreg (convert-ir size))
            (init-vreg (convert-ir init))
            (dest (next-vreg)))
        (emit (tac-make-vector dest size-vreg init-vreg))
        dest))

    (vector-ref (vec index)
      (convert-binop #'tac-vector-ref vec index))

    (vector-set (vec index value)
      (let ((vec-vreg (convert-ir vec))
            (index-vreg (convert-ir index))
            (value-vreg (convert-ir value)))
        (emit (tac-vector-set vec-vreg index-vreg value-vreg))
        value-vreg))

    (vector-length (vec)
      (convert-unop #'tac-vector-length vec))

    ;; === Symbol Operations ===
    (make-symbol (name)
      (convert-unop #'tac-make-symbol name))

    (symbol-name (sym)
      (convert-unop #'tac-symbol-name sym))

    (intern (str)
      (convert-unop #'tac-intern str))

    ;; === Keyword Operations ===
    (keyword-name (kw)
      (convert-unop #'tac-keyword-name kw))

    ;; === System ===
    (exit (code)
      (let ((code-vreg (convert-ir code)))
        (emit (tac-exit code-vreg))
        code-vreg))

    (error (message)
      (let ((msg-vreg (convert-ir message)))
        (emit (tac-error msg-vreg))
        msg-vreg))))

;;; Helper functions for binary and unary operations

(defun convert-binop (constructor left right)
  "Convert a binary operation."
  (let ((l (convert-ir left))
        (r (convert-ir right))
        (dest (next-vreg)))
    (emit (funcall constructor dest l r))
    dest))

(defun convert-unop (constructor value)
  "Convert a unary operation."
  (let ((v (convert-ir value))
        (dest (next-vreg)))
    (emit (funcall constructor dest v))
    dest))
