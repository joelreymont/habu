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
                ;; Data movement
                :tac-lit :tac-nil :tac-t :tac-move :tac-var :tac-setvar
                :tac-global :tac-set-global
                ;; Arithmetic
                :tac-add :tac-sub :tac-mul :tac-div :tac-mod :tac-neg
                ;; Comparison
                :tac-eq :tac-eql :tac-lt :tac-gt :tac-le :tac-ge :tac-zerop
                ;; Logical/bitwise
                :tac-not :tac-band :tac-bor :tac-bxor :tac-bsh :tac-bnot
                ;; Control flow
                :tac-label :tac-goto :tac-if :tac-ifnot :tac-return
                :tac-block-begin :tac-block-end :tac-return-from
                :tac-continue :tac-dolist-init :tac-dolist-next
                :tac-dotimes-init :tac-dotimes-next
                ;; Function calls
                :tac-param :tac-arg :tac-call :tac-funcall
                :tac-lambda :tac-lambda-ref :tac-tail-call
                ;; List operations
                :tac-cons :tac-car :tac-cdr :tac-list :tac-length
                :tac-setcar :tac-setcdr :tac-nthcdr
                ;; Type predicates
                :tac-null :tac-consp :tac-symbolp :tac-stringp :tac-numberp
                :tac-keywordp :tac-functionp :tac-get-tag :tac-set-tag
                ;; String operations
                :tac-string-length :tac-string-ref :tac-string-concat :tac-string-lit
                :tac-make-string :tac-make-string-from-vector :tac-string-equal :tac-string-set
                ;; Buffer operations
                :tac-buffer-byte-ref :tac-buffer-byte-set :tac-buffer-to-string
                ;; Vector operations
                :tac-make-vector :tac-vector-ref :tac-vector-set :tac-vector-length
                ;; Symbol operations
                :tac-make-symbol :tac-make-symbol-from-string :tac-symbol-name :tac-intern :tac-symbol-lit
                ;; Keyword operations
                :tac-keyword-name :tac-keyword-lit
                ;; File I/O
                :tac-read-file :tac-write-file :tac-write-bytes :tac-println
                :tac-sys-read :tac-sys-read-byte :tac-sys-write :tac-sys-write-char
                :tac-sys-open :tac-sys-close
                ;; System/Low-level
                :tac-system :tac-mmap :tac-mmap-jit :tac-munmap
                :tac-pthread-jit-write-protect :tac-sys-dcache-flush :tac-sys-icache-invalidate
                :tac-funcall-ptr :tac-mem-set-byte :tac-mem-load-64 :tac-mem-load-byte
                ;; Heap/Runtime access
                :tac-get-intern-table :tac-set-intern-table
                :tac-get-keyword-table :tac-set-keyword-table
                :tac-get-lambda-counter :tac-set-lambda-counter
                :tac-get-symbol-counter :tac-set-symbol-counter
                :tac-get-symbol-table :tac-set-symbol-table
                :tac-get-symtab-offset :tac-get-symtab-count
                :tac-get-frame-pointer :tac-get-code-base
                :tac-set-global-vars :tac-get-global-vars
                :tac-get-cmdline-args
                ;; Multiple values
                :tac-values :tac-mvb
                ;; System
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

    ;; === List Mutations ===
    (setcar (cell value)
      (let ((cell-vreg (convert-ir cell))
            (value-vreg (convert-ir value)))
        (emit (tac-setcar cell-vreg value-vreg))
        value-vreg))

    (setcdr (cell value)
      (let ((cell-vreg (convert-ir cell))
            (value-vreg (convert-ir value)))
        (emit (tac-setcdr cell-vreg value-vreg))
        value-vreg))

    (nthcdr (n list)
      (let ((n-vreg (convert-ir n))
            (list-vreg (convert-ir list))
            (dest (next-vreg)))
        (emit (tac-nthcdr dest n-vreg list-vreg))
        dest))

    ;; === Type Tag Operations ===
    (get-tag (value)
      (convert-unop #'tac-get-tag value))

    (set-tag (value tag)
      (let ((value-vreg (convert-ir value))
            (tag-vreg (convert-ir tag))
            (dest (next-vreg)))
        (emit (tac-set-tag dest value-vreg tag-vreg))
        dest))

    ;; === String Mutations ===
    (make-string (length init)
      (let ((len-vreg (convert-ir length))
            (init-vreg (convert-ir init))
            (dest (next-vreg)))
        (emit (tac-make-string dest len-vreg init-vreg))
        dest))

    (make-string-from-vector (vec)
      (convert-unop #'tac-make-string-from-vector vec))

    (string-equal (left right)
      (convert-binop #'tac-string-equal left right))

    (string-set (str index value)
      (let ((str-vreg (convert-ir str))
            (index-vreg (convert-ir index))
            (value-vreg (convert-ir value)))
        (emit (tac-string-set str-vreg index-vreg value-vreg))
        value-vreg))

    ;; === Buffer Operations ===
    (buffer-byte-ref (buf index)
      (convert-binop #'tac-buffer-byte-ref buf index))

    (buffer-byte-set (buf index value)
      (let ((buf-vreg (convert-ir buf))
            (index-vreg (convert-ir index))
            (value-vreg (convert-ir value)))
        (emit (tac-buffer-byte-set buf-vreg index-vreg value-vreg))
        value-vreg))

    (buffer-to-string (buf length)
      (convert-binop #'tac-buffer-to-string buf length))

    ;; === Symbol Operations Extended ===
    (make-symbol-from-string (str)
      (convert-unop #'tac-make-symbol-from-string str))

    ;; === File I/O ===
    (read-file (path)
      (convert-unop #'tac-read-file path))

    (write-file (path content)
      (let ((path-vreg (convert-ir path))
            (content-vreg (convert-ir content)))
        (emit (tac-write-file path-vreg content-vreg))
        content-vreg))

    (write-bytes (fd bytes)
      (let ((fd-vreg (convert-ir fd))
            (bytes-vreg (convert-ir bytes)))
        (emit (tac-write-bytes fd-vreg bytes-vreg))
        bytes-vreg))

    (println (value)
      (let ((v (convert-ir value)))
        (emit (tac-println v))
        v))

    (sys-read (fd buf count)
      (let ((fd-vreg (convert-ir fd))
            (buf-vreg (convert-ir buf))
            (count-vreg (convert-ir count))
            (dest (next-vreg)))
        (emit (tac-sys-read dest fd-vreg buf-vreg count-vreg))
        dest))

    (sys-read-byte (fd)
      (convert-unop #'tac-sys-read-byte fd))

    (sys-write (fd buf count)
      (let ((fd-vreg (convert-ir fd))
            (buf-vreg (convert-ir buf))
            (count-vreg (convert-ir count))
            (dest (next-vreg)))
        (emit (tac-sys-write dest fd-vreg buf-vreg count-vreg))
        dest))

    (sys-write-char (fd char)
      (let ((fd-vreg (convert-ir fd))
            (char-vreg (convert-ir char)))
        (emit (tac-sys-write-char fd-vreg char-vreg))
        char-vreg))

    (sys-open (path flags mode)
      (let ((path-vreg (convert-ir path))
            (flags-vreg (convert-ir flags))
            (mode-vreg (convert-ir mode))
            (dest (next-vreg)))
        (emit (tac-sys-open dest path-vreg flags-vreg mode-vreg))
        dest))

    (sys-close (fd)
      (let ((fd-vreg (convert-ir fd))
            (dest (next-vreg)))
        (emit (tac-sys-close dest fd-vreg))
        dest))

    ;; === System/Low-level ===
    (system (cmd)
      (convert-unop #'tac-system cmd))

    (mmap (addr length prot flags fd offset)
      (let ((addr-vreg (convert-ir addr))
            (len-vreg (convert-ir length))
            (prot-vreg (convert-ir prot))
            (flags-vreg (convert-ir flags))
            (fd-vreg (convert-ir fd))
            (offset-vreg (convert-ir offset))
            (dest (next-vreg)))
        (emit (tac-mmap dest addr-vreg len-vreg prot-vreg flags-vreg fd-vreg offset-vreg))
        dest))

    (mmap-jit (length)
      (convert-unop #'tac-mmap-jit length))

    (munmap (addr length)
      (let ((addr-vreg (convert-ir addr))
            (len-vreg (convert-ir length))
            (dest (next-vreg)))
        (emit (tac-munmap dest addr-vreg len-vreg))
        dest))

    (pthread-jit-write-protect (enable)
      (let ((enable-vreg (convert-ir enable)))
        (emit (tac-pthread-jit-write-protect enable-vreg))
        enable-vreg))

    (sys-dcache-flush (addr length)
      (let ((addr-vreg (convert-ir addr))
            (len-vreg (convert-ir length)))
        (emit (tac-sys-dcache-flush addr-vreg len-vreg))
        addr-vreg))

    (sys-icache-invalidate (addr length)
      (let ((addr-vreg (convert-ir addr))
            (len-vreg (convert-ir length)))
        (emit (tac-sys-icache-invalidate addr-vreg len-vreg))
        addr-vreg))

    (funcall-ptr (ptr args)
      (let ((ptr-vreg (convert-ir ptr))
            (arg-vregs (mapcar #'convert-ir args))
            (dest (next-vreg)))
        ;; Emit args
        (loop for vreg in arg-vregs
              for i from 0
              do (emit (tac-arg i vreg)))
        (emit (tac-funcall-ptr dest ptr-vreg arg-vregs))
        dest))

    (mem-set-byte (addr value)
      (let ((addr-vreg (convert-ir addr))
            (value-vreg (convert-ir value)))
        (emit (tac-mem-set-byte addr-vreg value-vreg))
        value-vreg))

    (mem-load-64 (addr)
      (convert-unop #'tac-mem-load-64 addr))

    (mem-load-byte (addr)
      (convert-unop #'tac-mem-load-byte addr))

    ;; === Heap/Runtime Access ===
    (get-intern-table ()
      (let ((dest (next-vreg)))
        (emit (tac-get-intern-table dest))
        dest))

    (set-intern-table (value)
      (let ((v (convert-ir value)))
        (emit (tac-set-intern-table v))
        v))

    (get-keyword-table ()
      (let ((dest (next-vreg)))
        (emit (tac-get-keyword-table dest))
        dest))

    (set-keyword-table (value)
      (let ((v (convert-ir value)))
        (emit (tac-set-keyword-table v))
        v))

    (get-lambda-counter ()
      (let ((dest (next-vreg)))
        (emit (tac-get-lambda-counter dest))
        dest))

    (set-lambda-counter (value)
      (let ((v (convert-ir value)))
        (emit (tac-set-lambda-counter v))
        v))

    (get-symbol-counter ()
      (let ((dest (next-vreg)))
        (emit (tac-get-symbol-counter dest))
        dest))

    (set-symbol-counter (value)
      (let ((v (convert-ir value)))
        (emit (tac-set-symbol-counter v))
        v))

    (get-symbol-table ()
      (let ((dest (next-vreg)))
        (emit (tac-get-symbol-table dest))
        dest))

    (set-symbol-table (value)
      (let ((v (convert-ir value)))
        (emit (tac-set-symbol-table v))
        v))

    (get-symtab-offset ()
      (let ((dest (next-vreg)))
        (emit (tac-get-symtab-offset dest))
        dest))

    (get-symtab-count ()
      (let ((dest (next-vreg)))
        (emit (tac-get-symtab-count dest))
        dest))

    (get-frame-pointer ()
      (let ((dest (next-vreg)))
        (emit (tac-get-frame-pointer dest))
        dest))

    (get-code-base ()
      (let ((dest (next-vreg)))
        (emit (tac-get-code-base dest))
        dest))

    (set-global-vars (value)
      (let ((v (convert-ir value)))
        (emit (tac-set-global-vars v))
        v))

    (get-global-vars ()
      (let ((dest (next-vreg)))
        (emit (tac-get-global-vars dest))
        dest))

    (get-cmdline-args ()
      (let ((dest (next-vreg)))
        (emit (tac-get-cmdline-args dest))
        dest))

    ;; === Control Flow Extended ===
    (block (id body)
      ;; Named block for return-from
      (let ((end-label (next-label "BLOCK_END"))
            (dest (next-vreg)))
        (emit (tac-block-begin id))
        ;; Store end-label somewhere for return-from to find
        ;; For now, compile body and use result
        (let ((result (convert-ir body)))
          (emit (tac-move dest result))
          (emit (tac-block-end id))
          (emit (tac-label end-label))
          dest)))

    (return-from (id value)
      (let ((v (convert-ir value)))
        (emit (tac-return-from id v))
        v))

    (loop (body)
      ;; Infinite loop - exit via return-from
      (let ((loop-label (next-label "LOOP"))
            (dest (next-vreg)))
        (emit (tac-nil dest))
        (emit (tac-label loop-label))
        (convert-ir body)
        (emit (tac-goto loop-label))
        dest))

    (continue ()
      (emit (tac-continue))
      (let ((dest (next-vreg)))
        (emit (tac-nil dest))
        dest))

    (dolist (var-offset list-ir body end-label)
      (let ((list-vreg (convert-ir list-ir))
            (dest (next-vreg)))
        (emit (tac-dolist-init dest var-offset list-vreg))
        (let ((loop-label (next-label "DOLIST_LOOP")))
          (emit (tac-label loop-label))
          (emit (tac-dolist-next dest var-offset list-vreg end-label))
          (convert-ir body)
          (emit (tac-goto loop-label))
          (emit (tac-label end-label)))
        dest))

    (dotimes (var-offset count-ir body end-label)
      (let ((count-vreg (convert-ir count-ir))
            (dest (next-vreg)))
        (emit (tac-dotimes-init dest var-offset count-vreg))
        (let ((loop-label (next-label "DOTIMES_LOOP")))
          (emit (tac-label loop-label))
          (emit (tac-dotimes-next dest var-offset count-vreg end-label))
          (convert-ir body)
          (emit (tac-goto loop-label))
          (emit (tac-label end-label)))
        dest))

    ;; === Functions Extended ===
    (lambda-ref (name captures)
      (let ((dest (next-vreg))
            (cap-vregs (mapcar #'convert-ir captures)))
        (emit (tac-lambda-ref dest name cap-vregs))
        dest))

    (tail-call (name args)
      (let ((arg-vregs nil))
        (dolist (arg args)
          (push (convert-ir arg) arg-vregs))
        (setf arg-vregs (nreverse arg-vregs))
        (loop for vreg in arg-vregs
              for i from 0
              do (emit (tac-arg i vreg)))
        (emit (tac-tail-call name arg-vregs))
        ;; Tail call doesn't return here, but need a vreg
        (let ((dest (next-vreg)))
          (emit (tac-nil dest))
          dest)))

    ;; === Multiple Values ===
    (values (vals)
      (let ((val-vregs (mapcar #'convert-ir vals)))
        (emit (tac-values val-vregs))
        ;; Return first value as primary
        (if val-vregs (car val-vregs)
            (let ((dest (next-vreg)))
              (emit (tac-nil dest))
              dest))))

    (mvb (vars expr body)
      ;; Multiple-value-bind
      (emit (tac-mvb vars expr body))
      ;; Compile body for result
      (convert-ir body))

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
