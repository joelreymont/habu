;;;; Liveness Analysis
;;;;
;;;; Computes which virtual registers are live at each program point.
;;;; Used by register allocation to determine when registers can be reused.

(defpackage :habu.liveness
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.tac :tac-instr :tac-def :tac-use :tac-label)
  (:export :compute-liveness :live-interval :make-live-interval
           :live-interval-vreg :live-interval-start :live-interval-end))

(in-package :habu.liveness)

;;; Live interval: [start, end) for a virtual register
(deftype live-interval :record
  (vreg nil)
  (start 0)
  (end 0))

;;; Build label -> index map
(defun build-label-map (instrs)
  "Map label names to instruction indices."
  (let ((map (make-hash-table :test 'equal)))
    (loop for instr in instrs
          for i from 0
          when (and (consp instr) (eq (car instr) :tac-label))
          do (setf (gethash (cadr instr) map) i))
    map))

;;; Compute live-out sets using dataflow analysis
(defun compute-live-sets (instrs)
  "Compute live-in and live-out sets for each instruction.
   Returns: (values live-in-vec live-out-vec) - vectors of hash-tables"
  (let* ((n (length instrs))
         (instr-vec (coerce instrs 'vector))
         (live-in (make-array n :initial-element nil))
         (live-out (make-array n :initial-element nil))
         (label-map (build-label-map instrs)))

    ;; Initialize sets
    (dotimes (i n)
      (setf (aref live-in i) (make-hash-table))
      (setf (aref live-out i) (make-hash-table)))

    ;; Iterate until fixpoint
    (let ((changed t))
      (loop while changed do
        (setf changed nil)
        ;; Process in reverse order for efficiency
        (loop for i from (1- n) downto 0
              for instr = (aref instr-vec i)
              do
              (let ((old-in-size (hash-table-count (aref live-in i)))
                    (old-out-size (hash-table-count (aref live-out i))))

                ;; live-out[i] = union of live-in[succ] for all successors
                (dolist (succ (successors instr i n label-map))
                  (maphash (lambda (k v)
                             (declare (ignore v))
                             (setf (gethash k (aref live-out i)) t))
                           (aref live-in succ)))

                ;; live-in[i] = use[i] ∪ (live-out[i] - def[i])
                (let ((def (tac-def instr))
                      (uses (tac-use instr)))
                  ;; Copy live-out to live-in
                  (maphash (lambda (k v)
                             (declare (ignore v))
                             (setf (gethash k (aref live-in i)) t))
                           (aref live-out i))
                  ;; Remove def
                  (when def
                    (remhash def (aref live-in i)))
                  ;; Add uses
                  (dolist (u uses)
                    (when (integerp u)  ; only track vregs
                      (setf (gethash u (aref live-in i)) t))))

                ;; Check if changed
                (when (or (/= old-in-size (hash-table-count (aref live-in i)))
                          (/= old-out-size (hash-table-count (aref live-out i))))
                  (setf changed t))))))

    (values live-in live-out)))

;;; Get successors of an instruction
(defun successors (instr idx n label-map)
  "Return list of successor instruction indices."
  (cond
    ;; Unconditional branch
    ((and (consp instr) (eq (car instr) :tac-goto))
     (let ((target (gethash (cadr instr) label-map)))
       (if target (list target) nil)))

    ;; Conditional branch - two successors
    ((and (consp instr) (member (car instr) '(:tac-if :tac-ifnot)))
     (let ((target (gethash (caddr instr) label-map))
           (fallthrough (if (< (1+ idx) n) (1+ idx) nil)))
       (remove nil (list target fallthrough))))

    ;; Return - no successors
    ((and (consp instr) (eq (car instr) :tac-return))
     nil)

    ;; Default - fall through
    (t
     (if (< (1+ idx) n) (list (1+ idx)) nil))))

;;; Compute live intervals from live sets
(defun compute-intervals (instrs live-in live-out)
  "Compute live intervals for all vregs.
   Returns: list of live-interval structs"
  (let ((intervals (make-hash-table))  ; vreg -> (start . end)
        (n (length instrs)))

    ;; Scan through all instructions
    (loop for i from 0 below n
          do
          ;; Every vreg live at this point extends its interval
          (maphash (lambda (vreg v)
                     (declare (ignore v))
                     (let ((interval (gethash vreg intervals)))
                       (if interval
                           (setf (cdr interval) (max (cdr interval) (1+ i)))
                           (setf (gethash vreg intervals) (cons i (1+ i))))))
                   (aref live-in i))
          (maphash (lambda (vreg v)
                     (declare (ignore v))
                     (let ((interval (gethash vreg intervals)))
                       (if interval
                           (setf (cdr interval) (max (cdr interval) (1+ i)))
                           (setf (gethash vreg intervals) (cons i (1+ i))))))
                   (aref live-out i)))

    ;; Convert to list of live-interval structs
    (let ((result nil))
      (maphash (lambda (vreg range)
                 (push (make-live-interval :vreg vreg
                                           :start (car range)
                                           :end (cdr range))
                       result))
               intervals)
      ;; Sort by start position
      (sort result #'< :key #'live-interval-start))))

;;; Main entry point
(defun compute-liveness (tac-instrs)
  "Compute live intervals for TAC instructions.
   Returns: list of live-interval structs sorted by start position"
  (multiple-value-bind (live-in live-out)
      (compute-live-sets tac-instrs)
    (compute-intervals tac-instrs live-in live-out)))
