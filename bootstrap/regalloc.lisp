;;;; Register Allocation - Linear Scan
;;;;
;;;; Allocates physical registers to virtual registers using linear scan.
;;;; Spills to stack when registers are exhausted.

(defpackage :habu.regalloc
  (:use :cl)
  (:shadowing-import-from :habu.types :deftype :match :match*)
  (:import-from :habu.liveness :compute-liveness :live-interval
                :live-interval-vreg :live-interval-start :live-interval-end)
  (:export :allocate-registers :allocation-result
           :allocation-result-vreg-to-reg :allocation-result-spills
           :allocation-result-stack-size))

(in-package :habu.regalloc)

;;; Allocation result
(deftype allocation-result :record
  (vreg-to-reg nil)   ; hash-table: vreg -> physical reg (0-15) or :spill
  (spills nil)        ; list of spilled vregs
  (stack-size 0))     ; number of stack slots needed

;;; Available registers for allocation
;;; ARM64: x9-x15 are caller-saved temps (7 registers)
;;; We avoid x0-x7 (args), x19-x28 (callee-saved), x29 (fp), x30 (lr)
(defparameter *allocatable-regs* '(9 10 11 12 13 14 15))
(defparameter *num-regs* (length *allocatable-regs*))

;;; Linear scan register allocation
(defun allocate-registers (tac-instrs)
  "Allocate physical registers to virtual registers.
   Returns: allocation-result struct"
  (let* ((intervals (compute-liveness tac-instrs))
         (vreg-to-reg (make-hash-table))
         (active nil)           ; list of (interval . reg) sorted by end
         (free-regs (copy-list *allocatable-regs*))
         (spills nil)
         (spill-slot 0))

    ;; Process intervals in order of start position
    (dolist (interval intervals)
      (let ((vreg (live-interval-vreg interval))
            (start (live-interval-start interval))
            (end (live-interval-end interval)))

        ;; Expire old intervals
        (setf active
              (remove-if (lambda (entry)
                           (let ((int (car entry))
                                 (reg (cdr entry)))
                             (when (<= (live-interval-end int) start)
                               ;; This interval has expired, free its register
                               (push reg free-regs)
                               t)))
                         active))

        ;; Try to allocate a register
        (if free-regs
            ;; Have a free register
            (let ((reg (pop free-regs)))
              (setf (gethash vreg vreg-to-reg) reg)
              ;; Insert into active list, sorted by end
              (setf active (merge 'list
                                  (list (cons interval reg))
                                  active
                                  #'<
                                  :key (lambda (e) (live-interval-end (car e))))))

            ;; No free register - spill something
            (let ((spill-candidate (find-spill-candidate active interval)))
              (if (and spill-candidate
                       (> (live-interval-end (car spill-candidate))
                          end))
                  ;; Spill the candidate (it lives longer)
                  (let ((spill-vreg (live-interval-vreg (car spill-candidate)))
                        (reg (cdr spill-candidate)))
                    ;; Mark as spilled
                    (setf (gethash spill-vreg vreg-to-reg) :spill)
                    (push spill-vreg spills)
                    (incf spill-slot)
                    ;; Give its register to current interval
                    (setf (gethash vreg vreg-to-reg) reg)
                    ;; Remove from active, add new
                    (setf active (remove spill-candidate active))
                    (setf active (merge 'list
                                        (list (cons interval reg))
                                        active
                                        #'<
                                        :key (lambda (e) (live-interval-end (car e))))))

                  ;; Spill current interval (it lives longer or no candidate)
                  (progn
                    (setf (gethash vreg vreg-to-reg) :spill)
                    (push vreg spills)
                    (incf spill-slot)))))))

    (make-allocation-result :vreg-to-reg vreg-to-reg
                            :spills (nreverse spills)
                            :stack-size spill-slot)))

;;; Find best spill candidate
(defun find-spill-candidate (active current-interval)
  "Find the best interval to spill from active list.
   Prefers intervals that end latest."
  (when active
    ;; Return the one with latest end (last in sorted list)
    (car (last active))))
