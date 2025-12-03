;;; slot-debug.lisp - Stack slot debugging utilities for Habu
;;;
;;; Detects temp slot conflicts during codegen and helps debug crashes.
;;; Usage:
;;;   (enable-slot-debug)   ; Turn on slot tracking
;;;   (disable-slot-debug)  ; Turn off
;;;   (check-slot-conflicts ir td) ; Check for conflicts in IR at temp depth td

(in-package :habu)

;;; Global state for slot tracking
(defvar *slot-debug-enabled* nil
  "When true, track slot usage during codegen")

(defvar *slot-usage-stack* nil
  "Stack of ((slot . ir-tag) ...) lists for nested IR")

(defvar *slot-conflicts* nil
  "List of detected conflicts: ((slot ir1 ir2) ...)")

(defun enable-slot-debug ()
  "Enable slot conflict detection"
  (setf *slot-debug-enabled* t)
  (setf *slot-usage-stack* nil)
  (setf *slot-conflicts* nil)
  (format t "Slot debugging enabled~%"))

(defun disable-slot-debug ()
  "Disable slot conflict detection"
  (setf *slot-debug-enabled* nil)
  (format t "Slot debugging disabled~%"))

(defun slot-offset (depth)
  "Calculate stack offset for temp slot at given depth.
   Mirrors temp-slot function: offset = 0x40 + depth * 8"
  (+ #x40 (* depth 8)))

(defun record-slot-use (slot-depth ir-tag mode)
  "Record that a slot is being used.
   MODE is :write or :read."
  (when *slot-debug-enabled*
    (let* ((offset (slot-offset slot-depth))
           (current-uses (car *slot-usage-stack*))
           (existing (assoc offset current-uses)))
      (when (and (eq mode :write) existing)
        ;; Slot already written - potential conflict
        (let ((prev-ir (cdr existing)))
          (unless (eq prev-ir ir-tag)
            (push (list offset prev-ir ir-tag) *slot-conflicts*)
            (format t "~%WARNING: Slot conflict at offset ~A (0x~X):~%  ~A writes, but ~A already wrote there~%"
                    offset offset ir-tag prev-ir))))
      ;; Record the usage
      (if existing
          (setf (cdr existing) ir-tag)
          (push (cons offset ir-tag) (car *slot-usage-stack*))))))

(defun push-slot-scope ()
  "Enter a new scope for slot tracking"
  (when *slot-debug-enabled*
    (push nil *slot-usage-stack*)))

(defun pop-slot-scope ()
  "Exit current slot scope"
  (when *slot-debug-enabled*
    (pop *slot-usage-stack*)))

;;; Analysis functions

(defun analyze-funcall-slots (td num-args)
  "Analyze slot allocation for funcall-ir.
   Returns list of (slot-name depth offset) for each slot used."
  (let* ((x24-slot (+ td 0))
         (x20-slot (+ td 1))
         (x30-slot (+ td 2))
         (code-slot (+ td 3))
         (env-slot (+ td 4))
         (arg-base (+ td 5)))
    (append
     (list (list 'x24-save x24-slot (slot-offset x24-slot))
           (list 'x20-save x20-slot (slot-offset x20-slot))
           (list 'x30-save x30-slot (slot-offset x30-slot))
           (list 'code-addr code-slot (slot-offset code-slot))
           (list 'env env-slot (slot-offset env-slot)))
     (loop for i from 0 below num-args
           collect (list (format nil "arg~A" i)
                        (+ arg-base i)
                        (slot-offset (+ arg-base i)))))))

(defun print-funcall-slot-layout (td num-args &optional (param-space 0) (stack-space 0))
  "Print the slot layout for a funcall-ir call.
   Shows how offsets change after sp adjustment."
  (let* ((slots (analyze-funcall-slots td num-args))
         (total-offset (+ param-space stack-space)))
    (format t "~%Funcall-ir slot layout (td=~A, num-args=~A):~%" td num-args)
    (format t "  param-space=~A, stack-space=~A, total-offset=~A~%"
            param-space stack-space total-offset)
    (format t "~%  Before sp adjustment:~%")
    (format t "    ~20A ~8A ~8A~%" "Name" "Depth" "Offset")
    (format t "    ~20A ~8A ~8A~%" "----" "-----" "------")
    (dolist (s slots)
      (format t "    ~20A ~8A 0x~4,'0X~%" (first s) (second s) (third s)))
    (when (> total-offset 0)
      (format t "~%  After sp adjustment (sp -= ~A):~%" total-offset)
      (format t "    ~20A ~8A ~8A ~10A~%" "Name" "Depth" "OldOff" "NewOff")
      (format t "    ~20A ~8A ~8A ~10A~%" "----" "-----" "------" "------")
      (dolist (s slots)
        (format t "    ~20A ~8A 0x~4,'0X 0x~4,'0X~%"
                (first s) (second s) (third s) (+ (third s) total-offset))))))

(defun check-funcall-slot-overlap (td num-args total-offset)
  "Check if code-slot + total-offset overlaps with any arg slot.
   Returns t if there's a conflict."
  (let* ((code-slot-depth (+ td 3))
         (code-offset (slot-offset code-slot-depth))
         (adjusted-code-offset (+ code-offset total-offset))
         (arg-base (+ td 5))
         (conflict nil))
    (dotimes (i num-args)
      (let ((arg-offset (slot-offset (+ arg-base i))))
        (when (= adjusted-code-offset arg-offset)
          (format t "~%CONFLICT: code-slot adjusted offset 0x~X == arg~A offset 0x~X~%"
                  adjusted-code-offset i arg-offset)
          (setf conflict t))))
    conflict))

;;; Crash analysis

(defun analyze-crash-offset (crash-offset td num-args param-space stack-space)
  "Analyze a crash by offset.
   Given the offset that caused the crash, determine what slot it corresponds to."
  (let* ((slots (analyze-funcall-slots td num-args))
         (total-offset (+ param-space stack-space)))
    (format t "~%Analyzing crash at offset 0x~X:~%" crash-offset)
    (format t "  td=~A, num-args=~A, total-offset=~A~%" td num-args total-offset)

    ;; Check if it matches any slot before adjustment
    (format t "~%  Checking pre-adjustment slots:~%")
    (dolist (s slots)
      (when (= crash-offset (third s))
        (format t "    MATCH: ~A at depth ~A~%" (first s) (second s))))

    ;; Check if it matches any slot after adjustment
    (when (> total-offset 0)
      (format t "~%  Checking post-adjustment slots (offset + ~A):~%" total-offset)
      (dolist (s slots)
        (when (= crash-offset (+ (third s) total-offset))
          (format t "    MATCH: ~A at depth ~A (adjusted from 0x~X)~%"
                  (first s) (second s) (third s)))))))

;;; High-level analysis for the current bug

(defun diagnose-funcall-bug ()
  "Diagnose the current funcall-ir offset adjustment bug.
   Based on the crash at FOLD-BINOP + 1876 with offset 0x88."
  (format t "~%=== Diagnosing funcall-ir offset bug ===~%")
  (format t "~%Crash details:~%")
  (format t "  - Location: FOLD-BINOP + 1876~%")
  (format t "  - Faulting instruction: ldr x9, [sp, #0x88]~%")
  (format t "  - x9 loaded value: 0x6 (nil)~%")
  (format t "  - Expected: code address~%")

  ;; Work backwards from the crash offset
  (format t "~%Working backwards from offset 0x88 = 136:~%")

  ;; If this is code-slot + total-offset:
  (format t "~%Hypothesis 1: 0x88 is code-slot + total-offset~%")
  (format t "  If total-offset = 32 (0x20), code-slot = 0x68 = 104~%")
  (format t "  temp-slot(depth) = 0x40 + depth*8 = 104~%")
  (format t "  depth = (104-64)/8 = 5, so code-slot is at td+3=5, td=2~%")
  (print-funcall-slot-layout 2 3 32 0)

  ;; Check for the actual conflict
  (format t "~%Checking for slot conflicts with td=2, 3 args:~%")
  (check-funcall-slot-overlap 2 3 32)

  (format t "~%Analysis:~%")
  (format t "  The store at +1832 uses offset 0x88, same as the load.~%")
  (format t "  But the store is for x0 (a cons cell), not x10 (code addr).~%")
  (format t "  This means arg 2 is being stored at the same offset~%")
  (format t "  that code-slot+total_offset is trying to load from!~%")

  (format t "~%The bug:~%")
  (format t "  1. Code address stored at [sp+0x68] before sp adjustment~%")
  (format t "  2. Arg 2 stored at [sp+0x88] before sp adjustment~%")
  (format t "  3. sp adjusted by 32~%")
  (format t "  4. Load from [new_sp+0x88] expects code addr, gets arg 2!~%")
  (format t "~%  But wait - after adjustment:~%")
  (format t "  - Code addr at [new_sp + 0x68 + 0x20] = [new_sp + 0x88]~%")
  (format t "  - Arg 2 at [new_sp + 0x88 + 0x20] = [new_sp + 0xa8]~%")
  (format t "  These should be different!~%")

  (format t "~%  Unless... the store at +1832 happens AFTER the code addr store~%")
  (format t "  but BEFORE the sp adjustment, overwriting code-slot!~%"))

;; Run diagnosis immediately when loaded
;; (diagnose-funcall-bug)
