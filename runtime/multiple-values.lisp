;;;; runtime/multiple-values.lisp - Multiple return values support for Habu
;;;; Phase 1: Global values array with fixed size

(in-package :habu-runtime)

(export '(runtime-values-set
          runtime-values-get
          runtime-values-0
          runtime-values-1
          runtime-values-2
          runtime-values-3
          runtime-values-4
          *multiple-values-count*
          *multiple-values-array*))

;;; Multiple values storage
;;; Phase 1: Simple global array (not thread-safe, but sufficient for bootstrap)

(defconstant +max-values+ 16 "Maximum number of secondary values in Phase 1")

(defvar *multiple-values-count* 0
  "Number of values currently stored (including primary)")

(defvar *multiple-values-array* (make-array +max-values+ :initial-element 0)
  "Array storing secondary return values (indices 0-15)")

;;; Set multiple values
;;; Called by (values ...) form
;;; Primary value is returned normally in RAX/X0
;;; Secondary values are stored in the array

(defun runtime-values-set (count &rest values)
  "Set multiple values. Count is total number of values (including primary).
   values list contains ALL values (primary + secondary).
   Returns the primary value (first value or 0 if count=0)."
  (when (> count +max-values+)
    (error "Too many values: ~D (max ~D in Phase 1)" count +max-values+))

  (setf *multiple-values-count* count)

  (cond
    ((= count 0)
     ;; No values - return nil
     0)

    ((= count 1)
     ;; Single value - just return it, no array writes needed
     (first values))

    (t
     ;; Multiple values - store secondary values in array
     (let ((secondary-values (rest values)))
       (loop for val in secondary-values
             for i from 0
             do (setf (aref *multiple-values-array* i) val)))
     ;; Return primary value
     (first values))))

;;; Get a specific value by index
;;; Index 0 = primary value (but usually passed separately)
;;; Index 1+ = secondary values from array

(defun runtime-values-get (index primary-value)
  "Get the Nth value (0-indexed).
   index: which value to get (0 = primary, 1+ = secondary)
   primary-value: the primary return value (for index 0)
   Returns the requested value, or 0 (nil) if index >= count."
  (cond
    ;; Index out of bounds
    ((>= index *multiple-values-count*)
     0)  ; Return nil for missing values

    ;; Primary value
    ((= index 0)
     primary-value)

    ;; Secondary value from array (index-1 because array starts at 0 for 2nd value)
    (t
     (aref *multiple-values-array* (1- index)))))

;;; Get all values as a list (for debugging/testing)
(defun runtime-values-to-list (primary-value)
  "Convert current multiple values to a list.
   Useful for testing and debugging."
  (loop for i from 0 below *multiple-values-count*
        collect (runtime-values-get i primary-value)))

;;; Reset values (called at start of evaluation contexts)
(defun runtime-values-reset ()
  "Reset multiple values to single-value mode.
   Sets count to 1 (as if last form returned 1 value)."
  (setf *multiple-values-count* 1))

;;; Fixed-arity versions for FFI trampolines
;;; These are more efficient and easier to call from generated code

(defun runtime-values-0 ()
  "Return 0 values. Returns 0 (nil) as primary value."
  (setf *multiple-values-count* 0)
  0)

(defun runtime-values-1 (val1)
  "Return 1 value. Just returns val1, no array access needed."
  (setf *multiple-values-count* 1)
  val1)

(defun runtime-values-2 (val1 val2)
  "Return 2 values. val1 is primary, val2 is stored in array."
  (setf *multiple-values-count* 2)
  (setf (aref *multiple-values-array* 0) val2)
  val1)

(defun runtime-values-3 (val1 val2 val3)
  "Return 3 values. val1 is primary, val2-val3 stored in array."
  (setf *multiple-values-count* 3)
  (setf (aref *multiple-values-array* 0) val2)
  (setf (aref *multiple-values-array* 1) val3)
  val1)

(defun runtime-values-4 (val1 val2 val3 val4)
  "Return 4 values. val1 is primary, val2-val4 stored in array."
  (setf *multiple-values-count* 4)
  (setf (aref *multiple-values-array* 0) val2)
  (setf (aref *multiple-values-array* 1) val3)
  (setf (aref *multiple-values-array* 2) val4)
  val1)
