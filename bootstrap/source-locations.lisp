;;; Source Location Tracking for Habu Compiler
;;; ============================================================
;;;
;;; This module provides source location tracking through the
;;; compilation pipeline. Locations are tracked as:
;;; - byte position (from reader)
;;; - line/column (computed from position)
;;; - filename (threaded through pipeline)
;;;
;;; Location info is stored in a parallel metadata table, keeping
;;; IR structures unchanged.

#+sbcl (in-package :habu)

;;; ============================================================
;;; Source Location Structure
;;; ============================================================

(defstruct (srcloc (:constructor make-srcloc (file line col pos)))
  "Source location: file path, line number (1-based), column (0-based), byte position"
  file    ; filename string or nil for <stdin>
  line    ; line number (1-indexed)
  col     ; column (0-indexed)
  pos)    ; byte position in source

(defun srcloc-to-string (loc)
  "Format source location as FILE:LINE:COL"
  (if loc
      (format nil "~A:~D:~D"
              (or (srcloc-file loc) "<stdin>")
              (srcloc-line loc)
              (srcloc-col loc))
      "<unknown>"))

;;; ============================================================
;;; Position to Line/Column Conversion
;;; ============================================================

(defun count-newlines-before (source pos)
  "Count newlines in source[0..pos-1]. Returns (line . last-newline-pos).
   Line is 1-indexed (first line is line 1)."
  (let ((line 1)
        (last-nl -1)
        (i 0))
    (loop while (< i pos) do
      (when (char= (char source i) #\Newline)
        (incf line)
        (setq last-nl i))
      (incf i))
    (cons line last-nl)))

(defun pos-to-line-col (source pos)
  "Convert byte position to (line . column).
   Line is 1-indexed, column is 0-indexed."
  (let* ((result (count-newlines-before source pos))
         (line (car result))
         (last-nl (cdr result))
         (col (- pos last-nl 1)))
    (cons line (max 0 col))))

(defun pos-to-srcloc (source pos &optional filename)
  "Create a srcloc from source string and byte position."
  (let* ((lc (pos-to-line-col source pos))
         (line (car lc))
         (col (cdr lc)))
    (make-srcloc filename line col pos)))

;;; ============================================================
;;; Located Forms
;;; ============================================================
;;;
;;; A located-form wraps a form with its source location.
;;; This is used during reading to preserve position info.

(defstruct (located-form (:constructor make-located-form (form loc)))
  "A form with source location metadata"
  form   ; the actual Lisp form
  loc)   ; srcloc structure

(defun unwrap-form (x)
  "Extract the form from a potentially located wrapper"
  (if (located-form-p x)
      (located-form-form x)
      x))

(defun form-location (x)
  "Get the location of a potentially located form"
  (if (located-form-p x)
      (located-form-loc x)
      nil))

(defun unwrap-forms (forms)
  "Recursively unwrap all located-forms in a form tree"
  (cond
    ((null forms) nil)
    ((located-form-p forms) (unwrap-forms (located-form-form forms)))
    ((consp forms) (cons (unwrap-forms (car forms))
                         (unwrap-forms (cdr forms))))
    (t forms)))

;;; ============================================================
;;; Location-Aware Reading
;;; ============================================================

(defun read-all-with-locations (source &optional filename)
  "Read all forms from source, preserving source locations.
   Returns list of located-form structures.
   Use unwrap-forms to get plain forms for backward compatibility."
  #+sbcl
  (let ((len (string-length source)))
    (labels ((ra (pos acc)
               (let ((p2 (skip-ws source pos)))
                 (if (>= p2 len)
                     (reverse acc)
                     (let* ((r (sys:read source p2))
                            (form (car r))
                            (end-pos (cdr r)))
                       ;; Skip reader conditional markers
                       (if (reader-skip-marker-p form)
                           (ra end-pos acc)
                           (let* ((loc (pos-to-srcloc source p2 filename))
                                  (located (make-located-form form loc)))
                             (ra end-pos (cons located acc)))))))))
      (ra 0 nil)))
  #-sbcl
  (let ((pos 0)
        (acc nil)
        (source-len (string-length source)))
    (while (< pos source-len)
      (setq pos (skip-ws source pos))
      (if (< pos source-len)
          (let* ((result (habu-read source pos))
                 (form (car result))
                 (end-pos (cdr result))
                 (loc (pos-to-srcloc source pos filename))
                 (located (make-located-form form loc)))
            (process-package-form form)
            (setq acc (cons located acc))
            (setq pos end-pos))))
    (reverse acc)))

;;; ============================================================
;;; IR Metadata Table
;;; ============================================================
;;;
;;; Parallel table mapping IR nodes to source locations.
;;; Uses eq for identity comparison (IR nodes are conses).

(defvar *ir-locations* nil
  "Hash table mapping IR nodes to source locations during compilation")

(defun make-ir-location-table ()
  "Create a new IR location table"
  (make-hash-table :test 'eq))

(defun reset-ir-locations ()
  "Reset the global IR location table"
  (setq *ir-locations* (make-ir-location-table)))

(defun ir-set-location (ir loc)
  "Associate source location with an IR node"
  (when (and *ir-locations* ir loc)
    (setf (gethash ir *ir-locations*) loc))
  ir)

(defun ir-get-location (ir)
  "Get source location for an IR node, or nil"
  (when *ir-locations*
    (gethash ir *ir-locations*)))

(defun ir-copy-location (from-ir to-ir)
  "Copy location from one IR node to another"
  (let ((loc (ir-get-location from-ir)))
    (when loc
      (ir-set-location to-ir loc)))
  to-ir)

;;; ============================================================
;;; Location-Aware Compiler Wrapper
;;; ============================================================

(defun compile-with-locations (expr env fenv &optional loc)
  "Compile expression and associate location with result IR.
   If expr is a located-form, extract location from it."
  (let* ((actual-loc (or loc (form-location expr)))
         (actual-expr (unwrap-form expr))
         (ir (compile-expr-full actual-expr env fenv)))
    (when actual-loc
      (ir-set-location ir actual-loc))
    ir))

;;; ============================================================
;;; Located Form Compile-Forms Entry Point
;;; ============================================================

(defun compile-forms-with-locations (located-forms &optional filename)
  "Compile forms that have source locations attached.
   Initializes location tracking table and threads locations through IR."
  (declare (ignore filename))
  (reset-ir-locations)
  ;; Unwrap forms for backward-compatible compile-forms
  (let ((forms (mapcar #'unwrap-form located-forms)))
    ;; Future: Build location map for top-level forms and thread through IR
    ;; For now, just compile normally
    (compile-forms forms)))

;;; ============================================================
;;; Deliver with Source Locations
;;; ============================================================

(defvar *current-source-file* nil
  "Current source filename being compiled (for debug info)")

(defvar *function-locations* nil
  "Alist of (function-name . srcloc) for current compilation")

(defun deliver-with-locations (source output-path &key filename (heap-size #x4000000))
  "Compile source string to native executable with source location tracking.
   FILENAME: original source filename for debug info (optional)
   HEAP-SIZE: heap size in bytes (default 64MB)"
  ;; Read with location tracking
  (let* ((located-forms (read-all-with-locations source filename))
         (*current-source-file* filename)
         (*function-locations* (collect-function-locations located-forms)))
    ;; Store locations for debug info generation
    (reset-ir-locations)
    ;; Compile using regular deliver
    ;; The debug info generation in codegen will pick up *function-locations*
    (deliver source output-path heap-size)))

(defun deliver-file-with-locations (source-path output-path &optional (heap-size #x4000000))
  "Compile Lisp file to native executable with source location tracking."
  (deliver-with-locations (native-read-file source-path) output-path
                          :filename source-path
                          :heap-size heap-size))

;;; ============================================================
;;; Debug Info Integration
;;; ============================================================

(defun collect-function-locations (located-forms)
  "Extract (name . srcloc) alist for all defuns in located forms"
  (let ((result nil))
    (dolist (lf located-forms)
      (let ((form (unwrap-form lf))
            (loc (form-location lf)))
        (when (and (consp form) (eq (car form) 'defun) loc)
          (let ((name (cadr form)))
            (push (cons name loc) result)))))
    (nreverse result)))

(defun emit-source-locations-table (fn-locs)
  "Emit binary table mapping function offsets to source locations.
   Format per entry: u32 fn-offset, u32 line, u16 col, u16 filename-idx"
  (let ((entries nil))
    (dolist (entry fn-locs)
      (let* ((name (car entry))
             (loc (cdr entry))
             (line (srcloc-line loc))
             (col (srcloc-col loc)))
        (push (list name line col) entries)))
    (nreverse entries)))

;;; ============================================================
;;; Tests
;;; ============================================================

(defun test-pos-to-line-col ()
  "Test position to line/column conversion"
  (let ((source "line1
line2
line3"))
    ;; Position 0 = line 1, col 0
    (assert (equal (pos-to-line-col source 0) '(1 . 0)))
    ;; Position 5 = line 1, col 5 (the newline)
    (assert (equal (pos-to-line-col source 5) '(1 . 5)))
    ;; Position 6 = line 2, col 0 (start of line2)
    (assert (equal (pos-to-line-col source 6) '(2 . 0)))
    ;; Position 11 = line 2, col 5 (the newline)
    (assert (equal (pos-to-line-col source 11) '(2 . 5)))
    ;; Position 12 = line 3, col 0
    (assert (equal (pos-to-line-col source 12) '(3 . 0)))
    (format t "test-pos-to-line-col: PASS~%")))

(defun test-read-with-locations ()
  "Test reading with location tracking"
  (let* ((source "(defun foo (x) x)
(defun bar (y) y)")
         (forms (read-all-with-locations source "test.lisp")))
    (assert (= (length forms) 2))
    ;; First form at line 1
    (let ((loc1 (form-location (car forms))))
      (assert (= (srcloc-line loc1) 1))
      (assert (string= (srcloc-file loc1) "test.lisp")))
    ;; Second form at line 2
    (let ((loc2 (form-location (cadr forms))))
      (assert (= (srcloc-line loc2) 2)))
    (format t "test-read-with-locations: PASS~%")))

(defun test-source-locations ()
  "Run all source location tests"
  (test-pos-to-line-col)
  (test-read-with-locations)
  (format t "All source location tests passed!~%"))
