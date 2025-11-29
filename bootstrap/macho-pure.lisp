;;; Pure Mach-O Linker - For native compilation (no SBCL dependencies)
;;;
;;; This provides pure versions of functions that use SBCL-specific constructs.
;;; It should be loaded AFTER macho.lisp to override specific functions.

(in-package :habu)

;;; Pure version of buf-zeros (replaces dotimes with labels)
(defun buf-zeros (count)
  "Create a list of COUNT zeros using pure recursion"
  (labels ((make-zeros (n acc)
             (if (<= n 0)
                 acc
                 (make-zeros (- n 1) (cons 0 acc)))))
    (make-zeros count nil)))

;;; Pure version of length for lists (use CL length or provide pure version)
;;; This is needed because native code doesn't have CL:length
(defun pure-list-length (lst)
  "Pure version of length for lists"
  (labels ((len (l n)
             (if (null l)
                 n
                 (len (cdr l) (+ n 1)))))
    (len lst 0)))

;;; Native write file - writes string to file using sys-* primitives
;;; This is the pure Habu version that uses sys-open, sys-write, sys-close
#-sbcl
(defun native-write-file (path content)
  "Write string CONTENT to file PATH (native Habu version)"
  (let* ((path-len (string-length path))
         ;; O_WRONLY | O_CREAT | O_TRUNC = 0x601
         (fd (sys-open path #x601 #o644)))
    (if (>= fd 0)
        (let* ((len (string-length content))
               (written (sys-write fd content len)))
          (sys-close fd)
          written)
        -1)))
