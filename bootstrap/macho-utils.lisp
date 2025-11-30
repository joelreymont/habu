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
#-sbcl
(defun list-length (lst)
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
         ;; Mode 0755 = rwxr-xr-x for executables
         (fd (sys-open path #x601 #o755)))
    (if (>= fd 0)
        (let* ((len (string-length content))
               (written (sys-write fd content len)))
          (sys-close fd)
          written)
        -1)))

;;; Native write file for executables - creates with +x permission
;;; The SBCL version is defined in macho.lisp (loaded first)
;;; This is the native Habu version only
#-sbcl
(defun native-write-executable (path content)
  "Write executable file - uses mode 0755 for +x permission (native version)"
  (let ((fd (sys-open path #x601 #o755)))
    (if (>= fd 0)
        (let* ((len (string-length content))
               (written (sys-write fd content len)))
          (sys-close fd)
          written)
        -1)))
