;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; host-compat.lisp - Host Compatibility Layer for Unified Compiler
;;;
;;; DESIGN: Use unique prefixed names (h-*) to avoid CL conflicts.
;;; The unified compiler uses HOST:h-* functions which map to host-specific
;;; implementations.

(defpackage :host
  (:use :cl)
  (:export
   ;; String operations (h- prefix to avoid CL conflicts)
   #:h-string=
   #:h-string-length
   #:h-string-ref
   #:h-string-upcase
   #:h-substring
   #:h-string-concat

   ;; Symbol operations
   #:h-intern
   #:h-symbol-name
   #:h-make-symbol
   #:h-gensym

   ;; I/O
   #:h-file-read-all
   #:h-file-write-bytes
   #:h-print-string
   #:h-print-int
   #:h-print-newline

   ;; Error handling
   #:h-error
   #:h-fatal

   ;; Alist utilities (for portable hash-table replacement)
   #:h-acons
   #:h-alist-get
   #:h-alist-set
   #:h-alist-remove

   ;; Feature detection
   #:h-host-name))
