;;;; package.lisp - Habu package definitions
;;;;
;;;; This file must be loaded before all other Habu files.

;;; SYS: Internal compiler implementation
;;; ARM64 encoders, IR operations, codegen - all internal
(defpackage :sys
  (:use :cl)
  (:shadow #:read #:compile)  ; Shadow CL versions to define our own
  (:export
   ;; Reader and compiler (shadowed from CL)
   #:read #:compile
   ;; String primitives (for runtime)
   #:string-length #:string-ref #:make-string-from-vector
   #:string-concat #:number-to-string
   ;; Vector primitives
   #:make-vector #:vector-set))

;;; HABU: Public compiler API
;;; Use habu:deliver, habu:compile-program, etc.
(defpackage :habu
  (:use :cl :sys)
  (:shadowing-import-from :sys #:read)  ; Use SYS reader
  (:shadow #:trace #:untrace #:eval #:compile #:compile-file #:load #:disassemble
           #:*compile-verbose* #:*compile-print* #:*load-verbose* #:*load-print*
           #:macro-function #:macroexpand #:macroexpand-1)  ; Shadow CL versions
  (:export
   ;; Public compiler API (clean names)
   #:read-all           ; Parse source string to forms
   #:compile-program    ; Compile forms to ARM64 bytecode
   #:deliver            ; Compile source to native executable (mmap heap)
   #:deliver-file       ; Compile file to native executable
   ;; Disassembler
   #:disassemble
   #:disasm
   #:disassemble-bytes
   #:disassemble-form
   #:disassemble-bytecode
   #:disassemble-arm64-instr
   #:habu-disassemble
   ;; Optimizer
   #:optimize-ir
   ;; Evaluator and compiler (CL-spec)
   #:eval
   #:compile
   #:habu-compile
   ;; Internal compiler functions (for tests and self-hosting)
   #:eval-ir #:eval-forms #:codegen #:codegen-main
   #:eval-ir-with-fns #:compile-forms
   #:compile-expr #:compile-expr-v2 #:compile-expr-full
   #:compile-program-simple #:compile-program-with-symtab
   #:compile-to-bytecode #:compile-defun #:compile-lambda #:compile-labels
   #:self-compile
   ;; Codegen internals
   #:reset-symbol-table #:prologue #:epilogue
   ;; Mach-O and linking (mmap heap)
   #:mmap-heap-init-code
   #:wrap-bytecode-with-mmap-heap
   #:build-macho-executable-mmap-heap
   #:write-macho-executable-mmap-heap
   ;; JIT infrastructure
   #:jit-alloc-code
   #:jit-cache-flush-code
   #:resolve-calls-simple
   #:link-fasls
   #:generate-gc-fasl
   ;; FASL support (CL-compatible)
   #:compile-file        ; Compile source file to FASL
   #:load                ; Load FASL or source file
   #:compile-to-fasl     ; Internal: compile forms to FASL
   #:link-modules        ; Link multiple FASL modules
   #:*compile-verbose* #:*compile-print*
   #:*load-verbose* #:*load-print*
   ;; FASL format constants
   #:+fasl-header-size+ #:+fasl-function-size+
   #:+fasl-relocation-size+ #:+fasl-import-size+
   #:+fn-flag-exported+ #:+fn-flag-entry+
   ;; Wrapper size constants (single source of truth for Mach-O generation)
   #:+heap-wrapper-size+ #:+mmap-wrapper-size+
   ;; JIT executor (from executor.lisp)
   #:jit-eval #:jit-compile-expression #:jit-disasm
   #:jit-test #:jit-run-tests #:tag-fixnum #:untag-fixnum
   ;; Re-export CL functions used in compiled code
   #:append #:reverse #:length
   ;; Re-export system primitives for convenience
   #:string-length #:string-ref #:make-string-from-vector
   #:make-vector #:vector-set
   #:string-concat #:number-to-string
   ;; Compiler configuration toggles
   #:*use-register-allocation*
   #:*use-generational-gc*
   ;; Trace facility
   #:trace
   #:untrace
   #:trace-function
   #:untrace-function
   #:traced-p
   #:list-traced
   #:*traced-functions*
   #:*trace-depth*
   ;; Expansions
   #:expand-match
   #:expand-cond
   #:expand-and
   #:expand-or
   #:expand-let*
   #:expand-dotimes
   #:expand-dolist
   ;; Macro system
   #:macro-function
   #:macroexpand
   #:macroexpand-1
   #:*macro-table*
   ;; Reader
   #:habu-read
   #:habu-read-from-string
   #:native-read-file
   ;; ARM64 code generation
   #:fn-fixed-prologue
   #:fn-fixed-epilogue
   #:flatten-code-keep-markers-and-calls
   #:code-size
   ;; Debug info infrastructure
   #:make-debug-collector
   #:debug-add-function
   #:debug-build-table
   #:extract-debug-vars
   #:emit-debug-table
   #:write-debug-info
   #:run-debug-info-tests
   #:*debug-info*
   ;; DWARF debug info
   #:dwarf-make-abbrev-table-with-vars
   #:dwarf-emit-function-with-vars
   #:dwarf-location-env-slot
   ;; Source location tracking
   #:make-srcloc
   #:srcloc-file
   #:srcloc-line
   #:srcloc-col
   #:srcloc-pos
   #:srcloc-to-string
   #:pos-to-line-col
   #:pos-to-srcloc
   #:make-located-form
   #:located-form-form
   #:located-form-loc
   #:unwrap-form
   #:unwrap-forms
   #:form-location
   #:read-all-with-locations
   #:compile-forms-with-locations
   #:deliver-with-locations
   #:deliver-file-with-locations
   #:collect-function-locations
   #:*current-source-file*
   #:*function-locations*
   #:test-source-locations))
