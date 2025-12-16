# No Code Duplication

**NEVER duplicate code into habu0.lisp without express permission.**

The build system concatenates files. To share code:
1. Put shared code in `shared/*.lisp`
2. Add the file to `tools/build.lisp` file list
3. Code is automatically available in habu0

**Wrong:**
```lisp
;; In habu0.lisp - DUPLICATING shared/ir.lisp code
(defun ir-lit (v) (list :ir :lit v))  ; NO! This duplicates shared/ir.lisp
```

**Right:**
```lisp
;; In tools/build.lisp - add to file list
(dolist (file '("shared/types.lisp"
                "shared/ir.lisp"      ; <-- Add here
                "shared/macros.lisp"
                ...))
```

The build concatenates all files into one compilation unit. Shared code works everywhere.
