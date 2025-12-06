# Habu Stage Command

Build and test self-compilation stages for bootstrapping verification.

## Arguments
- `$ARGUMENTS` - Stage number (1, 2, 3) or "verify" to check fixed point

## FASL Build System

**NEVER concatenate source files.** Use the FASL system:

```lisp
;; Compile source to FASL
(habu:compile-file "source.lisp" :output-file "output.fasl")

;; Link FASLs into executable
(habu:link-fasls '("a.fasl" "b.fasl") "/path/to/binary" :include-gc t)
```

## Stage 1: SBCL compiles Habu to native

Build Stage 1 using ASDF-loaded compiler with FASL system:

```lisp
;; In SBCL with habu system loaded:
(asdf:load-system :habu)

;; Compile each bootstrap module to FASL
(habu:compile-file "bootstrap/prelude.lisp")
(habu:compile-file "bootstrap/expand.lisp")
(habu:compile-file "bootstrap/reader.lisp")
(habu:compile-file "bootstrap/compiler.lisp")
(habu:compile-file "bootstrap/optimize.lisp")
(habu:compile-file "bootstrap/codegen.lisp")
(habu:compile-file "bootstrap/macho.lisp")
(habu:compile-file "bootstrap/main.lisp")  ; contains (defun main ...)

;; Link into Stage 1 binary
(habu:link-fasls '("bootstrap/prelude.fasl"
                   "bootstrap/expand.fasl"
                   "bootstrap/reader.fasl"
                   "bootstrap/compiler.fasl"
                   "bootstrap/optimize.fasl"
                   "bootstrap/codegen.fasl"
                   "bootstrap/macho.fasl"
                   "bootstrap/main.fasl")
                 "/tmp/habu_stage1"
                 :include-gc t)
```

**Required modules** (in dependency order):
1. `prelude.lisp` - CL functions (zerop, truncate, apply, etc.)
2. `expand.lisp` - Macro expansion (expand-match, expand-cond, etc.)
3. `reader.lisp` - S-expression reader
4. `compiler.lisp` - Core compiler
5. `optimize.lisp` - Optimization passes (TCO)
6. `codegen.lisp` - ARM64 code generator
7. `macho.lisp` - Mach-O executable writer
8. `main.lisp` - Entry point with `(defun main () ...)`

Verify Stage 1:
```bash
/tmp/habu_stage1
# Expected exit code: 42 (or per main.lisp)
```

## Stage 2: Stage 1 compiles Habu

Once Stage 1 can self-compile:

```bash
/tmp/habu_stage1 --compile bootstrap/*.lisp --output /tmp/habu_stage2
```

## Stage 3: Stage 2 compiles Habu

```bash
/tmp/habu_stage2 --compile bootstrap/*.lisp --output /tmp/habu_stage3
```

## Fixed Point Verification

Compare Stage 2 and Stage 3:
```bash
sha256sum /tmp/habu_stage2 /tmp/habu_stage3
cmp /tmp/habu_stage2 /tmp/habu_stage3
```

If binaries are identical, fixed point is achieved.

## Current Status

- Stage 1: IN PROGRESS - FASL system working, testing full build
- Stage 2: BLOCKED - Needs Stage 1 completion
- Stage 3: BLOCKED - Needs Stage 2

## Key FASL Functions

| Function | Purpose |
|----------|---------|
| `compile-file` | Compile source file to .fasl |
| `compile-to-fasl` | Compile forms to .fasl |
| `link-fasls` | Link .fasl files into Mach-O |
| `load-fasl-file` | Load .fasl for introspection |
