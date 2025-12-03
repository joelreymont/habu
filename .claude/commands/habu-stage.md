# Habu Stage Command

Build and test self-compilation stages for bootstrapping verification.

## Arguments
- `$ARGUMENTS` - Stage number (1, 2, 3) or "verify" to check fixed point

## Stage 1: SBCL compiles Habu to native

Build Stage 1 using ASDF-loaded compiler:

```bash
sbcl --dynamic-space-size 4096 --noinform --non-interactive \
  --eval '(require :asdf)' \
  --eval '(push #P"/Users/joel/Work/habu/bootstrap/" asdf:*central-registry*)' \
  --eval '(asdf:load-system :habu)' \
  --eval '
(let* ((gc-src (habu::native-read-file "bootstrap/gc.lisp"))
       (reader-src (habu::native-read-file "bootstrap/reader.lisp"))
       (compiler-src (habu::native-read-file "bootstrap/compiler.lisp"))
       (optimize-src (habu::native-read-file "bootstrap/optimize.lisp"))
       (codegen-src (habu::native-read-file "bootstrap/codegen.lisp"))
       (macho-src (habu::native-read-file "bootstrap/macho-utils.lisp"))
       (main "(sys-exit 42)")
       (full-source (concatenate (quote string)
                                 gc-src reader-src compiler-src
                                 optimize-src codegen-src macho-src
                                 main)))
  (habu:deliver full-source "/tmp/habu_stage1"))'
```

Verify Stage 1:
```bash
/tmp/habu_stage1
# Expected exit code: 42
```

## Stage 2: Stage 1 compiles Habu

Once Stage 1 can self-compile (currently blocked by reader issues):

```bash
/tmp/habu_stage1 bootstrap/gc.lisp bootstrap/reader.lisp \
  bootstrap/compiler.lisp bootstrap/optimize.lisp \
  bootstrap/codegen.lisp bootstrap/macho-utils.lisp \
  /tmp/habu_stage2
```

## Stage 3: Stage 2 compiles Habu

```bash
/tmp/habu_stage2 [same sources] /tmp/habu_stage3
```

## Fixed Point Verification

Compare Stage 2 and Stage 3:
```bash
sha256sum /tmp/habu_stage2 /tmp/habu_stage3
cmp /tmp/habu_stage2 /tmp/habu_stage3
```

If binaries are identical, fixed point is achieved.

## Current Status

- Stage 1: WORKS - Compiles and runs (~1.1MB)
- Stage 2: BLOCKED - Stage 1 reader crashes on file input
- Stage 3: BLOCKED - Needs Stage 2

## Known Blockers

1. `(read-all "literal-string")` crashes in Stage 1
2. `(compile-forms ...)` crashes in Stage 1
3. Stack overflow with deeply recursive reader functions

## Output Format

```
HABU STAGE 1 BUILD
==================

Loading compiler via ASDF...
Reading source files...
  gc.lisp: 8,234 bytes
  reader.lisp: 12,456 bytes
  compiler.lisp: 85,234 bytes
  optimize.lisp: 15,678 bytes
  codegen.lisp: 78,901 bytes
  macho-utils.lisp: 5,678 bytes
  Total: 296,082 bytes

Compiling to native ARM64...
Output: /tmp/habu_stage1

Result:
  Binary size: 1,101,728 bytes
  Exit code: 42 (success)
```
