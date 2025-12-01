Load Habu compiler via ASDF

## Instructions

Load the Habu bootstrap compiler using ASDF which handles all file dependencies automatically.

### Steps

1. Run ASDF to load the system:
```bash
sbcl --noinform \
  --eval '(require :asdf)' \
  --eval '(push #p"/Users/joel/Work/habu/bootstrap/" asdf:*central-registry*)' \
  --eval '(asdf:load-system "habu")' \
  --eval '(format t "~%Habu loaded successfully~%")' \
  --eval '(format t "Package HABU exports: ~A~%" (length (loop for s being the external-symbols of :habu collect s)))' \
  --quit
```

2. If there are compilation errors, report:
   - Which file failed
   - The specific error message
   - Suggest fixes based on dependency issues

3. For interactive use (REPL with Habu loaded):
```bash
sbcl --noinform \
  --eval '(require :asdf)' \
  --eval '(push #p"/Users/joel/Work/habu/bootstrap/" asdf:*central-registry*)' \
  --eval '(asdf:load-system "habu")' \
  --eval '(in-package :habu)'
```

### ASDF System Structure

The `bootstrap/habu.asd` file defines dependencies:
- `compiler-sbcl` - Core bootstrap compiler
- `optimize` - Optimization passes (TCO, etc.)
- `reg-alloc` - Register allocation nanopasses
- `macho` - Mach-O linker
- `reader` - Lisp reader
- `compiler` - Self-hosted compiler
- `codegen` - ARM64 code generator
- `macho-utils` - Native Mach-O utilities

### Troubleshooting

If ASDF fails:
1. Check for circular dependencies in habu.asd
2. Verify all files exist in bootstrap/
3. Look for undefined function errors (missing :depends-on)
4. Clear ASDF cache: `rm -rf ~/.cache/common-lisp/`
