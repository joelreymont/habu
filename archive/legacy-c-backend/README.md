# Legacy C Backend Artifacts (Archived)

These files were part of the old C backend pipeline. The project now mandates a tiny C runtime only; all compilation logic must live in Lisp. The artifacts below are kept purely for historical reference and must not be used in active build/test flows:

- `c-codegen.lisp` — C code generator.
- `ir-to-c.lisp` — IR to C translator.
- `compile-habu.sh` — helper script to drive the C backend.
- Binaries: `habu-enhanced`, `habu-extended`, `habu-prog`, `habu-rec`, `habu-jit`, `complete-macho-gen` (see `bin/`).

If you need to look at the old approach, read them here; do not resurrect them in the main tree.
