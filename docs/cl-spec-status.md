# Common Lisp Specification Compatibility

Habu targets ANSI Common Lisp compatibility (ANSI INCITS 226-1994) plus gradual typing and contracts. Progress is tracked at the external symbol level and verified against an extracted SBCL symbol set.

## Source of Truth

- Symbol audit: `docs/cl-symbols.md`
- Symbol set: `docs/cl-symbols-sbcl.txt` (978 external `COMMON-LISP` symbols)
- Verify: `python3 tools/cl_symbols_audit.py`
- Keep `docs/cl-symbols.md` header counts in sync with the verifier output.

## What This Does (and Doesn't) Guarantee

- Passing the symbol audit means the external symbol set is present and `docs/cl-symbols.md` is internally consistent.
- Full ANSI CL parity also depends on semantics, edge cases, and error behavior; those gaps are tracked as dots under `.dots/habu-cl-spec-parity-6821074c/`.

## Current Snapshot (2026-02-05)

- Symbol status: `✓ 978 | ⚠ 0 | ✗ 0`
- Row status: `✓ 1011 | ⚠ 0 | ✗ 0`

## Plan

- `dot tree habu-cl-spec-parity-6821074c`
- `dot ready`
