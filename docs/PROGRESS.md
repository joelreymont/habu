# ANSI Common Lisp Progress

## Source of Truth

- Symbol audit: `docs/cl-symbols.md`
- Symbol set: `docs/cl-symbols-sbcl.txt` (978 external `COMMON-LISP` symbols)

## Update / Verify

- Verify audit: `python3 tools/cl_symbols_audit.py`
- Keep `docs/cl-symbols.md` header counts in sync with the verifier output.

## Workflow

1. `dot ready`
2. `dot on <id>`
3. Implement + tests
4. Update the relevant rows in `docs/cl-symbols.md` (status/location/notes)
5. `python3 tools/cl_symbols_audit.py`
6. `dot off <id> -r "..."` + `jj describe -m "..."` + `jj git push`
