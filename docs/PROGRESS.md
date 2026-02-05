# ANSI Common Lisp Implementation Progress

Tracking:
- Symbol set: `docs/cl-symbols-sbcl.txt` (978 external symbols)
- Audit table: `docs/cl-symbols.md` (duplicates allowed for multi-role symbols)
- Validator: `python3 tools/cl_symbols_audit.py`

Status (symbols): ✓ 955 | ⚠ 10 | ✗ 13

Work plan:
- Dot tree: `.dots/habu-cl-spec-parity-6821074c/`

Next missing (✗):
- call-method
- copy-structure
- equalp
- fdefinition
- function-lambda-expression
- get-setf-expansion
- integerp
- invalid-method-error
- load-logical-pathname-translations
- make-method
- method-combination-error
- realp
- standard

Next partial (⚠):
- &allow-other-keys
- &environment
- &whole
- compilation-speed
- debug
- declaration
- optimize
- safety
- space
- speed

