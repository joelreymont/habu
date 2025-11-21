# Pure-Lisp Entrypoint Plan (Small Steps)

Goal: establish a Lisp-only runner (no C backend) that loads our compiler/repl code and is ready to expand with runtime-address wiring and tests.

1) Add a SBCL driver `run-habu.lisp` that:
   - Loads `habu-arm64-codegen.lisp` to ensure definitions are available in Lisp.
   - Prints a short TODO banner describing next wiring steps (runtime addresses, load path, JIT harness).
2) Add a shell wrapper `run-habu-lisp.sh` to invoke SBCL with the driver (no C backend, no generated C).
3) Update `SESSION_CONTEXT.md` to reference the new plan and entrypoint.
4) Smoke test the wrapper (expect banner + successful load), capture the outcome.
5) Commit the changes.
