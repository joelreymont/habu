# src/habu — the native engine

- `prims.f` — the one primitive table: name, effect and reference (`prim-ref.f`)
  per row; `primitive-registry.f` refuses a body without a row and the reverse.
- `habu1.f`, `habu2.f` — the ARM64 kernel builder: primitive bodies, then the
  kernel's routines (throw recovery, the exit hook);
  `rt.f` is its part.
- `layout.f` — image, dictionary and snapshot layout and the DATA bands.
- `interpreter.f` (with `repl.f`) and `main.f` —
  the interpret loop and command line in Habu (`docs/architecture.md`
  "Interpreter").
- `*-x64.f` — the x86-64 engine (`kernel-x64.f` bodies, `boot-x64.f` entry).
- `snap*.f` — saved images; `*-abi.f` — fixed cells shared with
  source-loaded code.
