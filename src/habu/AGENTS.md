# src/habu — the native engine

- `prims.f` — the one primitive table: name, effect and reference (`prim-ref.f`)
  per row; `primitive-registry.f` refuses a body without a row and the reverse.
- `habu1.f`, `habu2.f` — the ARM64 engine builder: primitive bodies and seed
  dictionary, then the tier-0 JIT, the machine-code outer interpreter and the
  `--load` prefix; `jit.f`, `rt.f`, `regalloc.f` are its parts.
- `layout.f` — image, dictionary and snapshot layout and the DATA bands.
- `interpret.f` (`outer.f`, `packages.f`, `definers.f`, `repl.f`) and `main.f` —
  the interpret loop and command line in Habu (`docs/architecture.md`
  "Interpreter").
- `*-x64.f` — the x86-64 engine (`kernel-x64.f` bodies, `boot-x64.f` entry).
- `aot-*.f`, `snap*.f` — capture and saved images; `*-abi.f` —
  fixed cells shared with source-loaded code.
