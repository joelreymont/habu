# bootstrap — the Gforth seed

Recovery only: `tools/bootstrap.sh` runs it when no `bin/hb` exists; the
procedure is `docs/bootstrap.md`.

- `cg/*.fs` — the seed: `cg/forth.fs` writes `hb-stage0`, an ARM64 engine with
  its own copy of the ARM64 primitive bodies (`FPRIM`) and the engine's
  `--load` prefix (`PFX-FILES`); `sys.fs` selects the OS seam by `HABU_TARGET`;
  `elf.fs`, `macho.fs`, `sign.fs` write and sign the image.
- `habu.fs`, `habu-lib.fs` and `src/*.fs` — the Gforth stack-effect checker the
  seed's assembler (`cg/asm.fs`) checks its own recipes with.
- `habu-repl.fs`, `habu-tui.fs`, `examples.fs` — that checker's REPL, TUI and
  examples, run under Gforth.
