# TASK — self-host the lint tooling in habu

## Problem statement

Complete. The build-time parity, shadow, and clobber linters are self-hosted
Habu scripts under `tools/lint/`, run by `bin/hb` in `test/run.sh`, and the old
Python implementations have been deleted.

### Bootstrap Input Decision

`shadow-lint` reads only habu sources (`src/habu/habu1.f` + the snap srclist).

**`parity-lint` and `clobber-lint` inherently read the frozen bootstrap emitters**
(`bootstrap/cg/forth.fs`, `jit.fs`, `regalloc.fs`, `prof.fs`, `rt.fs`, `crash.fs`):

- `parity-lint`'s *entire function* is to token-diff each `bootstrap/cg/*.fs`
  builder word against its `src/habu/*.f` twin, by name, to catch silent divergence
  in the near-transcription emitters. It cannot work without reading forth.fs.
- `clobber-lint`'s file list spans both the bootstrap and the port (it models the
  register-clobber graph across both dialects).

The chosen contract is to keep those frozen bootstrap reads read-only. That
preserves the existing parity and clobber checks without putting gforth back on
the daily execution path.

## Status

### Done
- **`tools/lint/lib.f`** — habu string/file library: `READ-FILE` (slurp via
  open/read/close), `STR=`/`STR=CI`/`PREFIX?`/`FIND-SUB`/`CONTAINS?`,
  `TOKENIZE` (whitespace tokens, strips `\` and — when `PARENS?` set — `( )`
  comments, tracks column-0 via `TBOL`), `TOK`/`TOK0?`/`TEOL?`/`TOK=` (stack-based),
  `BMOVE`/`FOLD`/`FOLD-TO`, and bounded intern/set helpers.
- **`tools/lint/parity-lint.f`** — reproduces the parity divergence check.
- **`tools/lint/shadow-lint.f`** — reproduces the prim-shadow check.
- **`tools/lint/clobber-lint.f`** — reproduces the register-clobber model.
- **`test/run.sh`** feeds each linter through `bin/hb`.

## Habu gotchas

Durable tooling gotchas live in `docs/forth.md`; this task file is historical
status for the self-hosted lint migration.

## How to run / verify
```
cat tools/lint/lib.f tools/lint/parity-lint.f  | bin/hb
cat tools/lint/lib.f tools/lint/shadow-lint.f  | bin/hb
cat tools/lint/lib.f tools/lint/clobber-lint.f | bin/hb
```
