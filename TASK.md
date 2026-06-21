# TASK — self-host the lint tooling in habu

## Problem statement

Complete. The build-time shadow and clobber linters are self-hosted Habu scripts
under `tools/lint/`, run by `bin/hb` in `test/run.f`, and the old host
implementations have been deleted. The old boot-vs-port parity lint is retired:
the native build-fixpoint installer is the active parity proof. The default
lint gate has no bootstrap inputs.

### Bootstrap Input Decision

`shadow-lint` reads only habu sources (`src/habu/habu1.f` + the snap srclist).
`clobber-lint` reads only the live native emitter sources under `src/habu/`.
Bootstrap emitters are no longer inputs to the default lint gate.

## Status

### Done
- **`tools/lint/lib.f`** — habu string/file library: `READ-FILE` (slurp via
  open/read/close), `STR=`/`STR=CI`/`PREFIX?`/`FIND-SUB`/`CONTAINS?`,
  `TOKENIZE` (whitespace tokens, strips `\` and — when `PARENS?` set — `( )`
  comments, tracks column-0 via `TBOL`), `TOK`/`TOK0?`/`TEOL?`/`TOK=` (stack-based),
  `BMOVE`/`FOLD`/`FOLD-TO`, and bounded intern/set helpers.
- **`tools/lint/shadow-lint.f`** — reproduces the prim-shadow check.
- **`tools/lint/clobber-lint.f`** — reproduces the register-clobber model.
- **`tools/host-lint.f`** — gates retired host-script workflow tokens and stale
  file names so the default workflow stays self-hosted.
- **`test/run.f`** feeds each linter through `bin/hb`.

## Habu gotchas

Durable tooling gotchas live in `docs/forth.md`; this task file is historical
status for the self-hosted lint migration.

## How to run / verify
```
cat tools/lint/lib.f tools/lint/shadow-lint.f  | bin/hb
cat tools/lint/lib.f tools/lint/clobber-lint.f | bin/hb
```
