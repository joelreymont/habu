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
- **`tools/lint/text.f`** — checked habu string/file library: `READ-FILE`
  (slurp via open/read/close), `STR=`/`STR=CI`/`PREFIX?`/`FIND-SUB`/
  `CONTAINS?`, `BMOVE`/`FOLD`/`FOLD-TO`, and split/path helpers.
- **`tools/lint/token.f`** — checked whitespace tokenizer used by lint tools.
- **`tools/lint/lib.f`** — PAT scanners and bounded intern/set
  helpers that are still being split into checked foundations.
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
bin/hb --load tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/lint/shadow-lint.f
bin/hb --load tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/lint/clobber-lint.f
```
