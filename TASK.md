# TASK — self-host the lint tooling in habu

## Problem statement

The project has three build-time linters written in **Python** (`tools/parity-lint.py`,
`tools/clobber-lint.py`, `tools/shadow-lint.py`). The standing mandate is that
project tooling must be **self-hosted in habu**, not Python (gforth is already
bootstrap-only; carrying a Python dependency for tooling contradicts the
self-hosting goal). The work is to reimplement all three in habu, run by `bin/hb`,
then delete the `.py` versions.

### The structural tension to resolve first (DECISION NEEDED)

`shadow-lint` reads only habu sources (`src/habu/habu1.f` + the snap srclist) — clean.

**`parity-lint` and `clobber-lint` inherently read the frozen bootstrap emitters**
(`bootstrap/cg/forth.fs`, `jit.fs`, `regalloc.fs`, `prof.fs`, `rt.fs`, `crash.fs`):

- `parity-lint`'s *entire function* is to token-diff each `bootstrap/cg/*.fs`
  builder word against its `src/habu/*.f` twin, by name, to catch silent divergence
  in the near-transcription emitters. It cannot work without reading forth.fs.
- `clobber-lint`'s file list spans both the bootstrap and the port (it models the
  register-clobber graph across both dialects).

This conflicts with the "forth.fs is bootstrap-only, frozen, stop going back to it"
stance. The Python versions read these files too — but self-hosting them makes the
habu tool depend on (read-only) the frozen bootstrap tree.

**Decision required before continuing:**
1. Port parity + clobber as-is (they read the frozen `bootstrap/cg/*.fs`, read-only
   — same inputs as the `.py`), **or**
2. Drop/retire parity + clobber (the bootstrap is frozen and decoupled, so a
   boot-vs-port diff may be obsolete), keeping only shadow-lint self-hosted, **or**
3. Re-scope parity/clobber to analyze only the habu ports (changes what they check).

## Status

### Done and pushed (master)
- **`tools/lint/lib.f`** — habu string/file library: `READ-FILE` (slurp via
  open/read/close), `STR=`/`STR=CI`/`PREFIX?`/`FIND-SUB`/`CONTAINS?`,
  `TOKENIZE` (whitespace tokens, strips `\` and — when `PARENS?` set — `( )`
  comments, tracks column-0 via `TBOL`), `TOK`/`TOK0?`/`TEOL?`/`TOK=` (stack-based),
  `BMOVE`/`FOLD`/`FOLD-TO`. Verified: habu1.f → 3875 tokens, 105 BOL-defs = `grep -c '^: '`.
- **`tools/lint/shadow-lint.f`** — reproduces `shadow-lint.py` (clean, 88 prims),
  teeth verified (catches `EMIT` case-insensitively + `open`). DONE.

### In progress (uncommitted, do not have a clean run)
- **`tools/lint/parity-lint.f`** — full port written. Fixed two habu bugs so far:
  - `DEF-END` used `j` as a local — `i`/`j` are loop-index keywords, silently
    resolve to the keyword (garbage), NOT the argument. Renamed to `dj`.
  - `DEF-END` used `exit` inside a locals+loop word; rewrote flag-based (`DEDONE`).
  - **Current blocker:** `WALK-BOOT` crashes (SIGSEGV) on `forth.fs`. Suspected
    `FSCR` (32 KB) or `DBUF` (128 KB) overflow, or a forth.fs def whose closing `;`
    is not detected as end-of-line so `DEF-END` returns `TN#` and `FILTER` then
    processes a huge range and overruns `FSCR`. Was bisecting which def when stopped.
    Debug it **in habu** (instrument the walker), not by grepping forth.fs.

### Not started (dotted)
- `clobber-lint.f` — register model + tables, region/clobber-set closure,
  call-site liveness (3 dots).
- def-walker + set/intern generalization; gate integration + delete the `.py`.

Open dots: `dot ls` (7 lint dots remain).

## habu gotchas discovered (apply when writing more habu tooling)
- **`i` and `j` are loop-index keywords — never use as local names.** They compile
  to the (compile-only) loop index, not your argument; silent garbage.
- `{: :}` locals do **not** accept `-- outputs` (binds them as extra locals →
  stack underflow). Put the effect in a `( -- )` comment.
- habu locals nest only ~2 deep reliably; make leaf helpers **stack-based**.
- Data region is 2 MB (`DATA-SIZE=$200000`); the installed `bin/hb` already uses
  ~931 KB, leaving ~1.1 MB. Size buffers small (file buf 128 KB, token arrays
  `TMAX=$6000`). Overshoot → writes past the region → SIGSEGV.
- Missing words: `pick`, `+!`, `within`. Use variables / explicit increment.
- Compile-only words (`begin`/`while`/`repeat`, `[' ]`, `i`, `?do`) cannot run at
  the interpret (top) level — wrap loops/ticks in a `: WORD ;`.
- The engine prints an undefined word to **stderr** then spills the rest of the
  failed def to the interpreter — a stray token on stderr means a word failed to
  compile (look earlier in that def).

## How to run / verify
```
cat tools/lint/lib.f tools/lint/shadow-lint.f  | bin/hb     # shadow (clean)
cat tools/lint/lib.f tools/lint/parity-lint.f  | bin/hb     # parity (crashes — WIP)
python3 tools/parity-lint.py        # reference: "parity-lint: 0 divergence(s)"
python3 tools/shadow-lint.py        # reference: "shadow-lint: clean"
```
Gate integration (final): feed `lib.f` + each linter to `bin/hb`, exit nonzero on
a finding, replace the `python3` calls in the gate, delete the three `.py` files.
