# habu — Checked Forth

A checked stack-effect system for Forth, hosted by Gforth. LLM-generated Forth is
verified by a checker (typed, row-polymorphic stack effects) rather than trusting
the model to track the stack by hand. See `PLAN.md` for design and roadmap.

## Conventions

- **Project-defined Forth words are UPPER-CASE; built-ins stay as-is/lower-case.**
  Define and call our words in upper case (`SQUARE`, `CHECKED:`); keep core words
  lower-case (`dup`, `drop`, `if`, `?do`). Lower-case only for `\` comments and
  prose outside code.
- Gforth target: **0.7.9** at `~/.local/bin/gforth` (built from source; see
  `LESSONS.md`). Keep `~/.local/bin` ahead of `/opt/homebrew/bin` on PATH.

## Forth style (BLOCKING)

See **`docs/forth.md`** for the full Forth standards. In short: new public/library
Forth defaults to checked typed definitions; unchecked code must be an explicit
tested boundary; our words UPPER-CASE (built-ins as-is); hyphens not underscores;
small factored words with a `( in -- out )` comment each; shallow stacks (factor /
`{: :}` locals, no deep juggling); `throw` named codes, never silent; named
constants; and a `T{ … -> … }T` test for every word.

- **One concern per file** — never bundle unrelated responsibilities (parser vs
  renderer vs DB vs data table). Split at responsibility seams; it aids review
  and lets files be built in parallel. See `docs/forth.md` § Files.

## Lessons

- **Read `docs/forth.md` and `LESSONS.md` at the start of every session, and
  update `LESSONS.md` after any finding, mistake, or insight.** The former is the
  coding standard; the latter is the project's concise running memory.
- `LESSONS.md` holds lessons only — what worked, what didn't, and why. It is NOT
  for API reference, code snippets, Forth standards, or language patterns; those
  go in `docs/` (especially `docs/forth.md`).

## Workflow

- VCS is `jj` (Jujutsu). One change per commit; 50-char imperative subject; no emoji.
- Commit after each significant change or feature; include new files.
- Gate: `( cd test && ./run.sh )` — Habu-native, no gforth, <10 s. Gforth is
  bootstrap-only: use `tools/bootstrap.sh` to recover from no native binary, and
  `tools/bootstrap-oracle.sh` only when changing bootstrap seed/reference code.
