# LLM.md — Operating Protocol for Coding Agents

habu is **checked Forth**: a definition with a typed `( in -- out )` fails to
compile unless its body's inferred stack effect matches. Use the compiler to
prove your stack discipline. This file is the protocol. Follow it exactly.

## 0. Read first (every session)
- `AGENTS.md` — conventions and the gate.
- `docs/forth.md` — Forth style rules (BLOCKING).
- `LESSONS.md` (top) — the project's running memory; the latest findings.
- `STATUS.md` — the current self-check counts and known gaps.

## 1. Conventions (BLOCKING)
- **Our words UPPER-CASE** (`SQUARE`, `AVG2`); built-in gforth words stay
  lower-case (`dup`, `if`, `>r`, `?do`). Never upper-case a built-in.
- **Hyphens, never underscores.** Short names (`buf`, `ctx`, `idx`).
- One concern per file. Small words; factor when the stack gets deep.

## 2. Write small CHECKED words
- Every **public** word carries an explicit typed `( in -- out )`:
  `: SQUARE ( i64 -- i64 ) dup * ;`. A typed sig turns checking on for that word.
- Types: `i64 u8 u32 cell bool char str addr`; type vars `a b c`; row vars
  `R S T` (a leading one = the stack tail); quotations `[ in -- out ]`; the
  optional return-stack clause `… | rin -- rout`. `n` is the generic int.
- **Prefer `{: a b :}` locals over deep stack juggling.** If you reach for
  `rot -rot pick roll`, stop: factor a helper or bind locals.
- Quotations are `[: … ;]`; apply with `execute` (or a combinator: `dip keep`).
  A quotation param is recorded, so combinator call sites are checked.

## 3. Verify it typechecks
- Run a definition through the native engine:
  `printf ': SQUARE ( i64 -- i64 ) dup * ;\n' | bin/hb`
  An accepted def is silently added; a rejected one is dropped. For interactive
  **verify mode** (body vs declared sig), prepend `' CHECK! set-check`.
- For repair loops, prefer the wrapper:
  `./tools/check.sh --json-errors --all-errors file.f`.
  It exits nonzero on checker failure and emits one schema-versioned JSON object
  per failed top-level definition with file/line/column/byte spans. Use
  `tools/diag-to-sarif.py` when the diagnostics need to enter CI/review UIs.
- Use `tools/public-signatures.py file.f` to expose typed public words as a
  machine-readable manifest for an agent or downstream package.
- Authoritative verdict (-1 certified / 0 rejected / 1 uncheckable) via the
  gforth-hosted checker — same harness as `test/t-sh-verify.fs`: feed
  `: NAME ( sig ) body ;` to the `V` word there. `CHECK` = infer the body's
  effect; `CHECK!` = verify body against its declared `( in -- out )`.

## 4. On checker rejection: FIX THE BODY, NOT THE SIGNATURE
- A rejection means the body's real effect ≠ the declared `( in -- out )`. The
  signature is the spec. **Fix the word body to satisfy the signature.**
- Change the signature ONLY when a test or spec proves the original sig wrong —
  and then update the test alongside it. Never widen a sig to silence the checker.

## 5. TRUST is the last resort (audited)
- `TRUST` declares an effect without checking the body (FFI, metaprogramming,
  `create…does>`): `s" MYWORD" s" n -- n" trust`. Callers are still checked.
- **Never use TRUST without:** (a) a `TRUSTED.md` audit entry — word, asserted
  effect, why it can't be inferred, who verified it; and (b) a `T{ … -> … }T`
  test proving the runtime behavior matches the asserted effect.
  `tools/trust-lint.py` enforces this.

## 6. Test every word
- Add a `T{ … -> … }T` for each word as you write it — happy path plus each
  error/edge. A word without a test is unfinished. Tests live in
  `test/t-<file>.fs`. For errors, assert the exact THROW code.

## 7. Run the gate
- `( cd test && ./run.sh )` — habu-native, no gforth, < 10 s. Must be green.
- Gforth is bootstrap-only. Use `tools/bootstrap-oracle.sh` only when changing
  bootstrap seed/reference code or validating recovery from no native binary.

## 8. Record lessons
- On any new finding, mistake, or insight, add a lesson to `LESSONS.md` (lessons
  only; API/patterns go in `docs/`). Every checker/codegen change needs an
  adversarial review hunting for false-certifies: "the gate is green" is
  necessary, not sufficient, for a soundness property.

## 9. Stay in scope
- When fixing a checker / codegen / REPL bug, fix the root cause and nothing
  else. One concern per change; one change per commit (`jj`, 50-char imperative).
