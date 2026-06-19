# LLM.md — Operating Protocol for Coding Agents

habu is **checked Forth**: a definition with a typed `( in -- out )` fails to
compile unless its body's inferred stack effect matches. Use the compiler to
prove your stack discipline. This file is the protocol. Follow it exactly.

## 0. Read first (every session)
- `AGENTS.md` — conventions and the gate.
- `docs/forth.md` — Forth style rules (BLOCKING).
- `docs/parallel-agents.md` — map-reduce protocol for parallel dot work.
- `LESSONS.md` (top) — the project's running memory; the latest findings.
- `STATUS.md` — the current self-check counts and known gaps.

## 1. Conventions (BLOCKING)
- **Our words UPPER-CASE** (`SQUARE`, `AVG2`); built-in Forth words stay
  lower-case (`dup`, `if`, `>r`, `?do`). Never upper-case a built-in.
- **Hyphens, never underscores.** Short names (`buf`, `ctx`, `idx`).
- One concern per file. Small words; factor when the stack gets deep.

## 2. Write small CHECKED words
- Every **public** word carries an explicit typed `( in -- out )`:
  `: SQUARE ( i64 -- i64 ) dup * ;`. A typed sig turns checking on for that word.
- Types: `i64 u8 u32 cell bool char str addr`; type vars `a b c`; row vars
  `R S T` (a leading one = the stack tail); quotations
  `[ in -- out [| rin -- rout] ]`; the optional top-level return-stack clause
  `… | rin -- rout`. `n` is the generic int.
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
  `docs/repair-diagnostics.md` as the Repair diagnostic schema, and use
  `tools/diag-to-sarif.f` when the diagnostics need to enter CI/review UIs.
- Use `tools/public-signatures.f file.f` to expose typed public words as a
  machine-readable manifest for an agent or downstream package.
- Authoritative verdict (-1 certified / 0 rejected / 1 uncheckable) is native:
  use `bin/hb` for quick checks and `tools/check.sh --json-errors --all-errors`
  for files. `CHECK` infers the body's effect; `CHECK!` verifies the body
  against its declared `( in -- out )`. No-binary recovery uses
  `tools/seed.sh /path/to/hb-seed`.

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
  `tools/trust-lint.f` enforces this.

## 6. Test every word
- Add a `T{ … -> … }T` for each word as you write it — happy path plus each
  error/edge. A word without a test is unfinished. Put focused tests in the
  owning native gate file or `tools/*-test.sh` fixture. For errors, assert the
  exact THROW code.

## 7. Run the gate
- `( cd test && ./run.sh )` — habu-native, no gforth, < 10 s. Must be green.
- If `bin/hb` is missing, install a trusted native seed with
  `tools/seed.sh /path/to/hb-seed`; the seed immediately rebuilds current source
  through `tools/build.sh`.

## 8. Record lessons
- On any new finding, mistake, or insight, add a lesson to `LESSONS.md` (lessons
  only; API/patterns go in `docs/`). Every checker/codegen change needs an
  adversarial review hunting for false-certifies: "the gate is green" is
  necessary, not sufficient, for a soundness property.

## 9. Stay in scope
- When fixing a checker / codegen / REPL bug, fix the root cause and nothing
  else. One concern per change; one change per commit (`jj`, 50-char imperative).

## 10. Scorecard
- Correctness: `first_pass_checker=certified`, `first_pass_tests=true`, and
  `tests_passed=true`.
- Repair quality: minimize `repair_iterations`, `checker_iterations`, and
  `diagnostic_count`; every diagnostic must identify the failing definition,
  token/span, expected stack, actual stack, stable error code, and
  machine-readable `repair_class`. See `docs/repair-diagnostics.md` for the
  Repair diagnostic schema. Record `diagnostic_token`,
  `diagnostic_span`, `diagnostic_expected`, `diagnostic_actual`,
  `diagnostic_code`, `diagnostic_repair_class`, and `all_errors_stable` in
  result rows so repair feedback quality is scored, not assumed.
- Diagnostic usefulness requires an ablation, not just replayable rows. Compare
  the same checked Habu tasks with structured repair packets, raw checker text,
  and blind generic feedback. Claims that Habu is better for LLM-generated code
  need improved pass rate, fewer repair rounds, or lower token/wall effort in
  the structured arm against the raw/blind arms.
- Safety: `trust_uses=0` unless the task explicitly requires an audited boundary;
  `signature_weakened=false` always. Fix bodies before signatures.
- Cost: track `tokens_used`, `wall_ms`, and `final_chars`; fast feedback matters
  because checker calls sit inside the LLM repair loop.
- Coverage: report results by benchmark category, not just aggregate pass rate.
  A model that passes arithmetic but fails quotations, return-stack code, strings,
  files, or AOT-safe programs is not done. The native validator fails the
  reference benchmark unless those required categories are present in
  `bench/llm/tasks.tsv`.
- Live benchmark rows use `schema_version:2`. Required identity and context:
  `run_id`, `model_id`, `arm`, `task_id`, `trial_id`, `task_family`, `model`,
  `model_version`, `model_date`, `trial`, `task_order`, `k_trials`, and
  `order_seed`. If the registry cannot know model version/date, record the stable
  nonempty value `unknown`.
- Required live outcome evidence: `outcome`, `rounds`, `first_pass`,
  `first_pass_checker`, `first_pass_tests`, `tests_passed`,
  `repair_iterations`, `checker_iterations`, diagnostic quality booleans,
  `tokens`, `tokens_used`, `wall_ms`, `source_chars`, `final_chars`, trust and
  signature fields, `runtime_ms`, `runtime_repetitions`, `runtime_warmups`, and
  `runtime_status`.
- Required replay evidence: inline `prompt`, `raw_response`,
  `extracted_candidate`, `checker_diagnostics`, `repair_packet`, `test_output`,
  and `final_bundle`, each paired with a `*_sha256` field.

Run the reference scorecard with `bench/llm/run.sh`. It validates the task set,
the checked reference solutions, functional tests, and the JSONL metric schema.
To summarize a real model attempt, place per-task candidate files under a
candidate directory (`1.f` or repair rounds as `1/1.f`, `1/2.f`, ...), run
`bench/llm/run-attempts.sh CANDIDATE_DIR`, then pass the resulting JSONL to the
native validator; add `--json` for a machine-readable summary with failure
buckets and per-category coverage. Date-stamped run IDs use `*-YYYY-MM-DD` and
are validated by the native date parser.
Run `bench/llm/perf.sh` for quick latency measurements of the checker,
functional tests, metric validator, property-test smoke, and microbench smoke;
add `--full` when rebuild and AOT build/runtime timings are needed.
