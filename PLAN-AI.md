# PLAN-AI.md — habu as an LLM codegen target: benchmark, results, and verification guide

This document records the **habu-vs-mainstream LLM-codegen benchmark** effort and gives
another agent everything needed to **independently verify the conclusions**. The original
plan (a within-habu checker A/B control) was superseded; the goal that drove the final work
is below.

Run verification commands from the repository root.

---

## 1. Goal (what we set out to measure)

> **Is habu a good language for an LLM to produce code in, vs mainstream languages, on
> complex tasks?** For each task an LLM writes the solution in raw Habu,
> library-assisted Habu, stdlib-assisted Habu, scaffold-assisted Habu,
> JavaScript, Python, TypeScript, and Rust; we compile/check and run it against
> io-vectors, and measure the cost to reach a correct solution. The question is
> *how Habu stacks up against mainstream languages as a codegen target*, and
> whether an LLM-facing checked stdlib closes the raw-pointer gap — NOT (as an
> earlier iteration wrongly focused on) whether Habu's checker helps in
> isolation.

Two earlier task sets were rejected as too easy (a strong model one-shots them, so they don't
discriminate): single-function integer katas (gcd, fib, …) and fixed-size linear algebra
(2×2 matmul, cross product). The final suite is **algorithms over an integer array** —
genuinely hard for an LLM in habu because they need typed pointers, in-place mutation, and
concatenative loops, while being one-liners in JS/Rust.

---

## 2. The result (claims to be verified)

### Current cross-language array evidence

There is no valid current cross-language array conclusion after the Codex
truncation RCA. The last attempted 15 task × 8 arm × k=5 run is invalid as
language evidence: 248 rows were `outcome:error`, every one had
`checker_diagnostics="model output truncated"`, and 246 had no extracted
candidate. The run mostly measured the Codex event-stream capture contract, not
Habu.

The harness now uses Codex `--output-last-message` and reads the final-answer
file as the candidate. Rerun the matrix before making any Habu-vs-mainstream
claim.

### Expanded Habu-Forth evidence

The previous expanded local evidence was 58 Forth/checker/system tasks × 5
trials = 290 trials. This is a **Forth-only Codex run** for the
`habu-forth` repair arm. It evaluates whether Codex can produce checked Habu
from diagnostics across the expanded task surface; it is not a cross-language
Rust/JS comparison.

| model | arm | rows | task pass@5 | trial pass | certified | tests passed | repair iterations | diagnostic fields | trust/signature weakening |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| Codex | habu-forth | 290 | 54/58 = 93.10% | 72.41% | 195 | 210 | 25 | 290/290 | 0 |

Expanded-run conclusions:

1. **Habu is usable but not done.** Codex reaches green checked Habu on 54 of
   58 task groups at k=5 with the repair driver. That is real progress over
   anecdotal examples, but it is not "best for LLMs" yet.
2. **Diagnostics are complete and replayable.** Every row carries the expected
   diagnostic/replay fields (`token`, `span`, `expected`, `actual`, `code`,
   `repair_class`, and stable all-errors replay), with zero false rejects,
   zero `TRUST` use, and zero signature weakening.
3. **The hard misses are concentrated.** The remaining task-pass failures are
   in quotation/combinator territory: quotation reaches 2/3 task pass@5 and
   combinator reaches 1/4. Most arithmetic, control, locals, polymorphic,
   memory, parsing, and diagnostic-repair groups reach 100% task pass@5.
4. **Repair helps but does not erase missing language/library shape.** The run
   records 95 checker rejections, 25 repair iterations, and 15 repaired rows.
   The next work is still the same: make the checked DSL/stdlib surface encode
   the patterns models now struggle to synthesize.

### Legacy cross-language array baseline

An earlier local evidence set covered 10 array tasks × 4 arms × 2 trials =
80 trials. The fourth arm, `habu-lib`, gives Habu checked array helpers (`A@`,
`A!`, `A-SWAP`, `MIRROR-INDEX`, `EVEN?`) so the run directly compares raw Habu
against a small LLM-facing checked library.

| language | trial pass | first-try green | task pass@k | mean output-tokens-to-green | max |
|---|---|---|---|---|---|
| Habu raw | 90% | 80% | 100% | **753** | 4494 |
| Habu + array helpers | 95% | 95% | 100% | **630** | 3483 |
| JavaScript | 85% | 85% | 100% | 100 | 314 |
| Rust | 95% | 90% | 100% | 78 | 200 |

Legacy cross-language conclusions:
1. **Task-level correctness parity, trial-level misses.** In that
   four-arm evidence, every arm reaches a correct solution for every task under
   k=2 (task pass@k 100%). The stricter trial-level result is Habu raw 18/20,
   Habu + helpers 19/20, JS 17/20, Rust 19/20; the misses are recorded in
   local report as non-pass rows.
2. **A large but SKEWED effort gap.** "Output tokens to green" counts generated output
   tokens on passing trials with positive token counts. It is not direct access
   to hidden reasoning, but it is a useful generation-effort proxy. The
   distribution is **bimodal**:
   - *Simple elementwise loops* (ARR-SUM, SQ-EACH, NEGATE-EACH, ARR-MAX): habu ≈ **1×**
     (comparable or cheaper — terse source, regular shape).
   - *Index tracking / carried state / in-place rearrangement* (ARGMAX, REVERSE, RUNMAX,
     PREFIXSUM, COUNT-EVEN): the hard tail remains. Worst measured raw-Habu row:
     ARGMAX ≈ **4494 tokens vs 77** (58×).
   - Net mean: raw Habu ≈ **9.6×** the cheapest mainstream arm; helper Habu ≈ **8.1×**
     the cheapest mainstream arm and ≈ **0.8×** raw Habu.
3. **Cause:** the corpus-familiarity tax. habu's typed pointers (`arr:ptr`), `i cells arr + @`/`!`
   indexing, and in-place concatenative loops have ~zero pretraining, so the model reasons each
   step from first principles — cheap when the stack shape is obvious, expensive when it must
   juggle. The helper arm did not close the mean-token gap in this run; it slightly
   reduced the mean vs raw Habu but still showed hard-tail spikes. It helped REVERSE,
   COUNT-EVEN, ARGMAX, and RUNMAX in this run, but hurt PREFIXSUM and several simple
   loops.

All passing rows in that evidence had positive output-token counts.

---

## 3. HOW TO VERIFY (do these in order)

You need the native binary `bin/hb`, `node`, and `rustc`. If `bin/hb` is missing,
recover from a trusted seed with the checked command in `docs/seed.md`; it runs
the native build-fixpoint installer from current source.

### V1 — Harness is sound (deterministic, no LLM, no tokens)
```
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f bench/llm/grade.f bench/llm/grade-test.f
                                # -> grade-test: ok
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/json-write.f bench/llm/fixture-text.f bench/llm/run-expanded-bench-test.f
                                # -> run-expanded-bench-test: ok
```
`grade-test` proves the isolated run+grade spine classifies a correct/wrong/non-certifying/
trapping/looping candidate correctly. `run-expanded-bench-test.f` drives the
native benchmark dispatcher with STUB models (canned answers), checks the JSONL
it emits, and proves foreign arms dispatch through `bin/hb --load` instead of
retired shell launchers.

### V2 — Tasks are FEASIBLE in habu and the io-vector ground truth is correct
```
bin/hb < bench/llm/ref-solutions.f      # -> prints REF-OK
```
`ref-solutions.f` is the certified answer key: the 10 habu reference words + assertions over
the same io-vectors as `bench-tasks.tsv`. This single command is a complete proof: `bin/hb`
auto-checks typed definitions, so a non-certifying word would be unpublished and the run would
error — getting **REF-OK** means *all 10 definitions certify* **and** *every io-vector value
matches*. (To see per-definition verdicts, prepend a check hook:
`{ echo ": HK CHECK dup . ; ' HK set-check"; bench/llm/ref-solutions.f's def section; } | bin/hb`
prints `-1` per certified word. The native `tools/check.f` runner on the def section alone also returns rc 0;
do NOT run the checker runner on the whole file — it executes the runtime assertions in its checking
harness and hangs.)

### V3 — Verify a local cross-language array comparison
```
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/json.f tools/json-file.f tools/argv.f bench/llm/report.f -- /tmp/habu-array-expanded.jsonl /tmp/habu-llm-perf.json > /tmp/habu-RESULTS-array-expanded.md
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/date.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/json.f tools/json-file.f tools/argv.f bench/llm/validate-results-lib.f bench/llm/validate-results.f -- --json /tmp/habu-array-expanded.jsonl > /tmp/habu-array-expanded-validate.json
```
Generated JSONL/JSON/Markdown reports are ignored by git. Archive large replay
evidence outside the repo.

### V4 — Reproduce the expanded Forth-only benchmark (uses a live model; costs tokens; non-deterministic)
```
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/json-write.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/time.f bench/llm/perf-lib.f bench/llm/perf.f -- --json > /tmp/habu-llm-perf.json
MODEL_ID=codex BENCH_FORTH_MODES=repair BENCH_TASK_IDS=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,56,57,58,59,60,61,71,72,73,74,75,76,77 BENCH_PERF_JSON=/tmp/habu-llm-perf.json BENCH_RESULTS=/tmp/habu-RESULTS-expanded.md bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/argv.f bench/llm/manifest.f bench/llm/run-expanded-bench.f -- 5 /tmp/habu-run-expanded.jsonl
```
Then review `/tmp/habu-RESULTS-expanded.md`. Exact token counts WILL differ
run-to-run (model nondeterminism). Verify the shape, not exact tokens.

### V5 — Spot-check a single live cell
```
MODEL_ID=codex BENCH_TASK_IDS=49 BENCH_ARRAY_ARMS=habu-a \
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/argv.f bench/llm/manifest.f bench/llm/run-expanded-bench.f -- 1 /tmp/argmax.jsonl
```
Emits one raw-Habu JSONL row. Expect `outcome:pass` with a token count far above
the JS/Rust cost for the same task when run through the native expanded runner
with `BENCH_ARRAY_ARMS` selecting those arms. To spot-check the helper arm,
select `habu-lib`; the native driver emits `arm:"habu-lib"` and bundles
`habu-array-lib.f` before checking/grading the candidate.

---

## 4. Harness architecture (what each file does)

- `bench/llm/bench-tasks.tsv` — the 10 tasks. Columns: `id name sig conv spec vectors`. `conv` ∈
  `{as, aa}` (array→scalar, array→array). Vectors use `[..]` for arrays, e.g.
  `[3 1 4] -> 8` (as) or `[3 1 2] -> [2 1 3]` (aa). **Single source of truth** — every arm's
  test harness is generated from these vectors.
- `bench/llm/foreign-vectors.f` — checked vector snippet emitters for
  JavaScript, Python, TypeScript, and Rust. Native drivers share this path so
  the per-language harnesses come from one checked implementation.
- `bench/llm/habu-preamble.txt` — the in-context teaching for the raw Habu arm (typed-pointer
  locals, `i cells arr + @`/`!` indexing, `?do … loop`, explicit-boolean conditions, in-place
  rule). This is the *only* habu knowledge the model gets; the corpus-familiarity tax is what
  remains after this teaching.
- `bench/llm/habu-array-lib.f` — checked helper library for the `habu-lib` arm: array read/write,
  indexed increment, swap, mirror index, and even predicate. The helpers are bundled before the
  candidate, so the model writes against certified Habu code rather than an unchecked foreign API.
- `bench/llm/habu-preamble-lib.txt` — in-context teaching for the helper arm. It tells the model
  to prefer `A@`, `A!`, `A-SWAP`, `MIRROR-INDEX`, and `EVEN?` over raw address arithmetic.
- `bench/llm/drive-array-habu.f` — Habu array driver. The mode selects raw
  Habu (`arm:"habu-a"`), helper (`arm:"habu-lib"`), stdlib, or skeleton arms.
  Prompt = preamble + task; model output -> extract def -> bundle helper
  library when selected -> `tools/check.f` (certify) -> on reject feed the
  checker diagnostic back; on certify, grade via `bench/llm/grade.f`.
- `bench/llm/drive-forth.f` — checked-Habu manifest driver run by `bin/hb --load`.
  The expanded runner invokes it directly with checked Habu argv. Feedback mode selects
  the repair signal for the same task: `repair` (`arm:"habu-forth"`) sends normalized
  repair packets, `raw` (`arm:"habu-forth-raw"`) sends raw checker output, and `blind`
  (`arm:"habu-forth-blind"`) sends only generic failure feedback. This ablation is the
  evidence path for whether Habu diagnostics help LLM repair, separate from replay
  artifact auditability.
- `bench/llm/drive-js.f`, `drive-python.f`, `drive-ts.f`, `drive-rust.f` —
  native wrappers for the foreign arms. `f(a)` returns a number (`as`) or
  array/list/`Vec` (`aa`); repair uses the language runner diagnostics and
  vector-test failures through checked Habu orchestration.
- `bench/llm/grade.f` — runs a candidate in an isolated, timeout-bounded child so a trap/hang
  is *recorded, not fatal*; classifies `pass|fail|reject|trap|timeout`. For habu it builds the
  array in memory (`here , ,`) and runs the io-vectors via generated `G=` assertions.
- `bench/llm/parse-resp.f` — extracts the completion text + **output_tokens** from
  `claude -p --output-format json`. Input tokens are deliberately excluded (Claude Code harness
  overhead ~7–22K/call + prompt caching distort them); output tokens track generated-token cost.
- `bench/llm/run-expanded-bench.f <k>` — orchestrator: selected tasks × selected arms × k trials → local JSONL,
  then the Habu report reducer → local Markdown. Drivers are invoked with `</dev/null` (else `claude -p`
  swallows the loop's stdin) and `|| true` (a failing driver must not abort the sweep).
- `bench/llm/report.f` — aggregates local JSONL → Markdown (trial pass, first-try
  green, task pass@k, non-pass rows, wall time, mean rounds, median/mean/max output tokens,
  per-task token table with raw/best and lib/best ratios, verdict).
- `bench/llm/ref-solutions.f` — certified habu answer key (see V2).
- `bench/llm/grade-test.f`, `bench/llm/run-expanded-bench-test.f`, and the
  driver-specific `*-test.f` files — the deterministic native teeth (see V1).

Also delivered to `habu` master earlier in this effort: a native `depth ( -- n )` primitive
(`src/habu/habu1.f`, `src/core/checker.f`) — a standard Forth core word habu lacked. It is NOT
used by this benchmark (which grades by value, not stack depth) but is a sound standalone
addition.

---

## 5. Threats to validity / honest caveats

- **Cross-language token comparison is confounded** and is *not* the headline. habu source is
  terser (fewer tokens for the same logic), which would bias habu LOW; on the hard tasks the
  reasoning cost overwhelms this and biases habu HIGH. The robust signals are **pass-rate**
  (parity) and the **direction + magnitude of the skew** (cheap on elementwise, expensive on
  juggling), which are insensitive to per-token terseness.
- **All Habu arms use the checker** (it is how you should write Habu); a rejection costs a
  repair round. This helps Habu by localizing errors but also means over-strict
  rejections cost rounds. The current array run records zero checker false
  rejects, so the main failure mode is model code failing to certify or pass
  vectors, not checker overreach.
- **Task pass@k hides trial misses.** The current array run shows this clearly:
  `habu-stdlib` is 80% task pass@5 but only 36% trial pass. Use both numbers.
  The expanded Forth-only run is 93.10% task pass@5 and 72.41% trial pass.
- **Model nondeterminism.** Live Codex runs are not bit-reproducible. For fresh
  live reruns, verify the shape, not exact tokens. The Habu side (engine,
  checker, grading, validation, and report reduction) is deterministic.
- **Output-tokens-as-effort** is a proxy. It is generated-token cost, not direct hidden
  reasoning. It correlates with wall time in the data (ARGMAX raw Habu: 4494 tokens / 63 s vs
  JS: 77 / 6 s), but a model that emits less verbose output could shift absolute numbers;
  the *ratio* across languages is the durable signal.
- **Scope.** The current cross-language comparison is 15 single-array tasks.
  Harder tasks (sorting, binary search, maps, files, processes, property tests,
  and NxN matrices over memory) are still needed before any broad "best for LLMs"
  claim.

---

## 6. Provenance

- Historical generated benchmark artifacts were removed from git. Keep future
  JSONL/JSON/Markdown benchmark outputs in `/tmp`, release artifacts, or another
  explicit artifact store, not in repository commits.
- The `depth` primitive is a separate commit already on `habu` master.
