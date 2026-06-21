# PLAN-AI.md — habu as an LLM codegen target: benchmark, results, and verification guide

This document records the **habu-vs-mainstream LLM-codegen benchmark** effort and gives
another agent everything needed to **independently verify the conclusions**. The original
plan (a within-habu checker A/B control) was superseded; the goal that drove the final work
is below.

Run verification commands from the repository root.

---

## 1. Goal (what we set out to measure)

> **Is habu a good language for an LLM to produce code in, vs mainstream languages, on
> complex tasks?** For each task an LLM (`claude -p`) writes the solution in raw Habu,
> library-assisted Habu, JavaScript, and Rust; we compile/check and run it against
> io-vectors, and measure the cost to reach a correct solution. The question is *how
> Habu stacks up against Rust/JS as a codegen target*, and whether an LLM-facing checked
> stdlib closes the raw-pointer gap — NOT (as an earlier iteration wrongly focused on)
> whether Habu's checker helps in isolation.

Two earlier task sets were rejected as too easy (a strong model one-shots them, so they don't
discriminate): single-function integer katas (gcd, fib, …) and fixed-size linear algebra
(2×2 matmul, cross product). The final suite is **algorithms over an integer array** —
genuinely hard for an LLM in habu because they need typed pointers, in-place mutation, and
concatenative loops, while being one-liners in JS/Rust.

---

## 2. The result (claims to be verified)

The committed evidence is 10 array tasks × 4 arms × 2 trials = 80 trials
(`bench/llm/results/run.jsonl`, summarized in `bench/llm/RESULTS.md`). The fourth
arm, `habu-lib`, gives Habu checked array helpers (`A@`, `A!`, `A-SWAP`,
`MIRROR-INDEX`, `EVEN?`) so the run directly compares raw Habu against a small
LLM-facing checked library.

| language | trial pass | first-try green | task pass@k | mean output-tokens-to-green | max |
|---|---|---|---|---|---|
| Habu raw | 90% | 80% | 100% | **753** | 4494 |
| Habu + array helpers | 95% | 95% | 100% | **630** | 3483 |
| JavaScript | 85% | 85% | 100% | 100 | 314 |
| Rust | 95% | 90% | 100% | 78 | 200 |

**Conclusions:**
1. **Task-level correctness parity, trial-level misses.** In the committed
   four-arm evidence, every arm reaches a correct solution for every task under
   k=2 (task pass@k 100%). The stricter trial-level result is Habu raw 18/20,
   Habu + helpers 19/20, JS 17/20, Rust 19/20; the misses are recorded in
   `RESULTS.md` as non-pass rows.
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

All passing rows in the committed evidence have positive output-token counts. The
per-task token table is in `RESULTS.md`.

---

## 3. HOW TO VERIFY (do these in order)

You need the native binary `bin/hb`, `node`, and `rustc`. If `bin/hb` is missing,
recover from a trusted seed with the checked command in `docs/seed.md`; it runs
the native build-fixpoint installer from current source.

### V1 — Harness is sound (deterministic, no LLM, no tokens)
```
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f bench/llm/grade.f bench/llm/grade-test.f
                                # -> grade-test: ok
./bench/llm/bench-test.sh      # -> PASS: array drivers (as + aa, 4 arms + habu repair)
```
`grade-test` proves the isolated run+grade spine classifies a correct/wrong/non-certifying/
trapping/looping candidate correctly. `bench-test` drives raw Habu, library
Habu, JavaScript, and Rust with STUB models (canned answers) for both
conventions and checks the JSONL each emits — including that Habu's repair loop
fires on a checker rejection (rounds=2).

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

### V3 — Reproduce the benchmark (uses real `claude -p`; costs tokens; non-deterministic)
```
./bench/llm/perf.sh --json > bench/llm/results/perf.json
BENCH_PERF_JSON=bench/llm/results/perf.json bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/process.f lib/process-argv.f lib/argv.f bench/llm/manifest.f bench/llm/run-expanded-bench.f -- 2 bench/llm/results/run-expanded.jsonl
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/process.f lib/process-argv.f lib/time.f lib/date.f lib/argv.f tools/json.f bench/llm/expanded-report.f -- bench/llm/results/run-expanded.jsonl bench/llm/results/perf.json > /tmp/RESULTS-expanded.md
```
Then compare `/tmp/RESULTS.md` to the committed `bench/llm/RESULTS.md`. Exact token counts
WILL differ run-to-run (model nondeterminism), but the **shape** must reproduce: pass@k ≈ 100%
for all arms, trial pass close to the table in §2, and Habu's per-task tokens ≈ 1× on the
elementwise tasks and many-× (especially ARGMAX) on the index/state/in-place tasks. The
committed `run.jsonl` is the exact evidence behind all four arms in §2.

### V4 — Spot-check a single live cell
```
sh bench/llm/drive-habu.sh 4 ARGMAX "ptr a n -- i64" \
   "Return the index of the maximum element; on ties the smallest index." as \
   "[3 1 4 1 5] -> 4; [9 1 1] -> 0; [1 5 5 2] -> 1; [5] -> 0" a </dev/null
```
Emits one raw-Habu JSONL row. Expect `outcome:pass` with a token count far above the JS/Rust
cost for the same task (run `drive-js.sh` / `drive-rust.sh` with the same args, dropping the
trailing `a`, to compare). To spot-check the helper arm, rerun the same command with trailing
`lib` instead of `a`; the driver emits `arm:"habu-lib"` and bundles `habu-array-lib.f` before
checking/grading the candidate.

---

## 4. Harness architecture (what each file does)

- `bench/llm/bench-tasks.tsv` — the 10 tasks. Columns: `id name sig conv spec vectors`. `conv` ∈
  `{as, aa}` (array→scalar, array→array). Vectors use `[..]` for arrays, e.g.
  `[3 1 4] -> 8` (as) or `[3 1 2] -> [2 1 3]` (aa). **Single source of truth** — every arm's
  test harness is generated from these vectors.
- `bench/llm/lib.sh` — sourced helpers. `hb_test/js_test/rust_test <conv> …` generate the
  per-language test harness from a task's vectors; `emit_row` writes a JSONL metrics line.
  (Note: `hb_test` forces `IFS=' '` internally — it builds the habu array with `here v , v , …`
  and must split on spaces regardless of the caller's IFS.)
- `bench/llm/habu-preamble.txt` — the in-context teaching for the raw Habu arm (typed-pointer
  locals, `i cells arr + @`/`!` indexing, `?do … loop`, explicit-boolean conditions, in-place
  rule). This is the *only* habu knowledge the model gets; the corpus-familiarity tax is what
  remains after this teaching.
- `bench/llm/habu-array-lib.f` — checked helper library for the `habu-lib` arm: array read/write,
  indexed increment, swap, mirror index, and even predicate. The helpers are bundled before the
  candidate, so the model writes against certified Habu code rather than an unchecked foreign API.
- `bench/llm/habu-preamble-lib.txt` — in-context teaching for the helper arm. It tells the model
  to prefer `A@`, `A!`, `A-SWAP`, `MIRROR-INDEX`, and `EVEN?` over raw address arithmetic.
- `bench/llm/drive-habu.sh` — Habu driver. The final argument selects `a` (raw Habu,
  `arm:"habu-a"`) or `lib` (helper arm, `arm:"habu-lib"`). Prompt = preamble + task;
  `claude -p` -> extract def -> bundle helper library when selected -> `tools/check.f`
  (certify) → on reject feed the checker diagnostic back (≤5 rounds); on certify, grade via
  `grade.sh`.
- `bench/llm/drive-forth.f` — checked-Habu manifest driver run by `bin/hb --load`.
  The expanded runner invokes it directly with checked Habu argv. Feedback mode selects
  the repair signal for the same task: `repair` (`arm:"habu-forth"`) sends normalized
  repair packets, `raw` (`arm:"habu-forth-raw"`) sends raw checker output, and `blind`
  (`arm:"habu-forth-blind"`) sends only generic failure feedback. This ablation is the
  evidence path for whether Habu diagnostics help LLM repair, separate from replay
  artifact auditability.
- `bench/llm/drive-js.sh`, `drive-rust.sh` — JS/Rust arms. `f(a)` returns a number (`as`) or
  array/`Vec` (`aa`); repair on node test failures / rustc errors + test failures.
- `bench/llm/grade.f` — runs a candidate in an isolated, timeout-bounded child so a trap/hang
  is *recorded, not fatal*; classifies `pass|fail|reject|trap|timeout`. For habu it builds the
  array in memory (`here , ,`) and runs the io-vectors via generated `G=` assertions.
- `bench/llm/parse-resp.f` — extracts the completion text + **output_tokens** from
  `claude -p --output-format json`. Input tokens are deliberately excluded (Claude Code harness
  overhead ~7–22K/call + prompt caching distort them); output tokens track generated-token cost.
- `bench/llm/run-expanded-bench.f <k>` — orchestrator: selected tasks × selected arms × k trials → `run-expanded.jsonl`,
  then the Habu report reducer → `RESULTS.md`. Drivers are invoked with `</dev/null` (else `claude -p`
  swallows the loop's stdin) and `|| true` (a failing driver must not abort the sweep).
- `bench/llm/report.f` — aggregates `run.jsonl` → `RESULTS.md` (trial pass, first-try
  green, task pass@k, non-pass rows, wall time, mean rounds, median/mean/max output tokens,
  per-task token table with raw/best and lib/best ratios, verdict).
- `bench/llm/ref-solutions.f` — certified habu answer key (see V2).
- `bench/llm/grade-test.f`, `bench-test.sh` — the deterministic teeth (see V1).

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
- **Both Habu arms use the checker** (it's how you'd really write habu); a rejection costs a
  repair round. This *helps* habu (localizes errors) but also means over-strict rejections cost
  rounds. Observed repair rounds were low (raw mean 1.11, helper mean 1.0), so this is a minor
  factor here.
- **Task pass@k hides trial misses.** Every arm is 100% at task pass@k because each task has at
  least one green trial, but trial pass ranges from 85% to 95%. Use both numbers.
- **Model nondeterminism.** `claude -p` is not bit-reproducible. k=2; verify the *shape*, not
  exact tokens. The habu side (engine, checker, grading) is fully deterministic.
- **Output-tokens-as-effort** is a proxy. It is generated-token cost, not direct hidden
  reasoning. It correlates with wall time in the data (ARGMAX raw Habu: 4494 tokens / 63 s vs
  JS: 77 / 6 s), but a model that emits less verbose output could shift absolute numbers;
  the *ratio* across languages is the durable signal.
- **Scope.** 10 single-array tasks, two conventions. Harder tasks (sorting, binary search,
  NxN matrices over memory) would likely *widen* the tail — an obvious next step.

---

## 6. Provenance

- The original three-arm harness code, `RESULTS.md`, and `run.jsonl` landed in jj commit
  `ce34f03f bench: habu vs JS/Rust on array/memory algorithms`. `run.jsonl` is tracked as the
  evidence record; only `*.log` under `results/` is gitignored.
- The four-arm harness adds `habu-lib` as a checked-library A/B against raw Habu; the committed
  `run.jsonl` includes all four arms.
- The `depth` primitive is a separate commit already on `habu` master.
