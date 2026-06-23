# habu plan - LLM-native Checked Forth

This is the active top-level plan. The old checker/self-host implementation plan
is complete enough to leave the top-level roadmap: `bin/hb` self-hosts, rebuilds
to a fixpoint, and the native gate is the source of truth. Historical checker
details belong in focused implementation docs. Current work is to make Habu the
best practical target for LLM-generated checked systems scripts and small
programs, without weakening checked Forth.

Detailed benchmark evidence and reproduction commands live in `PLAN-AI.md`.
Forth coding standards live in `docs/forth.md`.

## Current Diagnosis

The decisive current cross-language array evidence is a Codex k=5 run:

| arm | rows | task pass@5 | trial pass | mean output tokens | median runtime ms |
|---|---:|---:|---:|---:|---:|
| Habu raw | 75 | 5/15 = 33% | 7% | 1181 | 242 |
| Habu + array helpers | 75 | 6/15 = 40% | 9% | 743 | 266 |
| Habu + stdlib | 75 | 12/15 = 80% | 36% | 642 | 461 |
| Habu + skeleton | 75 | 8/15 = 53% | 17% | 801 | 241 |
| JavaScript | 75 | 15/15 = 100% | 100% | 79 | 1 |
| Python | 75 | 15/15 = 100% | 100% | 70 | 1 |
| TypeScript | 75 | 15/15 = 100% | 100% | 74 | 1 |
| Rust | 75 | 15/15 = 100% | 100% | 84 | 1 |

Raw evidence: `bench/llm/results/run-array-expanded.jsonl`; summary:
`bench/llm/RESULTS-array-expanded.md`.

Conclusion: Habu is not the best LLM target yet on array/memory tasks. The
checked stdlib arm is real progress: it improves task pass@5 by 47 percentage
points over raw Habu and cuts passing-row output tokens roughly in half. It
still trails every mainstream baseline by 20 task-pass points, 64 trial-pass
points, and roughly 8-9x generated output-token effort. This is the actionable
gap the remaining plan must close.

The current expanded Habu-only live evidence is a Codex Forth-only k=5 run:

| arm | rows | task pass@5 | trial pass | diagnostics complete | trust/signature weakening |
|---|---:|---:|---:|---:|---:|
| habu-forth | 290 | 54/58 = 93.10% | 72.41% | 290/290 | 0 |

This proves the checked repair harness is useful and replayable across the
expanded Habu task surface, but it does not prove Habu is the best LLM target
yet. The remaining task-pass failures are concentrated in quotation and
combinator tasks, so the plan still needs stronger checked DSL/library shapes
and diagnostics before rerunning cross-language baselines.

The older committed four-arm LLM benchmark has 80 live rows:

| arm | trial pass | task pass@k | mean output tokens | max output tokens |
|---|---:|---:|---:|---:|
| Habu raw | 90% | 100% | 753 | 4494 |
| Habu + array helpers | 95% | 100% | 630 | 3483 |
| JavaScript | 85% | 100% | 100 | 314 |
| Rust | 95% | 100% | 78 | 200 |

Habu is viable today: every benchmark task has a green Habu solution under k=2.
It is not yet the best LLM target. The gap is concentrated in hard tails:

- Index tracking: `ARGMAX`.
- In-place rearrangement: `REVERSE`.
- Carried state over arrays: `PREFIXSUM`, `RUNMAX`.
- Predicate/count loops: `COUNT-EVEN`.

The first helper arm proved raw pointer arithmetic is only part of the problem.
`A@`, `A!`, `A-SWAP`, and related helpers made `REVERSE` much easier, but the
model still had to invent loop skeletons and accumulator invariants by hand. The
durable fix is a checked DSL and standard library that encode those invariants.

## What "Best For LLM Use" Means

Habu should win on the work it is designed for: small checked programs, build and
test scripts, data/file/process glue, and performance-sensitive kernels that
benefit from a self-hosted native target. "Best" is measured, not asserted.

Hard gates:

- `habu-stdlib` task pass@k is at least the best mainstream baseline on the
  expanded benchmark.
- `habu-stdlib` trial pass is within 5 percentage points of the best mainstream
  baseline.
- Diagnostic-quality fields are present for 100% of checker rejections.
- Public stdlib code is checked typed Habu, except audited boundaries covered by
  tests and trust lint.
- The full native gate passes.

Current gate status from the 600-row cross-language array run: benchmark gates
are **not met**. `habu-stdlib` is 80% task pass@5 vs 100% for JS/Python/TS/Rust,
and 36% trial pass vs 100% for those baselines. Diagnostic completeness is met
for this run, with 600/600 diagnostic fields present, zero false rejects, zero
`TRUST` use, and zero signature weakening.

Win conditions:

- Array hard-tail token ratios are below 3x the best mainstream baseline.
- Median repair rounds for `habu-stdlib` are 1.
- Mean output tokens for `habu-stdlib` are below 2x Rust on checked/system tasks
  and below 2x mainstream dynamic-language arms on script/glue tasks.
- Runtime of generated Habu code is faster than mainstream dynamic-language arms
  for equivalent kernels and competitive with Rust for small kernels where
  startup does not dominate.
- Generated Habu uses stable stdlib words instead of raw stack gymnastics in
  most passing solutions.

Non-goals:

- Portability work in this phase.
- Weakening the checker to make examples pass.
- Hiding raw Habu results. Raw Habu remains the control arm.
- Treating prompt wording as the main fix. Prompts can improve, but the durable
  fix is checked library structure plus repair-oriented diagnostics.

## Design Principles

- Checked code first. New public/library words default to checked typed Habu.
- One public binary: `bin/hb`. Shell helpers may concatenate libraries, but no
  new public build-only binary is introduced.
- Standard-library words are not benchmark cheating. JS, Rust, TypeScript, and
  dynamic scripting ecosystems win partly because their libraries encode idioms.
  Habu needs equivalent checked idioms.
- Prefer high-level kernels before clever prompts. The model should compose
  certified words, not rediscover invariants.
- Keep raw arms in benchmarks to measure improvement honestly.
- Measure separate axes: pass rate, repair rounds, token effort, wall time,
  runtime, diagnostics, and category coverage.
- No silent fallbacks. Capacity, bounds, unsupported feature, and parse failures
  get explicit named errors.

## Baseline Contracts

These are not future features; they are constraints the LLM-native plan must
preserve while adding library and benchmark surface.

### Public Binary And Invocation

The public command is `hb` through `bin/hb`. User-facing docs, scripts, examples,
benchmark drivers, and prompts must not introduce public executables named
`hbi`, `habu`, or any maker/build-only binary.

`bin/hb` owns all normal entry modes:

- tty and `argc <= 1`: start the checked REPL.
- tty and `argc > 1`: run `argv[1]` as a script with remaining args exposed to
  Habu.
- `--load source... -- args...`: explicit source-list script mode. The loader
  reads only the listed source files before `--`; fd 0 is left untouched as tool
  data, and script args start after `--`.
- non-tty stdin with bytes: run stdin as the program, regardless of `argc`; args
  remain visible to the program unless the explicit `--load` source-list mode is
  selected. This is required for property/benchmark seeds.
- non-tty stdin at EOF and `argc > 1`: run `argv[1]` as a script.

Build scripts are ordinary Habu scripts loaded by these modes. Shell wrappers may
only set final environment values, create and export private `HB_TMP`, launch
`bin/hb`, install already-validated final artifacts, and propagate exit status.
Build policy, source selection, step graph decisions, artifact expectations,
checker certification, fixpoint comparison, and fallback logic belong in checked
Habu libraries and scripts.

Baked REPL, stepper, and debug source are trusted engine UI. Snapshots may
prepend `0 set-check` while baking that UI, but must reinstall a `CHECK!` hook
before user input. The gate must prove typed definitions are checked after a
rebuild/snapshot.

### Native-Only Trust Root

Daily work uses `bin/hb`, `tools/build-fixpoint-main.f -- install`, and `bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f`.
No-binary recovery runs `tools/seed.f tools/seed-main.f` under a trusted native
`hb` seed, then immediately rebuilds current source. A gforth or hosted
bootstrap path must not become a normal gate, benchmark dependency, or
documented workflow.

Historical bootstrap sources may remain only as inert reference material. They
must not feed default lint, benchmark, build, or self-check paths. The native
fixpoint build and native gate are the active parity proof.

Verification must assert that `bin/` exposes no public executable except `hb`
after rebuild, and that maker/build engines remain private temp artifacts under
`HB_TMP`.

### Soundness Gate

The property harness in `test/prop-test.f` is a standing soundness gate, not an
optional experiment. It uses the standard `depth ( -- n )` primitive to measure
runtime stack arity without sentinel values, runs generated programs in-process
through `evaluate`, and fails only on certified-but-wrong behavior.

The stdlib/property work in this plan may extract reusable generators and test
helpers, but it must not weaken the existing checker-soundness gate.
Verification must include both the default property smoke and an argv-preserving
pipeline run such as `bin/hb 2 10 < test/prop-test.f`.

### LLM-Facing Documentation

`docs/stdlib.md` is the authoritative LLM-facing stdlib surface. `LLM.md`,
`docs/forth.md`, `docs/stdlib.md`, `STATUS.md`, `TRUSTED.md`, and
machine-readable signature manifests must agree. The prompt surface for agents
comes from the same checked public signatures and documented contract sections
that the gate validates, not from hand-copied examples that can drift.

Tracked dots:

- `caf-017ff627bf8096be` Reverify LLM baseline contracts.
- `caf-dda01a0bccb3d15c` Specify build shell boundary.
- `caf-f6a5010c93899801` Port build step model to Habu.
- `caf-5db04ad3950fc01c` Port build fixpoint driver.
- `caf-ec42e1261b644134` Audit LLM dot dependency graph.

## Representation Decision

The long-term public model is `slice a`: one logical array value. The engine does
not currently expose a native `slice a` type, so v1 uses the existing checked
representation:

```forth
ptr a n
```

where `ptr a` is the first element and `n` is the length. Low-level indexed words
also use `n` for indexes because the current type grammar cannot distinguish a
length from an index:

```forth
A@ ( ptr a n -- a )  \ n is an index here, not a length
```

Docs and prompts may call this an "array view", but signatures must stay in the
current grammar until a real `slice a` type is implemented and benchmarked. When
the meaning of an `n` matters, documentation and tests must name it as length,
index, start, or count in prose.

Future `slice a` support is allowed only after v1 proves the stdlib shape. It
must not block Phase 1.

Other domain handles also stay inside today's type grammar. Memory-backed handles
use `ptr u8 n` buffers in v1: the pointer is the storage base and `n` is the
buffer capacity or compiled byte length, as documented per word. `addr` is only
for opaque handles that are never dereferenced by checked code, or for an audited
TRUST boundary that converts to a typed pointer. Regex and map docs may call
these values `rx` or `map` in prose, but checked signatures must remain typed
pointers until dedicated concrete handle types exist.

Tracked dot:

- `caf-b530d2dcfb50767a` Specify stdlib handle representation.

## File Layout

Add a `lib/` tree for reusable checked Habu:

```text
lib/
  errors.f
  array.f
  string.f
  regex.f
  map.f
  fs.f
  process.f
  argv.f
  test.f
  property.f
  build.f
  time.f
  date.f
  std.manifest

docs/
  stdlib.md
  llm-scorecard.md

tools/
  bundle-lib.f
  repair-packet.f
```

Native scripts still load a single source file. `tools/bundle-lib.f` owns
concatenation of selected `lib/*.f` files plus the script driver into a temporary
file and then runs `bin/hb bundled.f args...`. This preserves the one-binary
contract while giving scripts a stable library surface.

Each library gets:

- A focused `lib/<name>.f`.
- A focused `lib/<name>-test.f` or existing test fixture retargeted to `lib/`.
- Documentation in `docs/stdlib.md`.
- Public-signature coverage where useful.
- No unrelated responsibilities in the same file.

`docs/llm-scorecard.md` records the benchmark and repair fields used by live
model runs. `LLM.md` remains the concise operating protocol agents read before
coding.

`lib/errors.f` owns stdlib throw codes. It reserves named ranges by library, and
every fallible public word gets exact error tests. Local ad hoc negative codes
must not leak into public stdlib APIs.

`TRUST` in `lib/` is permitted only when the checker cannot express the boundary.
The same audit rules as engine code apply: `TRUSTED.md` entry, focused runtime
tests, public-signature coverage, and trust lint scanning both `src/` and `lib/`.

Tracked dots:

- `caf-a486f45e913299c2` Create stdlib layout and manifest.
- `caf-ea7ebca2557a64fd` Define stdlib error policy.
- `caf-20ce687c0730c333` Extend trust lint to lib.

## Phase 1 - Checked Array Stdlib

Goal: make current array hard tails library composition.

Create `lib/array.f` and move the benchmark-private helper surface there. Effects
below use the v1 representation.

Indexed helpers use nominal `len`, `idx`, and `count` roles at the public API.
Convert plain numeric task inputs with `>LEN`, `>IDX`, and `>COUNT` at the call
boundary. The roles make length/index/count mixups checker-visible while the
implementation erases them to cells for arithmetic and address generation.

```forth
A@             ( ptr a len idx -- a )     \ pointer, length, index
A!             ( a ptr a len idx -- )     \ value, pointer, length, index
A+!            ( n ptr i64 len idx -- )   \ delta, pointer, length, index
A-SWAP         ( ptr a len idx idx -- )   \ pointer, first index, second index
A-CHECK-INDEX  ( len idx -- )             \ throws on invalid index
A-CHECK-RANGE  ( len idx count -- )       \ throws on invalid range
LAST-INDEX     ( len -- idx )
MIRROR-INDEX   ( len idx -- idx )
EVEN?         ( n -- bool )
```

Bounds semantics:

- `A-CHECK-INDEX` throws `E-A-BOUNDS` when length is negative, index is negative,
  or index is greater than or equal to length.
- `A-CHECK-RANGE` throws `E-A-BOUNDS` when length, start, or count is negative,
  or when `start + count > length`.
- `LAST-INDEX` throws `E-A-EMPTY` when length is less than or equal to zero.
- Whole-array words that require at least one element throw `E-A-EMPTY`; empty
  no-op words state that explicitly.

Whole-array scalar kernels:

```forth
A-SUM         ( ptr i64 len -- i64 )  \ empty => 0
A-MIN         ( ptr i64 len -- i64 )  \ length must be > 0
A-MAX         ( ptr i64 len -- i64 )  \ length must be > 0
A-COUNT-EVEN  ( ptr i64 len -- count )
A-ARGMAX      ( ptr i64 len -- idx )  \ length > 0, ties choose smallest index
A-MAX-INDEX   ( ptr i64 len -- idx )  \ alias for A-ARGMAX
```

Whole-array mutating kernels:

```forth
A-REVERSE-RANGE!  ( ptr a len idx count -- )
A-REVERSE!        ( ptr a len -- )
A-PREFIX-SUM!     ( ptr i64 len -- )  \ empty is no-op
A-RUNMAX!         ( ptr i64 len -- )  \ empty is no-op
A-FILL!           ( a ptr a len -- )
```

Array errors:

```forth
E-A-EMPTY   \ operation requires len > 0
E-A-BOUNDS  \ checked helper receives invalid index/range
```

Implementation constraints:

- Start with checked Habu definitions.
- Use `TRUST` only if the checker cannot express a necessary higher-order
  boundary, and then keep that boundary tiny.
- Empty-array behavior is explicit in tests.
- Tie behavior for `A-ARGMAX` is tested.
- Mutating words are tested by reading the final array back.

Benchmark integration:

- Keep `habu-a` raw.
- Keep `habu-lib` for the first helper arm if it remains useful historically.
- Add `habu-stdlib`, loading `lib/array.f` through the bundle mechanism.
- `habu-stdlib` prompts instruct the model to call the stdlib word when it
  directly matches the task.

Success for Phase 1:

- `ARGMAX`, `REVERSE`, `PREFIXSUM`, and `RUNMAX` hard-tail ratios drop below 3x
  mainstream best in the `habu-stdlib` arm.
- Native benchmark fixtures cover the new arm without live model calls through
  `bench/llm/run-expanded-bench-test.f` and the driver-specific checked tests.
- `lib/array.f` public words certify through the native `tools/check.f` runner.

Tracked dots:

- `caf-fa509d91be8db3c7` Define LLM stdlib surface.
- `caf-583555f1af00460c` Add stdlib bundle mechanism.
- `caf-8f6e22a01fe1c7f7` Promote array helpers to stdlib.
- `caf-973cc1cf71a64a4b` Add array bounds and empty errors.
- `caf-bd4c836040edce85` Add scalar array hard-tail kernels.
- `caf-bb6d52fb5ec8151c` Add mutating array hard-tail kernels.
- `caf-eca720800d681475` Wire benchmark to stdlib arrays.

## Phase 2 - Checked Array Combinators

Goal: support new array tasks by composition, not one-off kernels.

Public combinators:

```forth
A-MAP!          ( ptr a len [ a -- a ] -- )
A-MAPI!         ( ptr a len [ idx a -- a ] -- )
A-FOLD          ( ptr a len b [ b a -- b ] -- b )
A-FOLDI         ( ptr a len b [ b idx a -- b ] -- b )
A-SCAN!         ( ptr i64 len i64 [ i64 i64 -- i64 ] -- )
A-SCAN1!        ( ptr i64 len [ i64 i64 -- i64 ] -- )
A-FIND-INDEX    ( ptr a len [ a -- bool ] -- n )
A-FIND-INDEXI   ( ptr a len [ idx a -- bool ] -- n )
```

Semantics:

- `A-MAP!` replaces each element with the quotation result.
- `A-MAPI!` also passes the zero-based index as `idx`.
- `A-FOLD` returns the final accumulator and does not mutate the array.
- `A-FOLDI` also passes the zero-based index as `idx`.
- `A-SCAN!` threads an explicit initial accumulator; each new accumulator is
  stored into the current element.
- `A-SCAN1!` uses the first element as the initial accumulator; empty input is a
  no-op.

Checker policy:

- First attempt: implement as checked Habu using existing quotation execution and
  row-polymorphic signatures.
- If a public combinator cannot be inferred, use one audited `TRUST` per
  combinator signature, with runtime/property tests proving the asserted effect.
- Any checker limitation discovered while implementing the combinators gets its
  own dot; do not hide it in library code.

Tests:

- Unit examples for map, indexed map, fold, indexed fold, scan, scan1.
- Property tests against simple reference loops for generated arrays.
- Negative tests for quotation type mismatch once repair diagnostics exist.

Tracked dots:

- `caf-b0c0e08bc3723750` Expand checked array combinators.
- `caf-fb4983450627a58b` Document repair diagnostic schema.
- `caf-8d5f0a90a453fbbb` Add repair-class hint text.

## Phase 3 - Repair Packets

Goal: convert checker failures into compact, actionable LLM repair input.

The native `tools/check.f` runner with `--json-errors --all-errors` already exposes structured
diagnostics. Add `tools/repair-packet.f` to turn those diagnostics into a stable
schema and prompt fragment.

JSON packet schema:

```json
{
  "schema_version": 1,
  "kind": "habu_repair_packet",
  "word": "NAME",
  "token": "dup",
  "token_index": 3,
  "file": "path.f",
  "line": 12,
  "column": 8,
  "byte_start": 120,
  "byte_end": 123,
  "declared_effect": "i64 -- i64",
  "inferred_effect": "i64 -- i64 i64",
  "expected": "i64",
  "actual": "i64 i64",
  "return_stack": {
    "expected": "",
    "actual": ""
  },
  "code": "E-MISMATCH",
  "repair_class": "remove_producer",
  "hint": "The body leaves an extra value. Consume or remove the producer at token dup.",
  "source_excerpt": ": BAD ( i64 -- i64 ) dup ;",
  "diagnostic_count": 1
}
```

Text packet format for prompts:

```text
HABU_REPAIR v1
word: NAME
token: dup
declared: i64 -- i64
expected: i64
actual: i64 i64
repair_class: remove_producer
hint: The body leaves an extra value. Consume or remove the producer at token dup.
source: : BAD ( i64 -- i64 ) dup ;
```

Required repair classes:

- `add_producer`
- `remove_producer`
- `fix_type`
- `fix_return_stack`
- `fix_branch`
- `fix_loop`
- `fix_quotation`
- `fix_signature_syntax`
- `unknown_word`
- `unsafe_word`

Repair fixtures:

- One deterministic checker fixture per `repair_class`.
- Fixture output includes the repair packet and verifies stable token/span,
  expected/actual, code, class, hint, and source excerpt fields.
- Multi-diagnostic inputs produce deterministic ordering and packet counts.

Benchmark driver changes:

- `bench/llm/drive-array-habu.f` uses JSON diagnostics and repair packets for all
  checker rejections.
- The native driver feeds the candidate definition plus the repair packet, not
  raw wrapper stderr.
- Deterministic checked bench tests assert packet use without live model calls.

Tracked dots:

- `caf-095648ac69941804` Add repair class fixtures.
- `caf-be7e76a1a9232ab0` Add LLM repair packet tool.
- `caf-fcf16bb7fe724292` Use repair packets in Habu driver.

## Phase 4 - Benchmark V2

Goal: prove Habu as an LLM target across real surfaces, not only one array suite.

Arms:

- `habu-a`: raw Habu.
- `habu-lib`: legacy small helper arm, kept while useful for continuity.
- `habu-stdlib`: full public stdlib arm.
- `habu-skeleton`: stdlib plus checked signatures/tests/skeletons where the
  model fills the body.
- `habu-forth`: checked Habu with structured repair packets after checker
  rejection.
- `habu-forth-raw`: the same checked Habu tasks with raw checker diagnostics fed
  back instead of normalized repair packets.
- `habu-forth-blind`: the same checked Habu tasks with generic failure feedback
  and no diagnostic detail in the repair prompt.
- `js`
- `rust`
- `dynamic-scripting`
- `typescript`

Task manifest:

- V2 uses one canonical manifest, `bench/llm/tasks.tsv`; legacy
  `bench/llm/bench-tasks.tsv` is either imported into it or retired.
- The manifest records task id, category, feature tags, harness kind, language
  signatures, prompt spec, and vectors/reference data.
- Live runs, reference solutions, validators, and reports all read the same
  manifest.

Task categories:

- Arrays and mutation.
- Strings and parsing.
- Regex subset.
- Maps/counting/grouping.
- Files and paths.
- Processes and command capture.
- Time and date.
- Property tests.
- Build scripts.
- Diagnostics/repair.
- AOT-safe programs.
- Core checked-Forth features: quotations, return stack, row-polymorphic
  combinators, control/loops, and memory/pointers.

Minimum task count:

- 10 tasks per mature category.
- 5 tasks per newly added category until the harness stabilizes.
- Every category has checked Habu reference solutions before live model runs.

Run matrix:

- Main evidence: k=5 per task/arm/model.
- Smoke evidence: k=2 for cheap iteration.
- At least two model families once drivers support them.
- V2 row identity is `(run_id, model_id, arm, task_id, trial_id)`.
- `trial_id` is required. `trial_pass = passed_trials / k`.
  `task_pass@k = any passed trial for that task/arm/model`. Repair rounds are
  not trials.
- Every row records model command, model label, date/run id, arm, category, task
  id, prompt hash, outcome, and generated-code runtime where available.
- Every row uses the scorecard field names from `LLM.md`:
  `first_pass_checker`, `first_pass_tests`, `tests_passed`,
  `repair_iterations`, `checker_iterations`, `diagnostic_count`,
  `diagnostic_token`, `diagnostic_span`, `diagnostic_expected`,
  `diagnostic_actual`, `diagnostic_code`, `diagnostic_repair_class`,
  `all_errors_stable`, `tokens_used`, `wall_ms`, `final_chars`, `trust_uses`,
  and `signature_weakened`.
- Candidate sources are stored per task and repair round so failures can be
  replayed without rerunning a model.
- Each live row points to an artifact directory containing the prompt, raw model
  response, extracted candidate, checker diagnostics, repair packet, test output,
  final bundle, and sha256 hashes.
- `bench/llm/models.tsv` or equivalent defines model label, command, args, output
  parser, token fields, timeout, and environment requirements. Reports stratify
  by model and do not pool model families without per-model tables.
- `runtime_ms` is warmed candidate execution over fixed inputs/repetitions. It
  is separate from model wall time, checker time, compile/build time, and
  feedback-loop latency.

Report requirements:

- Trial pass and task pass@k.
- First-try pass.
- Checker/repair-round distribution.
- Diagnostic completeness by field and repair class.
- Output-token median, mean, p90, max.
- Wall-time median, mean, p90, max.
- Runtime median and p90 for passing candidates.
- Trust-use and signature-weakening counts.
- Per-category tables.
- Raw-vs-stdlib and stdlib-vs-skeleton deltas.
- Structured-vs-raw-vs-blind diagnostic ablations for checked Habu tasks. Replay
  artifacts prove auditability; this ablation is what tests whether Habu's
  checker feedback actually helps LLM-generated code converge.
- Missing-token/runtime rows excluded only from the affected metric, not from
  reliability.
- Feedback-loop latency from native `bench/llm/perf.f`: checker, functional tests,
  metric validator, property-test smoke, microbench smoke, and `--full` rebuild
  plus AOT timings.
- Repair-packet effectiveness by `repair_class`: fixture coverage, success rate,
  repair rounds, and token deltas.
- Limitations section: nondeterminism, k/N confidence, token proxy limits,
  scaffold fairness, library comparability, task-selection bias, hardware/runtime
  environment, and deterministic-vs-live evidence boundaries.

Native validator requirements:

- Reject missing required categories.
- Reject mixed schema versions.
- Reject missing diagnostic-quality fields.
- Reject result/task drift.
- Accept exactly the configured k trials per `(model_id, arm, task_id)` and
  reject duplicate full row identities.
- Keep reference rows zero-trust unless the task explicitly covers audited
  boundaries.
- Reject date/run ids that do not parse through native date helpers.
- Reject missing candidate replay artifacts for live benchmark evidence.

Tracked dots:

- `caf-53349d53fddaaa2f` Define LLM benchmark schema v2.
- `caf-53e1e0552be5d91c` Define V2 matrix identity.
- `caf-0342cd385732563b` Define benchmark trial metrics.
- `caf-d0147cdd624136f8` Specify live replay artifacts.
- `caf-9a6964ec8a6c01de` Unify benchmark task manifests.
- `caf-8647df7823d6236d` Add stdlib benchmark task families.
- `caf-318396f468464887` Add array benchmark V2 tasks.
- `caf-e332223f5ddd39f9` Add string benchmark V2 tasks.
- `caf-a153463219dcb080` Add regex benchmark V2 tasks.
- `caf-e172702436c276ad` Add map benchmark V2 tasks.
- `caf-49542db8d7552b3a` Add file benchmark V2 tasks.
- `caf-bb37ad27763abec7` Add process benchmark V2 tasks.
- `caf-8521330d435326dd` Add time date benchmark V2 tasks.
- `caf-0d23e19c717bb81b` Add property build benchmark V2 tasks.
- `caf-1a508208521a82ed` Add diagnostic benchmark V2 tasks.
- `caf-91586f86fb3c66e3` Add AOT-safe benchmark V2 tasks.
- `caf-a88640cb0f473220` Add required checker benchmark categories.
- `caf-d7f03ace8147b8fb` Split benchmark task-family work.
- `caf-4f8283e4cb195398` Add stdlib and skeleton arms.
- `caf-d38354169bd4ca5a` Add dynamic-script and TypeScript baselines.
- `caf-8de22befc7a622c5` Support multi-model benchmark runs.
- `caf-f82efaed1ac9fb0a` Add benchmark model registry.
- `caf-a1d9a9539fa534fa` Measure generated code runtime.
- `caf-c097dda4f5febd7d` Define generated runtime protocol.
- `caf-1a93909abd2f6ec6` Report category and arm deltas.
- `caf-0704c29bd03061c0` Report LLM feedback latency.
- `caf-e59e3678678e3c59` Add benchmark limitations section.
- `caf-1040c3e49ba6bd97` Run expanded LLM benchmark.

## Phase 5 - Broader LLM Stdlib

Goal: make Habu useful for typical agent-authored scripts, not only arrays.

### Strings

Promote existing checked string helpers from `tools/string.f` to `lib/string.f`,
then add builders and split helpers.

Required public surface:

```forth
BYTE-COPY       ( ptr u8 ptr u8 n -- )
ASCII-LOWER     ( n -- n )
ASCII-UPPER     ( n -- n )
STR=            ( ptr u8 n ptr u8 n -- bool )
STR=CI          ( ptr u8 n ptr u8 n -- bool )
STARTS-WITH?    ( ptr u8 n ptr u8 n -- bool )
ENDS-WITH?      ( ptr u8 n ptr u8 n -- bool )
FIND-SUB        ( ptr u8 n ptr u8 n -- n )
CONTAINS?       ( ptr u8 n ptr u8 n -- bool )
INDEX-OF        ( ptr u8 n n -- n )
COUNT-CHAR      ( ptr u8 n n -- n )
LTRIM           ( ptr u8 n -- ptr u8 n )
RTRIM           ( ptr u8 n -- ptr u8 n )
TRIM            ( ptr u8 n -- ptr u8 n )
```

Add:

```forth
SB-RESET        ( -- )
SB-APPEND       ( ptr u8 n -- )
SB-APPEND-C     ( n -- )
SB$             ( -- ptr u8 n )
SPLIT-NEXT      ( ptr u8 n n n -- ptr u8 n n bool )
STR>NUMBER?     ( ptr u8 n -- n bool )
```

### Regex

Implement a bounded capture-free subset:

- Literals.
- Dot.
- `^` and `$`.
- Character classes and negated classes.
- Escaped metacharacters.
- `?`, `*`, `+`.

Excluded in v1:

- Captures.
- Backreferences.
- Lookaround.
- Alternation unless a bounded Thompson/NFA plan is written first.

Public surface:

```forth
RX-COMPILE   ( ptr u8 len ptr u8 len -- len )
RX-MATCH?    ( ptr u8 len ptr u8 len -- bool )
RX-FIND      ( ptr u8 len ptr u8 len -- off len bool )
RX-COUNT     ( ptr u8 len ptr u8 len -- count )
```

`RX-COMPILE` takes pattern bytes plus a caller-provided bytecode buffer and
capacity, then returns the compiled byte length. Match/find/count take input text
plus compiled bytecode pointer/length. Regex must fail closed on malformed
patterns or capacity overflow.

### Maps

Implement a fixed-capacity open-addressed string-key map.

Public surface:

```forth
MAP-INIT    ( ptr a count -- )
MAP-HAS?    ( ptr a count ptr u8 len -- bool )
MAP-GET     ( ptr a count ptr u8 len -- n bool )
MAP-SET     ( n ptr a count ptr u8 len -- )
MAP-COUNT@  ( ptr a -- count )
MAP-EACH    ( ptr a count [ ptr u8 len n -- ] -- )
```

The `ptr a` storage is caller-owned and sized by a `count` capacity. Key strings
use `ptr u8 len`. Internal slot layout stays private to `lib/map.f`.

Errors:

```forth
E-MAP-FULL
E-MAP-BAD-CAP
```

### Files

Promote `tools/fs.f` into `lib/fs.f`, then add bounded read/write helpers.

Public surface:

```forth
EXISTS?      ( ptr u8 n -- bool )
FILE?        ( ptr u8 n -- bool )
DIR?         ( ptr u8 n -- bool )
BASENAME     ( ptr u8 n -- ptr u8 n )
JOIN-PATH    ( ptr u8 n ptr u8 n ptr u8 -- n )
WALK-FILES   ( ptr u8 n [ ptr u8 n -- ] -- )
READ-ALL     ( ptr u8 n ptr u8 n -- n )
WRITE-ALL    ( ptr u8 n ptr u8 n -- )
APPEND-FILE  ( ptr u8 n ptr u8 n -- )
```

`WALK-FILES` is either a checked quotation combinator or one audited TRUST
boundary with tests proving callback invocation, recursion-buffer isolation, and
error behavior. Traversal is depth-first over regular files. Entries inside one
directory follow the platform directory-stream order; callers needing lexical
order must collect and sort separately. Recursive walks use per-depth path and
directory buffers so a child walk cannot corrupt the parent record. Callback
path pointers are transient and must be copied before storage. Its focused
fixture is `lib/fs-test.f`, wired into the native gate.

### Processes

Promote tested process/PTY helpers into `lib/process.f`.

Public surface:

```forth
RUN-RC       ( ptr u8 n -- n )
SPAWN-IO     ( ptr u8 n n n n -- n )
WAIT-RC      ( n -- n )
RUN-CAPTURE  ( ptr u8 n ptr u8 n ptr u8 n n -- n n n )
```

`RUN-RC` must be checker-modeled or implemented as an audited checked wrapper
around modeled primitives. Do not publish examples that rely on unchecked
`run-rc` if `spawn-io wait-rc` is the checked path.

Process contracts:

- Public wrappers accept counted paths/commands and own NUL termination before
  calling pathz primitives.
- Negative fd values mean inherit/default as documented; nonnegative fd values
  are passed through explicitly.
- Parent-only pipe/PTY fds are marked `FD_CLOEXEC` before spawning and closed in
  the parent on every success and failure path.
- `RUN-CAPTURE` arguments are command string, stdout buffer/capacity, stderr
  buffer/capacity, and timeout milliseconds. It returns stdout length, stderr
  length, and rc in that order. Output overflow throws `E-PROC-TRUNCATED`
  rather than silently truncating. Timeout reaps or terminates the child before
  throwing `E-PROC-TIMEOUT`.

### Time And Date

Promote checked time/date helpers into `lib/time.f` and `lib/date.f`.

Public surface:

```forth
EPOCH-SECONDS     ( -- n )
MONO-NS           ( -- n )
PARSE-YMD         ( ptr u8 n -- n n n bool )
FORMAT-YMD        ( n n n ptr u8 n -- n )
FORMAT-EPOCH-UTC  ( n ptr u8 n -- n )
```

`EPOCH-SECONDS` and `MONO-NS` wrap checker-modeled native primitives. Date
formatters use caller-provided buffers and throw named stdlib errors on
insufficient capacity or invalid ranges.

### Args, Tests, Properties, Builds

Promote checked helper APIs:

- `lib/argv.f`: generic argv parser and mock argv hooks.
- `lib/test.f`: `T=`, `TTRUE`, string assertions, error assertions, final report.
- `lib/property.f`: PRNG, seed/count handling, simple generators, shrink helper.
- `lib/build.f`: checked command steps, temp dirs, artifact paths, source
  validation, fail-closed status reporting.

These libraries must split checked reusable helpers from unchecked harness
boundaries. `evaluate`, source-string generation, raw argv/envp cells, and build
driver process exits stay in small named boundaries with TRUST/audit entries
when needed; pure helpers get public checked signatures.

Planned public effects are documented in `docs/stdlib.md`, including
`T= ( n n -- )`, `PROP-RUN ( n n [ -- ] -- )`, and
`BUILD-STEP ( ptr u8 n [ -- n ] -- )`. `lib/property.f` may expose an audited
`evaluate` boundary for generated checked source, but deterministic PRNG,
generator, and shrink words remain checked. `lib/build.f` owns Habu-side build
policy and artifact validation; shell wrappers only provide the boundary named
above.

Tracked dots:

- `caf-61df19e626202e0d` Promote string helpers to stdlib.
- `caf-621496778c458969` Add string split and builder helpers.
- `caf-25bb3fc08a52da9b` Specify checked regex subset.
- `caf-f4123f6ae266987a` Add regex parser scanner.
- `caf-0347899117bed799` Implement regex matcher core.
- `caf-a83149f222b1aac2` Expose regex find helpers.
- `caf-b530d2dcfb50767a` Specify stdlib handle representation.
- `caf-0a6ff9f39e4a2e49` Add fixed-cap map layout.
- `caf-735f45ee596f0512` Implement map get and set.
- `caf-fd1a85a426a2ef61` Add map iteration helpers.
- `caf-82a46b35b9564cbb` Promote filesystem helpers.
- `caf-a9ea2e9d4156889b` Specify file walking trust boundary.
- `caf-64e5c28b27b9eb63` Add checked file read write helpers.
- `caf-eac3764a23af6c5f` Promote process helpers.
- `caf-cda110101ecae382` Specify checked process API contracts.
- `caf-3be1f3c4fa8dc632` Add process capture API.
- `caf-6805905af9f91380` Implement capture pipe setup.
- `caf-ac0f5cf530e13b15` Implement capture drain loop.
- `caf-4d7ba155d5d94ecb` Add capture timeout handling.
- `caf-d296857ab4e4d367` Add capture overflow behavior.
- `caf-ef7b9f82ac094a86` Add process capture fixtures.
- `caf-7c20d9950db54298` Promote argv parser to stdlib.
- `caf-d4cbea9483117138` Add checked test helper library.
- `caf-ae58608572f6027a` Add checked build helper library.
- `caf-c2f1d2930cccf7cc` Extract property helper library.
- `caf-3453ea81d309ca46` Specify test property build APIs.
- `caf-feceb19bbdb145aa` Promote time date helpers to stdlib.
- `caf-cd8026d9eb7831ef` Add stdlib example scripts.

## Critical Path

Work in this order unless a dependency proves wrong:

1. Re-verify baseline contracts: `hb` entry modes, native-only build/gate, and
   property soundness smoke.
2. Specify the build shell/Habu boundary and the `lib/` TRUST/error/handle
   policies.
3. `docs/stdlib.md`, `docs/llm-scorecard.md`, `lib/` layout, and manifest.
4. `tools/bundle-lib.f`.
5. `lib/array.f` with promoted helpers, bounds errors, and hard-tail kernels.
6. Deterministic `habu-stdlib` benchmark arm.
7. Repair-packet schema, fixtures, and driver feedback.
8. Array combinators and property tests.
9. Benchmark v2 schema/report/validator with unified manifest, matrix identity,
   replay artifacts, model registry, and runtime protocol.
10. Broader stdlib categories.
11. Expanded live benchmark.

Do not start regex/maps/process capture before the array stdlib and bundle
mechanism exist. They need the same packaging and test conventions.

## Verification

Relevant focused checks before commits. Run the checks that match the changed
surface; new surfaces must add their fixture before their dot closes.

```sh
printf ': SQ ( i64 -- i64 ) dup * ; 7 SQ .\n' | bin/hb
printf ': PIPED ( -- n ) 7 ; PIPED .\n' | bin/hb ignored-script.f arg
bin/hb 2 10 < test/prop-test.f
bin/hb < test/prop-test.f
test "$(find bin -type f -perm -111 ! -name hb -print | wc -l)" -eq 0
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-common.f bench/llm/run-lib.f bench/llm/run.f
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/json-write.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/time.f lib/date.f bench/llm/perf-lib.f bench/llm/perf.f
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/source.f tools/argv.f tools/check.f -- lib/array.f
bin/hb --load lib/errors.f lib/array.f lib/array-test.f
bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f bench/llm/grade.f bench/llm/grade-test.f
bin/hb < bench/llm/ref-solutions.f
```

Full gate before merging implementation stacks:

```sh
bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/run.f
```

Live benchmark evidence after benchmark-surface changes:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/json-write.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/time.f lib/date.f bench/llm/perf-lib.f bench/llm/perf.f -- --json > bench/llm/results/perf.json
MODEL_ID=codex BENCH_FORTH_MODES=repair BENCH_TASK_IDS=1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,56,57,58,59,60,61,71,72,73,74,75,76,77 BENCH_PERF_JSON=bench/llm/results/perf.json bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/argv.f bench/llm/manifest.f bench/llm/run-expanded-bench.f -- 5 bench/llm/results/run-expanded.jsonl
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f lib/process.f lib/process-argv.f lib/process-env.f lib/argv.f tools/date.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/json.f tools/json-file.f bench/llm/validate-results-lib.f bench/llm/expanded-report.f -- bench/llm/results/run-expanded.jsonl bench/llm/results/perf.json > /tmp/RESULTS-expanded.md
cmp /tmp/RESULTS-expanded.md bench/llm/RESULTS-expanded.md
```

Cross-language array evidence for final comparison claims:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/json.f tools/json-file.f tools/argv.f bench/llm/report.f -- bench/llm/results/run-array-expanded.jsonl bench/llm/results/perf-array-expanded.json > /tmp/RESULTS-array-expanded.md
cmp /tmp/RESULTS-array-expanded.md bench/llm/RESULTS-array-expanded.md
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/date.f tools/lint/text.f tools/lint/token.f tools/lint/lib.f tools/json.f tools/json-file.f tools/argv.f bench/llm/validate-results-lib.f bench/llm/validate-results.f -- --json bench/llm/results/run-array-expanded.jsonl > /tmp/run-array-expanded-validate.json
```

Fresh live model runs are nondeterministic. Review `/tmp/RESULTS-expanded.md`
for the expected shape before replacing `bench/llm/RESULTS-expanded.md`. For
committed legacy JSONL evidence, report generation must be deterministic:

```sh
bin/hb --load lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/json.f tools/json-file.f tools/argv.f bench/llm/report.f -- bench/llm/results/run.jsonl > /tmp/RESULTS.md
cmp /tmp/RESULTS.md bench/llm/RESULTS.md
```

For final benchmark claims, run k=5 or higher and commit JSONL evidence in a
separate commit from harness/library changes.

## Completion Checklist

The plan is done only when all of these are true:

- `bin/hb` is the only public binary, and REPL/stdin/script invocation modes are
  covered by docs and tests, including pipe-with-args and empty-stdin script
  fallback.
- Daily build/test paths stay native-only; no gforth or hosted bootstrap path is
  reintroduced into the default workflow.
- The build shell/Habu boundary is documented, and build policy/step validation
  lives in Habu scripts/libraries.
- `test/prop-test.f` remains in the gate and keeps using `depth`, not a stack
  sentinel, as the runtime arity oracle.
- `lib/` exists and every public stdlib word has a checked effect or audited
  trust entry.
- `TRUST` lint covers `lib/`, stdlib errors are centrally named, and memory-backed
  handles use typed pointers rather than unchecked `addr` access.
- `docs/stdlib.md`, `docs/llm-scorecard.md`, and `LLM.md` document the public
  surface, scorecard fields, and agent protocol without drift.
- `tools/bundle-lib.f` lets `hb` scripts use selected libraries without adding
  another public binary.
- Array hard-tail kernels exist and are tested.
- Array combinators exist and are tested.
- Repair packets are consumed by Habu benchmark drivers.
- Benchmark v2 has required categories, a unified task manifest, matrix identity,
  replay artifacts, model registry, runtime protocol, and native schema
  validation.
- Time/date, file, process, map, regex, test, property, and build helper contracts
  are documented and tested.
- Expanded live benchmark evidence is committed.
- Success criteria in this file are met or the remaining misses have new dots
  with measured evidence.
