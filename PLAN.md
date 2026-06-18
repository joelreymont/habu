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

The committed four-arm LLM benchmark has 80 live rows:

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

Win conditions:

- Array hard-tail token ratios are below 3x the best mainstream baseline.
- Median repair rounds for `habu-stdlib` are 1.
- Mean output tokens for `habu-stdlib` are below 2x Rust on checked/system tasks
  and below 2x TypeScript/Python on script/glue tasks.
- Runtime of generated Habu code is faster than Python/TypeScript for equivalent
  kernels and competitive with Rust for small kernels where startup does not
  dominate.
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
- Standard-library words are not benchmark cheating. JS, Rust, Python, and
  TypeScript win partly because their libraries encode idioms. Habu needs
  equivalent checked idioms.
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

- `bin/hb` on a tty starts the checked REPL.
- `program | bin/hb` runs stdin as a batch/pipeline program and writes to
  stdout/stderr.
- `bin/hb script.f args...` runs `script.f` with the remaining args exposed to
  Habu.
- `bin/hb args... < script.f` keeps stdin as the program source and exposes args
  to the script, which is required for property/benchmark seeds.

Build scripts are ordinary Habu scripts loaded by these modes. Shell wrappers may
choose sources, make private temp dirs, and concatenate a bundle, but the build
logic itself belongs in checked Habu libraries and scripts.

### Native-Only Trust Root

Daily work uses `bin/hb`, `tools/build.sh`, and `( cd test && ./run.sh )`.
No-binary recovery uses `tools/seed.sh /path/to/hb-seed`, then immediately
rebuilds current source. A gforth or hosted bootstrap path must not become a
normal gate, benchmark dependency, or documented workflow.

Historical bootstrap sources may remain only as inert reference material. They
must not feed default lint, benchmark, build, or self-check paths. The native
fixpoint build and native gate are the active parity proof.

### Soundness Gate

The property harness in `test/prop-test.f` is a standing soundness gate, not an
optional experiment. It uses the standard `depth ( -- n )` primitive to measure
runtime stack arity without sentinel values, runs generated programs in-process
through `evaluate`, and fails only on certified-but-wrong behavior.

The stdlib/property work in this plan may extract reusable generators and test
helpers, but it must not weaken the existing checker-soundness gate.

### LLM-Facing Documentation

`LLM.md`, `docs/forth.md`, `docs/stdlib.md`, `STATUS.md`, `TRUSTED.md`, and
machine-readable signature manifests must agree. The prompt surface for agents
should come from the same checked public signatures that the gate validates, not
from hand-copied examples that can drift.

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

Other domain handles also stay inside today's type grammar. Regex and map
handles are `addr` in v1; docs may call them `rx` or `map` in prose, but checked
signatures use `addr` until dedicated concrete types exist.

## File Layout

Add a `lib/` tree for reusable checked Habu:

```text
lib/
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
  std.manifest

docs/
  stdlib.md
  llm-scorecard.md

tools/
  bundle-lib.sh
  repair-packet.f
```

Native scripts still load a single source file. `tools/bundle-lib.sh` owns
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

## Phase 1 - Checked Array Stdlib

Goal: make current array hard tails library composition.

Create `lib/array.f` and move the benchmark-private helper surface there. Effects
below use the v1 representation.

Indexed helpers:

```forth
A@             ( ptr a n -- a )       \ pointer, index
A!             ( a ptr a n -- )       \ value, pointer, index
A+!            ( n ptr i64 n -- )     \ delta, pointer, index
A-SWAP         ( ptr a n n -- )       \ pointer, first index, second index
A-CHECK-INDEX  ( n n -- )             \ length, index; throws on invalid index
A-CHECK-RANGE  ( n n n -- )           \ length, start, count; throws on invalid range
LAST-INDEX    ( n -- n )
MIRROR-INDEX  ( n n -- n )
EVEN?         ( n -- bool )
```

Whole-array scalar kernels:

```forth
A-SUM         ( ptr i64 n -- i64 )  \ empty => 0
A-MIN         ( ptr i64 n -- i64 )  \ length must be > 0
A-MAX         ( ptr i64 n -- i64 )  \ length must be > 0
A-COUNT-EVEN  ( ptr i64 n -- i64 )
A-ARGMAX      ( ptr i64 n -- i64 )  \ length > 0, ties choose smallest index
```

Whole-array mutating kernels:

```forth
A-REVERSE!     ( ptr a n -- )
A-PREFIX-SUM!  ( ptr i64 n -- )  \ empty is no-op
A-RUNMAX!      ( ptr i64 n -- )  \ empty is no-op
A-FILL!        ( a ptr a n -- )
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
- `bench/llm/bench-test.sh` covers the new arm without live model calls.
- `lib/array.f` public words certify through `tools/check.sh`.

Tracked dots:

- `caf-fa509d91be8db3c7` Define LLM stdlib surface.
- `caf-583555f1af00460c` Add stdlib bundle mechanism.
- `caf-8f6e22a01fe1c7f7` Promote array helpers to stdlib.
- `caf-eca720800d681475` Wire benchmark to stdlib arrays.

## Phase 2 - Checked Array Combinators

Goal: support new array tasks by composition, not one-off kernels.

Public combinators:

```forth
A-MAP!    ( ptr a n [ a -- a ] -- )
A-MAPI!   ( ptr a n [ n a -- a ] -- )
A-FOLD    ( ptr a n b [ b a -- b ] -- b )
A-FOLDI   ( ptr a n b [ b n a -- b ] -- b )
A-SCAN!   ( ptr i64 n i64 [ i64 i64 -- i64 ] -- )
A-SCAN1!  ( ptr i64 n [ i64 i64 -- i64 ] -- )
```

Semantics:

- `A-MAP!` replaces each element with the quotation result.
- `A-MAPI!` also passes the zero-based index.
- `A-FOLD` returns the final accumulator and does not mutate the array.
- `A-FOLDI` also passes the zero-based index.
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

`tools/check.sh --json-errors --all-errors` already exposes structured
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

Benchmark driver changes:

- `bench/llm/drive-habu.sh` uses JSON diagnostics and repair packets for all
  checker rejections.
- The driver feeds the candidate definition plus the repair packet, not raw
  wrapper stderr.
- Deterministic bench tests assert packet use without live model calls.

Tracked dots:

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
- `js`
- `rust`
- `python`
- `typescript`

Task categories:

- Arrays and mutation.
- Strings and parsing.
- Regex subset.
- Maps/counting/grouping.
- Files and paths.
- Processes and command capture.
- Property tests.
- Build scripts.
- Diagnostics/repair.
- AOT-safe programs.

Minimum task count:

- 10 tasks per mature category.
- 5 tasks per newly added category until the harness stabilizes.
- Every category has checked Habu reference solutions before live model runs.

Run matrix:

- Main evidence: k=5 per task/arm/model.
- Smoke evidence: k=2 for cheap iteration.
- At least two model families once drivers support them.
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
- Missing-token/runtime rows excluded only from the affected metric, not from
  reliability.
- Feedback-loop latency from `bench/llm/perf.sh`: checker, functional tests,
  metric validator, property-test smoke, microbench smoke, and `--full` rebuild
  plus AOT timings.

Native validator requirements:

- Reject missing required categories.
- Reject mixed schema versions.
- Reject missing diagnostic-quality fields.
- Reject result/task drift.
- Keep reference rows zero-trust unless the task explicitly covers audited
  boundaries.
- Reject date/run ids that do not parse through native date helpers.
- Reject missing candidate replay artifacts for live benchmark evidence.

Tracked dots:

- `caf-53349d53fddaaa2f` Define LLM benchmark schema v2.
- `caf-8647df7823d6236d` Add stdlib benchmark task families.
- `caf-4f8283e4cb195398` Add stdlib and skeleton arms.
- `caf-d38354169bd4ca5a` Add Python and TypeScript baselines.
- `caf-8de22befc7a622c5` Support multi-model benchmark runs.
- `caf-a1d9a9539fa534fa` Measure generated code runtime.
- `caf-1a93909abd2f6ec6` Report category and arm deltas.
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
RX-COMPILE   ( ptr u8 n -- addr )
RX-MATCH?    ( ptr u8 n addr -- bool )
RX-FIND      ( ptr u8 n addr -- n n bool )
RX-COUNT     ( ptr u8 n addr -- n )
```

Regex must fail closed on malformed patterns or capacity overflow.

### Maps

Implement a fixed-capacity open-addressed string-key map.

Public surface:

```forth
MAP-INIT    ( addr n -- )
MAP-HAS?    ( addr ptr u8 n -- bool )
MAP-GET     ( addr ptr u8 n -- n bool )
MAP-SET     ( n addr ptr u8 n -- )
MAP-COUNT   ( addr -- n )
MAP-EACH    ( addr [ ptr u8 n n -- ] -- )
```

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

### Args, Tests, Properties, Builds

Promote:

- `lib/argv.f`: generic argv parser and mock argv hooks.
- `lib/test.f`: `T=`, `TTRUE`, string assertions, error assertions, final report.
- `lib/property.f`: PRNG, seed/count handling, simple generators, shrink helper.
- `lib/build.f`: checked command steps, temp dirs, artifact paths, source
  validation, fail-closed status reporting.

Tracked dots:

- `caf-61df19e626202e0d` Promote string helpers to stdlib.
- `caf-621496778c458969` Add string split and builder helpers.
- `caf-25bb3fc08a52da9b` Specify checked regex subset.
- `caf-f4123f6ae266987a` Add regex parser scanner.
- `caf-0347899117bed799` Implement regex matcher core.
- `caf-a83149f222b1aac2` Expose regex find helpers.
- `caf-0a6ff9f39e4a2e49` Add fixed-cap map layout.
- `caf-735f45ee596f0512` Implement map get and set.
- `caf-fd1a85a426a2ef61` Add map iteration helpers.
- `caf-82a46b35b9564cbb` Promote filesystem helpers.
- `caf-64e5c28b27b9eb63` Add checked file read write helpers.
- `caf-eac3764a23af6c5f` Promote process helpers.
- `caf-3be1f3c4fa8dc632` Add process capture API.
- `caf-7c20d9950db54298` Promote argv parser to stdlib.
- `caf-d4cbea9483117138` Add checked test helper library.
- `caf-ae58608572f6027a` Add checked build helper library.
- `caf-c2f1d2930cccf7cc` Extract property helper library.
- `caf-cd8026d9eb7831ef` Add stdlib example scripts.

## Critical Path

Work in this order unless a dependency proves wrong:

1. Re-verify baseline contracts: `hb` entry modes, native-only build/gate, and
   property soundness smoke.
2. `docs/stdlib.md`, `docs/llm-scorecard.md`, `lib/` layout, and manifest.
3. `tools/bundle-lib.sh`.
4. `lib/array.f` with promoted helpers and hard-tail kernels.
5. Deterministic `habu-stdlib` benchmark arm.
6. Repair-packet schema and driver feedback.
7. Array combinators and property tests.
8. Benchmark v2 schema/report/validator.
9. Broader stdlib categories.
10. Expanded live benchmark.

Do not start regex/maps/process capture before the array stdlib and bundle
mechanism exist. They need the same packaging and test conventions.

## Verification

Relevant focused checks before commits. Run the checks that match the changed
surface; new surfaces must add their fixture before their dot closes.

```sh
printf ': SQ ( i64 -- i64 ) dup * ; 7 SQ .\n' | bin/hb
bin/hb < test/prop-test.f
./bench/llm/run.sh
./bench/llm/perf.sh
./tools/check.sh lib/array.f
./lib/array-test.sh
./bench/llm/bench-test.sh
./bench/llm/grade-test.sh
bin/hb < bench/llm/ref-solutions.f
```

Full gate before merging implementation stacks:

```sh
( cd test && ./run.sh )
```

Live benchmark evidence after benchmark-surface changes:

```sh
sh bench/llm/run-bench.sh 2
node bench/llm/report.js bench/llm/results/run.jsonl > /tmp/RESULTS.md
```

Fresh live model runs are nondeterministic. Review `/tmp/RESULTS.md` for the
expected shape before replacing `bench/llm/RESULTS.md`. For committed JSONL
evidence, report generation must be deterministic:

```sh
node bench/llm/report.js bench/llm/results/run.jsonl > /tmp/RESULTS.md
cmp /tmp/RESULTS.md bench/llm/RESULTS.md
```

For final benchmark claims, run k=5 or higher and commit JSONL evidence in a
separate commit from harness/library changes.

## Completion Checklist

The plan is done only when all of these are true:

- `bin/hb` is the only public binary, and REPL/stdin/script invocation modes are
  covered by docs and tests.
- Daily build/test paths stay native-only; no gforth or hosted bootstrap path is
  reintroduced into the default workflow.
- `test/prop-test.f` remains in the gate and keeps using `depth`, not a stack
  sentinel, as the runtime arity oracle.
- `lib/` exists and every public stdlib word has a checked effect or audited
  trust entry.
- `docs/stdlib.md`, `docs/llm-scorecard.md`, and `LLM.md` document the public
  surface, scorecard fields, and agent protocol without drift.
- `tools/bundle-lib.sh` lets `hb` scripts use selected libraries without adding
  another public binary.
- Array hard-tail kernels exist and are tested.
- Array combinators exist and are tested.
- Repair packets are consumed by Habu benchmark drivers.
- Benchmark v2 has required categories and validates its own schema natively.
- Expanded live benchmark evidence is committed.
- Success criteria in this file are met or the remaining misses have new dots
  with measured evidence.
