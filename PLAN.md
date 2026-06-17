# habu plan - LLM-native Checked Forth

This is the active top-level plan. The old checker/self-host build plan is no
longer the driving plan: `bin/hb` self-hosts, rebuilds to a fixpoint, and the
native gate is the source of truth. Historical type-system and checker details
belong in focused docs near the implementation. Current work is to make Habu a
strong language for LLM-generated programs without weakening checked Forth.

Detailed benchmark evidence and reproduction commands live in `PLAN-AI.md`.
Forth coding standards live in `docs/forth.md`.

## Current diagnosis

The four-arm LLM benchmark has 80 live rows:

| arm | trial pass | task pass@k | mean output tokens | max output tokens |
|---|---:|---:|---:|---:|
| Habu raw | 90% | 100% | 753 | 4494 |
| Habu + array helpers | 95% | 100% | 630 | 3483 |
| JavaScript | 85% | 100% | 100 | 314 |
| Rust | 95% | 100% | 78 | 200 |

Habu is viable: every task has a green Habu solution under k=2. The gap is
effort on hard tails:

- Index tracking: `ARGMAX`.
- In-place rearrangement: `REVERSE`.
- Carried state over arrays: `PREFIXSUM`, `RUNMAX`.
- Predicate/count loops: `COUNT-EVEN`.

The first helper arm proved pointer arithmetic is only part of the problem.
`A@`, `A!`, `A-SWAP`, and related helpers made `REVERSE` much easier, but did
not remove the state/loop invariant burden. The remaining problem is that the
model still has to synthesize loop skeletons and accumulator invariants by hand.

## Objective

Make Habu an LLM-native checked language by moving common hard-tail invariants
into checked Habu libraries and benchmarkable generation surfaces.

The model should mostly compose certified words. It should not routinely invent
raw pointer loops, stack juggling, or hidden accumulator protocols for common
tasks.

## Design principles

- Checked code first. New public library words are checked typed Habu unless the
  checker cannot express the boundary.
- Trust is an audit boundary, not a convenience. Any `TRUST` row needs a reason,
  tests, and lint visibility.
- Standard-library words are not benchmark cheating. JS and Rust win partly
  because their stdlibs encode idioms. Habu needs equivalent checked idioms.
- Keep raw arms. Raw Habu remains a benchmark arm so we can measure language
  improvements instead of hiding regressions.
- Measure every claim. Each ergonomics improvement needs benchmark deltas, not
  anecdotes.
- Prefer data structures and combinators that improve generated programs and
  human-written Habu at the same time.

## Target user surface

Hard-tail examples should become direct compositions:

```forth
: REVERSE ( slice i64 -- )
  A-REVERSE! ;

: PREFIXSUM ( slice i64 -- )
  0 [: + ;] A-SCAN! ;

: RUNMAX ( slice i64 -- )
  [: max ;] A-SCAN1! ;

: ARGMAX ( slice i64 -- i64 )
  A-ARGMAX ;
```

If the engine does not yet have a native `slice a` type, the first stdlib can
still use the current `( ptr a n )` representation internally. The public plan
is to converge on a `slice a` surface so LLM prompts and examples describe one
array value instead of a pointer/length pair.

## Phase 1 - checked array stdlib

Goal: make current benchmark hard tails simple library composition.

Implement checked words for:

- Scalar folds: `A-SUM`, `A-MIN`, `A-MAX`, `A-COUNT-EVEN`, `A-ARGMAX`.
- In-place transforms: `A-REVERSE!`, `A-PREFIX-SUM!`, `A-RUNMAX!`.
- Existing primitives promoted from the benchmark helper file: `A@`, `A!`,
  `A+!`, `A-SWAP`, `LAST-INDEX`, `MIRROR-INDEX`, `EVEN?`.

Requirements:

- Each word has a typed effect and focused tests.
- No raw benchmark-only helper remains outside the stdlib if it is generally
  useful.
- The benchmark gains a `habu-stdlib` arm distinct from `habu-a` and
  `habu-lib`.
- Success: `ARGMAX`, `REVERSE`, `PREFIXSUM`, and `RUNMAX` hard-tail ratios drop
  below 3x mainstream best in the stdlib arm.

Tracked dots:

- `caf-fa509d91be8db3c7` Define LLM stdlib surface.
- `caf-583555f1af00460c` Add stdlib bundle mechanism.
- `caf-8f6e22a01fe1c7f7` Promote array helpers to stdlib.
- `caf-eca720800d681475` Wire benchmark to stdlib arrays.

## Phase 2 - checked array combinators

Goal: support new tasks by composition, not one-off kernels.

Implement general checked combinators:

- `A-MAP!`
- `A-MAPI!`
- `A-FOLD`
- `A-FOLDI`
- `A-SCAN!`
- `A-SCAN1!`

The important checker requirement is quotation effects with row-polymorphic
carried state. If a combinator needs an audited `TRUST` signature first, keep it
small, documented, and covered by property tests.

Requirements:

- Combinator examples compile and run through `bin/hb`.
- Property tests compare combinator behavior against reference arrays.
- Diagnostics for quotation mismatch are clear enough for repair prompts.

Tracked dots:

- `caf-b0c0e08bc3723750` Expand checked array combinators.
- `caf-fb4983450627a58b` Document repair diagnostic schema.
- `caf-8d5f0a90a453fbbb` Add repair-class hint text.

## Phase 3 - benchmark the real target

Goal: evaluate Habu as an LLM language against realistic libraries and tasks.

Extend the benchmark along two axes:

- Arms: raw Habu, helper Habu, stdlib Habu, skeleton Habu, JS, Rust, Python,
  TypeScript.
- Categories: arrays, strings, maps, files, processes, property-test helpers,
  build scripts, and small end-to-end scripts.

The benchmark report must keep separate:

- Trial pass.
- Task pass@k.
- Repair rounds.
- Output-token effort.
- Wall time.
- Generated-code runtime.
- Category deltas and per-arm deltas.

Tracked dots:

- `caf-53349d53fddaaa2f` Define LLM benchmark schema v2.
- `caf-8647df7823d6236d` Add stdlib benchmark task families.
- `caf-4f8283e4cb195398` Add stdlib and skeleton arms.
- `caf-d38354169bd4ca5a` Add Python and TypeScript baselines.
- `caf-8de22befc7a622c5` Support multi-model benchmark runs.
- `caf-a1d9a9539fa534fa` Measure generated code runtime.
- `caf-1a93909abd2f6ec6` Report category and arm deltas.
- `caf-1040c3e49ba6bd97` Run expanded LLM benchmark.

## Phase 4 - broader LLM stdlib

Goal: make Habu useful for typical agent-authored scripts, not only array
benchmarks.

Promote checked helpers for:

- Strings: split, trim, contains, builders.
- Regex: checked subset parser, scanner, matcher, find helpers.
- Maps: fixed-cap map layout, get/set, iteration.
- Files: read/write helpers and path-safe wrappers.
- Processes: spawn/capture APIs.
- Args: small checked argv parser.
- Tests/builds: helpers for native property tests and build scripts.

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
- `caf-ae58608572f6027a58b` Add checked build helper library.
- `caf-c2f1d2930cccf7cc` Extract property helper library.
- `caf-cd8026d9eb7831ef` Add stdlib example scripts.

## Phase 5 - repair packets

Goal: turn checker failures into concise, actionable LLM repair input.

The driver should feed structured packets instead of raw stderr. Packets should
include:

- Schema version.
- Word and token.
- Declared effect.
- Expected and actual stack.
- Repair class.
- Short human hint.
- Minimal source excerpt.

Tracked dots:

- `caf-be7e76a1a9232ab0` Add LLM repair packet tool.
- `caf-fcf16bb7fe724292` Use repair packets in Habu driver.

## Success criteria

Habu becomes a strong LLM target when:

- `habu-stdlib` reaches task pass@k >= JS/Rust on the expanded benchmark.
- `habu-stdlib` trial pass is within 5 percentage points of the best mainstream
  baseline.
- Hard-tail output-token ratios are below 3x mainstream best for array tasks.
- Median repair rounds are 1 for stdlib Habu.
- Generated-code runtime is competitive for the same algorithmic task.
- All public stdlib words are checked or explicitly audited with `TRUST`.
- The full native gate passes.

## Verification commands

Use these before committing relevant changes:

```sh
./bench/llm/bench-test.sh
./bench/llm/grade-test.sh
bin/hb < bench/llm/ref-solutions.f
( cd test && ./run.sh )
```

Use this for live evidence when benchmark surfaces change:

```sh
sh bench/llm/run-bench.sh 2
node bench/llm/report.js bench/llm/results/run.jsonl > /tmp/RESULTS.md
```

Commit benchmark evidence separately from harness or stdlib code.

## Non-goals for this plan

- Portability work. That remains explicitly out of scope for this phase.
- Weakening the checker to make examples pass.
- Hiding raw Habu results. Raw Habu remains the control arm.
- Treating prompt wording as the primary fix. Prompts can improve, but the
  durable fix is checked library structure plus better diagnostics.
