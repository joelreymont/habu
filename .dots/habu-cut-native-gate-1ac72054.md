---
title: Cut native gate to 30s architecture
status: active
priority: 1
issue-type: task
created-at: "\"2026-06-27T00:05:16.000712+02:00\""
---

Root cause after commit 779b32f5: full gate still passes at 2m09.41s because heavyweight phases still cold-spawn many hb processes and rebuild/verify whole programs at boundary granularity. Target: redesign the gate so the frequent full check is around 30s by keeping coverage but eliminating per-case process builds where not required, batching boundary checks inside one hb when possible, using warm images/shared artifacts for heavy tools, and measuring each change with focused timings plus documented full-gate evidence. Commit and push each significant verified batch.

Checkpoint 2026-06-27: moved build-fixpoint source-shape assertions into the
engine build slice so stdlib tail no longer runs a duplicate full
build-fixpoint fixture. Focused engine build passed in 28.64s, focused stdlib
tail passed in 32.22s, and the documented full native gate passed in 2m07.75s.
Remaining long poles: AOT negative/positive, engine repair, tool-boundary, and
engine build/REPL contention.

Checkpoint 2026-06-27: moved positive AOT hb-build cases from spawned CLI
children to checked in-process `tools/hb-build-lib.f` helpers while keeping
negative and REPL boundary phases spawned. `test/gate-build-common.f` now models
the executable read buffer as runtime `lib/memory.f` storage instead of a large
static `allot`, so the shared common helper can compose with `hb-build-lib` in
one checked process. Focused AOT positive passed in 35.28s, AOT negative in
50.56s, REPL in 26.52s, `tools/hb-build-test.f` in 38.32s, and the documented
full native gate passed in 2m06.99s. Remaining long pole is still gate DAG
contention: stdlib tool-boundary, engine repair, AOT positive/negative, engine
build, and REPL overlap heavily.

Checkpoint 2026-06-27: cut AOT negative from six `hb-build` CLI failures to the
one hb-build-only closure-limit failure. Existing gate coverage already checks
strict signatures, bad checked effects, malformed definitions, REPL bad effects,
and AOT-unsafe token JSON through `tools/check-test.f`,
`tools/aot-lint-test.f`, `tools/hb-build-test.f`, and diagnostics slices.
Focused AOT negative passed in 19.59s, `tools/aot-lint-test.f` passed in 3.14s,
and the documented full native gate passed in 1m51.62s after rebasing onto the
FFI library change. Remaining long poles:
AOT positive, engine repair/build, stdlib tool-boundary, and REPL builder work.

Checkpoint 2026-06-27: split `tools/gate-json-assert.f` into a checked core plus
thin CLI entry so repair-hints tests can assert JSON schema/class/return-stack
in-process instead of spawning `gate-json-assert` for every rejected fixture.
Positive AOT coverage now uses two builds: the strict FIB binary still owns the
stripped-text and dynamic-ELF proof, while one bundled binary covers compact
direct calls, closure depth, long names, and parsing words. Focused
`check-repair-hints-test.f` passed in 28.44s, engine repair slice in 30.57s,
AOT-positive in 28.13s, stdlib tool-boundary in 46.02s, and the documented full
native gate passed in 1m43.35s after rebasing onto the `lib/fmt.f` change.
Remaining long poles: AOT positive under
contention, stdlib tool-boundary, engine build, and REPL builder work.

Checkpoint 2026-06-27: fixed the gate DAG so independent builder long poles
start at time zero instead of waiting for both warm images. `test/run.f` now
sets warm-tools env only for phases that actually use the tools image, starts
AOT-positive, engine build, and REPL alongside warm-image baking, then launches
warm-dependent phases as soon as both warm images pass. After rebasing onto
`df46d77b`, the documented full native gate passed in 1m34.87s. Remaining long poles: stdlib tool-boundary,
check-cli/debug/diagnostic slices after warm-up, and residual AOT-negative
contention.

Checkpoint 2026-06-27: measured pool contention on this 4-thread Orin target.
With the four-slot stdlib nested pool, full-gate wall times were: outer 12
default 1m33.44s, outer 10 1m34.62s, outer 8 1m31.92s, and outer 4 1m49.08s.
Set the checked gate pool default to 8 while preserving the 12-slot max for
explicit overrides, and raised stdlib nested slices from 2 to 4. The documented
default gate command passed in 1m29.40s. Remaining long poles: stdlib
tool-boundary, prop/snapshot/debug, and check-cli under full load.

Checkpoint 2026-06-27: `tools/warm-image-lib.f` was still spawning
`tools/public-signatures.f --trust` once per support file even though
`public-signatures` accepts a list of files. Batched trust export into one child
per warm image and raised the named runtime capture cap to model the measured
batched artifacts (77,191 bytes for tools warm, 69,126 for checker warm). Focused
warm-image tests passed; stdlib warm fell to 9.96s and checker warm to 9.14s in
isolation. The documented full native gate passed in 1m21.93s. Remaining long
poles: AOT-positive/build/REPL builder phases and stdlib tool-boundary under full
load.

Checkpoint 2026-06-27: `tools/check-repair-hints-test.f` no longer spawns
`tools/check-all-errors.f` once per rejecting fixture. The test writes one
batched source, runs one all-errors check, then uses new checked JSONL helpers in
`tools/gate-json-assert-core.f` to assert each row by `word` plus
`repair_class`, return-stack details, and row-effect details. Focused
`check-repair-hints-test.f` passed in 11.88s, and the owning engine repair slice
passed in 14.84s with the diagnostic run reporting 11.815s. Full-gate evidence
still pending for this checkpoint.
