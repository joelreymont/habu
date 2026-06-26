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
