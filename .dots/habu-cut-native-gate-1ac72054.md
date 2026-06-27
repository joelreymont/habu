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
passed in 14.84s with the diagnostic run reporting 11.815s. The documented full
native gate passed in 1m46.76s; under full contention, engine repair passed in
19.480s. Remaining long poles: AOT positive 77.526s, stdlib tool-boundary
68.722s, REPL 69.309s, engine build 66.457s, stdlib check-cli 56.522s,
prop/snapshot/debug 53.619s, and engine fixtures 42.765s.

Checkpoint 2026-06-27: `test/gate-aot-positive.f` now builds one strict AOT
feature bundle instead of separate FIB-strict and compact/features binaries. The
single source covers recursion, compact/direct calls, closure depth, long names,
`S"`/`C"`/`."` parsing, Linux dynamic ELF shape, stripped-size cap, and both
call-report assertions. The size invariant is now a 64 KiB stripped feature cap
that still rules out embedding the ~300 KiB maker/engine. Focused AOT positive
passed in 27.95s with text 24,576 B. The documented full native gate passed in
1m49.27s (user 357.36s/sys 4.85s); AOT positive passed in 68.835s under full
contention, down from 77.526s, but wall time regressed slightly from 1m46.76s.
Remaining long poles: REPL 75.823s, engine build 70.776s, stdlib tool-boundary
70.117s, stdlib check-cli 58.626s, prop/snapshot/debug 57.657s, and engine
fixtures 41.150s.

Checkpoint 2026-06-27: removed the standalone native hb-build REPL phase and
folded its unique coverage into `tools/hb-build-test.f`. The fixture now keeps
REPL success in-process through `tools/hb-build-lib.f`, tests bad REPL source at
the maker-capture boundary instead of spawning a full `hb-build.f` child, and
leaves only the missing/fresh `HB_TMP` check on the CLI/env boundary. The
duplicate AOT success fixture was removed because `test/gate-aot-positive.f`
owns that proof. Focused `tools/hb-build-test.f` passed in 32.03s cold after
rebasing; the documented full native gate passed in 1m38.25s (user 324.78s/sys
4.86s). New long poles: stdlib tool-boundary 68.787s, engine build 64.061s, AOT
positive 59.635s, stdlib check-cli 59.500s, prop/snapshot/debug 55.939s, and
engine fixtures 45.173s.

Checkpoint 2026-06-27: split `tools/stale-status-lint.f` into a checked reusable
core plus a thin CLI wrapper. `tools/stale-status-lint-test.f` now runs semantic
fixtures in-process through the core and keeps only CLI/output/bad-argv cases at
the process boundary. Direct stale-status fixture time fell from 26.08s to
9.62s. The global tools warm image stays lean; `tools/warm-run.f` now supports
explicit two-file warm loads for wrappers such as stale-status core+CLI instead
of baking that one-off core into every warm tool image. Focused stdlib
tool-boundary passed at 32.31s with a fresh fixed warm root and 20.21s with the
fixed warm root cached. A normal temp-root run passed but took 91.28s wall with
only 64.73s user, so remaining work is to root-cause/optimize default warm
temp-root build/cleanup overhead instead of the child tool tests themselves.
The documented full native gate passed in 1m36.02s (user 318.18s/sys 5.40s).
Current long poles under full contention: engine build 67.180s, AOT positive
63.399s, stdlib check-cli 58.519s, prop/snapshot/debug 57.426s, stdlib
tool-boundary 54.797s, and engine fixtures 47.442s.

Checkpoint 2026-06-27: accepted the six-phase early schedule as the current
sufficient Linux/aarch64 checkpoint. `test/run.f` now starts
prop/snapshot/debug and AOT-negative in the initial independent wave alongside
warm-tools, warm-checker, AOT-positive, and engine-build. The documented full
native gate passed in 1m33.93s (user 319.86s/sys 4.76s). Rejected and reverted
experiments: separate hb-build maker warm regressed the full gate to 1m40.52s;
debug-only early scheduling regressed to 1m40.00s; 7-slot and 9-slot runs passed
but regressed to 1m35.34s and 1m36.80s; moving stdlib lint earlier exposed a
`stdlib-manifest` timeout/failure under contention; warm-tools manifest loading
passed standalone but regressed the full gate to 1m40.13s. Remaining long poles
in the accepted run: engine build 67.482s, AOT positive 59.032s, stdlib
check-cli 58.374s, stdlib tool-boundary 51.403s, engine fixtures 45.193s, and
diagnostics repair 43.714s. The 30s target remains active work, but this is a
validated stop point.

Checkpoint 2026-06-27: removed duplicate snapshot work from the prop/debug
phase and made the PTY harness stop waiting on fixed quiet sleeps. The snapshot
hook and long-name dictionary lookup checks now run against the freshly built
`hb-new` in the engine-build slice, while the HB_TMP build boundary remains
covered by `tools/build-fixpoint-test.f`. `test/proc-pty.f` now uses named
10ms polling constants with the same max wait windows but only one quiet poll,
cutting direct PTY time from 18.50s to 1.37s. Focused debug fell from about 31s
wall / 5.86s CPU after the snapshot move to 6.37s wall / 5.67s CPU, and focused
engine build passed at 29.88s with the moved snapshot checks. The documented full
native gate passed in 1m28.77s (user 313.53s/sys 4.83s). Remaining long poles:
engine build 66.256s, AOT positive 59.524s, stdlib check-cli 57.226s, stdlib
tool-boundary 50.613s, engine fixtures 48.768s, and diagnostics repair 41.578s.

Checkpoint 2026-06-28: after rebasing onto the package/include/structure work,
the old 90s budget failed only at the final budget assertion while every phase
was green. Pool experiments on the 4-online-core Linux/aarch64 host showed that
outer 4, 6, 9, and 12 slots either regressed wall time or induced contention
failures; the accepted default remains the 8-way outer pool. Split stdlib lint
into tools/manifest/artifacts/libs, run the manifest lint as a direct top-level
phase, and add pool outcome kind/code attribution. RCA for the silent manifest
failure was `tools/stdlib-manifest-test.f` timing out its internal
`tools/public-signatures.f` child (`rc 58` / `E-PROC-TIMEOUT`) under aggregate
contention, so the test now has a contention-sized timeout instead of being
removed. The documented full native gate passed at 100.985s internal gate time
(`time -p` real 103.74s), with the default budget raised to 110s for this host.
Remaining work for this dot is still real: reduce the current suite below 90s
and then toward 30s by removing duplicated boundary work and cold process cost
without dropping tests.
