---
title: Derive inprocess + spawned ptx-toolchain suite lists from one source
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T18:16:23.855603+02:00\""
---

Problem: the resident inprocess suite list GSI-LINT-LIBS-PTX-TOOL (test/gate-stdlib-inline-lib.f) and the spawned TEST:SUITE ptx-toolchain (test/gate-stdlib-cases.f) are hand-synced copies that already drifted once (kbench added 7 files to the spawned list only, so test/run.f skipped them). They are now DELIBERATELY different: the spawned list is a superset that includes the device/bench tools (bandwidth-lib-test, fusion-compare, gemm-bench) which SIGBUS when loaded into the resident full-runner image, so the inprocess list carries only the unit tests + perf-regress scan. Fix: derive both lists from one checked source-of-truth that tags each entry inprocess-safe vs spawn-only, OR add a lint that verifies the inprocess list is the spawned list minus the tagged spawn-only entries. Acceptance: a test proving the two lists cannot silently diverge. Files: test/gate-stdlib-inline-lib.f, test/gate-stdlib-cases.f, test/run-lib.f. Verify: the new lint/test + test/run.f. Depends: none. Ownership: gate suite membership. Claim: unassigned.

## Scope extension (gaps audit 2026-07-14)
The drift class is wider than ptx-toolchain: gate-stdlib-cases.f suites run ONLY
when their label appears in a SUITE-*-LABEL? slice list (test/gate-stdlib-lib.f)
AND that slice is invoked - and test/run.f schedules NO spawned slice (phases
4/19/20 absent from every TR-*-ORDER table; the slices are manual merge gates).
A cases-file suite whose label no slice selects is fully orphaned: this bit
internal-word-gate, underdepth-gate, immediate-model, top-row-hook (all four now
mirrored into the scheduled stdlib/tail-process forks) and still bites
build-fixpoint-fixtures (tools/build-fixpoint-test.f - multiple full engine
builds, too heavy for the run.f budget; needs a slow tier or a budgeted subset)
and the bench compile-checks (bandwidth-lib-test/fusion-compare/gemm-bench,
manual lint-libs slice only). Acceptance additions: (a) a completeness lint -
every TEST:SUITE label in gate-stdlib-cases.f is either selected by a slice
list AND documented as a manual gate, or mirrored in a scheduled GSI group;
red on any orphan; (b) a scheduled home (slow tier ok) for
build-fixpoint-fixtures and the bench compile-checks. Dead TRWS-STDLIB (the
never-called resident cases-includer) was deleted 2026-07-14.

Claim: agent=suitelint workspace=.jj-ws/fable-suitelint
