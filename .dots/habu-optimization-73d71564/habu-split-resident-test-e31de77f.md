---
title: Split resident test support load
status: closed
priority: 2
issue-type: task
created-at: "2026-07-01T10:45:01.230279+02:00"
---

RCA 2026-07-01: default suite no longer uses top warm snapshots; full default is green but wall stays ~31-32s because test/run.f spends ~18s loading test/run-resident.f -> test/gate-runner-support.f serially inside the suite timer. Minimal common support loads in ~2.5s; gate-runner-support.f loads in ~16s. Correct fix: parent runner loads only pool/stats/common scheduling; each resident fork loads its phase-owned support/entry (stdlib, dictionary, diagnostics, engine, AOT, debug) after fork so support compilation is not one serial parent bottleneck. Acceptance: no warm snapshot/cache runner, default full suite <30000ms shell wall on macos-arm64-12x2, focused phase entries still pass, explicit tail-warm remains separate feature target.

Closed 2026-07-01: implemented tiny `test/run-resident.f` scheduler, phase-owned `test/run-worker-*.f` dispatch files, and explicit parent `test/run-shared-stdlib.f` setup. Pure phase-owned stdlib workers first exposed missing dependency order (`tools/json.f`, `lib/source.f`, AOT lint cores), then showed duplicate stdlib tool-base compiles under contention. Final architecture keeps scheduler/common small, loads common stdlib tool setup once as suite setup, and forks phase workers copy-on-write. Proof: parent load `bin/hb --load test/run-support.f test/run-lib.f test/run-resident.f` is 2.36s; shared stdlib setup load is 9.69s; final full `macos-arm64-12x2` hot direct suite passed at 26.311s internal / 28.66s shell wall with `top-phase=0`, `runner-build=0`, `warm-build=0`, `warm-sig=0`, `warm-snap=0`, `inner-hb=1`, `inner-hb-stdin=4`, `boundary=5`, `helper-spawn=30`.
