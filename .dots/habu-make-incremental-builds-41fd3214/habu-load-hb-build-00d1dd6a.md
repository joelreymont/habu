---
title: Load hb-build without recompiling it
status: open
priority: 3
issue-type: task
created-at: "2026-10-22T14:59:32.133121+03:00"
---

Problem: every hb-build invocation compiles tools/hb-build.f from source: 0.37 s, half of a 0.74 s no-change build and a fixed cost on every test that calls hb-build. Evidence: `hb-build --engine` builds timed with `/usr/bin/time -l`. Acceptance: hb-build runs from a cached captured image keyed on its sources and the engine, rebuilt only when they change; no-change build wall measured before/after; outputs unchanged. Files: tools/hb-build.f, tools/hb-build-lib.f, bin/hb launcher path. Verify: no-change and warm engine build timings, test/run.f rows that call hb-build. Depends: none. Needs a Plan design first. Claim: unassigned.

Measured 2026-10-31 at lane 11 (uyzslxrr), no-change warm engine build 46.0 s wall, dot habu-profile-a-warm-9f10659a, report carl-dots/.out/carl-file/s2prof-report.md: still exists and grew to 3.60 s per invocation (7.8% of a warm engine build): COLD-LOAD:RUN (tools/cold-load.f, called at tools/hb-build.f:10) 3.29 s, the hb-build.f requires ~0.31 s. `--engine` has no no-op path: HBB-BUILD-ENGINE tools/hb-build-lib.f:1084-1091 always spawns the child (comment 987-990).

Design (Opus Plan) at carl-dots/.out/carl-file/design-00d1dd6a.md: the engine records the files it was built from (path, SHA-256) and hb-build skips COLD-LOAD:RUN when the tree is the engine's root and every file hashes the same. Needs one new public engine word (SOURCE-PRODUCER:BUILT-FROM?, name Joel's); asked through the interview.

Joel (interview, 2026-10-31) rejected a separate hash-of-sources check: "why are we developing incremental building if we are gonna check hashes of source files?". The BUILT-FROM? design is dropped. Direction: hb-build's startup reload goes through the incremental per-file cache. First measure why reusing a file costs more than compiling it (profile: 156 reused files 6.41 s in the support child, against 3.29 s to T0-compile the whole startup set in process; not like-for-like, not yet measured side by side).
