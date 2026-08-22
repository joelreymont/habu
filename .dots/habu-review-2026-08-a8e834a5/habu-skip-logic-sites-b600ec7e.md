---
title: skip-logic sites in the gate
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.028311+02:00"
---

Problem: AGENTS.md forbids skip logic; measured sites: test/gate-engine-lib.f:688-692 GE-REGION-RATCHET exits 'page prediction only' whenever HOST-REGION-BUDGETS-MEASURED? is false - always on macOS (gate-size-attribution-test.f:1290-1294 only Linux has rows) and now always on Linux (LINUX-MEASURED? false), so the per-region __text ratchet enforces nothing on any target; gate-size-attribution-test.f:1324-1329 LINUX-SELF-CHECK exits when unmeasured; test/gate-aot-positive-lib.f:63-64 ASSERT-DYNAMIC-ELF skipped off Linux; test/aot-data-span-forge.f:405-406 and test/gate-env-stdin-tty-test.f:143-146 print 'skipped' off Linux and have never run outside DGX Spark; tools/judge/ref-test.f:261-270 and test/run.f:17-18 treat a missing C toolchain as a result. Acceptance: each site either runs on every supported target or fails closed with a named reason counted by the runner as NOT-RUN (never as pass); the region ratchet enforces once the Linux rows land. Files: as listed. Verify: the runner's report shows zero silent skips. Depends: the Linux rows dot. Ownership: gate runner. Claim: unassigned.
