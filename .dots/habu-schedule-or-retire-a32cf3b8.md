---
title: Schedule or retire the tail-process ratchet runner
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T18:50:31.295997+02:00"
---

Found by the schedule-lint disk audit (habu-make-schedule-lint-d9ca528d): test/gate-tail-process.f is on disk and nothing spawns it - no GSI list, no SUITE row, no require, no string literal in any reached source. It is NOT dead weight: line 41 is the ONLY place in the tree that enforces TAIL-BUDGET:PROCESS-MS (grep confirms; test/gate-engine-lib.f:2226 only mentions it in prose). Phase 35's tail-process group is served by test/run-worker-stdlib.f:43 and the standalone route is gate-runner-lib.f's 'tail-process' ARG0 dispatch, so this file's RUN duplicates a body that already has two callers - but neither caller checks elapsed against the budget. So the ratchet is unenforced today. Decide: give the budget check a runner (register it, or move the elapsed-vs-budget assert into the phase-35 body), or delete the file and the ratchet with it. Registering as-is makes a gate phase time-sensitive - see habu-cad-replay-test-8be2ba00, where a load-sensitive suite red 4/6 under concurrency on master too. Carries a schedule-lint: allow-unscheduled pragma naming this dot until then. Files: test/gate-tail-process.f, test/tail-ratchet.f, test/run-worker-stdlib.f. Depends: none.
