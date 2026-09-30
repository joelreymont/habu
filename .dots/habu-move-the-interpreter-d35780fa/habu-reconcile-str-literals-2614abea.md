---
title: "Reconcile string literals with allot's task guard"
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T13:18:38.591263+03:00"
---

Problem: the Habu interpret loop (I4b, `src/habu/outer.f` ROOM) reserves string-literal data through `allot`, whose task-live guard (BALLOT, `habu1.f:1719`; B-TASK-LIVE-GUARD `habu1.f:1300-1303`) exits 79 with no output while any task runs. The engine's interpret copies (C-ISDQ, C-ICQ, C-EISDQ, C-EICQ, C-EIDOTQ) advance DP with DP-CHECK only. Demonstrated by I4b's review: prelude starts a spinning task; `s" hi" type cr` prints `hi` rc 0 on the engine and exits 79 silently in the Habu loop. `docs/threads.md:40` promises a remote REPL evaluates while tasks run, so once evaluate moves to the Habu loop a typed string literal would end the process.
Acceptance: one rule for data-space reservation by interpret-time literals while tasks are live, applied to both routes (either the engine's copies take the guard, or the loop reserves through a row that makes the same data-space check the engine does); the rule stated in `docs/forth.md` or `docs/threads.md`; a route-equality case in `test/outer-interpret.f` with a live task.
Files: `src/habu/outer.f`, and `src/habu/habu2.f`/`src/habu/prims.f` if the rule changes the engine; `test/outer-interpret.f`; the doc.
Verify: spark: the case through both routes; rebuild, chain, gate if the engine changes.
Depends: habu-interpret-literal-keywords-0fa50d62 (I4b).
Ownership: krait (Intel lane).
Claim: unassigned.
