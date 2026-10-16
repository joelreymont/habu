---
title: Share the fixed task join waits
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.905189+03:00"
---

Problem: src/lib/serial-test.f JOIN-READER (:111-119) and src/lib/signal-test.f JOIN-TASK (:329-337) are identical apart from the task name: each polls a worker task under a deadline, pauses, and throws a named E-PROC-TIMEOUT. The parent's shared `TEST-WAIT:POLL` and `TEST-WAIT:EXPIRED` now express that loop.

**Files and edits**

- `src/lib/serial-test.f:111–119` and `src/lib/signal-test.f:329–337`: replace each local join loop with `TEST-WAIT:POLL` over the task's done predicate, then the existing join. Keep each wait's name in its diagnostic.
- Preserve the effective cutoff: `PROC-LEFT-MS` floors milliseconds, so the nanosecond comparison must not add the final fractional millisecond (`src/lib/process.f:302–314`). Compute the absolute deadline from the same budget the loop uses today.

**Proof**

Run the `serial` and `signal` rows that load these files (find them in `test/gate-stdlib-cases.f`). In a copy of each, keep the worker from finishing; require `P(<file> JOIN-READER)` / `P(<file> JOIN-TASK)`.

**Acceptance**

Both joins use the shared wait; diagnostics and cutoffs are unchanged; no local loop duplicates it.

Files: src/lib/serial-test.f, src/lib/signal-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
