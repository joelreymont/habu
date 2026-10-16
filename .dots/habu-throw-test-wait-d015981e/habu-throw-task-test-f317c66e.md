---
title: Throw task-test stop wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.647989+03:00"
blocks:
  - habu-share-a-named-85cc93f5
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/task-test.f:1181–1189`: replace `STOP-REACHED?` with void `STOP-REACH` using `UNTIL`, preserving equality rather than changing it to `>=`. Update `1238`, `1255`, and `1681`.

**Comments**

Replace the “FAIL” and “failed case” claims at `1152,1181–1182` with named deadline behavior.

**Proof**

Run `tasking-threads`: `src/lib/task-test.f` (`gate-stdlib-cases.f:1515–1517`). Suppress the copied early worker’s `STOP-EARLY-DONE` publication at `1230`; require `P(task-test STOP-REACH)`.

**Acceptance**

The three deadline assertions disappear. Actual wake, cleanup, task-error, and elapsed-time assertions remain.

Files: src/lib/task-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
