---
title: Throw process-task wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.665557+03:00"
blocks:
  - habu-share-a-named-85cc93f5
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/process-task-test.f`:

- `76–113`: handle a false `PTT-POLL-UNTIL-DONE` before entering `PTT-JOIN-CAPTOR`; close the polling pipe and report expiry.
- Remove the redundant unbounded pre-join spin at `92` once only completed captors reach that join.
- `PTT-WORKER-ERR:87–89`, joins at `91–97,187–191`: main names and throws a worker timeout before any result assertion.
- `PTT-WAIT:138–143`: retain worker-side recording through `E-PROC-TIMEOUT`; its join supplies the diagnostic.

**Reason**

**Inferred failure:** the census’s suggested throw after the join can still hang, because the join currently spins without a bound after the polling budget has already ended (`78–84,91–107`).

**Comments**

Replace `76–77`’s failed-case promise; describe bounded polling followed by joining only a completed captor. Update worker comments to identify main as the reporting boundary.

**Proof**

Run `process-tasks` (`gate-stdlib-cases.f:2566–2568`). One copy leaves the captor paused forever and expires the main budget: `P(process-task-test captor wait)`. Another expires a worker handshake before its peer’s budget: `P(process-task-test command task wait)` through `PTT-JOIN-ZERO`.

**Acceptance**

An expired main budget cannot enter the unbounded join. Capture/count mismatches remain genuine assertions.

Files: src/lib/process-task-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
