---
title: Throw process-task wait deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.665557+03:00"
closed-at: "2026-10-16T23:08:53.493652+03:00"
close-reason: "Landed: process-task-test's own waits (the captor wait, the command task wait) name themselves and throw E-PROC-TIMEOUT; PTT-JOIN-PAIR joins both workers, asserts only codes that are neither 0 nor E-PROC-TIMEOUT, answers true when both answered 0 and names and throws the deadline only when neither failed; PTT-GROW-CAPTURES reads nothing the workers left after a failed join. Proofs: row process-tasks rc 0; forced captor and command copies rc 67 with one named line each; b-dies and grow-b-dies copies rc 1 with -2501 only; the pre-fix grow copy rc 67 with an uncaught 7122. Astra review, two Mediums (a consequential peer deadline asserted; the grow case read unallocated buffers), fixed; Astra follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/process-task-test.f`:

- `76–113`: handle a false `PTT-POLL-UNTIL-DONE` before entering `PTT-JOIN-CAPTOR`; close the polling pipe and report expiry.
- Remove the redundant unbounded pre-join spin at `92` once only completed captors reach that join.
- `PTT-WORKER-ERR:87–89`, joins at `91–97,187–191`: main names and throws a worker timeout before any result assertion.
- `PTT-WAIT:138–143`: retain worker-side recording through `E-PROC-TIMEOUT`; its join supplies the diagnostic.
- `PTT-JOIN-CAPTOR` and `PTT-JOIN-ZERO` merge into `PTT-JOIN=`, moved above its first use: the captor joins with `PTT-CAPTOR PTT-CAPTURES PTT-JOIN=`, T0–T3 with `0 PTT-JOIN=`.
- `PTT-WAITER-BODY`: `PROC-OUTCOME>RC`, not `PROC-OUTCOME>DEADLINE-RC`. The latter turned the worker's 5 s deadline into rc 124, which main's `22 T=` reported as a FAIL; `PROC-OUTCOME>RC` throws `E-PROC-TIMEOUT`, which main names.
- `PTT-A`/`PTT-B` and `PTT-GROW-TA`/`PTT-GROW-TB` wait on each other: join each pair through `PTT-JOIN-CODE`, `PTT-FAILED?` and `PTT-JOIN-PAIR`, which join both, let a real error win, and name the deadline only when neither failed (the parent's peer-join rule).

**Reason**

**Inferred failure:** the census’s suggested throw after the join can still hang, because the join currently spins without a bound after the polling budget has already ended (`78–84,91–107`).

**Comments**

Replace `76–77`’s failed-case promise; describe bounded polling followed by joining only a completed captor. Update worker comments to identify main as the reporting boundary.

**Proof**

Run `process-tasks` (`gate-stdlib-cases.f:2566–2568`). One copy leaves the captor paused forever and expires the main budget: `P(process-task-test captor wait)`. Another expires a worker handshake before its peer’s budget: `P(process-task-test command task wait)` through `PTT-JOIN-PAIR`. A sleeper that outlives `PTT-RUN-MS` gives `P(process-task-test command task wait)` through `PTT-JOIN=`. A pair worker that throws a real error fails the row with that code, and its peer's deadline names nothing.

**Acceptance**

An expired main budget cannot enter the unbounded join. Capture/count mismatches remain genuine assertions.

Files: src/lib/process-task-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d6.
