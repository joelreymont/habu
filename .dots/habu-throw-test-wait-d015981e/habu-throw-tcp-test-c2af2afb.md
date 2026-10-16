---
title: Throw TCP test wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.630629+03:00"
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/net/tcp4-test.f`:

- `282–288`, `394–400`: migrate count and writer-end loops; update callers at `554`, `560`, `796`, `801`, `820`, `910`, `928`, `934`, `938`.
- `483–493`: throw only on `COUNTS?` expiry; retain false for a negative count and its assertions at `859`, `979`, `984`.
- `436–447`, callers `666,670`: classify exhausted readiness retries; distinguish a real `failed` result from `idle` instead of repeatedly passing failures through `READY?`.
- Positive timed consumers at `456,691,719,735`: name and throw `idle`; retain real poll-failure checks.
- `PARK-ANSWER` / `PARK-WORK`, `292–304`, and `T-PARKED-THREADS:548–565`: record idle as a timeout result and report it after joining, before `PARK-READY` is asserted.
- `944–967`: replace `FAILED-SLICE-ERRNO`’s exhausted-wait zero with a named timeout; retain errno assertions.

**Comments**

Correct `483–484`, `790`, `919`, and `944–945`: expiry is no failed count, failed halt assertion, or errno zero. Preserve completed-duration assertions and the full-buffer work-count test.

**Proof**

Run `tcp4` (`gate-stdlib-cases.f:2540–2542`). Copies force: pure count/end waits; nonnegative but unmatched queue counts; idle positive timed readiness; exhausted readiness turns; worker idle; and no reset observed across the existing slice retries. Require `P(tcp4-test <wait>)`.

Also retain a negative queue-count/poll-error copy as an ordinary failure, and leave intentional idle cases at `707,730` green.

**Acceptance**

Timeout, readiness, and actual I/O failure stay distinct. No bound changes.

Files: src/lib/net/tcp4-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
