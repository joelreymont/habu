---
title: Throw WebSocket stall wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.758380+03:00"
blocks:
  - habu-throw-websocket-lifecycle-2c12bc32
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/net/ws-test.f`:

- `HEARD-CLOSE?:1566–1574`, consumer `1734`: void wait with named expiry.
- `PRELOAD-ACKED-BY?:1593–1599`, consumer around `1691`: use `UNTIL`.
- `PONG-FILLED:1637–1662`: throw only for lack of progress; retain zero for queue-count failure and the assertion at `1715–1716`.
- `PONG-WAITER?:1667–1681`: split both clock failures from claim changes; preserve genuine false/error outcomes and their checks at `1730–1731`.
- `AIO-STOP:2500–2509`: retry busy only; named timeout at the unchanged drain deadline.

**Comments**

Correct `1566–1567` and `1637–1641`: no-progress expiry is not zero evidence of a failed fill. Explain the retained progress-reset clock. Clarify busy-only retry at `2500–2501`.

**Proof**

Run `websocket`, including Linux for its mutex-waiter branch. Copies force: no close; no preload acknowledgement; nonnegative queue counts without growth; expiry before futex wake; expiry immediately after a successful wake; and persistent `E-AIO-BUSY`. Require `P(ws-test <wait>)`.

Keep separate queue-count failure, claim-change, and non-busy AIO-error checks as ordinary failures/errors.

**Acceptance**

Clock expiry is distinct from vanished evidence or failed FFI. Completed-duration assertions stay unchanged.

Files: src/lib/net/ws-test.f. Verify: the rows and forced copies under Proof. Depends: habu-throw-websocket-lifecycle-2c12bc32. Ownership: the files above. Claim: unassigned.
