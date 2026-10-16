---
title: Throw curl server deadlines at the join
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.606639+03:00"
blocks:
  - habu-throw-curl-test-b42f5f17
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/net/curl-test.f`:

- Add one atomic pending-deadline reason beside server state at `100–113`, with named values for acknowledgement and retry waits.
- `STREAM-WAIT-ACK` / `SERVE-STREAM`, `457–478`: record expiry before closing; remove expiry’s `SERVER-BAD` increment.
- `RETRY-NEXT` / `SERVE-RETRY-PRIME`, `559–601`: record deadline separately from genuine poll/fixture failures; retain the latter’s counters.
- `START-SERVER` / `STOP-SERVER`, `727–737`: reset the reason, finish/join the server, close its listener, and report a pending deadline.
- Add a main-task check at client-result consumption: `MULTI-FETCH:1407–1410`, `STREAM-FIRST:1420–1434`, `STREAM-REMAINING:1437–1448`, and before the completion assertions at `1475` and `2101`.

`MULTI-FETCH` must check the server result after `CURL:AWAIT` and before `RECORD-RESULT`, so libcurl code 28 cannot preempt the recorded server cause. Do not put this join operation into `AWAIT-ONE`, which also runs in `DONE-WORK` (`1986`).

**Comments**

Revise `RETRY-NEXT:559–561`: `-1` still ends the fixture path, but a recorded deadline is distinct from a fixture fault.

**Proof**

Run `curl-http`. In separate copies, suppress `STREAM-ACK` and prevent both readiness alternatives in `RETRY-NEXT`; shorten only those copied waits. Require `P(curl-test STREAM-WAIT-ACK)` and `P(curl-test RETRY-NEXT)` through the main join, before result/counter failures.

**Acceptance**

Deadline publication precedes connection closure. Workers assert nothing about expiry. Genuine poll errors and server counts retain their checks.

Files: src/lib/net/curl-test.f. Verify: the rows and forced copies under Proof. Depends: habu-throw-curl-test-b42f5f17. Ownership: the files above. Claim: unassigned.
