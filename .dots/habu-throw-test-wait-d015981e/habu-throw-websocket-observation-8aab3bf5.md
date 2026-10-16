---
title: Throw WebSocket observation deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.729198+03:00"
closed-at: "2026-10-16T22:58:20.428634+03:00"
close-reason: "Landed: ws-test's own waits (AWAIT's idle arm, AWAITED, the handler's rest wait recorded as REST-EXPIRED and read by main's REST-AWAITED as one deadline) name themselves and throw E-PROC-TIMEOUT; deadline-only branches and TTRUEs are gone. Proofs: row ws rc 0; forced copies rc 67 with one named line each, the handler race 3 of 3. Fable review REVISE (Medium 1 fixed; Medium 2, per-site AWAITED names, declined under the family's <stem> <WAIT-WORD> naming); Astra follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/net/ws-test.f`:

- `AWAITED:274–284`: retain a private raw result wait for the handler and add the main-facing throwing wrapper. Preserve `POLL-MS`.
- `REST-SEEN:243`, `ZERO-FIRST:556–569`, `REST-HELD?:1366–1372`, `REST-CASE:1387–1408`: publish pending/success/timeout through the existing handoff using atomic access. On a recorded worker deadline, main closes the client, joins through HTTP stop, and reports before assertions.
- Remove the now-tautological REST readiness assertion; retain the actual early-byte and zero-time receive checks.
- Main `AWAITED` consumers at `1808–1810,1867–1873,1955–1965,2095,2171–2177`: remove only deadline-derived false branches.
- `AWAIT:664–672`: map idle to a named timeout.

Keep the independent `TASK:DONE?` condition at `2095` and `FLOOD-CODE` condition at `2171`; their failure branches are not made unreachable by successful observation.

**Comments**

Revise `274–275`, `243`, and `1384–1386` to describe explicit handoff and propagation rather than zero after expiry.

**Proof**

Run `websocket`: `src/lib/net/ws-test.f` (`gate-stdlib-cases.f:2561–2563`). Force a main probe to remain zero; separately hold `REST-QUEUED` at zero and expire the handler first; separately prevent a raw response becoming readable. Require `P(ws-test AWAITED)`, `P(ws-test handler rest wait)`, and `P(ws-test AWAIT)`.

**Acceptance**

A handler deadline reaches main through recorded state, not an HTTP 500 or later failed REST assertion. No off-limits HTTP file changes.

Files: src/lib/net/ws-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d9.
