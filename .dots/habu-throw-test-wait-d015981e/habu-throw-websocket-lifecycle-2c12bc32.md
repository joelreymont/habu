---
title: Throw WebSocket lifecycle wait deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.744756+03:00"
closed-at: "2026-10-16T23:16:06.590206+03:00"
close-reason: "Landed: ws-test's lifecycle waits AWAIT-QUIET, AWAIT-KEPT, AWAIT-END and AWAIT-CLOSE replace QUIET?, KEPT?, ENDED-BY? and CLOSED-BY?, keeping their sleeps and absolute deadlines, and name themselves and throw E-PROC-TIMEOUT; JOINED names a task's recorded deadline as 'ws-test task join' before any code reaches an assertion; the deadline-only TTRUEs and ended branches are gone, and GOODBYE-CUT's cut is the original seen. Proofs: row websocket rc 0; forced quiet, kept, end, close and join copies rc 67 with one named line each; the old KEPT? forced the same way rc 1 with 12 TFAILs. Astra review APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/net/ws-test.f`:

- Convert pure `QUIET?`, `KEPT?`, `ENDED-BY?`, and `CLOSED-BY?` at `1079–1086,1456–1462,1507–1512,2115–2121` to void waits, retaining their sleeps and absolute deadlines.
- Update QUIET uses at `1094,1103,1345,1353`; KEPT uses at `1490,1708,1805,1865,1936,2011,2108,2134,2168`.
- Simplify ended-result branches at `1742–1749,1829–1832,1879–1882,1901–1912,2073–2075`; `GOODBYE-CUT`’s successful cut condition becomes the original `seen`.
- Update `2139`.
- `JOINED:1295–1300`: report a recorded timeout before returning a code for assertion.

**Comments**

Correct false-result descriptions at `1076–1078,1455,1505–1506,2113–2114`, and “left where it waits” descriptions at `1723–1724,1823–1825,1859–1860`.

**Proof**

Run `websocket`. Separate copies hold the timeout counter, kept flag, task completion, and close flag short of success. Require `P(ws-test <replacement wait>)`. A joined-task copy records `E-PROC-TIMEOUT` and must satisfy `P(ws-test task join)`.

**Acceptance**

All deadline-only boolean assertions and guards are gone. Actual refusal codes and conditions controlling behavioral checks remain.

Files: src/lib/net/ws-test.f. Verify: the rows and forced copies under Proof. Depends: habu-throw-websocket-observation-8aab3bf5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d10.
