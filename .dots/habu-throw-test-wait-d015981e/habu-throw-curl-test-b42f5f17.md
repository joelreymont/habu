---
title: Throw curl test wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.589533+03:00"
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/net/curl-test.f`:

- `703–716`: name `WAIT-READY` and `WAIT-DONE` exhaustion without changing their turn counts.
- `827–839`: use the stdout reporter for `NOT-TIMEOUT`; retain the explicit expected-code-28 paths.
- `1355–1385`: migrate `REACHED?`, `ENDED?`, and `STOPPED?` to void wrappers and simplify `WAIT-HIT`.
- Update uses at `1505`, `1798`, `1831–1838`, `1878–1881`, `1893–1900`, `1924–1934`, `1956`, and `1997–2003`.
- Remove the exit-99 fallback at `1927–1929`.
- `1971–1976`: name `WAIT-PUBLISHED` expiry; keep its existing polling behavior.

**Comments**

Correct fd-2 wording at `827–829`, the “FAIL” promise at `1318–1320`, and the claim at `52` that a slow request is necessarily broken. Describe limits as bounds, not defect evidence.

**Proof**

Run `curl-http`: `src/lib/net/curl-test.f` (`gate-stdlib-cases.f:1646–1650`). Force copies of count, task-end, stop-retry, publication, and readiness-turn exhaustion. Each uses `P(curl-test <word>)` with the replacement wrapper’s name. Keep expected libcurl timeout cases green.

**Acceptance**

No deadline-only assertion or exit-99 path remains. Code 28 remains assertable only where explicitly expected.

Files: src/lib/net/curl-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
