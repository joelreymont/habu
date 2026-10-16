---
title: Throw pg-kill fixture wait deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.876312+03:00"
closed-at: "2026-10-17T00:25:34.199869+03:00"
close-reason: "Landed: pg-kill-test REPORT-READ (in LINE-READ?) and GONE-WITHIN name themselves and throw E-PROC-TIMEOUT; a short or failed report and a failed read or poll stay failures; SWEEP removes the socket directory under finally; CASE-END runs GT-POOL-KILL-ALL, then this process's PIPE-WR close and the sweep, under nested finally, so a kill throw still sweeps and the read end always closes. Proofs (macOS): row rc 0 in 10 s; forced copies named REPORT-READ (first and second line) and GONE-WITHIN (EXPECT-GONE and SWEEP) with rc 67, -2502 and the socket directory gone; a start throw keeps -2500; a kill-throw copy shows the sweep ran and -2503 wins; the mid-line control stays a TFAIL. Untested: Linux, the row through the gate pool, full gate. Review: lead fixed the kill-throw sweep skip before review; Astra full review APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`test/db/pg-kill-test.f`:

- `READY-WITHIN?:157–160`: distinguish poll errors.
- `LINE-READ?:164–172`, `REPORT-READ?:175–180`, `GONE-WITHIN?:206–214`: throw only on deadline expiry; retain malformed/oversized/short-report failures.
- Preserve real assertions at `239–254`.
- `SWEEP:278–283`, `CASE-END:287–291`: finish socket-directory and descriptor cleanup even if the final wait expires.

**Comments**

Clarify report failure versus report deadline. The minute-based fallback explanation at `274–277` remains; its expiry becomes a timeout.

**Proof**

Run the full `pg` row with both registered files. Copies withhold either report line and retain a pipe writer through the normal and sweep waits. Require `P(pg-kill-test REPORT-READ)` or `P(pg-kill-test GONE-WITHIN)`.

**Acceptance**

The deliberate pool timeout at `258–266` remains asserted. Unexpected fixture deadlines escape, and socket-directory cleanup still occurs.

Files: test/db/pg-kill-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d18.
