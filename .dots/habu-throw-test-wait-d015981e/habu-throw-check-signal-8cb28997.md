---
title: Throw check-signal fixture wait deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.825902+03:00"
closed-at: "2026-10-17T00:03:58.084977+03:00"
close-reason: "Landed: check-signal-test's own waits (REPORT-READ, FEED, CHECK-ENDED, GONE-WITHIN) name themselves and throw E-PROC-TIMEOUT on expiry; end of file, short reads and writes and poll errors stay failures; CASE-END closes every descriptor and clears the scratch whatever ending check.f or the wait for what it left throws, with this process's write end closed before that wait. Proofs (macOS): row check-signal rc 0; five forced copies rc 67 named with -2502; control rc 1 with TFAILs, no named line; a completion timeout followed by a log-read throw closes every descriptor and clears the scratch, the read error winning. Astra review: one Medium (CHECK-END ran before the protected cleanup), fixed by the lead; Astra follow-up: APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`test/check-signal-test.f`:

- Poll/report/gone helpers at `160–190`: distinguish negative poll errors and expiry.
- `FEED?:263–265`, `FEED-PAD?:272–278`, `FEED-REFUSED?:281–283`: throw on expiry; preserve false for an actual short write.
- `CHECK-ENDED-WITHIN?` / `CHECK-ENDED?`, `288–292`: convert pure completion waits to void waits.
- Update cases at `324–364,380–396`; remove only completion-deadline boolean guards.
- `CASE-END:400–410`: complete watch, feed, pipe, and scratch cleanup before propagating its wait error.

**Comments**

Describe successful completion or thrown expiry for the changed completion words. Preserve the intentional `check.f` deadline and exit-70 explanation at `366–390`.

**Proof**

Run `check-signal`: `test/check-signal-test.f` (`gate-stdlib-cases.f:2951–2953`). Copies withhold report bytes, leave stdin unwritable, prevent the process watch becoming ready, and retain a pipe writer. Require `P(check-signal-test <WAIT-WORD>)`; include an expiry inside `CASE-END`.

**Acceptance**

Deliberate subject deadlines still pass their assertions. Short writes and malformed reports remain genuine failures. Cleanup is not skipped by a new throw.

Files: test/check-signal-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d15.
