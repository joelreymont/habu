---
title: Share a named test wait deadline
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.569731+03:00"
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- Add `src/lib/test/wait.f` with the interface above.
- `src/lib/aio-test.f:172–197`: replace `REACHED?`, `ENDED?`, and `STOPPED?` with void local wrappers `REACH`, `AWAIT-END`, and `STOP-LOOP` using `UNTIL`.
- Update all 21 uses at `209`, `345–346`, `361`, `388`, `649`, `750–753`, `777–781`, and `797–802`. Preserve worker error, returned-value, cancellation, and resource assertions.
- `docs/gate.md:432–453`: extend the existing rule to named local waits and task joins; state stdout and unchanged deadlines.

**Comments**

Replace `aio-test.f:6–7`’s promise of “FAIL” with: an unfinished wait ends the row as a timeout; completed results are asserted. The stop-retry explanation at `188–190` remains valid.

**Proof**

Run `aio-uring`, whose file is `src/lib/aio-test.f` (`gate-stdlib-cases.f:1520–1522`).

Use three copies: force `REACH`’s count predicate false; remove the first case’s pipe poke so `AWAIT-END` expires; make only `STOP-LOOP`’s attempted stop remain busy. Require `P(aio-test REACH)`, `P(aio-test AWAIT-END)`, and `P(aio-test STOP-LOOP)`.

**Acceptance**

The proposed shared effect checks through real callers. Normal AIO cases pass, including deliberately expected timer outcomes. No constant-true wait assertions remain.

Files: docs/gate.md, src/lib/aio-test.f, src/lib/test/wait.f. Verify: the rows and forced copies under Proof. Depends: none. Ownership: the files above. Claim: unassigned.
