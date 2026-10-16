---
title: Share a named test wait deadline
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.569731+03:00"
closed-at: "2026-10-16T16:44:14.309986+03:00"
close-reason: "Landed: src/lib/test/wait.f (package TEST-WAIT) adds EXPIRED ( ptr u8 n -- ), which prints '<name>: a deadline expired' on stdout and throws E-PROC-TIMEOUT, and POLL ( R [ R -- R bool ] n ptr u8 n -- R ), which asks a predicate until true, pausing the task, and is EXPIRED at an absolute mono-ns deadline (until is a reserved control word, hence POLL). src/lib/aio-test.f replaces REACHED?/ENDED?/STOPPED? and their 21 TTRUE uses with REACH, AWAIT-END and STOP-LOOP; STOP-LOOP retries only E-AIO-BUSY and rethrows any other AIO:STOP code. docs/gate.md states the rule for named local waits and task joins. Proofs (macOS): bin/hb --load src/lib/aio-test.f rc 0 test: ok; tools/check.f on wait.f and aio-test.f rc 0; forced copies REACH, AWAIT-END and STOP-LOOP (busy ring) each rc 67 with the named stdout line and -2502 last; a non-busy stop error keeps its code (-9292, no deadline line). Not run: CASE-PLANTED-TIMER/POLL (Linux only). Astra review: one High (STOP-LOOP retried every error), fixed, focused follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- Add `src/lib/test/wait.f` with the interface above.
- `src/lib/aio-test.f:172–197`: replace `REACHED?`, `ENDED?`, and `STOPPED?` with void local wrappers `REACH`, `AWAIT-END`, and `STOP-LOOP` using `POLL`.
- Update all 21 uses at `209`, `345–346`, `361`, `388`, `649`, `750–753`, `777–781`, and `797–802`. Preserve worker error, returned-value, cancellation, and resource assertions.
- `docs/gate.md:432–453`: extend the existing rule to named local waits and task joins; state stdout and unchanged deadlines.

**Comments**

Replace `aio-test.f:6–7`’s promise of “FAIL” with: an unfinished wait ends the row as a timeout; completed results are asserted. The stop-retry explanation at `188–190` remains valid.

**Proof**

Run `aio-uring`, whose file is `src/lib/aio-test.f` (`gate-stdlib-cases.f:1520–1522`).

Use three copies: force `REACH`’s count predicate false; remove the first case’s pipe poke so `AWAIT-END` expires; make only `STOP-LOOP`’s attempted stop remain busy. Require `P(aio-test REACH)`, `P(aio-test AWAIT-END)`, and `P(aio-test STOP-LOOP)`.

**Acceptance**

The proposed shared effect checks through real callers. Normal AIO cases pass, including deliberately expected timer outcomes. No constant-true wait assertions remain.

Files: docs/gate.md, src/lib/aio-test.f, src/lib/test/wait.f. Verify: the rows and forced copies under Proof. Depends: none. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d1.
