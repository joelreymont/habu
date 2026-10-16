---
title: Throw pty and proc-watch wait deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.864085+03:00"
closed-at: "2026-10-17T00:24:04.579877+03:00"
close-reason: "Landed: pty-harness-test HANGUP, process-pty-tty-smoke GONE-WITHIN and proc-watch-smoke WATCH-READY name themselves and throw E-PROC-TIMEOUT; HANGUP closes the master under finally; WATCH-READY abandons the child (kill, close the watch, reap) before the timeout leaves, and a failed kill throws E-PROC-OUTPUT before the blocking reap; the zero-time probe (READY-NOW?) and the duration assertion stay assertions. Proofs (macOS): the three rows rc 0; forced copies named each wait (rc 67, -2502) with the master closed, the sleeper gone, the child reaped; the probe control stays a TFAIL; a failed-kill copy (signal 999, child held) ends rc 67, -2503, in 6 s with the child gone. Untested: Linux (DEAD-WATCH-LINUX, the Linux zero-time probe), full gate. Review: Astra full review found one Medium (ABANDON's blocking reap after a failed kill), fixed; Astra follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

- `src/lib/pty-harness-test.f:331–359`: make HANGUP a void wait; close the master before propagating its timeout.
- `test/process-pty-tty-smoke.f:276–300`: throw on disappearance expiry after the existing teardown; preserve invalid-pid failure.
- `test/proc-watch-smoke.f:90–99,122–150`: distinguish positive wait expiry from immediate probes and invalid readiness. Ensure child termination/reaping and watch closure before timeout propagation.

Preserve the zero-time zombie probe at `proc-watch-smoke.f:136` and the completed-duration assertion at `pty-harness-test.f:327`.

**Comments**

Update the changed hang-up/disappearance descriptions to say expiry throws. Keep platform-specific zombie behavior documented at `proc-watch-smoke.f:130–141`.

**Proof**

Run `pty-harness`, `proc-pty-tty-smoke`, and `proc-watch-primitive-smoke` (`gate-stdlib-cases.f:2535–2537,1510–1512,1489–1491`).

Copies retain the terminal slave, retain the reported descendant, or withhold child release. Require `P(pty-harness-test HANGUP)`, `P(process-pty-tty-smoke GONE-WITHIN)`, and `P(proc-watch-smoke WATCH-READY)`, with cleanup complete.

**Acceptance**

No child or watch is stranded by an early throw. Immediate probes and genuine invalid-pid/readiness results remain assertions.

Files: src/lib/pty-harness-test.f, test/proc-watch-smoke.f, test/process-pty-tty-smoke.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d17.
