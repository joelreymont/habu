---
title: Throw fork-hold wait deadlines
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.704327+03:00"
closed-at: "2026-10-16T23:13:13.024784+03:00"
close-reason: "Landed: fork-hold's FLAG-WAIT deadline, main's own at HOLD or the holder's from RELEASE's join, is named 'fork-hold FLAG-WAIT' and thrown; FORK-CHECK kills and reaps every child, then names 'fork-hold child exit' for a child still running at CHILD-MS before any assertion; any other holder or child code stays asserted against 0. FORK-UNDER-WALK and FORK-UNDER-ZIP-LOCK read the held lock before RELEASE and assert it after, so an expired holder, which has let go, is named first. Proofs: rows process-fork-wrappers, zip and pg-fork rc 0; forced hold, release and child copies rc 67 with one named line each (master code: rc 67 unnamed or rc 1 with TFAILs); forced walk copy (holder wait 2 s, child op sleeping 3 s) rc 67 'fork-hold FLAG-WAIT' with no TFAIL, the old caller order a TFAIL first. Astra review Medium (holder timeout classified after deadline-dependent lock assertions), fixed; Astra follow-up APPROVE."
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/test/fork-hold.f`:

- `FLAG-WAIT:40–47`, `HOLD:94–99`, `HOLDING:103–105`: retain raw task recording; name main’s HOLD timeout at its boundary.
- `FORK-CHECK:113–126`: after kill, reap, and descriptor closure, distinguish `END-CODE`’s timeout from actual errors before `ended 0 T=`.
- `RELEASE:128–135`: name and rethrow a joined holder timeout before asserting other results.

The latter two are additional defects read while tracing the census’s `FLAG-WAIT`.

`src/lib/fork-child-test.f` `FORK-UNDER-WALK` and `src/lib/zip-lifecycle-test.f` `FORK-UNDER-ZIP-LOCK`: read the held lock value before `FORK-HOLD:RELEASE` and assert it after, so a holder whose wait expired, and so let go, is named at RELEASE before any assertion reads the lock.

**Comments**

Replace `113–114`’s claim that completion inside `CHILD-MS` is asserted. State that completion status is asserted only after the wait completes.

**Proof**

Run `process-fork-wrappers` with both registered files, `zip` with its three, plus `pg-fork` (`gate-stdlib-cases.f:1501–1504,1640–1644`). Copies withhold holder readiness, expire the holder’s release wait, and keep a forked child alive through `CHILD-MS`. Require `P(fork-hold FLAG-WAIT)` or `P(fork-hold child exit)` at the responsible main boundary.

**Acceptance**

Children are killed and reaped before timeout propagation. A holder's deadline is not converted into an assertion failure; any other holder code stays asserted against 0, as every join in the family asserts a worker's real error.

Files: src/lib/test/fork-hold.f, src/lib/fork-child-test.f, src/lib/zip-lifecycle-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: agent=worker workspace=.jj-ws/jerry-wait-d8.
