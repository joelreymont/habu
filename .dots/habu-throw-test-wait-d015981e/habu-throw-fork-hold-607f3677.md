---
title: Throw fork-hold wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.704327+03:00"
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:POLL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`src/lib/test/fork-hold.f`:

- `FLAG-WAIT:40–47`, `HOLD:94–99`, `HOLDING:103–105`: retain raw task recording; name main’s HOLD timeout at its boundary.
- `FORK-CHECK:113–126`: after kill, reap, and descriptor closure, distinguish `END-CODE`’s timeout from actual errors before `ended 0 T=`.
- `RELEASE:128–135`: name and rethrow a joined holder timeout before asserting other results.

The latter two are additional defects read while tracing the census’s `FLAG-WAIT`.

**Comments**

Replace `113–114`’s claim that completion inside `CHILD-MS` is asserted. State that completion status is asserted only after the wait completes.

**Proof**

Run `process-fork-wrappers` with both registered files, plus `pg-fork` (`gate-stdlib-cases.f:1501–1504,1640–1644`). Copies withhold holder readiness, expire the holder’s release wait, and keep a forked child alive through `CHILD-MS`. Require `P(fork-hold FLAG-WAIT)` or `P(fork-hold child exit)` at the responsible main boundary.

**Acceptance**

Children are killed and reaped before timeout propagation. Holder errors are not converted into assertion failures.

Files: src/lib/test/fork-hold.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
