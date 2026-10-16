---
title: Throw gate-signal fixture wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.845537+03:00"
blocks:
  - habu-share-a-named-85cc93f5
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`test/gate-signal-test.f`:

- `READY-WITHIN?:253–256`: preserve boolean idle and reject real poll errors.
- `REPORTS-READ?:259–267`, positive `GONE-WITHIN?` waits at `270–278`: name expiry.
- `REPORTED-GONE?:301–309` and `ROOT-ENDED?:382–383`: convert pure deadline predicates to void waits.
- Update `SWEEP:314–319`, `EXPECT-GONE:323–329`, and `SIGNAL-BODY:418–436`.
- Protect all cleanup in `SIGNAL-END:440–446` and `DEADLINE-END:485–489`.

Keep `IGNORED-MS READY-WITHIN? TFALSE` and `0 GONE-WITHIN? TFALSE` unchanged (`414–416`). The latter is a probe, not a positive wait.

**Comments**

Retain the ignored-signal and deliberately killed-pool descriptions. Changed positive wait comments must distinguish deadline from EOF/read failure.

**Proof**

Run `gate-signal`: `test/gate-signal-test.f` (`gate-stdlib-cases.f:2945–2947`). Force report, root-watch, pipe-EOF, and reported-process disappearance waits in separate copies; require `P(gate-signal <wait>)`. Include cleanup expiry.

**Acceptance**

The expected-false and zero-time probe remain valid. The deliberate pool-timeout assertions at `469–480` still pass.

Files: test/gate-signal-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
