---
title: Throw capture-tree fixture wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T16:28:19.809345+03:00"
blocks:
  - habu-share-a-named-85cc93f5
---

Problem: the parent's rule (a wait whose clock ends throws E-PROC-TIMEOUT after one named stdout line; a task records it and main throws it at the join; no deadline raised) is broken in the files below. The parent holds the shared interface (`TEST-WAIT:EXPIRED`, `TEST-WAIT:UNTIL` in src/lib/test/wait.f), the rulings and the proof notation `P(name)`.

**Files and edits**

`test/capture-tree-test.f`:

- `READY-WITHIN?:96–99`: distinguish negative poll errors from zero-event expiry.
- `REPORT-READ?:103–111`, `GONE-WITHIN?:114–122`: name only deadline expiry; preserve EOF/short-report/read-failure meaning.
- `AWAIT-REPORT:128–130`, expected-timeout cases `186–211`: record whether the report wait itself expired.
- In the two `TTHROWSQ` cases expecting a capture timeout, catch first, rethrow a recorded report-wait timeout, then assert the expected capture code. Otherwise the wrong deadline can satisfy the expected-code assertion.
- `CASE-END:229–236`: ensure pipe closure even when its final wait expires.

**Comments**

Specify that report EOF is a failed report, while report deadline is a timeout. Keep the descriptions of deliberately expected capture timeouts.

**Proof**

Run `capture-tree`: `test/capture-tree-test.f` (`gate-stdlib-cases.f:2957–2959`). Withhold the report in a copy of the expected-capture-timeout case: require `P(capture-tree REPORT-READ)`, not a passing throw assertion followed by failed output checks. Separately expire the EOF wait and cleanup wait: `P(capture-tree GONE-WITHIN)`.

**Acceptance**

The intended capture deadlines remain asserted. Fixture deadlines escape those assertions, and descriptors close on both paths.

Files: test/capture-tree-test.f. Verify: the rows and forced copies under Proof. Depends: habu-share-a-named-85cc93f5. Ownership: the files above. Claim: unassigned.
