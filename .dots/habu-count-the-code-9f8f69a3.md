---
title: "Count the code after a routine's inner global label in clobber lint"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T08:20:45.425758+02:00"
---

Problem: in tools/lint/clobber-lint-core.f pass 1 (ROUTINE-SCAN, ROUTINE-ADVANCE-OPEN), a global L* label placed inside a routine starts the next routine; when the routine reaches it only by a branch (path ended before the label), the scan stops there, so the writes after that label are left out of the routine's clobber set and its callers miss them (a missed finding, not a false one). Reported by the fold i worker of lane sealed. Acceptance: a fixture where a routine branches forward to its own global label and writes a register after it, held by a caller across the BL: counted 1 (0 before); the label's own callers keep their sets; no other fixture count changes and the engine scan's sets change only where the shape occurs (report each). Files: tools/lint/clobber-lint-core.f, tools/lint/clobber-lint-test.f. Verify: the clobber-lint row. Depends: lane sealed landed (habu-answer-tools-check-745b0ccd). Claim: unassigned.
