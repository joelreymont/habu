---
title: "Keep pg's cleanup flag set when its hook fails"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T00:30:22.299684+02:00"
---

Problem: src/lib/pg.f FORGET-HANDLES (:684-685) stores false into REGISTERED and then rethrows a failed cancel's code. IMAGE-LIFECYCLE:PREPARE keeps a hook that throws (src/lib/image-lifecycle.f PREPARE: the throw skips REMOVE), so the next REGISTER-CLEANUP (:688) registers FORGET-HANDLES a second time; the rule above PREPARE says a failed callback never clears its owner's flag (src/lib/ffi-abi.f REGISTER-CLEANUP keeps it). Found by the pruning stack's comment review, 2026-11-06. Acceptance: REGISTERED is cleared only once the cancel succeeded; a test makes FORGET-HANDLES throw once, registers again and finds one entry, or the lane shows the failure cannot be produced from a program and says why. Files: src/lib/pg.f and its test. Verify: the pg tests. Depends: none. Ownership: src/lib/pg.f. Claim: unassigned.
