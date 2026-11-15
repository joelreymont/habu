---
title: Reject a top-level underflow in the check
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.610608+01:00"
---

Problem: --verify-only keeps no model of the top-level stack (src/habu/verify-source.f TOP-TOKEN :4135 drops numbers; TOP-RESOLVE :4095 only resolves names; RAW-TRUST-NEXT :2312 takes no operand), so `constant x constant x` is rejected as a duplicate at the second x while the run underflows at the first constant, and `variable v constant y`, `1 constant a constant b`, `create c ,` and `HB-TARGET-MACOS? ?TARGET` on Linux pass with rc 0. Reproducers in /tmp/jerry-constuf and /tmp/jerry-childrec/p/tgt.f. Acceptance: the three readers print the run's record (E-UNDERFLOW at the first constant; E-TARGET), rc 70. Depends: a design of the verifier's top-level stack. Claim: unassigned.
