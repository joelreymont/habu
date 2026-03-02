---
title: Audit remaining numeric-tower failures after non-numeric blockers are gone
status: open
priority: 3
issue-type: task
created-at: "2026-03-07T19:32:55.823358+01:00"
blocks:
  - habu-decompose-remaining-per-0c9e465d
---

src/runtime/primitives/arith.zig and src/runtime/primitives/type.zig. Root cause: some residual failures will be true missing-bignum/integer-overflow cases rather than general CL/runtime bugs. Fix: produce an exact blocked-test list once the non-numeric blockers are closed. Why: required to state honest semantic parity except numeric tower.
