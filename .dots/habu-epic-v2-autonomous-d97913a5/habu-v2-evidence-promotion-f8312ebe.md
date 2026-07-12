---
title: V2 evidence promotion typestate
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.570165+02:00"
blocks:
  - habu-v2-evidence-applicability-73ac58b9
---

Implement immutable Candidate -> Verified -> Measured -> PolicySatisfied -> Promoted transitions from MODEL-CAD-V2-PLAN.md:1917-1937. Policies bind model, weights, target, numeric policy, populations, verifier versions, thresholds, expiry, and rollback artifact. Acceptance: missing/stale/wrong-target evidence makes the transition unconstructible; transition never mutates candidate; policy change invalidates promotion; audit records exact obligation closure.
