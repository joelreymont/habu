---
title: V2 evidence promotion typestate
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-11T12:25:27.570165+02:00\""
blocks:
  - habu-v2-evidence-applicability-73ac58b9
---

Implement immutable Candidate -> Verified -> Measured -> PolicySatisfied -> Promoted transitions from MODEL-CAD-V2-PLAN.md:1917-1937. Policies bind model, weights, target, numeric policy, populations, verifier versions, thresholds, expiry, and rollback artifact. Acceptance: missing/stale/wrong-target evidence makes the transition unconstructible; transition never mutates candidate; policy change invalidates promotion; audit records exact obligation closure.

NOTE 2026-07-18 (capbud landing c06b0c14): this dot also owns the
obligation-discharge AUTHORITY model - who may discharge which
obligation (verifier identity vs class vs independence policy), the
missing piece for COMMIT-AUTHORIZED's third leg. The landed OBLIG
verifier/independence fields + APPLIC closure are the substrate.

Claim: agent=evprom workspace=.jj-ws/fable-evprom (owns new maki/db promotion files + tests + the folded discharge-authority model)
