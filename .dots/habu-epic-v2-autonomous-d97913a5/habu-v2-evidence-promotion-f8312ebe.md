---
title: V2 evidence promotion typestate
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:27.570165+02:00\\\"\""
closed-at: "2026-07-18T03:55:28.750113+02:00"
close-reason: "Promotion typestate + discharge-authority gate landed (ecc1a806): unforgeable staged proofs, evidence-gated unconstructible transitions, digest-bound policy invalidation, audit closure proven. Commit-store threading folded into deterministic-audit."
---

Implement immutable Candidate -> Verified -> Measured -> PolicySatisfied -> Promoted transitions from MODEL-CAD-V2-PLAN.md:1917-1937. Policies bind model, weights, target, numeric policy, populations, verifier versions, thresholds, expiry, and rollback artifact. Acceptance: missing/stale/wrong-target evidence makes the transition unconstructible; transition never mutates candidate; policy change invalidates promotion; audit records exact obligation closure.

NOTE 2026-07-18 (capbud landing c06b0c14): this dot also owns the
obligation-discharge AUTHORITY model - who may discharge which
obligation (verifier identity vs class vs independence policy), the
missing piece for COMMIT-AUTHORIZED's third leg. The landed OBLIG
verifier/independence fields + APPLIC closure are the substrate.

Claim: agent=evprom workspace=.jj-ws/fable-evprom (owns new maki/db promotion files + tests + the folded discharge-authority model)

RESOLVED 2026-07-18 (evprom lane, commit ecc1a806): ACCEPTANCE MET.
Package PROMOTE: Candidate->Verified->Measured->PolicySatisfied->Promoted
as sealed products with class-private proof tokens (unforgeable,
wrong-stage substitution statically rejected); transitions derive-never-
mutate and are UNCONSTRUCTIBLE without APPLIC-applicable evidence
(E-PROMO-UNAPPLICABLE; typed leg = the applicability sum queried
directly - the multi-cell-product refusal-channel deviation documented
per the policy.f precedent). PPOLICY spec binds all ten fields over
landed identities (populations conservatively config-id - no population
owner exists); digest-bound REVALIDATE proven both directions. THE
FOLDED AUTHORITY MODEL LANDED: DAUTH:AUTHORIZED-DISCHARGE folds verifier
class + independence (via OBLIG:DISCHARGE) + verifier IDENTITY (sealed
producer-id allowlist, CAPTOK discipline) -> ok/not-discharged/
unauthorized. Audit closure: recorded journal descriptor == recomputed
(AUDIT-MATCH). +6 proof-token mints classified (epic owner). Cross-
package bare-family-tail ambiguity found + fixed (authz-result rename;
LESSONS). COMMIT-STORE WIRING deferred honestly - needs obligation/
evidence context threading through transaction/commit-store parameters,
not one line; folded into habu-v2-deterministic-audit-428d27c2 (the
replay dot exercises the full gate).
