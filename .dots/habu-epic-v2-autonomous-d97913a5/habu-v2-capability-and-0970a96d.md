---
title: V2 capability and budget enforcement
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-11T12:25:27.142308+02:00\\\"\""
closed-at: "2026-07-18T01:00:59.095012+02:00"
close-reason: "Capability tokens + budget ledgers + COMMIT-AUTHORIZED landed (c06b0c14): forge-proof grants, subset-only attenuation, idempotent atomic charging, replay-equal digests - all five rules test-proven. Discharge authority folded into evidence-promotion."
blocks:
  - habu-v2-machine-action-a7357409
---

Implement finite capability tokens and monotonic budget ledgers for agent actions. Validate compute time, device time, storage, candidate count, retries, and external effects before dispatch and charge actual usage atomically. Acceptance: raw values cannot forge capabilities, nested actions cannot exceed parent authority, exhaustion returns typed evidence without partial commit, retry does not double charge, and replay reproduces charges. Align with habu-v2-types-finite-18bb1b35.

NOTE 2026-07-17 (atomic-txn landing c1cb38c8): this dot also owns the
deferred COMMIT validate-composition legs - wiring capability-set and
budget-ledger checks (and the obligation-discharge authority model) into
maki/db/commit-store.f COMMIT once the enforcement surfaces exist; the
landed TX:VALIDATE + APPLIC closure are the substrate.

Claim: agent=capbud workspace=.jj-ws/fable-capbud (mechanism legs with opaque capability codes; vocabulary content stays user-gated; owns new maki/db capability/budget files + commit-store COMMIT wiring)

RESOLVED 2026-07-18 (capbud lane, commit c06b0c14): MECHANISM ACCEPTANCE
MET. Package CAPTOK: grant = refined nominal handle over an append-only
authority pool (probe-proven the ONLY safe shape: private PRODUCT has no
construction surface, public PRODUCT MAKE accepts raw-n forges);
ATTENUATE is the sole derivation and is subset-only (cap-mask AND +
per-dimension budget <=, typed escape results, transitive containment
proven). LEDGER: monotonic remaining vector, RESERVE pure typed fit,
CHARGE idempotency-key-deduplicated atomic, DIGEST canonical
order-independent. COMMIT-AUTHORIZED wires both deferred validate legs
BEFORE publish (reject = state-identical to pre-marker crash; charge
exactly once per key on fresh publish). All five acceptance rules
test-proven; suite-table 128-cap handled by one aggregated wired entry
(children standalone + fail-closed). Capability CODES stay opaque per
the user-gated CAP dot; CAD-KIND:capability-id deliberately unused.
DEFERRED honestly: obligation-discharge AUTHORITY (who may discharge
which obligation) - no verifier-authority model exists; folded into
habu-v2-evidence-promotion-f8312ebe (the promotion typestate is that
model's home). Integration note: the refine-lint seed collision with
master's independent RAW>ACTION-ID fix resolved per the lane's own
recipe (GRANT renumbered 57, SEED#=58).
