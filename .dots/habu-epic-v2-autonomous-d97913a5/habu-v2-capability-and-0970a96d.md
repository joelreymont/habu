---
title: V2 capability and budget enforcement
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-11T12:25:27.142308+02:00\""
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
