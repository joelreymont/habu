---
title: V2 transaction journal schema
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:26.896307+02:00"
blocks:
  - habu-v2-canonical-artifact-ee5121b4
---

Implement MODEL-CAD-V2-PLAN.md:1832-1849 transaction data model. Define checked immutable transaction id, base revision, idempotency key, read set including negative lookups, write object set, dependency edges, capability set, budget ledger, obligations, and commit proposal. Acceptance: canonical round-trip, duplicate/conflicting writes reject, omitted read dependency rejects validation, and retry identity is stable. Files: new maki/db/transaction.f and focused tests.

NOTE 2026-07-17 (artifact contract round 2, 676d5a7b): this dot also owns
the rev-id family leg of the plan-23.9 foreign-id contract (owner REV/TX,
content-addressed at commit: constructor + refinements + wire codec pair),
and the JOURNAL package for audit-event-id (monotonic append sequence,
8-byte wire) rides the journal/object-store leg. Land them with the
transaction data model per the 23.9 surface spec.
