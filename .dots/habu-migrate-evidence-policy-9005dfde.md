---
title: Migrate evidence policy products and slots
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T11:47:20.734262+02:00"
---

Wave C13 of the unified-type migration program (.blackboard/migration-plan-20260726.md). maki/evidence/policy.f:90 PRODUCT gate-set 0 (6 fields incl. cross-package types CAD-KIND:schema-id and NPOL:dom) and :101 PRODUCT granted 0 (includes a private grant-proof TYPEFAMILY field - the proof-carrying record; DERIVE eq is impossible on it per the engine limit in LESSONS, do not fight that) - STRUCTURE per 6ef124d0c64e, FIELD lines byte-identical; consumers (promotion-policy.f, schema.f, transaction.f, capability.f, promote.f, cad.f) untouched. maki/evidence/schema.f:107/111/115/119 certify-slot/golden-slot/gradcheck-slot/profile-slot 0 (got-payload/none each) - full-mode payload ENUMs, FIELD names from source, consumers (policy.f) untouched. A1/6ef124d0 test patterns; per-family mutation kills; the granted STRUCTURE keeps its proof semantics (checker negatives: proof cannot be forged from raw n, foreign proof rejected - copy the mcfg negative shape). STOP conditions per the program plan. Acceptance: evidence suites, dependents through maki/test.f green; both diff lints; census verify identical. Claim: agent=mig-c13 workspace=.jj-ws/habu-mig-c13

Closed 2026-07-26: landed as a41b950d8234. Registry fingerprint across eleven families; proved same-type field swaps invisible to value round-trips (M1) - registry pins load-bearing.
