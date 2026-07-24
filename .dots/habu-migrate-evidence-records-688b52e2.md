---
title: Migrate evidence records
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-24T13:34:14.393458+02:00\""
---

Why: maki/evidence/schema.f still declares certified, golden, gradchecked, profiled, and bundle with legacy PRODUCT; they are the typed proof values on model promotion paths. Owner: maki/evidence/schema.f and maki/evidence/schema-test.f only. Replace those five declarations directly with STRUCTURE inside public EVID, preserving every field name/nominal schema/order, generated MAKE/UNMAKE spellings, widths/layouts, private proof-token minting, artifact identity threading, class-specific roles, bundle slot order, errors, allocation, and public API. Leave every slot SUMTYPE untouched. Retarget product comments. Forbidden: aliases, legacy parser edits, raw casts, proof/token or SUMTYPE migration, transition/policy redesign, caller edits, copied tests, or cleanup. Pre-change proof: exactly five executable PRODUCT declarations exist and the real suite already pins role swaps and private-token forgery. Acceptance: evidence schema suite passes before/after through CERTIFY/GOLDEN/GRADCHECK/PROFILE and bundle construction/consumption, exact wrong-role/private-mint negatives, artifact identity, layouts and effects; no executable PRODUCT remains; focused typed-local/package/trust and owning Maki gates pass.

Claim: agent=codex-evidence-structure workspace=.jj-ws/habu-migrate-evidence-records-688b52e2
