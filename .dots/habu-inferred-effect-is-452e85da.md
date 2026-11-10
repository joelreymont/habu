---
title: inferred_effect is written as unknown, with and without a trailing space
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.131788+02:00"
---

Problem: tools/check-verify-core.f:395 writes inferred_effect "unknown " (trailing space) and tools/check-all-errors-core.f:238 writes "unknown"; check-all-errors-core.f:236-237 (CA-JSON-DUP) writes declared_effect "unknown" where habu-a-packet-for-72692145 settled null for an effect that is not known. Reported by the null-effect worker. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a record whose effect is not known writes null in each effect field, the record check accepts it, and no writer spells unknown. Files: tools/check-verify-core.f, tools/check-all-errors-core.f, tools/gate-json-assert-core.f. Verify: the records of a duplicate definition and of a declaration refusal pass diag-contract. Depends: habu-a-packet-for-72692145 landed (same words). Ownership: the refusal record. Claim: unassigned.
