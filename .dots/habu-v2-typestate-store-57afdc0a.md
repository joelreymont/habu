---
title: "V2 typestate: store rehydrate"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T16:17:36.155279+02:00"
---

Implement sub-dot 6 of the R7 typestate addendum: MODEL-CAD-V2-PLAN.md:1702-1712 (design at 1280-1643). Rehydrating persisted store rows back into typed evidence/typestate values (durable replay loop REPLAY-ENSURE path) so persistence cannot launder untyped rows back in. Acceptance: rehydrate of a valid persisted row yields typed evidence; malformed/forged persisted rows reject with named codes; round-trip golden. Verify: typestate-test suite, store-replay focused tests, maki/test.f, typed-local-diff-lint. Depends: habu-v2-typestate-promotion-2266b236. Ownership: maki/store-replay.f + rehydrate tests (disjoint from sub-dot 5 files). Claim: unassigned.
