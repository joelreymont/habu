---
title: Add production owner and WID nominal roles
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:50:07.476675+02:00"
---

Full context: src/core/checker.f:4206-4210 models protected and owner WID primitives as undifferentiated n, while src/habu/aot-capture.f helpers use n for WIDs, owner/protected row indices, roles, counts, and blob/name offsets; test/owner-wid-role-swap.f proves only test-local nominal types. Cause: semantically distinct same-cell identities unify, so swaps and wrong-table lookups survive stack checking. Fix: define package-scoped production nominal roles for wid, public-wid, private-wid, owner-row-index, protected-row-index, owner-role, record-count, blob-offset, name-offset, and byte-count; expose checked conversions only at audited lookup/allocation boundaries; update primitive effects and owner/AOT APIs. Acceptance: negative checked fixtures reject every pairwise role swap and visibility refinement violation; valid lookup, capture, freeze, snapshot, and bootstrap paths pass; test-local aliases are removed; no new TRUSTED boundary.
