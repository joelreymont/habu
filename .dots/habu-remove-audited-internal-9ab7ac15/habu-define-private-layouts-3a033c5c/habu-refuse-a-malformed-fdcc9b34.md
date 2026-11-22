---
title: Refuse a malformed type entry where ER-READ reads it
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T15:36:21.208720+01:00"
---

Problem: a saved type entry (src/core/type-family.f ER-READ, ER-BODY, ER-VARIANT, ER-NODE) is read with gaps the removed self-checks used to catch later: ER-BODY accepts a negative variant count, ER-NODE's P n index is not bounded by the owner's arity before TFC-SCH-TERM reads TFC-VARS, and a sum's variants may carry positional schemas beside named fields (caught late by RANGE-BAD at SUM-NAMED-PAYLOAD-WALK?). ER-READ's round trip through ET-NODE cannot catch any of them. Acceptance: ER-READ refuses each with ER-BAD; RANGE-BAD and its late check go; the engine builds and the type suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-er.
