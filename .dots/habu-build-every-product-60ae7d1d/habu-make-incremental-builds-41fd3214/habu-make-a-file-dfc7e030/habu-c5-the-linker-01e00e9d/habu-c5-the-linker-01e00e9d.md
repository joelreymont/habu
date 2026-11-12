---
title: "C5: the linker carries facts; T0 ships"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:30:49.575281+02:00"
blocks:
  - habu-c4-a-build-84efe577
---

Problem: the cross-built hb must carry each record's facts text and boot from it; today the next generation still gets its effects from the baked signature pool (src/core/checker.f:23249-23258 CK-AOT-SIG-POOL). Acceptance: the cross-built hb copies DATA raw, so [32] is a plain number on no address list; the linker writes each primitive record's text from prims.f's row, the rows of create, variable and constant ending in a creates-effect tail, and lays every kernel text and creates tail as strings before the product's first file loads, so the product's created words find theirs; the cross-built hb boots with no signature pool, no registry section and no replay and checks a new file; before publication, a does> definer whose facts text does not round-trip or cannot be said is refused (src/core/check-hook.f:153 DOES-PUBLISHED runs after the definer's record is appended, so today it leaves [32] 0 without a refusal); T0 ships here. Depends: C4 (habu-c4-a-build-84efe577). Claim: unassigned, owner carl.
