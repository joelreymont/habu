---
title: Compact a hidden run by its instantiated width
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T15:20:00.401523+03:00"
---

src/core/render.f HID-RUN-REST compacts a hidden run by the DECLARED family width (fam TFAM-WIDTH@*), not the instantiated one, so a bundle with a wide argument always splits in the diagnostic: option<pt<>> with a wide slot renders as option<pt<>> @option.slot2<pt<>> (two terms for one value). Pinned today by test/type-decl-suite.f:2492,2531 (the lone @tdpbopt.slot2<tdpbw2<>> form). Found by the catch-stale lane (its stale<option<pt<>>> row shows the same split). Acceptance: the compaction uses the arg-aware width (T-WIDTH), one logical term per bundle in every render path (QREND row mode, REND-COLLECT), the two pins updated to the compacted form with the reason in the row. Depends: 89902bde. Ownership: hazel. Claim: unassigned.
