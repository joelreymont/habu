---
title: "Export a certified quotation's inner effect"
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:44:39.238566+02:00"
---

The checker persists all eight EN-QUOT fields (E-COPY* at checker.f:4474-4483, restored 4928-4937) and exports none; EFAM-XT is everything a quotation projects to. Add read-only exports beside the existing EFFECT-* block: EFFECT-DIN-QUOT/EFFECT-DOUT-QUOT (descend into term i's EN-QUOT re-latching the quot rows; false for non-quot terms; the latch is one global so save/restore it) and EFFECT-QUOT-SIMPLE? (true only when return rows neutral EN.C=EN.D, no throw edge EN.E=0, fall-through live EN.F=0 - EN.F is NOT optional: a never-returning quotation must not get code emitted after its call). Anything not simple the chain refuses by name. Same narrow-export shape as EN.E's EFFECT-DIN-SLOT precedent. Acceptance: the b-amb-style fixture pair (quotation term vs plain xt term with equal counts) separated; latch save/restore proven (nested query does not corrupt the outer). Files: src/core/checker.f, test/effect-read-api-test.f. Depends: none.
