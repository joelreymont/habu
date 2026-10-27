---
title: Bind a tier 0 locals group in one frame step
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T20:21:25.378033+02:00"
closed-at: "2026-10-27T21:50:21.359486+02:00"
close-reason: "Measured, not landed: master already adjusts sp once per {: :} group, so the lane only paired register-sourced stores into stp. zz 13.920G/13.792G vs base 13.799G/13.785G, tender 282.24G/282.07G vs 282.33G/282.15G: neutral. The hot groups (zz SCAN-CHAR, tender FACT-OWNER) bind parameters popped from memory (ldr x16 then str per cell), which stp from registers does not touch; tender's frame category is not in the locals bind."
---

Tier 0 binds each local with its own sub sp and moves parameters one cell at a time; tier 1 allocates the frame with the x30 save and moves pairs (stp/ldp). Frame handling is 21.3G of tender's gap (FACT-OWNER 11 vs 9 sp ops and 6 vs 2 frame insns, RECORD-KIND 7 vs 2, HANDLE-ENTRY, FIND-DOCUMENT, F-NEXT) and zz SCAN-CHAR's prologue binds 4 params a cell at a time. Acceptance: a {: ... :} group adjusts sp once and stores register pairs with stp where two adjacent slots come from registers; LOC-REC and slot offsets unchanged; pass-2 width-aware groups correct. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: jerry, workspace jerry-bindgrp.
