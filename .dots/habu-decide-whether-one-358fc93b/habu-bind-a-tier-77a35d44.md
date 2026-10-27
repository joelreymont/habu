---
title: Bind a tier 0 locals group in one frame step
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T20:21:25.378033+02:00"
---

Tier 0 binds each local with its own sub sp and moves parameters one cell at a time; tier 1 allocates the frame with the x30 save and moves pairs (stp/ldp). Frame handling is 21.3G of tender's gap (FACT-OWNER 11 vs 9 sp ops and 6 vs 2 frame insns, RECORD-KIND 7 vs 2, HANDLE-ENTRY, FIND-DOCUMENT, F-NEXT) and zz SCAN-CHAR's prologue binds 4 params a cell at a time. Acceptance: a {: ... :} group adjusts sp once and stores register pairs with stp where two adjacent slots come from registers; LOC-REC and slot offsets unchanged; pass-2 width-aware groups correct. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: unassigned.
