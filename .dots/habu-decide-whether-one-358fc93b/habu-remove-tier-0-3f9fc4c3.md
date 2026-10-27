---
title: Remove tier 0 copies for dup, over and back-edge renames
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T20:21:25.393911+02:00"
---

zz per-byte loops spend register copies tier 1 does not: PARSE-FILE-CONTENT 3 copies per byte (dup, over, back-edge rename; 21 vs 17 insns), SCAN-CHAR 2 (16 vs 13), SCAN-SPACE 3; zz moves 0.87G of 1.86G. Tender FIND-DOCUMENT 2 copies per miss. Acceptance: dup/over of a register value consumed before either copy is written emits no mov, and a back-edge reconcile whose layout differs only by a renaming the loop body can absorb emits no copies; tier0-inline-prims counts the movs. Design needed (Plan) before a lane. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: unassigned.
