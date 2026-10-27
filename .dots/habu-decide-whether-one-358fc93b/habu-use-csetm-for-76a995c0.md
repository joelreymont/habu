---
title: Use csetm for tier 0 flags
status: active
priority: 3
issue-type: task
created-at: "2026-10-27T20:21:25.386117+02:00"
---

Tier 0 materializes a comparison flag as cset + neg; tier 1 emits csetm. zz 0.36G (PARSE-FILE-CONTENT, SCAN-CHAR, SCAN-SPACE per byte), tender NULL? 1.0G. Acceptance: every tier 0 flag materialization emits one csetm; fused compare-and-branch paths unchanged. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: jerry, workspace jerry-csetm.
