---
title: Drop the tier 0 frame when the only call is the tail
status: closed
priority: 3
issue-type: task
created-at: "2026-10-27T20:21:25.369579+02:00"
closed-at: "2026-10-27T22:36:38.609208+02:00"
close-reason: "Landed: a tier 0 body whose only call is the tail drops its frame. Bench alone vs base: zz 13.799G -> 13.765G, tender 282.33G -> 281.17G run instructions."
---

zz FILE-BUF: tier 0 saves and restores x30 around a body whose only call is the final one, compiled as a tail branch to DYNAMIC-STORAGE:ADDRESS (13 vs 11 insns per call, 0.33G, all of zz's frame category). FRAME-CELL bit 0 is set by the call emitter before the call becomes a tail branch. Acceptance: a body whose only call becomes the tail branch gets the leaf treatment (no x30 save, no restore); a body with any other call keeps the frame; recurse and quotations unchanged. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: jerry, workspace jerry-tailframe.
