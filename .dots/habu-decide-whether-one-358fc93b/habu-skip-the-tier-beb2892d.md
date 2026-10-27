---
title: Skip the tier 0 leaf entry nop at call sites
status: active
priority: 2
issue-type: task
created-at: "2026-10-27T20:21:25.360695+02:00"
---

A tier 0 leaf body keeps a nop in its entry slot (EM-COMPILE-RET rewrites the link-register save in place so recorded addresses stay valid, src/habu/habu2.f ~3588-3620), and every call executes it: tender +6.8G (HANDLE-INDEX 7 vs 6, LOW@ 8 vs 7, VECTOR, USED, DOC-GENERATION). Acceptance: a direct call to a finished tier 0 leaf whose entry slot is the nop branches to entry+4; xt calls, execute and any address a record holds are unchanged. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: jerry, workspace jerry-nop.
