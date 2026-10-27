---
title: Inline constant-divisor / and mod at tier 0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T20:21:25.352521+02:00"
closed-at: "2026-10-27T21:23:56.355941+02:00"
close-reason: "Landed: / and mod by a literal in 1..65535 compile to movz+sdiv (+msub for mod) at tier 0, two literals fold; zero, wider or register divisors keep the call. / and mod join DEF-OP-KEYWORDS, so under the seal they are refused as package names on both targets like every op row. Alone: tender 309.0G -> 283.5G (-8.3%), zz 14.21G -> 14.16G. Full suite 501/501 and build-fixpoint-test ok on the integrated engine."
---

Tier 0 calls the baked / (6 insns: two pops, cbnz zero test, sdiv, push, ret) and mod (8) even when the divisor is a known nonzero literal; tier 1 inlines mov #c; sdiv and sdiv; msub. In tender HANDLE-ENTRY (n HANDLES-PER-BLOCK / and mod, divisor 256) this is 20 insns per call, 22.4G, the largest single tender cause. Acceptance: a / or mod whose divisor is a nonzero literal on the virtual stack emits sdiv (and msub for mod) on registers with no call and no zero test; a zero literal or a register divisor keeps today's call. Power-of-two shifts are not part of this dot (neither tier does them; truncating division needs a sign fixup). Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: jerry, workspace jerry-divi.
