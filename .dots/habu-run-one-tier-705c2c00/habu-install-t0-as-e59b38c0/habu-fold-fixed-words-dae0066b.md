---
title: Fold fixed words into T0 bodies
status: active
priority: 1
issue-type: task
created-at: "2026-10-29T04:43:33.346249+02:00"
---

Workspace: jerry-t0b on nspsunws. A5 profile: T0 calls a constant, variable or created word where the JIT and tier 1 fold its value (tier 1 HIR-WORD:RESOLVE-FIXED / EMIT-FIXED elaborate.f 880-887, 2313; JIT C-RUN-FIXED habu2.f ~3113). zz LF loop: (7+3+6) insns x 168.6M passes = 2.70G of the 9.03G run gap. Push the value as a constant (an address with its address-site row).
