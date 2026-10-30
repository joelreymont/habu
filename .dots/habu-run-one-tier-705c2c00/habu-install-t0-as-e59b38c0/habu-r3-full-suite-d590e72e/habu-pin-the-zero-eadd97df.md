---
title: Pin the zero-verdict rows to the JIT
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T09:19:36.027998+02:00"
closed-at: "2026-10-31T12:11:58.119022+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510, two generations converge, edit matrix F53/F105/F156 only, as master"
---

Workspace jerry-zjit. Fable feature review finding 2: lkkszsrswltv deleted test/compiler/native-hookless-reject.f's tier-0 zero-verdict rows (ZERO-TICK, ZERO-SHAPE, ZERO-CALL, ZERO-AMBIGUOUS-CALL/TICK, ZERO-USED, ZERO-USED-CAUGHT, ZERO-REDEFINE tier-0 branches) instead of pinning them to the JIT (t0-rows-rule rule 1). The JIT stays selectable; its drop path (habu2.f EM-COMPILE-PUBLISH-HOOKED) lost its coverage.

Review 2026-10-31: LAND. Pending before landing: the comment above SHADOWED in test/compiler/native-hookless-reject.f says the global and the used P-FORGET:W-FORGET make a bare W-FORGET ambiguous; the refusal is E-USING-SHADOW-GLOBAL (a used public shadows a global). Correct it in skpvotuw once lane t0fc (on top) is merged.
