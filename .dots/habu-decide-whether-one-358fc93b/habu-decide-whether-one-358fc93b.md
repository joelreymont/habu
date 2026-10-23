---
title: Decide whether one pass can replace tier 1
status: open
priority: 1
issue-type: task
created-at: "2026-10-23T18:58:18.936516+03:00"
---

Joel (2026-10-23): can ARM64 tier 0 emit code as fast as tier 1 without the IR, register allocator and optimizing passes? The optimizing layer was added on a model's claim that an IR is the way to fast code; no benchmark backed it. Evidence so far (2026-10-23, engine at intel dfa5151b, ~/.cache/tmp/jerry-tiers/): tier 0 compiles a trivial definition in 36 us, tier 1 in 640 us; tier-1 code ran 1.45x (FIB), 9.7x (SUMSQ) and 3.6x (MIX) faster. In those listings every tier-1 gain is single-pass-capable: primitives inlined (tier 0 calls them), stack values in registers, ?do index and limit in registers (tier 0 reloads them from a memory loop stack, 5 instructions per i), compare fused with an immediate; tier 1 hoists nothing and leaves redundant movs. Not single-pass: the counted-loop fold (loop.f), whole-word residency and register allocation. Until this is settled ARM64 keeps both tiers (docs/architecture.md). Acceptance: measured answers on the real corpora (habu, maki, zen, tender, etch, dots, zzusage, kiba, blackboard, loom) from the children below, and Joel's ruling recorded in docs/architecture.md. Claim: jerry.
