---
title: Hoist and unroll the small loops
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T09:41:37.378317+02:00"
---

Loop-invariant code motion and bounded unrolling on the frozen IR: hoist pure computations and loads the memory order proves invariant out of loop bodies (the schema effect flags + the existing memory-order machinery decide legality); unroll counted loops whose trip count is a small literal or whose body is under a derived size bound (derive the bound from the I-cache line economics the placement doc measured, not a guess). The corpus's byte loops and the workload's scan shapes are the witnesses. Acceptance: measured against the clang column per row; answers identical; the register pressure interaction with unrolling is held by the existing pool floors (an unroll that would spill is declined, stated in the pass).

Blocked by: habu-epic-hard-cut-a684f24d phases 1-6. Re-scoped: LICM only when typed memory/effect facts prove invariance and alias safety; unroll only when the measured size/runtime tradeoff is favorable and the allocator accepts the pressure, threshold derived from target cache/sequence costs.

UNBLOCKED (2026-08-05, user order): the optimization program proceeds NOW on the chain as it stands — the hard cut continues in parallel and is no longer a prerequisite. Standing acceptance for every optimization lane: name the corpus rows expected to improve BEFORE implementing; show the emitted instruction delta on them; every oracle answer preserved bit-for-bit incl. NaN; report BOTH gaps per touched row (chain-vs-clang closed, chain-vs-own-baseline gained) from tools/codegen-compare.f; re-pin the chain baseline with --update-chain only after the report is read; no regression on untouched rows outside a stated multi-objective trade. New instruction forms (madd/msub, ldp/stp, bitfields, ccmp, NEON) require Rocq rows in formal/Common/Insn.v with enc/wf/roundtrip before the emitter uses them — the CG-02 discipline, applied per-lane not deferred.
