---
title: Reorder cp.async issue after the compute burst
status: closed
priority: 1
issue-type: task
created-at: "2026-07-19T15:48:31.409479+02:00"
closed-at: "2026-07-19T17:17:51.601152+02:00"
---

THE 512-class lever, from the ring scout's proven verdict (habu-scout-triton-s-c62d1230, artifacts in the session scratchpad sass/): move next-tile cp.async issue from before the compute burst to after it, so ptxas hoists the loads into the tensor-core stall shadow. Exact changes the scout named: (1) MMA-PIPE-KLOOP-MULTI (cg-mma.f:1065) steady body becomes CPP-WAIT; CPP-SYNC; MMA-KTILE-DISPATCH; CPP-SYNC; MMA-CP-STAGE; CPP-COMMIT; ring-advance - and the steady wait literal changes from stages-1 to stages-2 (in-flight count at loop top drops by one with deferred issue); re-derive the epilogue drain literals for the new accounting. (2) MMA-PIPE-KLOOP-WITH (cg-mma.f:997), the SHIPPING s1/s2 winner path, gets the same reorder - this is where the head-to-head payoff lands. CORRECTNESS FIRST - the scout's transplant was a scheduling demo only: the new wait accounting must pass every mma-gemm-check row element-exact (all dtypes, both warp grids, epilogue combos) before any timing; byte-identity is NOT expected (state so in the emit-diff notes). Then doc-protocol timing under the pinned 13.3: predicted ~4.5% on deep stages, s1/s2 winners gain at all shapes, and depth becomes productive again - re-sweep stages 3-5 at 512 where occupancy does not bind (32 blocks; Triton wins 21.7 vs our 14.5 there today). Secondary follow-on recorded, NOT this dot: uniform-datapath ring addressing. SERIALIZE behind the wide-BN round-8 lane - same file.
