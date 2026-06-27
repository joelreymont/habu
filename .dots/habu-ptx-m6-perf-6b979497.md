---
title: "PTX M6 perf: warp-shfl block reduction"
status: open
priority: 3
issue-type: task
created-at: "2026-06-27T10:55:04.731339+02:00"
---

BLOCK-MAX/BLOCK-SUM in lib/ptx/cg-collective.f use a correct but O(B) thread-0 sequential fold over shared memory. Replace with the full-warp membermask shfl.down + per-warp shared staging + final warp reduce for GB/s. Keep the identity-seeded-inactive-lane invariant (EMIT-ROW-LOAD seeds -inf). Verify correct-vs-golden via tools/ptx/softmax-launch.f stays green and measure bandwidth for the step-5 eval matrix.
