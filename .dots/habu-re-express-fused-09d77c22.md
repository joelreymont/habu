---
title: "Re-express fused attention as a checked KERNEL: body"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T18:10:56.003236+02:00"
---

Sub-dot of habu-checker-capability-typed (e). Re-write lib/ptx/cg-attention.f EMIT-ATTN as a checked KERNEL: ATTN body using TILE-LOOP (online-softmax streaming reduction) + shared tiles + accumulator, delete the unchecked boundary, prove certifies + emits equivalent PTX + device-golden (tools/ptx/attention-device-test.f O[0]=1.0). Dep: blocked-by (b),(c); relates to flash-attention-optimization.
