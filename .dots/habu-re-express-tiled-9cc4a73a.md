---
title: "Re-express tiled GEMM as a checked KERNEL: body"
status: open
priority: 2
issue-type: task
created-at: "\"2026-06-27T18:10:55.997993+02:00\""
blocks:
  - habu-typed-pipelined-register-4d20acb5
---

Sub-dot of habu-checker-capability-typed (d). Once (b) shared-mem tile + (c) accumulator land (a=TILE-LOOP done), re-write lib/ptx/cg-matmul.f EMIT-MATMUL as a checked KERNEL: MM body composed from typed tile words (LOAD/STAGE/SMEM-LOAD/*. /+. /TILE-LOOP/STORE) instead of raw PTX, DELETE the unchecked-boundary note, and prove: certifies, emits equivalent PTX, stays device-golden (tools/ptx/matmul-device-test.f C[0][0]=64.0). Dep: blocked-by (b) and (c).

NOT READY 2026-07-16 (mmreexpr lane, honest BLOCKED, no edits): the premise
'capabilities (b)+(c) landed' holds only for the NAIVE tiling. EMIT-MATMUL is
the register-blocked cp.async double-buffered vec4-shared kernel; the landed
tile words cannot express it (evidence + golden anatomy in
habu-typed-pipelined-register-4d20acb5, which now blocks this dot). Spec
corrections: the device golden is tools/ptx/device-gold.f GEMM-GOLDEN (the
spec's matmul-device-test.f does not exist); a naive tile re-expression must
NOT be accepted as closure (perf -3.6x, breaks gemm-bench capture and the
byte-sensitive lower-mm.f/cg-mma.f verbatim scaffold reuse). Claim released.
