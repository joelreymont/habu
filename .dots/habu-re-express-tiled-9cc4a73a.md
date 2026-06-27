---
title: "Re-express tiled GEMM as a checked KERNEL: body"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T18:10:55.997993+02:00"
---

Sub-dot of habu-checker-capability-typed (d). Once (b) shared-mem tile + (c) accumulator land (a=TILE-LOOP done), re-write lib/ptx/cg-matmul.f EMIT-MATMUL as a checked KERNEL: MM body composed from typed tile words (LOAD/STAGE/SMEM-LOAD/*. /+. /TILE-LOOP/STORE) instead of raw PTX, DELETE the unchecked-boundary note, and prove: certifies, emits equivalent PTX, stays device-golden (tools/ptx/matmul-device-test.f C[0][0]=64.0). Dep: blocked-by (b) and (c).
