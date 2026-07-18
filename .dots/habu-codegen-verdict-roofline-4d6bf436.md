---
title: "Codegen verdict: roofline + SASS audit on GB10"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:07:54.449242+02:00"
---

Settle 'the code generator sucks' with numbers (goal dot: codegen verdict protocol). On spark: (a) run mem-bound kernel set vs measured GB10 DRAM ceiling; (b) blocked GEMM fp32 vs CUDA-core fp32 roofline (docs/kernel-principles.md method); (c) cuobjdump -sass audit of our PTX after ptxas: register pressure, dual-issue/scheduling quality, obvious waste (redundant moves, spills). Deliverable: verdict table + named deficiencies with SASS evidence; routes habu-ptx-opt-layer-325b9507 onto/off the critical path. Needs the infra wave's admissible gate NOT required — kernels run standalone via tools/ptx/bench.f.
