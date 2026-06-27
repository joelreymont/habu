---
title: Automatic AGGRESSIVE fusion (the beat-Triton lever)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T16:38:43.909898+02:00"
blocks:
  - habu-automatic-op-fusion-329aac27
---

Where checked Habu can BEAT Triton (not just match): fuse MORE than a Triton author would hand-write, automatically + proven. habu-automatic-op-fusion does elementwise chains; AGGRESSIVE fusion extends concatenation across the boundaries Triton users usually split at: (1) fuse elementwise PROLOGUE/EPILOGUE into GEMM/attention (bias+activation after matmul, scale before) so the intermediate never round-trips; (2) fuse across REDUCTIONS where safe (e.g. layernorm = reduce+normalize+affine in one kernel; softmax+matmul = the attention fusion); (3) whole-GRAPH fusion-boundary selection over a maki/ONNX model - greedily concatenate maximal fusible regions, with the checker proving each fused region's effect equals the unfused subgraph (a SAFETY a hand-fusing Triton author lacks). Win condition vs Triton: on a multi-op workload, Habu-auto-aggressively-fused moves strictly LESS global memory than hand-fused Triton that stopped at conventional kernel boundaries -> measured GB/s win, proven correct. Build on maki/fusion.f (concatenation = fusion) + the fusibility analysis (elementwise/same-shape fuse; shape-change/reduction are typed barriers unless the reduction-fusion rule applies). VERIFY: a fused (matmul->bias->relu) does 1 epilogue pass vs Triton's separate elementwise kernel; a fused layernorm beats a 3-kernel layernorm; device-correct; GB/s strictly above the split baseline. Deps: habu-automatic-op-fusion.
