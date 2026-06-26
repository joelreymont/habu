---
title: "PTX M11: attention + LLM experiment"
status: open
priority: 3
issue-type: task
created-at: "2026-06-25T13:43:16.950005+02:00"
blocks:
  - habu-ptx-m10-vectorization-f394cfe1
---

docs/ptx-sketch.md M11. Multi-tile rows, matmul/attention tile IR, shared staging + accumulator policy -> fused softmax->flash-attention (fp16/bf16). Then the LLM matrix (Habu-PTX vs Triton; pass@k, repair rounds, tokens-to-green, GB/s); no best-target claim until it validates.
