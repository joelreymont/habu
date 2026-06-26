---
title: "Maki: LLM-target eval harness + matrix"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.994684+02:00"
blocks:
  - habu-write-docs-maki-966162de
---

D. Implement BOTH evals per docs/maki/eval.md: (i) kernel-authoring matrix (Habu-PTX vs raw Triton; pass@k, repair-rounds, tokens-to-green, GB/s) and (ii) the maki model train/eval. Orchestrator is Habu-native; any external runner is a NAMED tested host-glue boundary tracked by a retire-it dot. Needs on-device CUDA (M1d) for GB/s + correctness. The thesis (checked kernels + verified gradients as a better LLM target) gets NO claim until this matrix produces the data.
- Files: maki/eval/ (Habu-native harness) + tracked host-glue boundary.
- Verify: the matrix runs end-to-end and emits the metrics; the better-target claim is gated behind the produced data.
- Dep: docs/maki/eval.md (habu-write-docs-maki-966162de) + maki ONNX + maki training + M9 bench (GB/s) + M11 + M1d device.
