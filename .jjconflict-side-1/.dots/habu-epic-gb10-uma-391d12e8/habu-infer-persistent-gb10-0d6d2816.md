---
title: "Infer: persistent GB10 executor"
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:40:05.130579+02:00"
blocks:
  - habu-match-gpt-2-2e478d93
---

Campaign only; do not dispatch. Build one persistent DGX Spark device path directly from the pinned GPT-2 assets and committed GPT2-REFERENCE outputs. Use the probed GB10 target sm_121a, canonical CUDA owner, sole device KV descriptor, and vector-paged DECODE-CG kernel. Delete the Maki CUDA wrappers; open one session; authenticate and stage the pinned safetensors file directly into session-owned weights; compile exact modules once; allocate workspaces once; and reuse one block plan. No GPT2LOAD or WSTORE product input, pack, second CUDA scope, host weight model, per-region process, per-token allocation or upload, fixed cap, unrolled graph, duplicate attention, second catalog, contiguous product cache, or compatibility target. Close when paged GPT-2 probes and logits match GPT2-REFERENCE and every CUDA, module, weight, workspace, and source owner releases on success and failure.
