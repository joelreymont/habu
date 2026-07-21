---
title: "Infer: end-to-end single-sequence engine"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T15:58:37.094041+02:00"
blocks:
  - habu-infer-fused-decode-77f72ca7
  - habu-infer-safetensors-loader-0b58e06a
  - habu-infer-gpt-2-412c6f04
  - habu-infer-sampling-ops-d9c456f7
  - habu-infer-paged-kv-53b72853
---

The phase-2 flagship gate: prompt in, tokens out, on the GB10. Wire the landed pieces end to end: GPT-2 BPE tokenizer (real vocab) -> safetensors weights (residency per the measured policy) -> prefill (composed path is fine at this milestone) -> the fused decode kernel appending into the paged KV cache -> sampling (greedy + the sampling module) -> detokenize. Acceptance: greedy continuation of fixed prompts matches the host-forward reference ids EXACTLY for 64+ tokens; run-twice bit-identical; measured tokens/sec recorded (decode steady-state, quiet box, the timing lane) as the number all later phases (batching, NVFP4) improve. Blockers: the decode kernel (primary), plus the loader, GPT-2 forward, sampling dots - wired in frontmatter.
