---
title: Compose tied wte/LM-head into the block
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T22:41:31.061036+02:00\""
---

Integrate the shared wte/LM-head parameter with summed gradients INSIDE the block composition (gptblock-attn-test.f binds wte slot 0 and wlm slot 12 as independent buffers, separately Adam-updated at :172/:178) - not a standalone trainer. The tying mechanism with summed grads landed 482af1a6 (recovered orphan); consume it. Prove: one storage, both gradient contributions accumulated, gradcheck on the tied parameter, training still reduces loss, run-twice locked.

Claim: agent=tiewte workspace=.jj-ws/fable-tiewte machine=spark (owns the tie-in-block change to maki/examples/nanogpt/gptblock-attn-test.f + consumed tie machinery; NOTE the external-golden lane concurrently edits the same test file - orchestrator hand-merges)
