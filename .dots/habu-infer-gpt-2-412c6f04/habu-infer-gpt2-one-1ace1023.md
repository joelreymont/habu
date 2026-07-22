---
title: "Infer GPT2: one transformer block"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:40:20.842956+02:00"
blocks:
  - habu-infer-gpt2-checked-54b99423
---

Why this exists:
real-weight GPT-2 needs a host reference block composed from landed LayerNorm, attention, projection, GELU, MLP, and residual operations at full dimensions.

Required result:
implement one inference-only checked block over typed spans with no hidden allocation. Training and automatic differentiation are deferred beyond the first release.

Done when:
selected internal checkpoints match the committed high-precision reference for one pinned block and prompt; wrong layer binding and workspace geometry reject.

Expected touch points: maki/infer/gpt2.f or new maki/infer/gpt2-block.f, focused test and reference fixture.
Smallest check: focused block parity test.
Prerequisites: checked model geometry.
Owned result: single host transformer block only.
Claim: unassigned.
