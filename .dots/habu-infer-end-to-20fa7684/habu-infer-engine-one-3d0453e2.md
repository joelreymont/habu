---
title: "Infer engine: one paged decode step"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.406908+02:00"
blocks:
  - habu-infer-engine-prefill-558f9003
  - habu-infer-decode-paged-66b6a16d
---

Why this exists:
the core runtime needs one transaction that consumes the current token, publishes the next KV state, runs paged attention, and returns logits without partial advancement.

Required result:
preflight append and kernel geometry, execute the model step, and commit sequence length only when the step succeeds.

Done when:
successful steps match the GPT-2 oracle; injected append, kernel, and projection failures leave the prior sequence valid and leak-free.

Expected touch points: new maki/infer/engine-decode.f, focused test, FILEMAP.md.
Smallest check: focused decode-step test and correctness-only GB10 parity.
Prerequisites: prefill into paged KV and paged real-model parity.
Owned result: one decode-step transaction only.
Claim: unassigned.
