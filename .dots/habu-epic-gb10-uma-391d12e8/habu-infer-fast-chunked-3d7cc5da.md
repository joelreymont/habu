---
title: "Infer: fast + chunked prefill"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:44:54.208511+02:00"
blocks:
  - habu-infer-modern-dense-b4d4aea1
---

Plan-of-record M7 prefill half: decode-only optimization is insufficient for RAG/agents/long prompts. Fast contiguous/tiled prefill attention path (the existing cg-attention machinery is the seed); CHUNKED prefill with bounded chunk size so long prompts never starve running decodes; same KV page layout as decode; measured at 1K/4K/16K+ regimes. Blocked on the modern-dense dot for real shapes - frontmatter.
