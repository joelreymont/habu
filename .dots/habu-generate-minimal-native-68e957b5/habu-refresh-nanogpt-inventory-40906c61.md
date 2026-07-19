---
title: Refresh nanoGPT inventory
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T23:38:18.830019+02:00"
---

docs/nanogpt-inventory.md:46-67 still classifies positional embedding, affine LayerNorm, MHA/causal masking, cross-entropy, tokenizer and batch design as absent or partial after their feature dots closed or changed state. The active GPT composition cites this inventory as architecture input, so stale rows can hide incomplete implementation behind old gaps or dispatch work that already landed. Rebuild every inventory row from current code and dot status, separating prototype golden, host production, trainable AD, batched semantics, device lowering and measured performance instead of one binary present/absent label. Link each incomplete dimension to its exact live owner, including corrective audit dots, and mark closed prototype claims that destruction review narrowed. Add a checked inventory-consistency lint derived from exact module/dot identifiers so closed/open changes cannot silently rot prose; fail on unknown, duplicate or status-mismatched owners. Preserve historical decisions in dated notes rather than current rows. Verify docs generation, owner links, filemap/dot/full gates. Files: docs/nanogpt-inventory.md, its checked generator/lint and owning gate.
