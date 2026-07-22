---
title: "Infer dense: pin product checkpoint"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.429028+02:00"
blocks:
  - habu-infer-m0-pinned-17b6e648
---

Why this exists:
M5 cannot be implemented against an unspecified 7-8B family.

Required result:
select one public dense decoder checkpoint satisfying GQA, RoPE, RMSNorm, SwiGLU, supported tokenizer, conventional MLP, and published reference; record exact revision and file digests plus the supported context and license.

Done when:
the contract names every required architecture field and artifact; no fallback family or moving revision is permitted.

Expected touch points: docs/inference-support.md and pinned artifact manifest.
Smallest check: Habu manifest validator against downloaded artifacts.
Prerequisites: M0 baseline contract.
Owned result: first product checkpoint identity only.
Claim: unassigned.
