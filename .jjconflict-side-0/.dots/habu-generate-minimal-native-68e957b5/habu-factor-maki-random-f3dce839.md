---
title: Factor Maki random generator
status: open
priority: 2
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T23:10:43.559598+02:00"
---

maki/xent-train.f:64-73 adds a third independently compiled Numerical Recipes LCG with constants 1664525/1013904223, mask, mutable state, next and unit conversion; from-scratch-model.f:57-71 and batch-loader.f:34-49 already carry equivalent copies. All three load together under maki/test.f, so this duplicates source, JIT and mutable state and invites stream-policy drift. Create one real RNG package with short API and explicit per-consumer state rather than a package-global variable. After unified lowering, use a package-owned lcg STRUCTURE containing only u32 state; NEXT-U32/UNIT/SIGNED-UNIT consume and return it explicitly so trainers/loaders own independent seeds and cannot cross-contaminate. Do not add an algorithm tag until a second algorithm actually shares the API. Preserve every existing bit stream and numerical golden, then make the Gaussian weight-init owner compose Box-Muller over the same lcg. Remove all three constant/helper copies and aliases. Add exact first-N stream snapshots per historical seed, interleaved independent generators, wraparound, deterministic retry, checker negatives for state/value swaps, and before/after source definitions/JIT/DATA/CODELEN and throughput. Files: one RNG library/test plus the three Maki consumers; coordinate habu-nanogpt-weight-init-b2fc5b4f. Block STRUCTURE work on habu-lowering-hash-unified-586f7881.
