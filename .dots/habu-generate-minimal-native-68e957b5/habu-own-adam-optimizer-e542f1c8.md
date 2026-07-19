---
title: Own Adam optimizer state
status: open
priority: 2
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T23:36:07.823435+02:00"
---

Adam bias-correction state is hand-coded four times: maki/adam-train.f, layernorm-affine-test.f, layernorm-affine-op-test.f and new pos-embed-test.f each define mutable beta-power cells plus reset/tick/correction helpers. optim-tensor.f owns OPTIM:TT-ADAM! but forces every caller to rebuild state and pass learning rate, beta1, beta2, epsilon and two correction scalars on every update. TP-T@ and production ADAM-T are dead step counters: they are reset/incremented but never read. This duplicates code/state, permits policy drift and makes independent optimizers depend on ambient globals. Extend package OPTIM with STRUCTURE adam-config, adam-state and parameter-view after unified lowering; initialize and thread state explicitly, derive bias corrections internally, and update any number of parameter views under one step without global cells. Do not include a step field unless the algorithm consumes or reports it. Migrate all four copies and remove their prefixes/dead cells without aliases. Preserve exact update streams and deterministic loss goldens. Add first/many-step external reference snapshots, two configs, interleaved optimizers, multi-parameter same-step behavior, reset/retry, invalid hyperparameters, state/value swap negatives and before/after source/definition/JIT/DATA/runtime measurements. Files: maki/optim-tensor.f, trainers and focused tests. Block STRUCTURE work on habu-lowering-hash-unified-586f7881; checkpoint persistence remains its existing owner.
